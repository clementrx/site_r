# 1 lib import ==== 
library(tidyverse)
library(tidymodels)

# 2 import data ==== 
df = read_csv(file = here::here('data','aw_fb_data.csv'))

df = df[,-c(1:2)]

colnames(df)

df_aw = df %>% filter(device == 'apple watch')
df_fb = df %>% filter(device == 'fitbit')

# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/ZS2Z2J


set.seed(123)
data_split <- initial_split(df_aw, # updated data
                            strata = activity)

train_data <- training(data_split) 
test_data <- testing(data_split)

df_rec = recipe(activity ~ . , data = train_data) 
  # step_normalize(all_numeric(), -all_outcomes()) %>% 
  # step_dummy(all_nominal(), -all_outcomes()) %>%
  # step_zv(all_numeric(), -all_outcomes()) %>% 
  # step_corr(all_numeric_predictors(), threshold = 0.8) 
summary(df_rec) # comment on recoit le fichier origine

df_rec = recipe(activity ~ . , data = train_data) %>% 
  step_num2factor(gender,
                  transform = function(x) x + 1,
                  levels = c('femme', 'homme')) %>% 
  step_rm(device) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_numeric(), -gender_homme) %>% 
  step_zv(all_numeric()) %>% 
  step_corr(all_numeric_predictors(), threshold = 0.7) 

encoded = df_rec %>%
  prep() %>%
  juice()

str(train_data)
str(encoded)

rf_spec <- 
  rand_forest(trees = 200) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification")

activity_wflow <- 
  workflow() %>% 
  add_model(rf_spec) %>% 
  add_recipe(df_rec)

activty_fit <- 
  activity_wflow %>% 
  fit(data = train_data)

# p value pour reg lin
# activty_fit %>% 
#   extract_fit_parsnip() %>% 
#   tidy()

predict(activty_fit, test_data)

activ_aug <- 
  augment(activty_fit, test_data) # prediction prob (au lieu de predict(type = 'pro'))

activ_aug %>%
  select(activity, .pred_class, contains(".pred_"))

activ_aug %>% 
  select(-.pred_class) %>% 
  roc_curve(truth = as.factor(activity), contains(".pred_")) %>% 
  autoplot()

activ_aug %>% 
  select(-.pred_class) %>% 
  roc_auc(truth = as.factor(activity), contains(".pred_"))

activ_aug %>% 
  conf_mat(activity, .pred_class) %>% 
  autoplot(type = "heatmap")

activ_aug %>% 
  accuracy(truth = as.factor(activity), .pred_class) 


set.seed(100)
cv_folds <-
  vfold_cv(train_data, 
            v = 3, 
           strata = activity) 

rf_spec <- 
  rand_forest() %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification")

xgb_spec <- 
  boost_tree() %>% 
  set_engine("xgboost") %>% 
  set_mode("classification") 

knn_spec <- 
  nearest_neighbor(neighbors = 4) %>% # we can adjust the number of neighbors 
  set_engine("kknn") %>% 
  set_mode("classification") 

rf_wflow <-
  workflow() %>%
  add_recipe(df_rec) %>% 
  add_model(rf_spec) 

xgb_wflow <-
  workflow() %>%
  add_recipe(df_rec) %>% 
  add_model(xgb_spec)

knn_wflow <-
  workflow() %>%
  add_recipe(df_rec) %>% 
  add_model(knn_spec)

rf_res <- 
  rf_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      accuracy,roc_auc, sens, spec),
    control = control_resamples(
      save_pred = TRUE)
  ) 

xgb_spec <- 
  xgb_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      accuracy,roc_auc, sens, spec),
    control = control_resamples(
      save_pred = TRUE)
  ) 

knn_res <- 
  knn_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      accuracy,roc_auc, sens, spec),
    control = control_resamples(
      save_pred = TRUE)
  ) 

rf_metrics <- 
  rf_res %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "Random Forest")

xgb_metrics <- 
  xgb_spec %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "XGB")

knn_metrics <- 
  knn_res %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "KNN")

model_compare <- bind_rows(
  rf_metrics,
  xgb_metrics,
  knn_metrics
) 

model_comp <- 
  model_compare %>% 
  select(model, .metric, mean, std_err) %>% 
  pivot_wider(names_from = .metric, values_from = c(mean, std_err)) 


model_comp %>% 
  arrange(mean_accuracy) %>% 
  mutate(model = fct_reorder(model, mean_accuracy)) %>% # order results
  ggplot(aes(model, mean_accuracy, fill=model)) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") +
  geom_text(
    size = 3,
    aes(label = round(mean_accuracy, 1), y = mean_accuracy + 0.08),
    vjust = 1
  )

last_fit_rf <- last_fit(rf_wflow, 
                        split = data_split,
                        metrics = metric_set(
                          recall, precision, f_meas, 
                          accuracy, kap,
                          roc_auc, sens, spec)
)

last_fit_rf %>% 
  collect_metrics()

library(vip)

last_fit_rf %>% 
  pluck(".workflow", 1) %>%   
  pull_workflow_fit() %>% 
  vip(num_features = 10)

last_fit_rf %>%
  collect_predictions() %>% 
  conf_mat(class, .pred_class) %>% 
  autoplot(type = "heatmap")

last_fit_rf %>% 
  collect_predictions() %>% 
  roc_curve(class, .pred_A, .pred_B,.pred_C,.pred_D,) %>% 
  autoplot()


rf_fit_workflow <- fit(rf_wflow, train_data)

df_results <- test_data %>%  
  bind_cols(
predict(rf_fit_workflow, test_data, type = 'prob'))




