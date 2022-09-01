library(tidyverse)
library(modeltime)
library(tidymodels)
library(timetk)
library(lubridate)
library(ggblanket)
library(readr)

# import des données
ts_data <- read_csv("data/paris_temp.csv") %>% 
  select(date, temp)

# fomrat prophet
ts_data$ds = ts_data$date
ts_data$y = ts_data$temp

ts_data <- ts_data %>% 
  select(-c(date, temp))

# plot
ts_data %>% 
  plot_time_series(ds, y)

# split
split = time_series_split(
  ts_data,
  assess = "15 days",
  cumulative = TRUE
)

split %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(ds, y)


# split_bis = initial_time_split(ts_data,
#                                prop = 0.90)

# bind_rows(
#   training(split) %>% mutate(data = 'train'),
#   testing(split) %>% mutate(data = 'test'),
# ) %>% 
#   gg_line(ds, y,
#           col = data )
  
# pre process
recipe_spec <- recipe(y ~ ds, training(split)) %>%
  step_timeseries_signature(ds) %>%
  step_rm(contains("am.pm"), contains("hour"), contains("minute"),
          contains("second"), contains("xts")) %>%
  step_fourier(ds, period = 365, K = 5) %>%
  step_dummy(all_nominal()) 
  

table = recipe_spec %>% prep() %>% juice()


rolling_origin(ts_data,
               initial = 1600, # initial historique
               assess = 15, # prev de test
               skip = 120) %>% # skip 
  mutate(train = map(splits, analysis),
         test = map(splits, assessment)) %>%
  select(id, train, test) %>%
  pivot_longer(-id) %>%
  unnest(value) %>%
  filter(id %in% c("Slice01", "Slice04", "Slice09")) %>%
  gg_line(x = ds,
          y = y,
          col = name,
          facet = id)

ts_k_folds = rolling_origin(ts_data,
                            initial = 1600, # initial historique
                            assess = 15, # prev de test
                            skip = 120) 

# sliding_period(ts_data, ds, period = "month", lookback = 5L, assess_stop = 90) %>%
#   mutate(train = map(splits, analysis),
#          test = map(splits, assessment)) %>%
#   select(id, train, test) %>%
#   pivot_longer(-id) %>%
#   unnest(value) %>%
#   filter(id %in% c("Slice001", "Slice002", "Slice003")) %>%
#   gg_line(x = ds,
#           y = y,
#           col = name,
#           facet = id)

# 6 mois + a chaque cross
# ts_k_folds = sliding_period(data_ts, ds, period = "month", lookback = Inf, 
#                             assess_stop = 6)

# comparaison prophet vs prophet boost
tune_prophet = function(splits){
  
  train_data = analysis(splits)
  test_data = assessment(splits)
  
  splits_calib <- initial_time_split(train_data, prop = 0.85)
  
  model_spec_prophet_boost <- prophet_boost(
    prior_scale_changepoints = 0.01,
    prior_scale_seasonality = 5
  ) %>%
    set_engine("prophet_xgboost") 
  
  workflow_fit_prophet_boost <- workflow() %>%
    add_recipe(recipe_spec) %>%
    add_model(model_spec_prophet_boost) %>%
    fit(training(splits_calib))
  
  # calibration pour avoir l'intervalle de confiance
  calib_table = workflow_fit_prophet_boost %>%  
    modeltime_calibrate(testing(splits_calib))
  
  future_prophet_boost = calib_table %>% 
    modeltime_refit(train_data) %>% 
    modeltime_forecast(
      new_data = test_data,
      actual_data = train_data
    )
  
  m_prophet = prophet::prophet(df = train_data,
                               seasonality.mode = 'additive',
                               changepoint.prior.scale = 0.01,
                               seasonality.prior.scale = 5)
  
  future = prophet::make_future_dataframe(m_prophet, periods = nrow(test_data),
                                          freq = 'day', include_history = FALSE)
  
  bind_rows(predict(m_prophet, future) %>% select(ds, yhat) %>% mutate(type = 'prophet'),
            future_prophet_boost %>% filter(.model_desc != 'ACTUAL') %>% 
              select(ds = .index, yhat = .value) %>% mutate(type = 'prophet_xgb')) %>% 
    left_join(test_data, by = 'ds')
  
}

# test sur les 24 splits
ts_tune = ts_k_folds %>% mutate(rest = map(splits, tune_prophet))

ts_tune %>% 
  select(id, rest) %>% 
  unnest(rest) %>% 
  group_by(id, type) %>% 
  arrange(ds) %>% 
  mutate(prev = paste0('prev_', row_number())) %>% 
  ungroup() %>% 
  select(prev, type, ds, yhat, y) %>% 
  group_by(prev, type) %>% 
  summarise(mae = mean(abs(y-yhat)),
            mape = mean(abs(y-yhat)/y)) %>% 
  ungroup() %>% 
  pivot_longer(cols=c(mae:mape), names_to="perf") %>% 
  pivot_wider(names_from = type, values_from = value) %>%
  filter(perf == 'mae') %>% 
  gg_point(x = prophet,
           y = prophet_xgb) +
  geom_abline() + 
  coord_obs_pred()

ts_tune %>% 
  select(id, rest) %>% 
  unnest(rest) %>% 
  group_by(id, type) %>% 
  arrange(ds) %>% 
  mutate(prev = paste0('prev_', row_number())) %>% 
  ungroup() %>% 
  select(prev, type, ds, yhat, y) %>% 
  group_by(prev, type) %>% 
  summarise(mae = mean(abs(y-yhat))) %>% 
  ungroup() %>% 
  group_by(type) %>% 
  summarise(mae_mean = mean(mae, na.rm = T))

ts_tune %>% 
  select(id, rest) %>% 
  unnest(rest) %>% 
  group_by(id, type) %>% 
  arrange(ds) %>% 
  mutate(prev = paste0('prev_', row_number())) %>% 
  ungroup() %>% 
  mutate(mae = abs(y-yhat),
            mape = abs(y-yhat)/y,
         date = as.Date(ds)) %>% 
  gg_point(x = date,
          y = mae,
          y_breaks = scales::breaks_pretty(), 
          # y_labels = scales::label_percent(),
          facet = type,
          col = type)


model_arima = arima_reg() %>% 
  set_engine("auto_arima") %>% 
  fit(y~ds, training(split))

# prophe
model_prophet  = prophet_reg(
  prior_scale_changepoints = 0.01,
  prior_scale_seasonality = 5) %>% 
  set_engine("prophet") %>% 
  fit(y~ds, training(split))

# prophet xgb
model_prophet_xgb  = prophet_boost(prior_scale_changepoints = 0.01,
                                   prior_scale_seasonality = 5) %>%
  set_engine("prophet_xgboost") 
  
workflow_fit_prophet_boost <- workflow() %>%
    add_model(model_prophet_xgb) %>%
    add_recipe(recipe_spec) %>%
    fit(training(split))


# model
model_table = modeltime_table(
  model_arima,
  model_prophet,
  workflow_fit_prophet_boost
)

# calibrate
calib_table = model_table %>% 
  modeltime_calibrate(testing(split))

calib_table %>%  modeltime_accuracy()

# test
calib_table %>% 
  modeltime_forecast(
    new_data = testing(split),
    actual_data = ts_data,
    keep_data = T
  ) %>% 
  plot_modeltime_forecast()

#
future_forecast = calib_table %>% 
  modeltime_refit(ts_data) 

future_pred = future_forecast%>% 
  modeltime_forecast(
    h = '15 days',
    actual_data = ts_data,
    keep_data = T
  )

future_pred %>% plot_modeltime_forecast()

