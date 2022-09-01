#Web scraping de lh'istorique des températures à Paris

library(janitor)
library(rvest)
library(dplyr)

# Url
base_url = 'https://www.historique-meteo.net/france/ile-de-france/paris/'

paris_weather = data.frame()

# boucle de web scrap
for (y in 2010:2022){
  
  for (m in 1:12){
    
    message(paste0(y," - ", m))
    
    if(m<10){
      m = paste0('0',m)
    }
    
    my_url <-paste0(
      base_url, y, '/', m, "/#jour"
    )
    
    month_weath <- my_url %>%
      read_html() %>%
      html_nodes('.col-lg-8') %>%
      html_table()
    
    table = as.data.frame(month_weath[1])
    
    if("X6" %in% colnames(table)){
      table_filter = table %>% 
        filter(is.na(X6) == T) %>% 
        mutate(date = lubridate::as_date(X1, format= '%d/%m/%Y'),
               temp = X3) %>% 
        filter(is.na(date) == F) %>% 
        select(date, temp) %>% 
        mutate(temp_min = as.numeric(str_match(temp, "Températures : \\s*(.*?)\\s*°C/")[,2]),
               temp_max = as.numeric(str_match(temp, "°C/\\s*(.*?)\\s*°CPrécipitations")[,2]),
               temp = (temp_min + temp_max)/2)
    }else{
      table_filter = table %>% 
        filter(is.na(X5) == F) %>% 
        mutate(date = lubridate::as_date(X1, format= '%d/%m/%Y'),
               temp = X3) %>% 
        filter(is.na(date) == F) %>% 
        select(date, temp) %>% 
        mutate(temp_min = as.numeric(str_match(temp, "Températures : \\s*(.*?)\\s*°C/")[,2]),
               temp_max = as.numeric(str_match(temp, "°C/\\s*(.*?)\\s*°CPrécipitations")[,2]),
               temp = (temp_min + temp_max)/2)
    }
    
  
    paris_weather = rbind(paris_weather, table_filter)
      
  }
  
}

write.csv(paris_weather,here::here('data', 'paris_temp.csv'), row.names = FALSE)




