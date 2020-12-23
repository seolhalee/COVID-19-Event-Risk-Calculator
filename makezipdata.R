library(shinyMatrix)
library(sp)
library(sf)
library(shinythemes)
library(rgdal)  # for vector work; sp package should always load with rgdal. 
library(dplyr)
library(shiny)
library(lubridate)

getDataZip <- function() {
  
  data <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv", stringsAsFactors = FALSE )
  zipdata <- read.csv( "https://raw.githubusercontent.com/nychealth/coronavirus-data/master/latest/last7days-by-modzcta.csv", stringsAsFactors = FALSE) 
  #countytozip <- st_read('map_data/us_zip_to_county.shp', stringsAsFactors = F) %>% as.data.frame %>% dplyr::select(c('Zip', 'Longitude', 'Latitude', 'GEOID', 'NAME')) %>% mutate(GEOID = as.numeric(GEOID)) 
  #st_write(countytozip, 'map_data/us_zip_to_county.geojson')
  countytozip <- st_read('map_data/us_zip_to_county.geojson')
  pop <- read.csv('map_data/county-population2.csv', stringsAsFactors = FALSE)
  popnyc <- read.csv('map_data/ZipNYCPop.csv', stringsAsFactors = FALSE)
  
  cur_date <- ymd(gsub("-", "", Sys.Date()))-2 
  past_date <- ymd(cur_date) - 14
  data_cur <<- data %>% filter(date == cur_date) %>% 
    mutate(fips = case_when(
      county == "New York City" ~ 99999,
      TRUE ~ as.numeric(fips)
    )) %>%
    dplyr::select(c(fips, cases, deaths))
  data_past <<- data %>%
    filter(date == past_date) %>%
    mutate(fips = case_when(
      county == "New York City" ~ 99999,
      TRUE ~ as.numeric(fips)
    )) %>%
    dplyr::select(fips = fips, cases_past = cases)
  data_join <<- data_cur %>%
    inner_join(data_past, by = "fips") %>%
    inner_join(pop, by = "fips") %>% 
    mutate(Nr = (cases-cases_past))
  
  
  zipfinal <<- countytozip %>% left_join(data_join, by=c('GEOID'='fips')) %>% 
    left_join(zipdata %>% dplyr::select(c('modzcta', 'modzcta_name', 'people_positive')), by=c('Zip'='modzcta')) %>% 
    left_join(popnyc, by=c('Zip'='zip'), suffix = c('_county', '_zip')) %>%
    mutate(Nr = if_else(is.na(people_positive), Nr, people_positive)) %>%
    mutate(pnI = if_else(is.na(people_positive), (1-(Nr*4)*10/14/pop_county), (1-(people_positive)*4*10/7/pop_zip)))
  write.csv(zipfinal, 'map_data/zipfinal.csv')
}

getDataZip()