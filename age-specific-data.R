## age specific rates

library(tidyverse); library(data.table); library(jsonlite); library(lubridate)

case_data <- fread("https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=newCasesBySpecimenDateAgeDemographics&format=csv")

case_data[age %in% c('05_09', '10_14', '15_19', '20_24'),] %>%
  ggplot(aes(date, rollingRate, group = age, colour = age)) +
  geom_line() +
  geom_vline(xintercept = as.Date('2021-09-01')) +
  scale_x_date(breaks = 'month') +
  labs(title = 'Age specific COVID rates in school age children')


admission_data <- fromJSON('https://coronavirus.data.gov.uk/api/v1/data?filters=areaType=nation;areaName=England&latestBy=&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22cumAdmissionsByAge%22:%22cumAdmissionsByAge%22%7D&format=json')


admission_data$data %>%
  unnest('cumAdmissionsByAge') %>%
  filter(age == '6_to_17') %>%
  mutate(date = as.Date(date),
         daily = value - lead(value)) %>%
  ggplot(aes(date, daily)) +
  geom_col() +
  geom_smooth(method = 'gam') +
  scale_x_date(breaks = 'month') 

vacc_data <- fromJSON('https://coronavirus.data.gov.uk/api/v1/data?filters=areaType=nation;areaName=England&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22vaccinationsAgeDemographics%22:%22vaccinationsAgeDemographics%22%7D&format=json', 
                      simplifyDataFrame = TRUE)

vacc_data$data %>% 
  unnest('vaccinationsAgeDemographics') %>%
  janitor::clean_names() %>%
  filter(age %in% c('16_17', '18_24', '25_29')) %>%
  ggplot(aes(as.Date(date), cum_vaccination_complete_coverage_by_vaccination_date_percentage, group = age, color = age
          )) +
  geom_line()

vacc_data$data %>% 
  unnest('vaccinationsAgeDemographics') %>%
  janitor::clean_names() %>%
  filter(age %in% c('16_17', '18_24', '25_29'), date == max(as.Date(date))) %>%
  select(5, 15)
  

vacc_data$data %>%
  unnest('vaccinationsAgeDemographics') %>%
  janitor::clean_names() %>%
  filter(age %in% c('16_17', '18_24', '25_29')) %>%
  dplyr::select(age, vaccine_register_population_by_vaccination_date ) %>%
  distinct()


death_data <- fromJSON('https://coronavirus.data.gov.uk/api/v1/data?filters=areaType=nation;areaName=England&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newDeaths28DaysByDeathDateAgeDemographics%22:%22newDeaths28DaysByDeathDateAgeDemographics%22%7D&format=json')
death_data$data %>%
  unnest('newDeaths28DaysByDeathDateAgeDemographics') %>%
  janitor::clean_names() %>%             
  filter(age %in% c('00_04', '05_09', '10_14', '15_19', '20_24', '25_29')) %>%
  mutate(monthly = zoo::as.yearmon(date)) %>%
  group_by(age, monthly) %>%
  summarise(month_tot = sum(rolling_sum)) %>%
  ggplot(aes(monthly, month_tot, group = age, fill = age
  )) +
  geom_col() +
  facet_wrap(~age) +
  zoo::scale_x_yearmon()
