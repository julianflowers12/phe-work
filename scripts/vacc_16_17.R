library(data.table); library(tidyverse)

here::here("data")

vacc_all <- fread("https://api.daisy.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=newPeopleVaccinatedCompleteByVaccinationDate&metric=cumPeopleVaccinatedCompleteByVaccinationDate&format=csv")
vacc_age <- jsonlite::fromJSON("https://daisy.coronavirus.data.gov.uk/api/v1/data?filters=areaType=nation;areaName=England&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22vaccinationsAgeDemographics%22:%22vaccinationsAgeDemographics%22%7D&format=json")

vacc_age <- vacc_age$data |>
  unnest("vaccinationsAgeDemographics")

vacc_all %>%
  write.csv(paste0(here::here("data/"), "vacc.csv"))

vacc_age %>%
  write.csv(paste0(here::here("data/"), "vacc_age.csv"))

vacc_age_sum <- vacc_age %>% 
  group_by(date = as.Date(date)) %>%
  summarise(tot = sum(cumPeopleVaccinatedCompleteByVaccinationDate), 
            daily = sum(newPeopleVaccinatedCompleteByVaccinationDate))

vacc_all_1 <- vacc_all %>%
  mutate(date = as.Date(date)) %>%
  left_join(vacc_age_sum) %>%
  mutate(young = newPeopleVaccinatedCompleteByVaccinationDate - daily, 
         young1 = cumPeopleVaccinatedCompleteByVaccinationDate - tot)

vacc_all_1 %>%
  ggplot(aes(date, young1)) +
  geom_col()

