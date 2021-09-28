## LSOA population manipulation

## based on latest ONS data - 2021 MYE, single year of age

##  https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates
#  SAPE23DT2

## get data - use person estimates

library(readxl)
library(data.table)
library(tidyverse)

temp <- tempfile()
lsoa_data <- curl::curl_download("https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2flowersuperoutputareamidyearpopulationestimates%2fmid2020sape23dt2/sape23dt2mid2020lsoasyoaestimatesunformatted.xlsx", temp)

lsoa_df <- readxl::excel_sheets(lsoa_data)

lsoa_df_1 <- readxl::read_xlsx(lsoa_data, sheet = "Mid-2020 Persons", skip = 4)

lsoa_df_1 %>%
  glimpse()

## convert to long format and save as rds

lsoa_df_long <- lsoa_df_1 %>%
  pivot_longer(names_to = "age", values_to = "population", cols = 8:ncol(.)) 

lsoa_df_long %>%
  saveRDS("data/lsoa_data.rds")

lsoa_0_4 <- lsoa_df_long %>%
  janitor::clean_names() %>%
  group_by(lsoa_code) %>%
  mutate(lsoa_less_than_5 = sum(population[1]:population[4])) %>%
  ungroup() %>%
  filter(age %in% c("0", "1", "2", "3", "4"))

lsoa_0_4 %>%
  group_by(la_name_2021_boundaries) %>%
  summarise(la_0_4 = sum(population))

#### Deprivation scores

lsoa_dep <- data.table::fread("https://opendatacommunities.org/downloads/cube-table?uri=http%3A%2F%2Fopendatacommunities.org%2Fdata%2Fsocietal-wellbeing%2Fimd2019%2Findices")

lsoa_dep_imd <- lsoa_dep[Measurement == "Score" & `Indices of Deprivation` == "a. Index of Multiple Deprivation (IMD)",]

lsoa_dep_imd %>%
  write_rds("data/lsoa_imd.rds")


lsoa_dep_imd %>%
  ggplot(aes(Value)) +
  geom_density()

lsoa_dep_imd %>%
  slice_max(Value)

### join data - add 2019 IMD scores

lsoa_df_long <- lsoa_df_long %>%
  janitor::clean_names()

lsoa_df_long_imd <- lsoa_df_long %>%
  left_join(lsoa_dep_imd, by = c("lsoa_code" = "FeatureCode"))

lsoa_df_long_imd %>%
  write_rds("data/lsoa_pop_imd.rds")
