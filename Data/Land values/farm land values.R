library(tidyverse)

counties.regions <- read_csv("Data/Join docs/county_regions.csv") %>%
  mutate(countyfp = formatC(countyfp, width = 3, flag = "0"),
         Name = str_to_title(Name),
         Name = str_replace(Name, "Q", "q"),
         Name = str_replace(Name, "Of The", "of the"))

# prepare land objects ----------------------------------------------------

land.values.2000.2008 <- read.csv("Data/Agriculture/Land values/ag land value 2000 - 2008, land econ.csv", header = TRUE) %>%
  mutate(County = str_replace(County, "McLeod", "Mcleod"),
         County = str_replace(County, "Q", "q")) %>%
  left_join(counties.regions, by = c("County" = "Name"))

land.values.2009.2017 <- read.csv("Data/Agriculture/Land values/ag land value 2009 - 2017, land econ.csv", header = TRUE) %>%
  mutate(County = str_replace(County, "McLeod", "Mcleod"),
         County = str_replace(County, "Q", "q")) %>%
  left_join(counties.regions, by = c("County" = "Name"))

land.values.2018 <- read.csv("Data/Agriculture/Land values/ag land value 2018, land econ.csv", header = TRUE) %>%
  mutate(County = str_replace(County, "McLeod", "Mcleod"),
         County = str_replace(County, "Q", "q")) %>%
  left_join(counties.regions, by = c("County" = "Name"))


mnland.values.2000.2008 <- land.values.2000.2008 %>%
  group_by(Year) %>%
  summarise(Number.of.jurisdictions.reporting = sum(Number.of.jurisdictions.reporting),
            Total.acres = sum(Total.acres),
            Total.estimated.value = sum(Total.estimated.value)) %>%
  ungroup() %>%
  mutate(Estimated.value.per.acre = Total.estimated.value / Total.acres,
         County = "Minnesota") %>%
  left_join(counties.regions, by = c("County" = "Name")) %>%
  mutate(Dem_Desc = ifelse(is.na(Dem_Desc), "Minnesota", Dem_Desc))

mnland.values.2009.2017 <- land.values.2009.2017 %>%
  group_by(Year)  %>%
  summarise(Number.of.jurisdictions.reporting = sum(Number.of.jurisdictions.reporting),
            Total.acres = sum(Total.acres),
            Total.estimated.value = sum(Total.estimated.value)) %>%
  ungroup() %>%
  mutate(Estimated.value.per.acre = Total.estimated.value / Total.acres,
         County = "Minnesota") %>%
  left_join(counties.regions, by = c("County" = "Name")) %>%
  mutate(Dem_Desc = ifelse(is.na(Dem_Desc), "Minnesota", Dem_Desc))

mnland.values.2018 <- land.values.2018 %>%
  group_by(Year)  %>%
  summarise(Number.of.jurisdictions.reporting = sum(Number.of.jurisdictions.reporting),
            Total.acres = sum(Total.acres),
            Total.estimated.value = sum(Total.estimated.value)) %>%
  ungroup() %>%
  mutate(Estimated.value.per.acre = Total.estimated.value / Total.acres,
         County = "Minnesota") %>%
  left_join(counties.regions, by = c("County" = "Name")) %>%
  mutate(Dem_Desc = ifelse(is.na(Dem_Desc), "Minnesota", Dem_Desc))


# combine land value objects ----------------------------------------------

land.values <- land.values.2000.2008 %>%
  rbind(land.values.2009.2017) %>%
  rbind(land.values.2018) %>%
  rbind(mnland.values.2000.2008) %>%
  rbind(mnland.values.2009.2017) %>%
  rbind(mnland.values.2018)
  
write_csv(land.values, "Data/Agriculture/Land values/Master-ag-land-values.csv")
