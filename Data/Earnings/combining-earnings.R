library(tidyverse)

counties.regions <- read_csv("Data/Join docs/county_regions.csv") %>%
  mutate(countyfp = formatC(countyfp, width = 3, flag = "0"),
         Name = str_to_title(Name),
         Name = str_replace(Name, "Q", "q"),
         Name = str_replace(Name, "Of The", "of the"),
         Dem_Desc = ifelse(Name == "Minnesota", "Minnesota", Dem_Desc))


# organize earnings objects --------------------------------------------------------

farm.earnings <- read_csv("Data/Earnings/bea-farm-earnings-county-2001-2018.csv") %>%
  mutate(countyfp = str_sub(GeoFips, 3, 5)) %>%
  gather(key = "year", value = "farm.earnings", 3:20) 

total.earnings <- read_csv("Data/Earnings/bea-total-earnings-county-2001-2018.csv") %>%
  mutate(countyfp = str_sub(GeoFips, 3,5)) %>%
  gather(key = "year", value = "total.earnings", 3:20)

farm.total.earnings.county <- farm.earnings %>%
  left_join(total.earnings, by = c("GeoFips", "countyfp", "GeoName", "year")) %>%
  left_join(counties.regions[,c(1,3:6)], by = "countyfp")

farm.total.earnings.edr <- farm.earnings %>%
  left_join(total.earnings, by = c("GeoFips", "countyfp", "GeoName", "year")) %>%
  left_join(counties.regions[,c(1,3:6)], by = "countyfp") %>%
  group_by(edr, year) %>%
  summarise(farm.earnings = sum(farm.earnings),
            total.earnings = sum(total.earnings)) %>%
  ungroup() %>%
  mutate(year = as.numeric(year))

farm.total.earnings.pr <- farm.earnings %>%
  left_join(total.earnings, by = c("GeoFips", "countyfp", "GeoName", "year")) %>%
  left_join(counties.regions[,c(1,3:6)], by = "countyfp") %>%
  group_by(planning.region, year) %>%
  summarise(farm.earnings = sum(farm.earnings),
            total.earnings = sum(total.earnings)) %>%
  ungroup() %>%
  mutate(year = as.numeric(year))


write_csv(farm.total.earnings.county, "Data/Earnings/Master-farm-total-earnings-county-2001-2018.csv")

write_csv(farm.total.earnings.edr, "Data/Earnings/Master-farm-total-earnings-edr-2001-2018.csv")

write_csv(farm.total.earnings.pr, "Data/Earnings/Master-farm-total-earnings-pr-2001-2018.csv")
