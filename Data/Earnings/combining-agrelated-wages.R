library(tidyverse)


# Join docs ---------------------------------------------------------------

counties.regions <- read_csv("Data/Join docs/county_regions.csv") %>%
  mutate(countyfp = formatC(countyfp, width = 3, flag = "0"),
         Name = str_to_title(Name),
         Name = str_replace(Name, "Q", "q"),
         Name = str_replace(Name, "Of The", "of the"),
         Dem_Desc = ifelse(Name == "Minnesota", "Minnesota", Dem_Desc),
         Name = str_replace(Name, "Mcleod", "McLeod"),
         Name = str_replace(Name, "St. Louis", "Saint Louis"))



# Combine ag-related industry wages ---------------------------------------

agrelated.wages.edr <- read_csv("Data/Earnings/qcew-ag-related-industries-edr.csv") %>%
  select(1,2,5,20:23,30) %>%
gather(key = "wage.type", value = "wages", 4:8) %>%
  group_by(periodyear, naicstitle, areaname) %>%
  mutate(wages = ifelse(is.na(wages) & wage.type == "totwageYear", mean(wages, na.rm = TRUE), wages)) %>%
  ungroup() %>%
  filter(wage.type == "totwageYear") %>%
  rename(edr = areaname) %>%
  mutate(edr = str_replace(edr, "  ", " "))

write_csv(agrelated.wages.edr, "Data/Earnings/Master-ag-related-ind-wages-edr.csv")


agrelated.wages.pr <- read_csv("Data/Earnings/qcew-ag-related-industries-pr.csv") %>%
  select(1,2,5,20:23,30) %>%
  gather(key = "wage.type", value = "wages", 4:8) %>%
  group_by(periodyear, naicstitle, areaname) %>%
  mutate(wages = ifelse(is.na(wages) & wage.type == "totwageYear", mean(wages, na.rm = TRUE), wages)) %>%
  ungroup() %>%
  filter(wage.type == "totwageYear") %>%
  mutate(areaname = str_replace(areaname, "Seven County Mpls-St Paul, MN", "Seven County Mpls-St Paul")) %>%
  rename(planning.region = areaname)

write_csv(agrelated.wages.pr, "Data/Earnings/Master-ag-related-ind-wages-pr.csv")

agrelated.wages.county <- read_csv("Data/Earnings/qcew-ag-related-industries-county.csv") %>%
  select(1,2,5,20:23,30) %>%
  gather(key = "wage.type", value = "wages", 4:8) %>%
  group_by(periodyear, naicstitle, areaname) %>%
  mutate(wages = ifelse(is.na(wages) & wage.type == "totwageYear", mean(wages, na.rm = TRUE), wages)) %>%
  ungroup() %>%
  filter(wage.type == "totwageYear") %>%
  mutate(areaname = str_replace(areaname, " County", "")) %>%
  left_join(counties.regions, by = c("areaname" = "Name"))

write_csv(agrelated.wages.county, "Data/Earnings/Master-ag-related-ind-wages-counties.csv")
  

