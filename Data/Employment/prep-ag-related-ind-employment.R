library(tidyverse)


# Prep regions ------------------------------------------------------------

counties.regions <- read_csv("Data/Join docs/county_regions.csv") %>%
  mutate(countyfp = formatC(countyfp, width = 3, flag = "0"),
         Name = str_to_title(Name),
         Name = str_replace(Name, "Q", "q"),
         Name = str_replace(Name, "Of The", "of the"),
         Dem_Desc = ifelse(Name == "Minnesota", "Minnesota", Dem_Desc),
         Name = str_replace(Name, "Mcleod", "McLeod"),
         planning.region = str_replace(planning.region, " Minnesota", ""),
         edr = str_replace(edr, "  ", " "))

regions <- counties.regions %>%
  distinct(edr, .keep_all = TRUE) %>%
  select(5,6)

# Prep county, edr, and pr objects ----------------------------------------

master.ag.emp.county <- read_csv("Data/Employment/qcew-ag-related-industries-emp-county.csv") %>%
  select(1,2,5,12,13,14,15,28) %>%
  gather(key = "emp.break", value = "emp", 4:8) %>%
  mutate(emp = ifelse(is.na(emp), 0, emp),
         areaname = str_replace(areaname, " County", ""),
         areaname = str_replace(areaname, "Saint Louis", "St. Louis")) %>%
  group_by(periodyear, naicstitle, areaname) %>%
  mutate(emp = ifelse(emp == 0 & emp.break == "empYear", round(mean(emp, na.rm = TRUE)), emp)) %>%
  ungroup() %>%
  filter(emp.break == "empYear") %>%
  left_join(counties.regions, by = c("areaname" = "Name")) %>%
  rename("Name" = "areaname")

master.ag.emp.ruca <- read_csv("Data/Employment/qcew-ag-related-industries-emp-county.csv") %>%
  select(1,2,5,12,13,14,15,28) %>%
  gather(key = "emp.break", value = "emp", 4:8) %>%
  mutate(emp = ifelse(is.na(emp), 0, emp),
         areaname = str_replace(areaname, " County", ""),
         areaname = str_replace(areaname, "Saint Louis", "St. Louis")) %>%
  ungroup() %>%
  group_by(periodyear, naicstitle, areaname) %>%
  mutate(emp = ifelse(emp == 0 & emp.break == "empYear", round(mean(emp, na.rm = TRUE)), emp)) %>%
  ungroup() %>%
  left_join(counties.regions, by = c("areaname" = "Name")) %>%
  filter(emp.break == "empYear") %>%
  group_by(periodyear, naicstitle, Dem_Desc) %>%
  summarise(emp = sum(emp)) %>%
  ungroup()
  
master.ag.emp.edr <- read_csv("Data/Employment/qcew-ag-related-industries-emp-edr.csv") %>%
  select(1,2,5,12,13,14,15,28) %>%
  gather(key = "emp.break", value = "emp", 4:8) %>%
  mutate(emp = ifelse(is.na(emp), 0, emp),
         areaname = str_replace(areaname, "  ", " ")) %>%
  group_by(periodyear, naicstitle, areaname) %>%
  mutate(emp = ifelse(emp == 0 & emp.break == "empYear", round(mean(emp, na.rm = TRUE)), emp)) %>%
  ungroup() %>%
  filter(emp.break == "empYear") %>%
  rename("edr" = "areaname") %>%
  left_join(regions, by = "edr")

master.ag.emp.pr <- read_csv("Data/Employment/qcew-ag-related-industries-emp-pr.csv") %>%
  select(1,2,5,12,13,14,15,28) %>%
  gather(key = "emp.break", value = "emp", 4:8) %>%
  mutate(emp = ifelse(is.na(emp), 0, emp),
         areaname = str_replace(areaname, " Minnesota", "")) %>%
  group_by(periodyear, naicstitle, areaname) %>%
  mutate(emp = ifelse(emp == 0 & emp.break == "empYear", round(mean(emp, na.rm = TRUE)), emp)) %>%
  ungroup() %>%
  filter(emp.break == "empYear") %>%
  rename("planning.region" = "areaname")


# Write csv ---------------------------------------------------------------

write_csv(master.ag.emp.county, "Data/Employment/Master-agrelated-emp-county.csv")

write_csv(master.ag.emp.ruca, "Data/Employment/Master-agrelated-emp-ruca.csv")

write_csv(master.ag.emp.edr, "Data/Employment/Master-agrelated-emp-edr.csv")

write_csv(master.ag.emp.pr, "Data/Employment/Master-agrelated-emp-pr.csv")
