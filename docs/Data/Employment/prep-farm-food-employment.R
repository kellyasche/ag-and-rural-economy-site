library(tidyverse)


# import join objects -----------------------------------------------------

counties.regions <- read_csv("Data/Join docs/county_regions.csv") %>%
  mutate(countyfp = formatC(countyfp, width = 3, flag = "0"),
         Name = str_to_title(Name),
         Name = str_replace(Name, "Q", "q"),
         Name = str_replace(Name, "Of The", "of the"),
         Dem_Desc = ifelse(Name == "Minnesota", "Minnesota", Dem_Desc))

regions <- read_csv("Data/Join docs/county_regions.csv") %>%
  select(5,6) %>%
  unique()

# organize farm employment objects --------------------------------------------------

farm.total.emp <- read_csv("Data/Employment/bea-farm-total-employment.csv") %>%
  drop_na(LineCode) %>%
  mutate(countyfp = str_sub(GeoFips, 3, 5)) %>%
  select(22, 2,4:21) %>%
  gather(key = "year", value = "jobs", 4:20) %>%
  spread(key = Description, value = jobs) %>%
  left_join(counties.regions[,c(1,5,6)], by = "countyfp") %>%
  rename(farm.emp = 4,
         total.emp = 5)

farm.total.emp.edr <- farm.total.emp %>%
  group_by(edr, year) %>%
  summarise(farm.emp = sum(farm.emp, na.rm = TRUE),
            total.emp = sum(total.emp)) %>%
  ungroup() %>%
  mutate(year = as.numeric(year),
         edr = str_replace(edr, "  ", " "),
         edr = fct_relevel(edr, "EDR 1 - Northwest", "EDR 2 - Headwaters", "EDR 3 - Arrowhead", "EDR 4 - West Central", "EDR 5 - North Central", "EDR 6E- Southwest Central", "EDR 6W- Upper Minnesota Valley", "EDR 7E- East Central", "EDR 7W- Central", "EDR 8 - Southwest", "EDR 9 - South Central", "EDR 10 - Southeast", "EDR 11 - 7 County Twin Cities", "Minnesota"))

farm.total.emp.pr <- farm.total.emp %>%
  group_by(planning.region, year) %>%
  summarise(farm.emp = sum(farm.emp, na.rm = TRUE),
            total.emp = sum(total.emp)) %>%
  ungroup() %>%
  mutate(year = as.numeric(year),
         planning.region = str_replace(planning.region, " Minnesota", ""),
         planning.region = str_replace(planning.region, ", MN", ""),
         planning.region = fct_relevel(planning.region, "Northwest", "Northeast", "Central", "Seven County Mpls-St Paul", "Southwest", "Southeast"))


# organize ag related industry employment objects -------------------------

agrelated.total.emp.edr <- read_csv("Data/Employment/qcew-ag-related-industries-edr.csv") %>%
  select(1,2,5,12:15,28) %>%
  gather(key = "emp.type", value = "emp", 4:8) %>%
  group_by(periodyear, naicstitle, areaname) %>%
  mutate(emp = ifelse(is.na(emp[emp.type == "empYear"]), mean(emp, na.rm = TRUE), emp)) %>%
  ungroup() %>%
  filter(emp.type == "empYear") %>%
  group_by(periodyear, areaname) %>%
  summarise(agrelated.emp = sum(emp[naicstitle != "Total, All Industries"], na.rm = TRUE),
            total.agrelated.emp = sum(emp[naicstitle == "Total, All Industries"])) %>%
  ungroup() %>%
  left_join(regions, by = c("areaname" = "edr")) %>%
  rename(edr = areaname) %>%
  mutate(edr = str_replace(edr, "  ", " "),
         edr = fct_relevel(edr, "EDR 1 - Northwest", "EDR 2 - Headwaters", "EDR 3 - Arrowhead", "EDR 4 - West Central", "EDR 5 - North Central", "EDR 6E- Southwest Central", "EDR 6W- Upper Minnesota Valley", "EDR 7E- East Central", "EDR 7W- Central", "EDR 8 - Southwest", "EDR 9 - South Central", "EDR 10 - Southeast", "EDR 11 - 7 County Twin Cities", "Minnesota"),
         planning.region = str_replace(planning.region, " Minnesota", ""),
         planning.region = fct_relevel(planning.region, "Northwest", "Northeast", "Central", "Seven County Mpls-St Paul", "Southwest", "Southeast"))

agrelated.total.emp.pr <- read_csv("Data/Employment/qcew-ag-related-industries-pr.csv") %>%
  select(1,2,5,12:15,28) %>%
  gather(key = "emp.type", value = "emp", 4:8) %>%
  group_by(periodyear, naicstitle, areaname) %>%
  mutate(emp = ifelse(is.na(emp[emp.type == "empYear"]), mean(emp, na.rm = TRUE), emp)) %>%
  ungroup() %>%
  filter(emp.type == "empYear") %>%
  group_by(periodyear, areaname) %>%
  summarise(agrelated.emp = sum(emp[naicstitle != "Total, All Industries"], na.rm = TRUE),
            total.agrelated.emp = sum(emp[naicstitle == "Total, All Industries"])) %>%
  ungroup() %>%
  rename(planning.region = areaname) %>%
  mutate(planning.region = str_replace(planning.region, " Minnesota", ""),
         planning.region = str_replace(planning.region, ", MN", ""),
         planning.region = fct_relevel(planning.region, "Northwest", "Northeast", "Central", "Seven County Mpls-St Paul", "Southwest", "Southeast"))


# Combine farm and ag-related employment by region ------------------------

farm.agrelated.emp.edr <- farm.total.emp.edr %>%
  left_join(agrelated.total.emp.edr, by = c("edr", "year" = "periodyear")) %>%
  filter(edr != "Minnesota")

write_csv(farm.agrelated.emp.edr, "Data/Employment/Master-farm-agrelated-emp-edr.csv")  

farm.agrelated.emp.pr <- farm.total.emp.pr %>%
  left_join(agrelated.total.emp.pr, by = c("planning.region", "year" = "periodyear")) %>%
  filter(planning.region != "Minnesota")

write_csv(farm.agrelated.emp.pr, "Data/Employment/Master-farm-agrelated-emp-pr.csv")
