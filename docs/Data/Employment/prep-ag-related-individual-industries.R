library(tidyverse)


# import join docs --------------------------------------------------------

counties.regions <- read_csv("Data/Join docs/county_regions.csv") %>%
  mutate(countyfp = formatC(countyfp, width = 3, flag = "0"),
         Name = str_to_title(Name),
         Name = str_replace(Name, "Q", "q"),
         Name = str_replace(Name, "Of The", "of the"),
         Dem_Desc = ifelse(Name == "Minnesota", "Minnesota", Dem_Desc))


# import and organize ag-related emp objects for edr and pr ---------------

agrelated.ind.emp.edr <- read_csv("Data/Employment/qcew-ag-related-industries-edr.csv") %>%
  select(1,2,5,12:15, 28) %>%
  gather(key = "emp.type", value = "emp", 4:8) %>%
  group_by(periodyear, areaname, naicstitle) %>%
  mutate(emp = ifelse(is.na(emp) & emp.type == "empYear", mean(emp[naicstitle != "Total, All Industries"], na.rm = TRUE), emp)) %>%
  ungroup() %>%
  filter(emp.type == "empYear") %>%
  filter(naicstitle != "Total, All Industries") %>%
  mutate(areaname = str_replace(areaname, "Seven County Mpls-St Paul, MN", "Seven County Mpls-St Paul")) %>%
  group_by(periodyear, areaname) %>%
  mutate(agrelated.emp = sum(emp)) %>%
  ungroup()


agrelated.ind.emp.pr <- read_csv("Data/Employment/qcew-ag-related-industries-pr.csv") %>%
  select(1,2,5,12:15, 28) %>%
  gather(key = "emp.type", value = "emp", 4:8) %>%
  group_by(periodyear, areaname, naicstitle) %>%
  mutate(emp = ifelse(is.na(emp) & emp.type == "empYear", mean(emp[naicstitle != "Total, All Industries"], na.rm = TRUE), emp)) %>%
  ungroup() %>%
  filter(emp.type == "empYear") %>%
  filter(naicstitle != "Total, All Industries") %>%
  mutate(areaname = str_replace(areaname, " Minnesota", "")) %>%
  group_by(periodyear, areaname) %>%
  mutate(agrelated.emp = sum(emp)) %>%
  ungroup()

write_csv(agrelated.ind.emp.edr, "Data/Employment/Master-agrelated-individual-ind-emp-edr.csv")
write_csv(agrelated.ind.emp.pr, "Data/Employment/Master-agrelated-individual-ind-emp-pr.csv")
