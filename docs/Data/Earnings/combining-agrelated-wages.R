library(tidyverse)


# Combine ag-related industry wages ---------------------------------------

agrelated.wages.edr <- read_csv("Data/Earnings/qcew-ag-related-industries-edr.csv") %>%
  select(1,2,5,20:23,30)
gather(key = "wage.type", value = "wages", 4:8) %>%
  group_by(periodyear, naicstitle, areaname) %>%
  mutate(wages = ifelse(is.na(wages) & wage.type == "totwageYear", mean(wages, na.rm = TRUE), wages)) %>%
  ungroup() %>%
  filter(wage.type == "totwageYear") %>%
  group_by(periodyear, areaname) %>%
  summarise(ag.related.wages = sum(wages[naicstitle != "Total, All Industries"], na.rm = TRUE),
            total.wages = sum(wages[naicstitle == "Total, All Industries"], na.rm = TRUE)) %>%
  ungroup()

agrelated.wages.pr <- read_csv("Data/Earnings/qcew-ag-related-industries-pr.csv") %>%
  select(1,2,5,20:23,30) %>%
  gather(key = "wage.type", value = "wages", 4:8) %>%
  group_by(periodyear, naicstitle, areaname) %>%
  mutate(wages = ifelse(is.na(wages) & wage.type == "totwageYear", mean(wages, na.rm = TRUE), wages)) %>%
  ungroup() %>%
  filter(wage.type == "totwageYear") %>%
  group_by(periodyear, areaname) %>%
  summarise(ag.related.wages = sum(wages[naicstitle != "Total, All Industries"], na.rm = TRUE),
            total.wages = sum(wages[naicstitle == "Total, All Industries"], na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(areaname = str_replace(areaname, "Seven County Mpls-St Paul, MN", "Seven County Mpls-St Paul"))
