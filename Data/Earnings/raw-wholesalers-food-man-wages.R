library(tidyverse)


# import ag wages ---------------------------------------------------------

ag.wages.county <- read_csv("Data/Earnings/Master-ag-related-ind-wages-counties.csv")

ag.wages.edr <- read_csv("Data/Earnings/Master-ag-related-ind-wages-edr.csv")

ag.wages.pr <- read_csv("Data/Earnings/Master-ag-related-ind-wages-pr.csv")


# Organize for raw merchant wholesalers -----------------------------------

raw.whole.ruca <- ag.wages.county %>%
  filter(naicstitle == "Farm Product Raw Material Merchant Wholesalers") %>%
  group_by(periodyear, Dem_Desc, naicstitle) %>%
  summarise(wages = sum(wages, na.rm = TRUE)) %>%
  ungroup()

write_csv(raw.whole.ruca, "Data/Earnings/raw-wholesaler-wages-ruca.csv")

raw.whole.edr <- ag.wages.edr %>%
  filter(naicstitle == "Farm Product Raw Material Merchant Wholesalers") %>%
  select(1,3,2,5)

write_csv(raw.whole.edr, "Data/Earnings/raw-wholesaler-wages-edr.csv")

raw.whole.pr <- ag.wages.pr %>%
  filter(naicstitle == "Farm Product Raw Material Merchant Wholesalers") %>%
  select(1,3,2,5)

write_csv(raw.whole.pr, "Data/Earnings/raw-wholesaler-wages-pr.csv")

# Organize for food manfucturing -----------------------------------

food.man.ruca <- ag.wages.county %>%
  filter(naicstitle == "Food Manufacturing") %>%
  group_by(periodyear, Dem_Desc, naicstitle) %>%
  summarise(wages = sum(wages, na.rm = TRUE)) %>%
  ungroup()

write_csv(food.man.ruca, "Data/Earnings/food-manufacturing-wages-ruca.csv")

food.man.edr <- ag.wages.edr %>%
  filter(naicstitle == "Food Manufacturing") %>%
  select(1,3,2,5)

write_csv(food.man.edr, "Data/Earnings/food-manufacturing-wages-edr.csv")

food.man.pr <- ag.wages.pr %>%
  filter(naicstitle == "Food Manufacturing") %>%
  select(1,3,2,5)

write_csv(food.man.pr, "Data/Earnings/food-manufacturing-wages-pr.csv")


