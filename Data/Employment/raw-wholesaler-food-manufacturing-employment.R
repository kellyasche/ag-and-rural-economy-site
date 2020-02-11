library(tidyverse)


# Import objects ----------------------------------------------------------

ag.emp.county <- read_csv("Data/Employment/Master-agrelated-emp-county.csv")

ag.emp.edr <- read_csv("Data/Employment/Master-agrelated-emp-edr.csv")

ag.emp.pr <- read_csv("Data/Employment/Master-agrelated-emp-pr.csv")



# Organize for raw merchant wholesalers -----------------------------------

raw.whole.ruca <- ag.emp.county %>%
  filter(naicstitle == "Farm Product Raw Material Merchant Wholesalers") %>%
  group_by(periodyear, Dem_Desc, naicstitle) %>%
  summarise(emp = sum(emp, na.rm = TRUE)) %>%
  ungroup()

write_csv(raw.whole.ruca, "Data/Employment/raw-wholesaler-employment-ruca.csv")

raw.whole.edr <- ag.emp.edr %>%
  filter(naicstitle == "Farm Product Raw Material Merchant Wholesalers") %>%
  select(1,3,2,5)

write_csv(raw.whole.edr, "Data/Employment/raw-wholesaler-employment-edr.csv")

raw.whole.pr <- ag.emp.pr %>%
  filter(naicstitle == "Farm Product Raw Material Merchant Wholesalers") %>%
  select(1,3,2,5)

write_csv(raw.whole.pr, "Data/Employment/raw-wholesaler-employment-pr.csv")

# Organize for food manufacturing -----------------------------------

food.man.ruca <- ag.emp.county %>%
  filter(naicstitle == "Food Manufacturing") %>%
  group_by(periodyear, Dem_Desc, naicstitle) %>%
  summarise(emp = sum(emp, na.rm = TRUE)) %>%
  ungroup()

write_csv(food.man.ruca, "Data/Employment/food-manufacturing-employment-ruca.csv")

food.man.edr <- ag.emp.edr %>%
  filter(naicstitle == "Food Manufacturing") %>%
  select(1,3,2,5)

write_csv(food.man.edr, "Data/Employment/food-manufacturing-employment-edr.csv")

food.man.pr <- ag.emp.pr %>%
  filter(naicstitle == "Food Manufacturing") %>%
  select(1,3,2,5)

write_csv(food.man.pr, "Data/Employment/food-manufacturing-employment-pr.csv")


