library(tidyverse)


# import master objects --------------------------------------------------------

farm.revenue <- read_csv("Data/Income and expenses/Master-farm-income-expenses.csv") %>%
  mutate(edr = str_replace(edr, "  ", " "),
         planning.region = str_replace(planning.region, " Minnesota", ""))

total.gross.sales.pr <- read_csv("Data/Business revenue/County/sales-use-tax-pr.csv") %>%
  filter(sales.type == "gross.sales") %>%
  select(1,2,4) %>%
  rename("gross.sales" = 3)

total.gross.sales.edr <- read_csv("Data/Business revenue/County/sales-use-tax-edr.csv") %>%
  filter(sales.type == "gross.sales") %>%
  select(1,2,4) %>%
  rename("gross.sales" = 3)

total.gross.sales.ruca <- read_csv("Data/Business revenue/County/sales-use-tax-ruc.csv") %>%
  filter(sales.type == "gross.sales") %>%
  select(1,2,4) %>%
  rename("gross.sales" = 3)

total.gross.sales.metro.non <- read_csv("Data/Business revenue/County/sales-use-tax-metro-non.csv") %>%
  filter(sales.type == "gross.sales") %>%
  select(1,3,4) %>%
  rename("gross.sales" = 3)


names(farm.revenue)



# Prep farm cash receipts ------------------------------------------------------------

farm.cash.county <- farm.revenue %>%
  select(1,2,3)

farm.cash.pr <- farm.revenue %>%
  select(1,2,11) %>%
  group_by(year, planning.region) %>%
  summarise(cash.receipts = sum(cash.receipts, na.rm = TRUE)) %>%
  ungroup()

farm.cash.edr <- farm.revenue %>%
  select(1,2,10) %>%
  group_by(year, edr) %>%
  summarise(cash.receipts = sum(cash.receipts, na.rm = TRUE)) %>%
  ungroup()

farm.cash.ruca <- farm.revenue %>%
  select(1,2,9) %>%
  group_by(year, Dem_Desc) %>%
  summarise(cash.receipts = sum(cash.receipts, na.rm = TRUE)) %>%
  ungroup()

farm.cash.metro.non <- farm.cash.pr %>%
  mutate(metro.non = ifelse(planning.region == "Seven County Mpls-St Paul", "Metro", "Non-metro")) %>%
  group_by(year, metro.non) %>%
  summarise(cash.receipts = sum(cash.receipts)) %>%
  ungroup()


# combine objects ---------------------------------------------------------

farm.cash.total.gross.pr <- farm.cash.pr %>%
  left_join(total.gross.sales.pr, by = c("year", "planning.region")) %>%
  drop_na(gross.sales) %>%
  rename("farm.cash.receipts" = 3)

farm.cash.total.gross.edr <- farm.cash.edr %>%
  left_join(total.gross.sales.edr, by = c("year", "edr")) %>%
  drop_na(gross.sales)  %>%
  rename("farm.cash.receipts" = 3)

farm.cash.total.gross.ruca <- farm.cash.ruca %>%
  left_join(total.gross.sales.ruca, by = c("year", "Dem_Desc")) %>%
  drop_na(gross.sales)  %>%
  rename("farm.cash.receipts" = 3)

farm.cash.total.gross.metro.non <- farm.cash.metro.non %>%
  left_join(total.gross.sales.metro.non, by = c("year", "metro.non")) %>%
  drop_na(gross.sales)  %>%
  rename("farm.cash.receipts" = 3)

write_csv(farm.cash.total.gross.pr, "Data/Marnie/farm-cash-receipts-total-gross-sales-pr.csv")

write_csv(farm.cash.total.gross.edr, "Data/Marnie/farm-cash-receipts-total-gross-sales-edr.csv")

write_csv(farm.cash.total.gross.ruca, "Data/Marnie/farm-cash-receipts-total-gross-sales-ruca.csv")

write_csv(farm.cash.total.gross.metro.non, "Data/Marnie/farm-cash-receipts-total-gross-sales-metro-non.csv")

names(farm.cash.pr)
names(total.gross.sales.pr)
