library(tidyverse)

rm(list = ls())


# Import objects to join --------------------------------------------------

farm.assets.libilities.equity <- read_csv("Data/Farm financials/farm-assets-liabilities-equity.csv") %>%
  gather(key = "year", value = "value", 3:18) %>%
  mutate(year = str_sub(year, 1, 4))

farm.cash.net.income <- read_csv("Data/Farm financials/farm-cash-net-income.csv") %>%
  gather(key = "year", value = "value", 3:18) %>%
  mutate(year = str_sub(year, 1, 4))

farm.current.assets.liabilities <- read_csv("Data/Farm financials/farm-current-assets-liablities.csv") %>%
  gather(key = "year", value = "value", 3:18) %>%
  mutate(year = str_sub(year, 1, 4))

farm.current.working.debt.asset.ratios <- read_csv("Data/Farm financials/farm-current-working-debt-asset-ratios.csv") %>%
  gather(key = "year", value = "value", 3:18) %>%
  mutate(year = str_sub(year, 1, 4))

farm.debt.payments <- read_csv("Data/Farm financials/farm-debt-payments.csv") %>%
  gather(key = "year", value = "value", 3:18) %>%
  mutate(year = str_sub(year, 1, 4))

farm.interest.payments <- read_csv("Data/Farm financials/farm-interest-payments.csv") %>%
  gather(key = "year", value = "value", 3:18) %>%
  mutate(year = str_sub(year, 1, 4))

farm.production.value <- read_csv("Data/Farm financials/farm-production-value.csv") %>%
  gather(key = "year", value = "value", 3:18) %>%
  mutate(year = str_sub(year, 1, 4))


# Develop measurements/ratios ---------------------------------------------

debt.to.asset.ratio <- farm.current.working.debt.asset.ratios %>%
  filter(VARIABLE == "Debt/asset ratio") %>%
  spread(key = VARIABLE, value = value)

debt.to.equity.ratio <- farm.assets.libilities.equity %>%
  filter(VARIABLE %in% c("Farm liabilities", "Farm equity")) %>%
  spread(key = VARIABLE, value = value) %>%
  mutate(debt.to.equity.ratio = `Farm liabilities` / `Farm equity`)
  
equity.to.asset.ratio <- farm.assets.libilities.equity %>%
  filter(VARIABLE %in% c("Farm equity", "Farm assets")) %>%
  spread(key = VARIABLE, value = value) %>%
  mutate(equity.to.asset.ratio = `Farm equity` / `Farm assets`)

current.ratio <- farm.current.working.debt.asset.ratios %>%
  filter(VARIABLE == "Current ratio") %>%
  spread(key = VARIABLE, value = value)

working.capital.ratio <- farm.current.working.debt.asset.ratios %>%
  filter(VARIABLE == "Working capital-to-expense ratio") %>%
  spread(key = VARIABLE, value = value)

farms <- farm.debt.payments %>%
  filter(VARIABLE == "Farms") %>%
  spread(key = VARIABLE, value = value)

production.value <- farm.production.value %>%
  filter(VARIABLE == "Total value of production")

production.per.farm <- production.value %>%
  left_join(farms[,c(2,3)], by = "year") %>%
  mutate(production.per.farm = (value * 1000) / Farms)

farm.debt.payments <- farm.debt.payments %>%
  filter(VARIABLE == "Principal/interest payments") %>%
  spread(key = VARIABLE, value = value)

debt.service.ratio <- production.per.farm %>%
  left_join(farm.debt.payments[,c(2,3)], by = "year") %>%
  rename(debt.payments = 7) %>%
  mutate(debt.service.ratio = production.per.farm / debt.payments)

net.income <- farm.cash.net.income %>%
  filter(VARIABLE == "Net farm income") %>%
  spread(key = VARIABLE, value = value)

times.interest.earned.ratio <- farm.interest.payments %>%
  filter(VARIABLE == "Interest") %>%
  spread(key = VARIABLE, value = value) %>%
  left_join(net.income[,c(2,3)], by = "year") %>%
  rename(net.farm.income = 4) %>%
  mutate(times.interest.earned.ratio = net.farm.income / Interest)

all.ratios <- debt.to.asset.ratio %>%
  left_join(debt.to.equity.ratio[,c(2,5)], by = "year") %>%
  left_join(equity.to.asset.ratio[,c(2,5)], by = "year") %>%
  left_join(current.ratio[,c(2,3)], by = "year") %>%
  left_join(working.capital.ratio[,c(2,3)], by = "year") %>%
  left_join(debt.service.ratio[,c(3,8)], by = "year") %>%
  left_join(times.interest.earned.ratio[,c(2,5)], by = "year") %>%
  rename(debt.asset.ratio = 3,
         current.ratio = 6,
         working.capital.ratio = 7) %>%
  select(2:9)

write_csv(all.ratios, "Data/Farm financials/solvency-ratios.csv")