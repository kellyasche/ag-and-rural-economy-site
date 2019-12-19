library(tidyverse)

counties.regions <- read_csv("Data/Join docs/county_regions.csv") %>%
  mutate(countyfp = formatC(countyfp, width = 3, flag = "0"),
         Name = str_to_title(Name),
         Name = str_replace(Name, "Q", "q"),
         Name = str_replace(Name, "Of The", "of the"))

cash.receipts <- read_csv("Data/Income and expenses/bea - cash receipts 2001 2018.csv") %>%
  gather(key = "year", value = "cash.receipts", 3:20) %>%
  mutate(year = as.integer(year),
         countyfp = str_sub(GeoFips, 3,5))

other.income <- read_csv("Data/Income and expenses/bea - other income farms 2001 2018.csv") %>%
  gather(key = "year", value = "other.income", 3:20) %>%
  mutate(year = as.integer(year),
         countyfp = str_sub(GeoFips, 3,5))

expenses <- read_csv("Data/Income and expenses/bea - production expenses farms 2001 2018.csv") %>%
  gather(key = "year", value = "expenses", 3:20) %>%
  mutate(year = as.integer(year),
         countyfp = str_sub(GeoFips, 3,5))

net.income <- read_csv("Data/Income and expenses/bea - realized net income farms 2001 2018.csv") %>%
  gather(key = "year", value = "net.income", 3:20) %>%
  mutate(year = as.integer(year),
         countyfp = str_sub(GeoFips, 3,5))

# combine -----------------------------------------------------------------

income.expenses <- cash.receipts %>%
  left_join(other.income[,c(3,4,5)], by = c("countyfp", "year")) %>%
  left_join(expenses[,c(3,4,5)], by = c("countyfp", "year")) %>%
  left_join(net.income[,c(3,4,5)], by = c("countyfp", "year"))


# combine with number of farm acres ---------------------------------------
acres <- read_csv("Data/Land values/Master-ag-land-values.csv")

income.expenses.acres <- income.expenses %>%
  left_join(acres[,c(2,4,7:11)], by = c("countyfp", "year" = "Year")) %>%
  mutate(cash.receipts = cash.receipts * 1000,
         other.income = other.income * 1000,
         expenses = expenses * 1000,
         net.income = net.income * 1000) %>%
  select(3:13)

write_csv(income.expenses.acres, "Data/Income and expenses/Master-farm-income-expenses.csv")
