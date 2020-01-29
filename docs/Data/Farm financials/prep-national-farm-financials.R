library(tidyverse)

rm(list = ls())


# Prep object -------------------------------------------------------------

data <- read_csv("Data/Farm financials/all-financials.csv") %>%
  rename(variable = 4) %>%
  mutate(variable = as.factor(variable)) %>%
  filter(variable %in% c("Current ratio", "Debt service ratio", "Debt to asset ratio, excl. operator dwellings", "Debt to equity ratio, excl. operator dwellings", "Equity to asset ratio, excl. operator dwellings", "Net farm income ratio", "Times interest earned")) %>%
  select(1,4,5,6,7,8)

write_csv(data, "Data/Farm financials/nation-farm-financial-ratios.csv")

names(data)
levels(data$variable)
head(data)
