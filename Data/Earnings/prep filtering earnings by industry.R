library(tidyverse)


# organize earnings by main industry --------------------------------------

earnings <- read_csv("Data/Earnings/CAINC5N_MN_2001_2018.csv") %>%
  filter(LineCode %in% c(35,81, 82, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000))

write_csv(earnings, "Data/Earnings/bea-earnings-by-industry-2001-2018.csv")

names(earnings)
