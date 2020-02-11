library(tidyverse)


# import and organize objects ---------------------------------------------

earnings <- read_csv("Data/Marnie/farm-total-earnings-ruca.csv")

emp <- read_csv("Data/Marnie/farm-total-emp-ruca.csv")

earnings.emp <- earnings %>%
  left_join(emp, by = c("Dem_Desc", "year"))

write_csv(earnings.emp, "Data/Marnie/farm-total-earnings-emp-ruca.csv")
