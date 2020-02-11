data(tidyverse)


# import and organize objects ---------------------------------------------

farm.earnings <- read_csv("Data/Marnie/farm-total-earnings-select-edrs.csv")

farm.emp <- read_csv("Data/Marnie/farm-total-emp-select-edrs.csv")

farm.earnings.emp <- farm.earnings %>%
  left_join(farm.emp[,c(1,3,4,5)], by = c("edr", "year"))

write_csv(farm.earnings.emp, "Data/Marnie/farm-total-earnings-emp-select-edrs.csv")

names(farm.earnings.emp)
