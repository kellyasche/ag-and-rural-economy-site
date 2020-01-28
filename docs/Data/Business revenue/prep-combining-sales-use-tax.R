library(tidyverse)

rm(list = ls())


# import sales use tax objects --------------------------------------------

data.2017 <- read_csv("Data/Business revenue/2017-sales-use-tax.csv") %>%
  rename(year = 1,
         description = 2,
         gross.sales = 3,
         taxable.sales = 4,
         sales.tax = 5,
         use.tax = 6,
         total.tax = 7, 
         number = 8) %>%
  mutate(naics = str_sub(description, 1,4),
         naics = str_replace(naics, "Tota", "Total"),
         description = str_sub(description, 6, -1),
         description = ifelse(description == "", "Total", description))

data.2003 <- read_csv("Data/Business revenue/2003-sales-use-tax.csv") %>%
mutate(year = 2003) %>%
  rename(naics= 1,
         gross.sales = 2,
         taxable.sales = 3,
         sales.tax = 4,
         use.tax = 5,
         total.tax = 6,
         number = 7) %>%
  left_join(data.2017[,c(2,9)], by = "naics") 

data.2004 <- read_csv("Data/Business revenue/2004-sales-use-tax.csv") %>%
  mutate(year = 2004) %>%
  rename(naics= 1,
         gross.sales = 2,
         taxable.sales = 3,
         sales.tax = 4,
         use.tax = 5,
         total.tax = 6,
         number = 7) %>%
  left_join(data.2017[,c(2,9)], by = "naics") %>%
  select(1:7, 9:10)

data.2005 <- read_csv("Data/Business revenue/2005-sales-use-tax.csv") %>%
  select(1:7) %>%
  mutate(year = 2005) %>%
  rename(naics= 1,
         gross.sales = 2,
         taxable.sales = 3,
         sales.tax = 4,
         use.tax = 5,
         total.tax = 6,
         number = 7) %>%
  left_join(data.2017[,c(2,9)], by = "naics") 

data.2006 <- read_csv("Data/Business revenue/2006-sales-use-tax.csv") %>%
  mutate(year = 2006) %>%
  rename(naics= 1,
         gross.sales = 2,
         taxable.sales = 3,
         sales.tax = 4,
         use.tax = 5,
         total.tax = 6,
         number = 7) %>%
  left_join(data.2017[,c(2,9)], by = "naics") 

data.2007 <- read_csv("Data/Business revenue/2007-sales-use-tax.csv") %>%
  mutate(year = 2007) %>%
  rename(naics= 1,
         gross.sales = 2,
         taxable.sales = 3,
         sales.tax = 4,
         use.tax = 5,
         total.tax = 6,
         number = 7) %>%
  left_join(data.2017[,c(2,9)], by = "naics") 

data.2008 <- read_csv("Data/Business revenue/2008-sales-use-tax.csv") %>%
  rename(year = 1,
         description = 2,
         gross.sales = 3,
         taxable.sales = 4,
         sales.tax = 5,
         use.tax = 6,
         total.tax = 7, 
         number = 8) %>%
  select(1:8) %>%
  mutate(naics = str_sub(description, 1,4),
         description = str_sub(description, 6, -1),
         year = ifelse(is.na(year), 2008, year),
         description = ifelse(is.na(description), "Total", description),
         naics = ifelse(is.na(naics), "Total", naics))

data.2009 <- read_csv("Data/Business revenue/2009-sales-use-tax.csv") %>%
  rename(year = 1,
         description = 2,
         gross.sales = 3,
         taxable.sales = 4,
         sales.tax = 5,
         use.tax = 6,
         total.tax = 7, 
         number = 8) %>%
  select(1:8) %>%
  mutate(naics = str_sub(description, 1,4),
         naics = str_replace(naics, "Tota", "Total"),
         description = str_sub(description, 6, -1),
         description = ifelse(description == "", "Total", description))

data.2010 <- read_csv("Data/Business revenue/2010-sales-use-tax.csv") %>%
  rename(year = 1,
         description = 2,
         gross.sales = 3,
         taxable.sales = 4,
         sales.tax = 5,
         use.tax = 6,
         total.tax = 7, 
         number = 8) %>%
  select(1:8) %>%
  mutate(naics = str_sub(description, 1,4),
         naics = str_replace(naics, "Tota", "Total"),
         description = str_sub(description, 6, -1),
         description = ifelse(description == "", "Total", description))

data.2011 <- read_csv("Data/Business revenue/2011-sales-use-tax.csv") %>%
  rename(year = 1,
         description = 2,
         gross.sales = 3,
         taxable.sales = 4,
         sales.tax = 5,
         use.tax = 6,
         total.tax = 7, 
         number = 8) %>%
  select(1:8) %>%
  mutate(naics = str_sub(description, 1,4),
         naics = str_replace(naics, "Tota", "Total"),
         description = str_sub(description, 6, -1),
         description = ifelse(description == "", "Total", description))

data.2012 <- read_csv("Data/Business revenue/2012-sales-use-tax.csv") %>%
  rename(year = 1,
         description = 2,
         gross.sales = 3,
         taxable.sales = 4,
         sales.tax = 5,
         use.tax = 6,
         total.tax = 7, 
         number = 8) %>%
  select(1:8) %>%
  mutate(naics = str_sub(description, 1,4),
         naics = str_replace(naics, "Tota", "Total"),
         description = str_sub(description, 6, -1),
         description = ifelse(description == "", "Total", description))

data.2013 <- read_csv("Data/Business revenue/2013-sales-use-tax.csv") %>%
  rename(year = 1,
         description = 2,
         gross.sales = 3,
         taxable.sales = 4,
         sales.tax = 5,
         use.tax = 6,
         total.tax = 7, 
         number = 8) %>%
  select(1:8) %>%
  mutate(naics = str_sub(description, 1,4),
         naics = str_replace(naics, "Tota", "Total"),
         description = str_sub(description, 6, -1),
         description = ifelse(description == "", "Total", description))

data.2014 <- read_csv("Data/Business revenue/2014-sales-use-tax.csv") %>%
  rename(year = 1,
         description = 2,
         gross.sales = 3,
         taxable.sales = 4,
         sales.tax = 5,
         use.tax = 6,
         total.tax = 7, 
         number = 8) %>%
  select(1:8) %>%
  mutate(naics = str_sub(description, 1,4),
         naics = str_replace(naics, "Tota", "Total"),
         description = str_sub(description, 6, -1),
         description = ifelse(description == "", "Total", description))

data.2015 <- read_csv("Data/Business revenue/2015-sales-use-tax.csv") %>%
  rename(year = 1,
         description = 2,
         gross.sales = 3,
         taxable.sales = 4,
         sales.tax = 5,
         use.tax = 6,
         total.tax = 7, 
         number = 8) %>%
  select(1:8) %>%
  mutate(naics = str_sub(description, 1,4),
         naics = str_replace(naics, "Tota", "Total"),
         description = str_sub(description, 6, -1),
         description = ifelse(description == "", "Total", description))

data.2016 <- read_csv("Data/Business revenue/2016-sales-use-tax.csv") %>%
  rename(year = 1,
         description = 2,
         gross.sales = 3,
         taxable.sales = 4,
         sales.tax = 5,
         use.tax = 6,
         total.tax = 7, 
         number = 8) %>%
  select(1:8) %>%
  mutate(naics = str_sub(description, 1,4),
         naics = str_replace(naics, "Tota", "Total"),
         description = str_sub(description, 6, -1),
         description = ifelse(description == "", "Total", description))


data <- data.2017 %>%
  rbind(data.2003, data.2004, data.2005, data.2006, data.2007, data.2008, data.2009, data.2010, data.2011, data.2012, data.2013, data.2014, data.2015, data.2016) %>%
  mutate(naics = str_replace(naics, "TOTAL", "Total"),
         description = ifelse(is.na(description) & naics == "Total", "Total", description),
         description = ifelse(naics == 2111, "OIL AND GAS EXTRACTION",
                              ifelse(naics == 4852, "Interurban and Rural Bus Transportation",
                                     ifelse(naics == 5181, "DATA PROCESSING",
                                            ifelse(naics == 5173, "Wired and Wireless Telecommunications Carriers",
                                                   ifelse(naics == 4530, "Miscellaneous Store Retailers",
                                                          ifelse(naics == 4870, "Scenic and Sightseeing Transportation",
                                                                 ifelse(naics == 5232, "Securities and Commodity Exchanges", description)))))))) %>%
  drop_na(description)
  

write_csv(data, "Data/Business revenue/sales-use-tax-2003-2017.csv")
