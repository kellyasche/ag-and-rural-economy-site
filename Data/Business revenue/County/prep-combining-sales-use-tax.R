library(tidyverse)


# Join doc ----------------------------------------------------------------

counties.regions <- read_csv("Data/Join docs/county_regions.csv") %>%
  mutate(countyfp = formatC(countyfp, width = 3, flag = "0"),
         Name = str_to_title(Name),
         Name = str_replace(Name, "Q", "q"),
         Name = str_replace(Name, "Of The", "of the"),
         Dem_Desc = ifelse(Name == "Minnesota", "Minnesota", Dem_Desc),
         Name = str_replace(Name, "Mcleod", "McLeod"),
         planning.region = str_replace(planning.region, " Minnesota", ""),
         edr = str_replace(edr, "  ", " "),
         Name = str_to_title(Name))



# prep gross sales --------------------------------------------------------

data.2003 <- read_csv("Data/Business revenue/County/2003-sales-use-tax-county.csv") %>%
  mutate(year = 2003) %>%
  rename(gross.sales = 2,
         taxable.sales = 3,
         sales.tax = 4,
         use.tax = 5,
         total.tax = 6,
         number = 7) %>%
  filter(!COUNTY %in% c("UNKNOWN/OTHER", "NON-MINNESOTA"))

data.2004 <- read_csv("Data/Business revenue/County/2004-sales-use-tax-county.csv") %>%
  mutate(year = 2004) %>%
  rename(gross.sales = 2,
         taxable.sales = 3,
         sales.tax = 4,
         use.tax = 5,
         total.tax = 6,
         number = 7) %>%
  filter(!COUNTY %in% c("UNKNOWN/OTHER", "NON-MINNESOTA"))


data.2005 <- read_csv("Data/Business revenue/County/2005-sales-use-tax-county.csv") %>%
  mutate(year = 2005) %>%
  rename(gross.sales = 2,
         taxable.sales = 3,
         sales.tax = 4,
         use.tax = 5,
         total.tax = 6,
         number = 7) %>%
  filter(!COUNTY %in% c("UNKNOWN/OTHER", "NON-MINNESOTA"))


data.2006 <- read_csv("Data/Business revenue/County/2006-sales-use-tax-county.csv") %>%
  mutate(year = 2006) %>%
  rename(gross.sales = 2,
         taxable.sales = 3,
         sales.tax = 4,
         use.tax = 5,
         total.tax = 6,
         number = 7) %>%
  filter(!COUNTY %in% c("UNKNOWN/OTHER", "NON-MINNESOTA"))


data.2007 <- read_csv("Data/Business revenue/County/2007-sales-use-tax-county.csv") %>%
  mutate(year = 2007) %>%
  rename(gross.sales = 2,
         taxable.sales = 3,
         sales.tax = 4,
         use.tax = 5,
         total.tax = 6,
         number = 7) %>%
  filter(!COUNTY %in% c("UNKNOWN/OTHER", "NON-MINNESOTA"))


data.2008 <- read_csv("Data/Business revenue/County/2008-sales-use-tax-county.csv") %>%
  rename(year = 1,
         gross.sales = 3,
         taxable.sales = 4,
         sales.tax = 5,
         use.tax = 6,
         total.tax = 7,
         number = 8) %>%
  select(1:8) %>%
  mutate(year = ifelse(is.na(year), 2008, year))

data.2009 <- read_csv("Data/Business revenue/County/2009-sales-use-tax-county.csv") %>%
  rename(year = 1,
         gross.sales = 3,
         taxable.sales = 4,
         sales.tax = 5,
         use.tax = 6,
         total.tax = 7,
         number = 8) %>%
  select(1:8)%>%
  mutate(year = ifelse(is.na(year), 2009, year))

data.2010 <- read_csv("Data/Business revenue/County/2010-sales-use-tax-county.csv") %>%
  mutate(year = 2010) %>%
  rename(COUNTY = 1,
         gross.sales = 2,
         taxable.sales = 3,
         sales.tax = 4,
         use.tax = 5,
         total.tax = 6,
         number = 7)

data.2011 <- read_csv("Data/Business revenue/County/2011-sales-use-tax-county.csv") %>%
  mutate(year = 2011) %>%
  rename(COUNTY = 1,
         gross.sales = 2,
         taxable.sales = 3,
         sales.tax = 4,
         use.tax = 5,
         total.tax = 6,
         number = 7)

data.2012 <- read_csv("Data/Business revenue/County/2012-sales-use-tax-county.csv") %>%
  mutate(year = 2012) %>%
  rename(COUNTY = 1,
         gross.sales = 2,
         taxable.sales = 3,
         sales.tax = 4,
         use.tax = 5,
         total.tax = 6,
         number = 7)

data.2013 <- read_csv("Data/Business revenue/County/2013-sales-use-tax-county.csv") %>%
  mutate(year = 2013) %>%
  rename(COUNTY = 1,
         gross.sales = 2,
         taxable.sales = 3,
         sales.tax = 4,
         use.tax = 5,
         total.tax = 6,
         number = 7)

data.2014 <- read_csv("Data/Business revenue/County/2014-sales-use-tax-county.csv") %>%
  mutate(year = 2014) %>%
  rename(COUNTY = 1,
         gross.sales = 2,
         taxable.sales = 3,
         sales.tax = 4,
         use.tax = 5,
         total.tax = 6,
         number = 7)

data.2015 <- read_csv("Data/Business revenue/County/2015-sales-use-tax-county.csv") %>%
  mutate(year = 2015) %>%
  rename(COUNTY = 1,
         gross.sales = 2,
         taxable.sales = 3,
         sales.tax = 4,
         use.tax = 5,
         total.tax = 6,
         number = 7)

data.2016 <- read_csv("Data/Business revenue/County/2016-sales-use-tax-county.csv") %>%
  mutate(year = 2016) %>%
  rename(COUNTY = 1,
         gross.sales = 2,
         taxable.sales = 3,
         sales.tax = 4,
         use.tax = 5,
         total.tax = 6,
         number = 7)

data.2017 <- read_csv("Data/Business revenue/County/2017-sales-use-tax-county.csv") %>%
  mutate(year = 2017) %>%
  rename(COUNTY = 1,
         gross.sales = 2,
         taxable.sales = 3,
         sales.tax = 4,
         use.tax = 5,
         total.tax = 6,
         number = 7)


# Combine -----------------------------------------------------------------

data <- data.2003 %>%
  rbind(data.2004, data.2005, data.2006, data.2007, data.2008, data.2009, data.2010, data.2011, data.2012, data.2013, data.2014, data.2015, data.2016, data.2017)  %>%
  mutate(COUNTY = trimws(COUNTY, which = c("both"))) %>%
  filter(!COUNTY %in% c("UNKNOWN/OTHER", "NON-MINNESOTA", "Non-Minnesota Co.", "Non-Minnesota Co", "Non-Minnesota County", "Non-Minnesota Co", "Mn Unknown County")) %>%
  rename(County = 1) %>%
  mutate(County = str_to_title(County),
         County = ifelse(is.na(County), "Total", County),
         County = str_replace(County, "Total", "Minnesota"),
         County = str_replace(County, "Lesueur", "Le Sueur"),
         County = str_replace(County, "St Louis", "St. Louis")) %>%
  left_join(counties.regions, by = c("County" = "Name"))

write_csv(data, "Data/Business revenue/County/sales-use-tax-county.csv")


# Organize into greater mn ------------------------------------------------

master <- read_csv("Data/Business revenue/County/sales-use-tax-county.csv") %>%
  mutate(Dem_Desc = fct_relevel(Dem_Desc, "Entirely rural", "Town/rural mix", "Urban/town/rural mix", "Entirely urban"),
         planning.region = fct_relevel(planning.region, "Northwest", "Northeast", "Central", "Seven County Mpls-St Paul", "Southwest", "Southeast"),
         edr = fct_relevel(edr, "EDR 1 - Northwest", "EDR 2 - Headwaters", "EDR 3 - Arrowhead", "EDR 4 - West Central", "EDR 5 - North Central", "EDR 6E- Southwest Central", "EDR 6W- Upper Minnesota Valley", "EDR 7E- East Central", "EDR 7W- Central", "EDR 8 - Southwest", "EDR 9 - South Central", "EDR 10 - Southeast", "EDR 11 - 7 County Twin Cities", "Minnesota"))


data.pr <- master %>%
  gather(key = "sales.type", value = "dollars", 2:6) %>%
  mutate(dollars = as.numeric(dollars)) %>%
  group_by(planning.region, year, sales.type) %>%
  summarise(dollars = sum(dollars, na.rm = TRUE)) %>%
  ungroup()

data.metro.non <- data.pr %>%
  group_by(year, sales.type) %>%
  summarise(dollars.metro = sum(dollars[planning.region == "Seven County Mpls-St Paul"]),
            dollars.non.metro = sum(dollars[planning.region != "Seven County Mpls-St Paul"])) %>%
  ungroup() %>%
  gather(key = "metro.non", value = "dollars", 3,4) %>%
  mutate(metro.non = ifelse(metro.non == "dollars.metro", "Metro", "Non-metro"))

data.edr <- master %>%
  gather(key = "sales.type", value = "dollars", 2:6) %>%
  mutate(dollars = as.numeric(dollars)) %>%
  group_by(edr, year, sales.type) %>%
  summarise(dollars = sum(dollars, na.rm = TRUE)) %>%
  ungroup()

data.ruca <- master %>%
  gather(key = "sales.type", value = "dollars", 2:6) %>%
  mutate(dollars = as.numeric(dollars)) %>%
  group_by(Dem_Desc, year, sales.type) %>%
  summarise(dollars = sum(dollars, na.rm = TRUE)) %>%
  ungroup()

write_csv(data.metro.non, "Data/Business revenue/County/gross-sales-metro-non.csv")

