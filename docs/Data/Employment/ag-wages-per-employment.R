
# Lirbary -----------------------------------------------------------------
library(tidyverse)
library(sf)
library(ggrepel)
library(scales)
library(shiny)
library(shinycssloaders)
library(ggiraph)
library(kableExtra)
library(rmapshaper)
library(cowplot)
library(DT)


# Themes ------------------------------------------------------------------

theme_bar <- theme_bw() +
  theme(panel.grid.major = element_line(color = "grey70", size = 0.1),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(face = "bold"),
        panel.border = element_blank(),
        legend.background = element_rect(fill = "transparent", color = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        text = element_text(size = 15),
        plot.caption = element_text(hjust = 0.5, face = "italic"),
        legend.text = element_text(margin = margin(l = 2, r = 5)))

theme_line <- theme_bw() +
  theme(legend.background = element_rect(fill = "transparent", color = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey70", size = 0.1),
        axis.ticks = element_blank(),
        axis.text = element_text(face = "bold"),
        panel.border = element_blank(),
        text = element_text(size = 15),
        plot.caption = element_text(hjust = 0.5, face = "italic"),
        legend.text = element_text(margin = margin(l = 2, r = 5)))

theme_sf <- theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "white"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        text = element_text(size = 15),
        plot.caption = element_text(hjust = 0.5, face = "italic"),
        legend.text = element_text(margin = margin(l = 2, r = 5)))

counties.regions <- read_csv("Data/Join docs/county_regions.csv") %>%
  mutate(countyfp = formatC(countyfp, width = 3, flag = "0"),
         Name = str_to_title(Name),
         Name = str_replace(Name, "Q", "q"),
         Name = str_replace(Name, "Of The", "of the"),
         Dem_Desc = ifelse(Name == "Minnesota", "Minnesota", Dem_Desc),
         Name = str_replace(Name, "Mcleod", "McLeod"),
         planning.region = str_replace(planning.region, " Minnesota", ""),
         edr = str_replace(edr, "  ", " "))

edr.pr <- counties.regions %>%
  distinct(edr, .keep_all = TRUE) %>%
  select(5,6) %>%
  mutate(edr = str_replace(edr, "  ", " "),
         planning.region = str_replace(planning.region, " Minnesota", ""))


color.ruca <- c("Entirely rural" = "#5CA81F", "Town/rural mix" = "#C7EF99", "Urban/town/rural mix" = "#d8b365", "Entirely urban" = "#a6611a", "Minnesota" = "black")

color.pr <- c("Northwest" = "#810f7c","Northeast" = "#fe9929", "Central" = "#076324", "Seven County Mpls-St Paul" = "#d8b365", "Southwest" = "#1f78b4", "Southeast" = "#d7301f", "Minnesota" = "black")

color.edr <- c("EDR 1 - Northwest" = "#b3cde3", "EDR 2 - Headwaters" = "#8c96c6", "EDR 3 - Arrowhead" = "#fe9929", "EDR 4 - West Central" = "#8856a7", "EDR 5 - North Central" = "#810f7c", "EDR 6E- Southwest Central" = "#e5f5f9", "EDR 6W- Upper Minnesota Valley" = "#bdc9e1", "EDR 7E- East Central" = "#99d8c9", "EDR 7W- Central" = "#2ca25f", "EDR 8 - Southwest" = "#74a9cf", "EDR 9 - South Central" = "#0570b0", "EDR 10 - Southeast" = "#d7301f", "EDR 11 - 7 County Twin Cities" = "#d8b365", "Minnesota" = "black")

color.label.edr <- c("EDR 1" = "#b3cde3", "EDR 2" = "#8c96c6", "EDR 3" = "#fe9929", "EDR 4" = "#8856a7", "EDR 5" = "#810f7c", "EDR 6E" = "#e5f5f9", "EDR 6W" = "#bdc9e1", "EDR 7E" = "#99d8c9", "EDR 7W" = "#2ca25f", "EDR 8" = "#74a9cf", "EDR 9" = "#0570b0", "EDR 10" = "#d7301f", "EDR 11" = "#d8b365", "Minnesota" = "black")

color.counties <- scale_color_brewer(palette = "Dark2",
                                     guide = guide_legend(ncol = 3))

color.ag.ind <- c("Crop Production" = "#a6cee3", "Animal Production and Aquaculture" = "#1f78b4", "Support Activities for Crop Production" = "#b2df8a", "Support Activities for Animal Production" = "#33a02c", "Food Manufacturing" = "#fb9a99", "Agricultural Implement Manufacturing" = "#e31a1c", "Farm Product Raw Material Merchant Wholesalers" = "#fdbf6f", "Farm Supplies Merchant Wholesalers" = "#ff7f00", "Pesticide, Fertilizer, and Other Agricultural Chemical Manufacturing" = "#cab2d6", "Total, All Industries" = "black", "NA" = "white")

mn_counties <- st_read("Data/Shapefiles/county shapefiles/MNCounties_MNDOT.shp", quiet = TRUE) %>%
  ms_simplify(keep = .01, keep_shapes = TRUE)


# Importing and organizing masters ----------------------------------------

ag.ind.wages.county <- read_csv("Data/Earnings/Master-ag-related-ind-wages-counties.csv")

ag.ind.wages.pr <- read_csv("Data/Earnings/Master-ag-related-ind-wages-pr.csv") %>%
  mutate(planning.region = str_replace(planning.region, " Minnesota", ""))

ag.ind.wages.edr <- read_csv("Data/Earnings/Master-ag-related-ind-wages-edr.csv") %>%
  left_join(edr.pr, by = "edr") %>%
  mutate(planning.region = fct_relevel(planning.region, "Northwest", "Northeast", "Central", "Seven County Mpls-St Paul", "Southwest", "Southeast"))

master.ag.emp.county <- read_csv("Data/Employment/Master-agrelated-emp-county.csv")

master.ag.emp.comb.county <- master.ag.emp.county %>%
  group_by(periodyear, Name, countyfp, Dem_Desc, edr, planning.region) %>%
  summarise(ag.emp = sum(emp[naicstitle != "Total, All Industries"]),
            total.emp = sum(emp[naicstitle == "Total, All Industries"])) %>%
  ungroup()

master.ag.emp.ruca <- read_csv("Data/Employment/Master-agrelated-emp-ruca.csv") %>%
  mutate(Dem_Desc = fct_relevel(Dem_Desc, "Entirely rural", "Town/rural mix", "Urban/town/rural mix", "Entirely urban"))

master.ag.emp.comb.ruca <- master.ag.emp.ruca %>%
  group_by(periodyear, Dem_Desc) %>%
  summarise(ag.emp = sum(emp[naicstitle != "Total, All Industries"]),
            total.emp = sum(emp[naicstitle == "Total, All Industries"])) %>%
  ungroup()

master.ag.emp.pr <- read_csv("Data/Employment/Master-agrelated-emp-pr.csv") %>%
  mutate(planning.region = str_replace(planning.region, ", MN", ""),
         planning.region = fct_relevel(planning.region, "Northwest", "Northeast", "Central", "Seven County Mpls-St Paul", "Southwest", "Southeast"))

master.ag.emp.comb.pr <- master.ag.emp.pr %>%
  group_by(periodyear, planning.region) %>%
  summarise(ag.emp = sum(emp[naicstitle != "Total, All Industries"]),
            total.emp = sum(emp[naicstitle == "Total, All Industries"])) %>%
  ungroup()


master.ag.emp.edr <- read_csv("Data/Employment/Master-agrelated-emp-edr.csv") %>%
  mutate(planning.region = str_replace(planning.region, ", MN", ""),
         planning.region = fct_relevel(planning.region, "Northwest", "Northeast", "Central", "Seven County Mpls-St Paul", "Southwest", "Southeast"),
         edr = fct_relevel(edr, "EDR 1 - Northwest", "EDR 2 - Headwaters", "EDR 3 - Arrowhead", "EDR 4 - West Central", "EDR 5 - North Central", "EDR 6E- Southwest Central", "EDR 6W- Upper Minnesota Valley", "EDR 7E- East Central", "EDR 7W- Central", "EDR 8 - Southwest", "EDR 9 - South Central", "EDR 10 - Southeast", "EDR 11 - 7 County Twin Cities", "Minnesota"))

master.ag.emp.comb.edr <- master.ag.emp.edr %>%
  group_by(periodyear, edr, planning.region) %>%
  summarise(ag.emp = sum(emp[naicstitle != "Total, All Industries"]),
            total.emp = sum(emp[naicstitle == "Total, All Industries"])) %>%
  ungroup()

ag.wages.emp.county <- ag.ind.wages.county %>%
  left_join(master.ag.emp.county[,c(1,2,5,6)], by = c("countyfp", "periodyear", "naicstitle")) %>%
  mutate(wages.emp = wages / emp)

ag.wages.emp.ruca <- ag.wages.emp.county %>%
  group_by(periodyear, Dem_Desc, naicstitle) %>%
  summarise(wages = sum(wages, na.rm = TRUE),
            emp = sum(emp, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(wages.emp = wages / emp,
         Dem_Desc = fct_relevel(Dem_Desc, "Entirely rural", "Town/rural mix", "Urban/town/rural mix", "Entirely urban"))

ag.wages.emp.pr <- ag.ind.wages.pr %>%
  left_join(master.ag.emp.pr[,c(1,2,3,5)], by = c("planning.region", "periodyear", "naicstitle")) %>%
  mutate(wages.emp = wages / emp,
         planning.region = fct_relevel(planning.region, "Northwest", "Northeast", "Central", "Seven County Mpls-St Paul", "Southwest", "Southeast"))

ag.wages.emp.edr <- ag.ind.wages.edr %>%
  left_join(master.ag.emp.edr[,c(1,2,3,5)], by = c("edr", "periodyear", "naicstitle")) %>%
  mutate(wages.emp = wages / emp,
         planning.region = fct_relevel(planning.region, "Northwest", "Northeast", "Central", "Seven County Mpls-St Paul", "Southwest", "Southeast"),
         edr = fct_relevel(edr, "EDR 1 - Northwest", "EDR 2 - Headwaters", "EDR 3 - Arrowhead", "EDR 4 - West Central", "EDR 5 - North Central", "EDR 6E- Southwest Central", "EDR 6W- Upper Minnesota Valley", "EDR 7E- East Central", "EDR 7W- Central", "EDR 8 - Southwest", "EDR 9 - South Central", "EDR 10 - Southeast", "EDR 11 - 7 County Twin Cities", "Minnesota"))



# Prep wages per employment -----------------------------------------------

ag.wages.emp.comb.ruca <- ag.wages.emp.ruca %>%
  group_by(periodyear, Dem_Desc) %>%
  summarise(ag.wages = sum(wages[naicstitle != "Total, All Industries"]),
            total.wages = sum(wages[naicstitle == "Total, All Industries"]),
            ag.emp = sum(emp[naicstitle != "Total, All Industries"]),
            total.emp = sum(emp[naicstitle == "Total, All Industries"])) %>%
  ungroup() %>%
  mutate(ag.wages.per.emp = ag.wages / ag.emp,
         total.wages.per.emp = total.wages / total.emp)

ag.wages.emp.comb.pr <- ag.wages.emp.pr %>%
  group_by(periodyear, planning.region) %>%
  summarise(ag.wages = sum(wages[naicstitle != "Total, All Industries"]),
            total.wages = sum(wages[naicstitle == "Total, All Industries"]),
            ag.emp = sum(emp[naicstitle != "Total, All Industries"]),
            total.emp = sum(emp[naicstitle == "Total, All Industries"])) %>%
  ungroup() %>%
  mutate(ag.wages.per.emp = ag.wages / ag.emp,
         total.wages.per.emp = total.wages / total.emp)

ag.wages.emp.comb.edr <- ag.wages.emp.edr %>%
  group_by(periodyear, edr, planning.region) %>%
  summarise(ag.wages = sum(wages[naicstitle != "Total, All Industries"]),
            total.wages = sum(wages[naicstitle == "Total, All Industries"]),
            ag.emp = sum(emp[naicstitle != "Total, All Industries"]),
            total.emp = sum(emp[naicstitle == "Total, All Industries"])) %>%
  ungroup() %>%
  mutate(ag.wages.per.emp = ag.wages / ag.emp,
         total.wages.per.emp = total.wages / total.emp)

ag.wages.emp.comb.county <- ag.wages.emp.county %>%
  group_by(periodyear, areaname, countyfp) %>%
  summarise(ag.wages = sum(wages[naicstitle != "Total, All Industries"]),
            total.wages = sum(wages[naicstitle == "Total, All Industries"]),
            ag.emp = sum(emp[naicstitle != "Total, All Industries"]),
            total.emp = sum(emp[naicstitle == "Total, All Industries"])) %>%
  ungroup() %>%
  mutate(ag.wages.per.emp = ag.wages / ag.emp,
         total.wages.per.emp = total.wages / total.emp,
         ag.wages.per.emp = ifelse(areaname == "Mahnomen", NA, ag.wages.per.emp)) %>%
  left_join(mn_counties[,c(4,7)], by = c("countyfp" = "FIPS_CODE"))


# Create 2018 map ---------------------------------------------------------

ggplot(filter(ag.wages.emp.comb.county, periodyear == 2018)) +
  geom_sf_interactive(aes(fill = ag.wages.per.emp, tooltip = paste(areaname, "\nYear: ", periodyear, "\nTotal wages in ag-related industries: ", dollar(ag.wages), "\nTotal employment in ag-related industries: ", comma(ag.emp), "\nWages per employment: ", dollar(ag.wages.per.emp), sep = ""), data_id = areaname, geometry = geometry)) +
  scale_fill_gradient(low = "white",
                      high = "#004529",
                      labels = dollar)+
  theme_sf+
  labs(title="2018 ag-related industry wages per employment")

ggsave(filename = "Data/Employment/ag-wages-per-employment-map.png", type = "cairo", dpi = "print", width = 7, height = 6)
