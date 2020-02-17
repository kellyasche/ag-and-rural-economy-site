
# Library -----------------------------------------------------------------

knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
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

color.counties <- scale_color_brewer(palette = "Dark2",
                                     guide = guide_legend(ncol = 3))

mn_counties <- st_read("Data/Shapefiles/county shapefiles/MNCounties_MNDOT.shp", quiet = TRUE) %>%
  ms_simplify(keep = .01, keep_shapes = TRUE)


# Create map --------------------------------------------------------------

ruca.map <- counties.regions %>%
  left_join(mn_counties[,c(4,7)], by = c("countyfp" = "FIPS_CODE")) %>%
  mutate(Dem_Desc = fct_relevel(Dem_Desc, "Entirely rural", "Town/rural mix", "Urban/town/rural mix", "Entirely urban"))


ggplot(filter(ruca.map, Dem_Desc != "Minnesota")) +
  geom_sf(aes(fill = Dem_Desc, geometry = geometry)) +
  scale_fill_manual(values = color.ruca,
                    guide = guide_legend(ncol = 2))+
  theme_sf+
  labs(title="Rura-urban commuting areas") +
  theme(text = element_text(size = 10),
        legend.position = "bottom")

ggsave(filename = "Data/Marnie/ruca-map.svg", dpi = "print", width = 6, height = 4)
