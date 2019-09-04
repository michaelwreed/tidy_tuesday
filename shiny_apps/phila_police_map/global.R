# global

library("shiny")
library("leaflet")
library("tidyverse")
library("ggmap")
library("ggplot2")


readRDS("map_data.RDS") %>%
  sample_n(10000)-> map_data