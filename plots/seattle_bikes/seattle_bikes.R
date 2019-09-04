#TidyTuesday
options(stringsAsFactors = FALSE)

# uncomment these lines to fetch data from original source
# bikes <- readr::read_csv(
#  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-02/bike_traffic.csv",
#  col_types = list(
#    col_datetime(format = "%m/%d/%Y %I:%M:%S %p"), 
#    col_character(), 
#    col_character(), 
#    col_double(), 
#    col_double()))

# faster to use bikes.RDS in the "tidy_tuesday/seattle_bikes" folder
bikes <- readRDS("bikes.RDS")

glimpse(bikes)

library(rgdal)
library(ggplot2)
library(gganimate)
library(ggmap)
theme_set(theme_bw())

crossing <- as.character(c("Broadway Cycle Track North Of E Union St",
              "Burke Gilman Trail",
              "Elliot Bay Trail",
              "39th Ave NE Greenway at NE 62nd St",
              "MTS Trail",
              "Sealth Trail",
              "NW 58th St Greenway at 22nd Ave"))

latitude <- as.double(c(47.614219,
              47.679552,
              47.619522,
              47.673787,
              47.590467,
              47.528034,
              47.670928))

longitude <- as.double(c(-122.320824,
               -122.265282,
               -122.361083,
               -122.285742,
               -122.286918,
               -122.281056,
               -122.384927))

seattle <- data.frame(crossing, latitude, longitude)

right_join(bikes, seattle, by = "crossing") -> bikes_map

glimpse(bikes_map)

ggmap(get_map(location = c(lon = -122.32, lat = 47.64),
              zoom = 12,
              #source = "stamen",
              maptype = "terrain")) +
  geom_point(data = bikes_map %>%
              group_by(date = date(date), crossing, latitude, longitude) %>%
              summarise(bike_count = sum(bike_count)) %>%
               filter(bike_count < 4000),
               aes(#label = bike_count,
                   group = date,
                   x = longitude,
                   y = latitude,
                   color = bike_count,
                   alpha = ifelse(is.na(bike_count), 0, 1),
                   size = bike_count)) +
  scale_color_gradient2(name = "Bikes \n per Day", low = "green", mid = "yellow", high = "red", midpoint = 1500, na.value = "white") +
  scale_size_continuous(range = c(0, 20)) +
  guides(alpha = "none", size = "none") +
  transition_time(date) +
  labs(title = "#TidyTuesday Seattle Bike Counters",
       subtitle = "{frame_time}",
       caption = "@50_first_data") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank()) -> p
  animate(p, fps = 6.5)
