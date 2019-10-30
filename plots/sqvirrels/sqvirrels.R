#SQVIRELS
library(tidyverse)
library(lubridate)
library(ggmap)
library(gganimate)

#register your google maps API key with ggmap::register_google(key = ). Mine is hidden.
source("secret/google_maps_api.R")

nyc_squirrels <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv"
  )


nyc_squirrels %>%
  mutate(
    date = mdy(date),
    jitter = case_when( # random times within the shifts (7:30 - 10:00am, 4:00 - 6:30pm)
      shift == "AM" ~ date + dhours(7) + dminutes(30 + sample(0:150, replace = TRUE)),
      shift == "PM" ~ date + dhours(16) + dminutes(sample(0:150, replace = TRUE))
    )
  ) %>%
  select(unique_squirrel_id,
    coat = primary_fur_color,
         jitter,
         lat,
         long) %>%
  filter(!is.na(coat)) ->
  map_data


ggmap(
  ggmap = get_map(
    location = "Central Park",
    zoom = 15,
    source = "stamen",
    maptype = "terrain-background",
    crop = FALSE
  ),
  extent = "device",
  base_layer = ggplot(data = map_data,
                      aes(x = long, y = lat)),
  maprange = TRUE
) +
  geom_point(aes(color = coat,
                 group = unique_squirrel_id), # force groups of size 1 to prevent object permanence
             size = 4,
             alpha = 0.65) +
  scale_color_manual(
    name = "Fur Color",
    values = c('#000000', '#784b41', '#878584'),
    labels = c("Black", "Cinnamon", "Gray")
  ) +
  theme(
    legend.position = c(1, 0),
    legend.justification = c(1, -.25),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    title = element_text(size = 14,
                         face = "italic")
  ) +
 labs(title = "The Great Squirrel Census",
       subtitle = "{frame_time}",
       caption = "#TidyTuesday @50_first_data")+
  transition_time(jitter) +
  enter_fade() +
  exit_fade() -> p

animate(p,
        nframes = 100,
        fps = 10,
        detail = 25)
