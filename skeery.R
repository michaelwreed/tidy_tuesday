library(tidyverse)
horror_movies <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")
glimpse(horror_movies)
View(horror_movies)

# proportion of NAs per column 
horror_movies %>%
  map(~sum(is.na(.x))/nrow(horror_movies))

# date histogram by month
horror_movies %>%
  mutate(release_date = as.Date(release_date,
                                format = '%d-%b-%y')) %>%
  ggplot(aes(x = release_date)) +
  geom_bar() +
  scale_x_date(date_breaks = "months") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45,
                                   vjust = 0.5))

# months are a little cluttered. let's try quarters

horror_movies %>%
  mutate(release_date = as.Date(release_date,
                                format = '%d-%b-%y')) %>%
  ggplot(aes(x = release_date)) +
  geom_bar() +
  scale_x_date(date_breaks = "3 months") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45,
                                   vjust = 0.5))

# strong seasonal trend. halloween movies??

horror_movies %>%
  mutate(release_date = as.Date(release_date,
                                format = '%d-%b-%y'),
         to_halloween = -difftime(release_date,
           lubridate::make_date(year = lubridate::year(release_date),
                                   month = 10,
                                   day = 31),
                                 unit = "days")) %>%
  ggplot(aes(x = to_halloween)) +
  geom_density(fill = "white") +
  geom_vline(xintercept = 0, color = "orange", lty = "dashed") +
  geom_point(data = data.frame(x = sample(300:-50, 75),
                               y = sample(seq(0.001, 0.005, by = 0.0001), 75, replace = TRUE)),
             aes(x = x,
                 y = y),
             shape = 8,
             color = "white",
             size = sample(seq(.1, .4, .001), 75, replace = TRUE)) +
  geom_point(aes(x = 285, y = 0.005), size = 11, color = "#dce736", shape = 19) +
  scale_x_reverse(name = "Days to Halloween",
                     breaks = seq(300, 0, -30),
                  expand = expand_scale(add = c(0, 0))) +
  scale_y_continuous(name = "Horror Movies",
                     expand = expand_scale(add = c(0, 0.0005))) +
  theme_classic() +
  theme(panel.background = element_rect(fill = "black"),
        axis.title.x = element_text(),
        axis.text.x = element_text(angle = 45,
                                   vjust = 0.5),
        axis.text.y = element_blank(),
      plot.title = element_text(hjust = 0.5)) +
  ggtitle("Spooky Szn")

