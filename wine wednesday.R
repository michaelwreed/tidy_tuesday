#TidyTuesday wine ratings
library(tidyverse)

wine_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")

wine_ratings %>%
  select(-1) -> wine

#EXPLORING VARS

# point (rating)
range(wine$points)


wine %>%
  ggplot(
    aes(x = points)
  ) +
  geom_histogram(binwidth = 1,
                 #looks
                 fill = "white",
                 color = "darkblue")

wine %>%
  ggplot(aes(x = points, fill = taster_name, color = taster_name)) +
  geom_density(alpha = 0.2)

# price
range(wine$price, na.rm = TRUE)

sum(is.na(wine$price)) # why are ~9,000 wines price-less?


wine %>%
  
  ggplot(
    aes(x = price)
  ) +

                     
geom_histogram(
  
                     fill = "white",
                     color = "darkblue"
) +
  
  scale_y_continuous(trans = "log10")


wine %>%
  ggplot(aes(x = price, fill = taster_name, color = taster_name)) +
  geom_density(alpha = 0.1) +
  scale_x_continuous(trans = "log")

# variety

unique(wine$variety)
n_distinct(wine$variety)

# these are in unicode which is sick but let's not
wine <- wine %>%
  mutate(variety = iconv(variety, from = "utf-8", to = "ascii"))

unique(wine$variety)
n_distinct(wine$variety)

# from 708 to 639 distinct varieties.. still a lot to work with. maybe split them at hyphens?

# first row number
wine <- wine %>%
  mutate(id = row_number())

wine %>%
  separate(variety, into = c("var1", "var2", "var3"), sep = "-") %>% 
  filter(!is.na(var3)) %>% select(var1:var3, everything()) %>% head()

# TIL: A G-S-M is a Grenache Syrah Mourvedre blend. gotta recode those.

#Also a lot of var2 values look like "style Red Blend" which means they can be dropped and replaced
# with a "blend" flag

# to be continued...?

