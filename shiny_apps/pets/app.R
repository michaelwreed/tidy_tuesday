# global

library(tidyverse)
library(ggplot2)
library(lubridate)
library(plotly)
library(shiny)
library(scales)


readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-26/seattle_pets.csv"
  ) -> pets_raw

pets_raw %>%
  mutate(license_issue_date = as.Date(fast_strptime(license_issue_date,
                                                    format = "%b %d %Y",
                                                    lt = FALSE)),
         zip_code = as.numeric(stringr::str_trunc(zip_code, 5, "left", ellipsis = "")),
         species = as.factor(species),
         start_day = floor_date(license_issue_date, "week")
  ) %>%
  filter(!is.na(animals_name),
         species %in% c("Cat", "Dog"),
         between(license_issue_date, as.Date("2017-01-01"), as.Date("2018-12-30"))) %>%
  droplevels() %>% 
  group_by(species, start_day) %>%
  summarize(n = n(),
            pname = first(animals_name, order_by = -n),
            pnum = sum(animals_name==pname)) -> pets

#ui

ui <- fluidPage(
  titlePanel("#TidyTuesday"),
  headerPanel("Cats & Dogs"),
  mainPanel(plotlyOutput('time_series'),
            textOutput("text_box"),
            uiOutput("link"))
)

# server

server <- function(input, output, session) {
  
  output$time_series <- renderPlotly({
    pets %>%
      ggplot(aes(x = start_day,
                 y = n,
                 fill = species,
                 text = pname)) +
      geom_bar(stat = "identity",
               position = "dodge",
               width = 7) +
      scale_x_date(date_breaks = "1 month",
                   labels = date_format("%Y-%b"),
                   expand = c(0,0)) +
      scale_y_continuous(expand = c(.025,0)) +
      theme(axis.text.x = element_text(angle = 45),
            panel.background = element_blank(),
            panel.grid.major.x = element_line(
              linetype = 2,
              color = "black"
            )) +
      labs(x = "Date",
           y = "Number of Pets") -> p
    
    ggplotly(p, tooltip = "text") %>%
      style(hoverinfo = "text")
  })
  
  output$text_box <- renderText("count of licenses issued for cats & dogs in Seattle, in 2017/18 binned by week. hover over a bar to see the most
                                popular name for that species for that week.")
  url <- a("@50_first_data", href="https://twitter.com/50_first_data")
  output$link <- renderUI({tagList("Twitter:", url)})
}

shinyApp(ui, server)