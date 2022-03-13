library(dash)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(plotly)
library(purrr)
library(ggthemes)

# Read in data
df <- read_csv("data/videoGame.csv")

# List of Years
year_list <- c(2013, 2014, 2015, 2016)

# Setup app and layout/frontend
app <- Dash$new()

app$layout(
  dbcContainer(
    list(
      dccGraph(id='plot-area'),
      dccDropdown(
        id='col-select',
        options = year_list %>%
          purrr::map(function(col) list(label = col, value = col)),
        value=c(2013, 2014, 2015, 2016),
        placeholder='Select Year',
        multi=F)
    )
  )
)

app$callback(
  output('plot-area', 'figure'),
  list(input('col-select', 'value')),
  # function for widget inputs and figure outputs
  function(xcol) {
    df <- df %>% drop_na() %>%
      select(Platform, Year, Critic_Score) %>%
      group_by(Platform, Year) %>%
      summarise(Mean_critic_score = mean(Critic_Score))
    p <- df %>%
      filter(Year == xcol) %>%
      ggplot() +
      aes(y = Mean_critic_score,
          x = as.factor(Platform)) +
      geom_col(show.legend = FALSE) +
      labs(y = "Mean Critic Score", x = "Years",
           title = "Mean Critic Score by Years") +
      ggthemes::scale_color_tableau()
    ggplotly(p)
    
  }
)

app$run_server(host = '0.0.0.0')
