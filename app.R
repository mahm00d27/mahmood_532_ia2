library(dash)
library(dashBootstrapComponents)
library(dashHtmlComponents)
library(dashCoreComponents)
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
year_list <- c(2013, 2014, 2015, 2016, 2017, 2018)

# Setup app and layout/frontend
app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout(
  dbcContainer(
    list(
      htmlH1(
        list(
          'Video Game Sales Analytics App (Critic Score)' 
        ),
        style=list(
          color='blue', 
          fontSize=40,
          backgroundColor='white',
          textAlign='left'
        )
        
      ),
      
      htmlDiv(
        list(
          htmlP(
            'Year'
          ),
          dccDropdown(
            id = 'col-select',
            value= year_list[1],
            options = year_list %>%
              purrr::map(function (col) list(label = col, value = col)),
            placeholder='Select Year',
            multi=FALSE
          ),
          dccGraph(id='plot-area')
        )
      )
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
      geom_bar(stat = 'identity') +
      ggthemes::scale_color_tableau()
    ggplotly(p)
    
  }
)

app$run_server(host = '0.0.0.0')