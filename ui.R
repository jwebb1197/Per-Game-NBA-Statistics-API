library(tidyverse)
library(httr)
library(rvest)
library(devtools)
library(jsonlite)
library(tidyverse)
library(plotly)
library(ggplot2)
library(shiny)
library(plotrix)
library(shinythemes)




nba <- read_html("https://www.basketball-reference.com/leagues/NBA_2020_per_game.html")
nba_table <- html_table(nba)
player_stats <- data.frame(nba_table)
player_stats <- player_stats[complete.cases(player_stats), ]
player_stats[, -c(2, 3, 5)] <- lapply(player_stats[, -c(2, 3, 5)], as.numeric)
players <- player_stats %>%
  select(Player) %>%
  unique()
teams <- player_stats %>%
  select(Tm) %>%
  unique() %>%
  arrange(Tm)


library(shiny)

shinyUI(
  fluidPage(
    theme = shinytheme("united"),

    # App Title
    titlePanel(h1("NBA Player Statistics API", align = "center")),
    navbarPage(
      "Menu",
      tabPanel(
        "Visualizations",

        # Sidebar layout with input and output definitions
        sidebarLayout(
          position = "left",

          # Sidebar panel for inputs
          sidebarPanel(

            # Input: Slider for the number of bins

            selectInput(
              inputId = "team",
              label = h3("select a team: "),
              choice = teams,
              selected = "NOP"
            ),

            selectInput(
              inputId = "xaxis",
              label = h3("select statistic 1: "),
              choice = colnames(player_stats[, -c(2, 3, 5)]) %>% sort(),
              selected = "X2P."
            ),


            selectInput(
              inputId = "yaxis",
              label = h3("select statistic 2: "),
              choice = colnames(player_stats[, -c(2, 3, 5)]) %>% sort(),
              selected = "FG."
            ),

            DT::dataTableOutput("dtab")
          ),




          # Main panel for displaying outputs
          mainPanel(
            tabsetPanel(
              # put it back here
              tabPanel("Scatterplot", plotlyOutput("plot3")),

              tabPanel(
                "Barplots",
                plotOutput("plot1", width = "100%"),
                plotOutput("plot2", width = "100%")
              ),


              tabPanel(
                "Pie Chart",
                selectInput(
                  inputId = "players",
                  label = h3("Select any NBA Player: "),

                  choice = player_stats %>%
                    separate(Player, c("first_name", "last_name"), sep = " ") %>%
                    arrange(last_name) %>%
                    unite(Player, first_name:last_name, sep = " ") %>%
                    select(Player),

                  selected = "Stephen Curry"
                ),

                column(8, plotlyOutput("their_stats", height = 900, width = "130%"))
              )
            )
          )
        )
      ),

      tabPanel(
        "Data Table: Per Game Player Stats of 2019-20 NBA Season",
        # Main panel for displaying outputs
        mainPanel(tableOutput("data"))
      ),
      tabPanel(
        "References",
        # Main panel for displaying outputs
        mainPanel(
          "API created by: Julia Webb, Patrick Soong, Kideok Kwon, Haoyu Ren;                
                              Web scraped data from: https://www.basketball-reference.com/leagues/NBA_2020_per_game.html "
        )
      )
    )
  )
)
