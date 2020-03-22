
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

shinyServer(function(input, output, session) {
  picked_team <- reactive({
    player_stats %>%
      filter(Tm == input$team) %>%
      select(Player, Tm, input$xaxis, input$yaxis)
  })


  picked_player <- reactive({
    player_stats %>%
      separate(Player, c("first_name", "last_name"), sep = " ") %>%
      arrange(last_name) %>%
      unite(Player, first_name:last_name, sep = " ") %>%
      filter(Player == input$players) %>%
      select(AST, STL, BLK, TRB, PTS)
  })

  player_team <- reactive({
    player_stats %>%
      separate(Player, c("first_name", "last_name"), sep = " ") %>%
      arrange(last_name) %>%
      unite(Player, first_name:last_name, sep = " ") %>%
      filter(Tm == input$team) %>%
      rename(X = input$xaxis, Y = input$yaxis)
  })









  #------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # Plots and Tables



  # plot that goes with x axis input, purple barplot
  output$plot1 <- renderPlot({
    t <- paste(input$team, ": ", "Average ", input$xaxis, " per Game ")

    ggplot(mapping = aes(x = reorder(picked_team()[, 1], picked_team()[, 3]), y = picked_team()[, 3], fill = picked_team()[, 3])) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      theme(legend.position = "none", plot.title = element_text(size = 17), axis.text.y = element_text(face = "bold", size = 12), axis.text.x = element_text(face = "bold", size = 12)) +
      labs(y = "", x = " ", title = t) +
      coord_flip()
  })





  # plot that goes with yaxis, blue barplot

  output$plot2 <- renderPlot({
    t <- paste(input$team, ": ", "Average ", input$yaxis, " per Game ")

    ggplot(mapping = aes(x = reorder(picked_team()[, 1], picked_team()[, 4]), y = picked_team()[, 4], fill = picked_team()[, 4])) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      theme(legend.position = "none", plot.title = element_text(size = 17), axis.text.y = element_text(face = "bold", size = 12), axis.text.x = element_text(face = "bold", size = 12)) +
      labs(y = " ", x = " ", title = t) +
      coord_flip()
  })


  # scatter plot
  output$plot3 <- renderPlotly({
    plot_ly(data = player_team(), x = ~X, y = ~Y, color = ~Player) %>%

      layout(
        autosize = F, width = 1020, height = 600,
        title = paste(input$team, ": ", input$yaxis, " against ", input$xaxis, "\n"),

        xaxis = list(title = paste("Average ", input$xaxis, " per Game")),
        yaxis = list(title = paste("Average ", input$yaxis, " per Game"))
      )
  })


  # pie chart
  output$their_stats <- renderPlotly({
    labels <- c("Assists (AST)", "Steals (STL)", "Blocks (BLK)", "Rebounds (TRB)", "Points (PTS)")
    values <- as.numeric(picked_player()[1, ])

    plot_ly(
      type = "pie", labels = labels, values = values,
      textinfo = "label",
      insidetextorientation = "radial"
    ) %>%
      layout(title = "5 Stat Breakdown: Rebounds (TRB),   Points (PTS),   Assists (AST),  Steals (STL),   Blocks (BLK) \n Average per Game")
  })



  # data table on left bar
  output$dtab <- DT::renderDataTable({
    picked_team() %>% select(Player, Tm, input$xaxis, input$yaxis)
  })


  # entire data tables
  output$data <- renderTable({
    player_stats
  })
})
