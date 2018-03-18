library(shiny)
library(plotly)

shinyUI(fluidPage(
  titlePanel("Heatmap Granularity"),
  sidebarLayout(
    sidebarPanel(
      tags$p( tags$i( "Final Project for Course 9 - Developing Data Products" ) ),
      tags$p( tags$i( "David Effendi, Mar 16 2018" ) ),
      tags$br(),
      tags$p( "This heatmap visualizes count of incidents on a time-of-day X weekday plane." ),
      tags$p( "Select a different time interval to see the heatmap at different levels of granularity." ),  
      radioButtons("tInterval", "Select Time Interval:   (mins)",
                   c(1, 5,15, 30, 60, 120, 180 ),
                   selected = 60,
                   inline = TRUE ),
      submitButton("Submit"),
      tags$p( tags$i( "Please be patient if the heatmap doesn't immediately show up") ),
      tags$p("This demonstrates the effects of different levels of granularity on heatmaps. When it is too fine (very granular), it might be hard to observe any pattern. And when it's too coarse (very aggregated), some details might be glossed over."),
      tags$br(),
      tags$h4("Server Computation & Reactivity:"),
      tags$p("The dataset is on a per-minute basis. Everytime the 'Submit' button is clicked, R take note of the chosen granularity value and performs re-grouping on the dataset. aggregating the number of incidents in each time interval, then pass the aggregated data to plotly to be turned into a heatmap."),
      tags$br(),
      tags$h4("Data Source:"),
      tags$a(href="https://data.cityofnewyork.us/Public-Safety/NYPD-Motor-Vehicle-Collisions/h9gi-nx95", "NYPD open data - Motor Vehicle Collisions")      
    ),
    mainPanel(
      h2( "Heatmap - number of incidents across time" ),
      plotlyOutput("plot1"),
      textOutput("text1")
    )
  )
))
