

library(shiny)

shinyUI(fluidPage(
      
      # Application title
      titlePanel("Course Project for Developing Data Products"),
      
      # Sidebar with a slider input for the number of bins
      sidebarLayout(
            sidebarPanel(
                  
                  sliderInput("Nds",
                              "Number of Nodes:",
                              min = 1,
                              max = 30,
                              value = 15),
                  sliderInput("Lnks",
                              "Number of Links:",
                              min = 1,
                              max = 100,
                              value = 20)
            ),
            
            # Show a plot of the generated distribution
            mainPanel(
                  plotOutput("distPlot")
            )
      )
))
