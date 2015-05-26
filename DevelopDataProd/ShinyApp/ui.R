

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
                              value = 20),
                  h3('Purpose:'),
                  h4('This App has been developed to understand the effect of fast greedy algorithm
                     in community detection. Data used here is randomly generated based on the number
                     of nodes and number of links that user specified.'),
                  h3('Usage:'),
                  h4('The user can change the number of nodes and links in the network. A random network 
                     will then generated. The results is plotted in the main panel with number of links 
                     sized and communities colored.')
                  
            ),
            
            # Show a plot of the generated distribution
            mainPanel(
                  plotOutput("distPlot")
            )
      )
))
