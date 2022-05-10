#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(colorfindr)

tree_rgb_all <- read.csv("data_raw/final_project/tree_rgb_sum/tree_rgb_sum_all.csv")
#tree_rgb_all <- read.csv("data_raw/final_project/tree_rgb/tree_rgb_all.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Heatwaves Impacts on Seedling Regeneration"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        h3("Color change of droughted tree seedlings over time"),
        p("Data collected using photographs from August 26 2021 to November 19 2021")
        ),
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("Colors", 
                   selectInput("PonderosaPine",
                               "Select Ponderosa Pine seedling:",
                               choices = tree_rgb_all %>% distinct(SpeciesID)
                   ),
                   sliderInput("week",
                               "Select week:",
                               min = min(tree_rgb_all$Week),
                               max = max(tree_rgb_all$Week),
                               value = 1,
                               step = 1,
                               sep = ""),
                   plotOutput("timelinePlot"))
      )
       
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    output$timelinePlot <- renderPlot({
      tree_rgb_all %>% 
        filter(SpeciesID == input$PonderosaPine & Week == input$week) %>% 
        plot_colors_3d(sample_size = 5000, marker_size = 2.5, color_space = "RGB")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
