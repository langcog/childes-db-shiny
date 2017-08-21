library(shiny)
library(shinyBS)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("spacelab"),
  
  bsCollapse(id = "doc", open = "title",
    bsCollapsePanel(title = h3("Frequency Counts"),
                    includeMarkdown("../docs/freq-description.md"),
                    value = "title",
                    style = "default")
  ),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("children_selector"),
      uiOutput("role_selector"),
      uiOutput("age_range"),
      sliderInput("age_binwidth", 
                  label="Bin size (months)", 
                  value=6, step=3,
                  min=0, max=24)
    ),
    
    mainPanel(
      tabsetPanel(selected = "Plot", 
                  tabPanel("Plot",
                           plotOutput("trajectory_plot")
                  ), 
                  tabPanel("Table",
                           dataTableOutput("trajectory_table")
                  )
      )
    )
  )
)