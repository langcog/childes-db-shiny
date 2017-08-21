library(shiny)
library(shinyBS)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("spacelab"),

  bsCollapse(id = "doc", open = "title",
    bsCollapsePanel(title = h3("CHILDES Population Viewer"),
                    includeMarkdown("../docs/pop-description.md"),
                    value = "title",
                    style = "default")
  ),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("children_selector"),
      selectizeInput(inputId = "measure",
                     label = "Measure", 
                     choices = c("Utterances", "Tokens (Words)"), 
                     selected = "Utterances", 
                     multiple = FALSE),
      selectizeInput(inputId = "roles_to_plot",
                     label = "Speakers", 
                     choices = c("Target Child", "All Speakers"), 
                     selected = "Target Child", 
                     multiple = FALSE),
      uiOutput("age_range"),
      sliderInput("age_binwidth", 
                  label="Bin size (months)", 
                  value=0, step=3,
                  min=0, max=12)
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