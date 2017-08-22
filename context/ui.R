library(shiny)
library(shinyBS)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("spacelab"),
  
  bsCollapse(id = "doc", open = "title",
             bsCollapsePanel(title = h3("Context Browser"),
                             includeMarkdown("../docs/context-description.md"),
                             value = "title",
                             style = "default")
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId = "collection",
                     label = "Collection", 
                     choices = collections,
                     selected = "Eng-NA", 
                     multiple = FALSE),
      uiOutput("corpus_selector"), 
      uiOutput("children_selector"),
      uiOutput("role_selector"),
      uiOutput("word_selector"),
      actionButton(inputId = "goButton", label = "Get Data")
    ),
    
    mainPanel(
      tabsetPanel(selected = "Plot", 
                  tabPanel("Plot",
                           plotOutput("context_plot")
                  ), 
                  tabPanel("Table",
                           dataTableOutput("context_table")
                  )
      )
    )
  )
)