ui <- fluidPage(
  theme = shinytheme("spacelab"),
  
  bsCollapse(id = "doc", open = "title",
             bsCollapsePanel(title = h3("Mean Length of Utterance"),
                             includeMarkdown("../docs/mlu-description.md"),
                             value = "title",
                             style = "default")
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId = "collection",
                     label = "Collection", 
                     choices = append(collections, "All", after = 0),
                     selected = "All", 
                     multiple = FALSE),
      uiOutput("corpus_selector"), 
      uiOutput("children_selector"),
      uiOutput("role_selector"),
      uiOutput("age_range"),
      sliderInput("age_binwidth", 
                  label="Bin size (months)", 
                  value=2, step=2,
                  min=0, max=24)
    ),
    
    mainPanel(
      tags$style(type = "text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"),
      tabsetPanel(selected = "Plot", 
                  tabPanel("Plot",
                           plotOutput("trajectory_plot")
                  ), 
                  tabPanel("Table",
                           br(),
                           downloadButton("download_table", "Download Table",
                                          class = "btn-default btn-xs"),
                           br(), br(),
                           dataTableOutput("trajectory_table")
                  )
      )
    )
  )
)