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
                     choices = collections,
                     selected = "Eng-NA", 
                     multiple = FALSE),
      uiOutput("corpus_selector"), 
      uiOutput("children_selector"),
      uiOutput("role_selector"),
      uiOutput("age_range"),
      sliderInput("age_binwidth", 
                  label="Bin size (months)", 
                  value=0, step=3,
                  min=0, max=24), 
      actionButton(inputId = "goButton", label = "Get Data")
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