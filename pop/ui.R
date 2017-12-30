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
      selectizeInput(inputId = "collection",
                     label = "Collection", 
                     choices = collections,
                     selected = "Eng-NA", 
                     multiple = FALSE),
      uiOutput("corpus_selector"), 
      uiOutput("children_selector"),
      uiOutput("role_selector"),
      selectizeInput(inputId = "measure",
                     label = "Measure", 
                     choices = c("Utterances", "Tokens (Words)"), 
                     selected = "Utterances", 
                     multiple = FALSE),
      # uiOutput("age_range"),
      sliderInput("age_binwidth", 
                  label="Bin size (months)", 
                  value=2, step=2,
                  min=0, max=12)
    ),
    
    mainPanel(
      tags$style(type = "text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"),
      tabsetPanel(selected = "Plot", 
                  tabPanel("Plot",
                           plotOutput("pop_plot")
                  ), 
                  tabPanel("Table",
                           br(),
                           downloadButton("download_table", "Download Table",
                                          class = "btn-default btn-xs"),
                           br(), br(),
                           dataTableOutput("pop_table")
                  )
      )
    )
  )
)