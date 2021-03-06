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
      uiOutput("collection_selector"),
      uiOutput("corpus_selector"), 
      uiOutput("children_selector"),
      uiOutput("role_selector"),
      uiOutput("word_selector"),
      uiOutput("age_range_selector"),
      uiOutput("age_binwidth_selector"),
      # Note that uiOutputs are necessary for bookmark to work
      bookmarkButton(label = "Share Analysis"),
      br(), br(),
      uiOutput("db_version_number")
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