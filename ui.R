library(shiny)

ui <- fluidPage(
  titlePanel("Mean Length of Utterance"),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("role_selector"),
      sliderInput("age_range", 
                  label="Ages to include (years)", 
                  value=c(1, 5), 
                  step=.25, min=0, max=8),
      sliderInput("age_binwidth", 
                  label="Bin size (months)", 
                  value=6, step=1, 
                  min=1, max=24)
    ),
    
    mainPanel(
      plotOutput("trajectory_plot")
    )
  )
)