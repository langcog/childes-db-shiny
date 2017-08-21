library(shiny)
library(tidyverse)
library(feather)
library(ggthemes)
library(magrittr)

# host = "ec2-54-68-171-132.us-west-2.compute.amazonaws.com"
# dbname = "childesdb"
# user = "childesdb"
# password = "uy5z4hf7ihBjf"
# con <-  DBI::dbConnect(RMySQL::MySQL(), host = host, user = user, dbname = dbname, password = password)

token_tbl <- dplyr::tbl(con, "token_frequency")
  result <- tbl_df(token_tbl)
  DBI::dbDisconnect(con)
  result
})


brown <- read_feather("../data/brown_tokens.feather")

DAYS_PER_YEAR <- 365.25
DAYS_PER_MONTH <- DAYS_PER_YEAR / 12
MONTHS_PER_YEAR <- 12

# MAIN SHINY SERVER
server <- function(input, output, session) {
  
  # --------------------- DATA ---------------------
  
  # ALL DATA
  all_data <- reactive({
    brown 
  })
  
  # FILTERED DATA
  data <- reactive({
    all_data() %>%
      filter(target_child_name %in% input$children_to_plot,
             speaker_role %in% input$roles_to_plot)
  })
  
  # KIDS IN DATA
  all_children <- reactive({
    all_data() %>%
      pull(target_child_name) %>%
      unique
  })
  
  # ROLES USED IN DATA
  all_roles <- reactive({
    all_data() %>%
      pull(speaker_role) %>%
      unique
  })
  
  # AGE MIN AND MAX FROM DATA
  age_min <- reactive({
    ifelse(is.null(data()$target_child_age), 1, min(data()$target_child_age))/DAYS_PER_YEAR
  })
  
  age_max <- reactive({
    ifelse(is.null(data()$target_child_age), 1, max(data()$target_child_age))/DAYS_PER_YEAR
  })
  
  # --------------------- UI ELEMENTS ---------------------
  
  # SELECTOR FOR KIDS
  output$children_selector <- renderUI({
    selectizeInput(inputId = "children_to_plot",
                   label = "Target Child (Corpus)", 
                   choices = all_children(), 
                   selected = "Adam", 
                   multiple = TRUE)
  })
  
  
  # SELECTOR FOR ROLES
  output$role_selector <- renderUI({
    selectizeInput(inputId = "roles_to_plot",
                   label = "Speakers", 
                   choices = all_roles(), 
                   selected = "Target_Child", 
                   multiple = TRUE)
  })
  
  # SLIDER FOR AGE RANGE
  output$age_range <- renderUI({
    # print(age_min())
    sliderInput("age_range", 
                label="Ages to include (years)", 
                value=c(age_min, age_max), 
                step=.25, min=floor(age_min()), max=ceiling(age_max()))
  })
  
  # --------------------- COMPUTATION OF MLUS ---------------------
  
  
  # COMPUTE MLUS
  mlus <- reactive({
    filtered_data <- data() %>%
      filter(target_child_age >= input$age_range[1] * DAYS_PER_YEAR,
             target_child_age <= input$age_range[2] * DAYS_PER_YEAR) 
    
    if(input$age_binwidth > 0) {
     filtered_data %<>%
        mutate(age_mo = target_child_age / DAYS_PER_MONTH, 
               age_mo_binned = floor(age_mo / input$age_binwidth) * input$age_binwidth, 
               age_y = (age_mo_binned + input$age_binwidth/2)/ MONTHS_PER_YEAR) 
    } else {
      filtered_data %<>%
        mutate(age_mo = target_child_age / DAYS_PER_MONTH, 
               age_y = age_mo / MONTHS_PER_YEAR) 
    }
    
    filtered_data %>%
      group_by(target_child_name, speaker_role, age_y) %>%
      summarise(mlu = signif(mean(length), digits = 2),
                n = n()) 
  })
  
  # --------------------- DISPLAY ---------------------
  
  
  # TRAJECTORY
  output$trajectory_plot <- renderPlot({
    ggplot(mlus(), 
           aes(x = age_y,
               y = mlu, 
               col = speaker_role)) +
      geom_point(aes(size = n)) +
      geom_smooth(se=FALSE, method = "loess", span=1) + 
      facet_wrap(~target_child_name) +
      ylab("Mean Length of Utterance") + 
      xlab("Target Child Age (years)") + 
      ylim(0, ceiling(max(mlus()$mlu))) +
      xlim(input$age_range[1], input$age_range[2]) +
      scale_colour_solarized(name = "Speaker Role") + 
      scale_size_continuous(name = "Number of Utterances") + 
      theme_few() +
      theme(legend.position = "bottom") 
  })
  
  # DATA TABLE
  output$trajectory_table <- renderDataTable({mlus()})
}
