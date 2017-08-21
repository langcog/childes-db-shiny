library(shiny)
library(tidyverse)
library(feather)
library(ggthemes)
library(magrittr)

# adam <- read_feather("adam.feather")
brown <- read_feather("../data/brown_utts.feather")

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
    if (input$roles_to_plot == "Target Child") {
      all_data() %>%
        filter(target_child_name %in% input$children_to_plot, 
               speaker_role == "Target_Child")
    } else {
      all_data() %>%
        filter(target_child_name %in% input$children_to_plot)
    }
  })
  
  # KIDS IN DATA
  all_children <- reactive({
    all_data() %>%
      pull(target_child_name) %>%
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
  pop_data <- reactive({
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
    
    pop_data <- filtered_data %>%
      group_by(target_child_name, age_y) %>%
      summarise(n_utts = n(), 
                n_words = sum(length)) 
    
    if (input$measure == "Utterances") {
      pop_data %>%
        mutate(n = n_utts)
    } else {
      pop_data %>%
        mutate(n = n_words)
    }
  })
  
  # --------------------- DISPLAY ---------------------
  
  
  # TRAJECTORY
  output$trajectory_plot <- renderPlot({
    p <- ggplot(pop_data(), 
           aes(x = age_y,
               y = n, 
               col = target_child_name)) +
      geom_point() +
      geom_smooth(se=FALSE, method = "loess", span=1) + 
      xlab("Target Child Age (years)") + 
      ylim(0, max(pop_data()$n)) +
      xlim(input$age_range[1], input$age_range[2]) +
      scale_colour_solarized(name = "Speaker Role") + 
      scale_size_continuous(name = "Number of Utterances") + 
      theme_few() +
      theme(legend.position = "bottom") 
    
    if (input$measure == "Utterances") {
      p + ylab("Number of Utterances") 
    } else {
      p + ylab("Number of Words") 
    }
  })
  
  # DATA TABLE
  output$trajectory_table <- renderDataTable({mlus()})
}
