library(shiny)
library(tidyverse)
library(feather)
library(ggthemes)

# adam <- read_feather("adam.feather")
brown <- read_feather("brown_utts.feather")

DAYS_PER_YEAR <- 365.25
DAYS_PER_MONTH <- DAYS_PER_YEAR / 12

# MAIN SHINY SERVER
server <- function(input, output, session) {
  
  # --------------------- DATA ---------------------
  data <- reactive({
    brown 
  })
  
  # KIDS IN DATA
  children <- reactive({
    data() %>%
      pull(target_child_name) %>%
      unique
  })
  
  # ROLES USED IN DATA
  roles <- reactive({
    data() %>%
      pull(speaker_role) %>%
      unique
  })
  
  # --------------------- UI ELEMENTS ---------------------
 
  # SELECTOR FOR KIDS
  output$children_selector <- renderUI({
    selectizeInput(inputId = "target_children",
                   label = "Target Child (Corpus)", 
                   choices = children(), 
                   selected = "Adam", 
                   multiple = TRUE)
  })
  
  
  # SELECTOR FOR ROLES
  output$role_selector <- renderUI({
    selectizeInput(inputId = "roles",
                   label = "Speakers", 
                   choices = roles(), 
                   selected = "Target_Child", 
                   multiple = TRUE)
  })
  
  
  # --------------------- COMPUTATION OF MLUS ---------------------
  
  
  # COMPUTE MLUS
  mlus <- reactive({
    data() %>%
      filter(target_child_name %in% input$target_children,
             speaker_role %in% input$roles,
             target_child_age >= input$age_range[1] * DAYS_PER_YEAR,
             target_child_age <= input$age_range[2] * DAYS_PER_YEAR) %>%
      mutate(age_mo = target_child_age / DAYS_PER_MONTH, 
             age_mo_binned = floor(age_mo / input$age_binwidth) * input$age_binwidth, 
             age_y = (age_mo_binned + input$age_binwidth/2)/12) %>%
      group_by(target_child_name, speaker_role, age_y) %>%
      summarise(mlu = signif(mean(length), digits = 2),
                n = n()) %>%
      ungroup
  })
  
  # --------------------- DISPLAY ---------------------
  
  
  # TRAJECTORY
  output$trajectory_plot <- renderPlot({
    ggplot(mlus(), 
           aes(x = age_y,
               y = mlu, 
               col = speaker_role)) +
      geom_point(aes(size = n)) +
      geom_smooth(se=FALSE, span=1) + 
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
  
  output$trajectory_table <- renderDataTable({mlus()})
}
