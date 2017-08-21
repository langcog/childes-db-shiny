library(shiny)
library(tidyverse)
library(feather)
library(ggthemes)

adam <- read_feather("adam.feather")

DAYS_PER_YEAR <- 365.25
DAYS_PER_MONTH <- DAYS_PER_YEAR / 12

# MAIN SHINY SERVER
server <- function(input, output, session) {
  
  # ------- PULL DATA -------
  utt_tbl <- reactive({
    adam
  })
  
  # ------- UI ELEMENTS -------
  
  # ROLES USED IN DATA
  roles <- reactive({
    utt_tbl() %>%
      pull(speaker_role) %>%
      unique
  })
  
  # SELECTOR FOR ROLES
  output$role_selector <- renderUI({
    selectizeInput(inputId = "roles",
                   label = "Speakers", 
                   choices = roles(), 
                   selected = "Target_Child", 
                   multiple = TRUE)
  })
  
  
  # ------- DATA -------
  
  # COMPUTE MLUS
  mlus <- reactive({
    utt_tbl() %>%
      filter(speaker_role %in% input$roles,
             target_child_age >= input$age_range[1] * DAYS_PER_YEAR,
             target_child_age <= input$age_range[2] * DAYS_PER_YEAR) %>%
      mutate(age_mo = target_child_age / DAYS_PER_MONTH, 
             age_mo_binned = floor(age_mo / input$age_binwidth) * input$age_binwidth, 
             age_y = (age_mo_binned + input$age_binwidth/2)/12) %>%
      group_by(speaker_role, age_y) %>%
      summarise(mlu = mean(length),
                n = n())
  })
  
  # ------- DISPLAY ELEMENTS -------
  
  # TRAJECTORY
  output$trajectory_plot <- renderPlot({
    ggplot(mlus(), 
           aes(x = age_y,
               y = mlu, 
               col = speaker_role,
               group = speaker_role)) +
      geom_point(aes(size = n)) +
      geom_smooth() + 
      ylab("Mean Length of Utterance") + 
      xlab("Target Child Age (years)") + 
      ylim(0, ceiling(max(mlus()$mlu))) +
      xlim(input$age_range[1], input$age_range[2]) +
      scale_colour_solarized(name = "Speaker Role") + 
      scale_size_continuous(name = "Number of Utterances") + 
      theme_few() +
      theme(legend.position = "Bottom") 
  })
}
