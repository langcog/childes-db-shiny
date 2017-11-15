server <- function(input, output, session) {
  
  # --------------------- DATA FOR SELECTORS ---------------------
  
  # CORPORA IN COLLECTION
  corpora <- reactive({
    req(input$collection)
    
    if ("All" %in% input$collection) {
      result <- corpora_df
    } else {
      result <- corpora_df %>%
        filter(collection_name == input$collection)
    }
    
    result %>%
      pull(corpus_name) %>%
      append("All", after = 0)
  })
  
  # CHILDREN IN CORPUS
  children <- reactive({
    req(input$corpus)
    
    if ("All" %in% input$corpus) {
      result <- participants_df
    } else {
      result <- participants_df %>%
        filter(corpus_name %in% input$corpus)
    }
    
    result %>%
      filter(role == "Target_Child", 
             !is.na(name)) %>%
      pull(name) %>%
      append("All", after = 0)
  })
  
  # ROLES USED IN DATA
  # note, other matches are by ID but roles are duplicated across corpora and so 
  # we want to match e.g. all "Mother"s
  roles <- reactive({
    req(input$children_to_plot)
    
    data()$speaker_role %>%
      unique %>%
      na.omit
  })
  
  # AGE MIN AND MAX FROM DATA
  age_min <- reactive({
    req(input$children_to_plot)
    
    ages_in_days <- data()$target_child_age %>%
      na.omit()
    
    ifelse(is.null(ages_in_days), 1, min(ages_in_days))/DAYS_PER_YEAR
  })
  
  age_max <- reactive({
    req(input$children_to_plot)
    
    ages_in_days <- data()$target_child_age %>%
      na.omit()
    
    ifelse(is.null(ages_in_days), 1, max(ages_in_days))/DAYS_PER_YEAR
  })
  
  # --------------------- ACTUAL DATA LOADING ---------------------
  
  # DATA
  data <- reactive({
    req(input$children_to_plot)
    
    if (!is.null(input$collection) &
        !is.null(input$corpus)) {
      get_speaker_statistics(collection = if("All" %in% input$collection) NULL else input$collection, 
                             corpus = if("All" %in% input$corpus) NULL else input$corpus,
                             child = if("All" %in% input$children_to_plot) NULL else input$children_to_plot)
    }
  })
  
  
  # --------------------- UI ELEMENTS FOR SELECTORS ---------------------
  
  # SELECTOR FOR CORPORA
  output$corpus_selector <- renderUI({
    selectizeInput(inputId = "corpus",
                   label = "Corpus", 
                   choices = corpora(), 
                   selected = corpora()[1],
                   multiple = TRUE)
  })
  
  # SELECTOR FOR CHILDREN
  output$children_selector <- renderUI({
    selectizeInput(inputId = "children_to_plot",
                   label = "Target Child", 
                   choices = children(), 
                   selected = children()[1], 
                   multiple = TRUE)
  })
  
  # SELECTOR FOR ROLES
  output$role_selector <- renderUI({
    selectizeInput(inputId = "roles_to_plot",
                   label = "Speakers", 
                   choices = roles(), 
                   selected = "Target_Child", 
                   multiple = TRUE)
  })
  
  # SLIDER FOR AGE RANGE
  output$age_range <- renderUI({
      sliderInput("age_range", 
              label="Ages to include (years)", 
              value=c(age_min, age_max), 
              step=.5, min=floor(age_min()), max=ceiling(age_max()))
  })
  
  # --------------------- COMPUTATION OF MLUS ---------------------
  
  
  # COMPUTE MLUS
  mlus <- reactive({
    req(input$roles_to_plot)
    req(input$age_range)
    req(data())
    
    print(data())
    
    filtered_data <- data() %>%
      filter(target_child_age >= input$age_range[1] * DAYS_PER_YEAR,
             target_child_age <= input$age_range[2] * DAYS_PER_YEAR, 
             speaker_role %in% input$roles_to_plot) 
    
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
    
    if("All" %in% input$children_to_plot) {
      filtered_data %>%
        group_by(speaker_role, age_y) %>%
        summarise(mlu = signif(mean(mlu), digits = 2),
                  n = sum(num_utterances)) 
    } else {
      filtered_data %>%
        group_by(target_child_name, speaker_role, age_y) %>%
        summarise(mlu = signif(mean(mlu), digits = 2),
                  n = sum(num_utterances)) 
    }
  })
  
  # --------------------- DISPLAY ---------------------
  
  # DOWNLOAD BUTTON
  output$download_table <- downloadHandler(
    filename = function() paste("mlu_table_v", version, ".csv", sep=""),
    content = function(file) {
      write.csv(freqs(), file, row.names = FALSE)
    })
  
  # TRAJECTORY
  output$trajectory_plot <- renderPlot({
    req(mlus())
    
    p <- ggplot(mlus(), 
           aes(x = age_y,
               y = mlu, 
               col = speaker_role)) +
      geom_point(aes(size = n)) +
      geom_smooth(se=FALSE, method = "loess", span=1) + 
      ylab("Mean Length of Utterance") + 
      xlab("Target Child Age (years)") + 
      ylim(0, ceiling(max(mlus()$mlu))) +
      xlim(input$age_range[1], input$age_range[2]) +
      scale_colour_solarized(name = "Speaker Role") + 
      scale_size_continuous(name = "Number of Utterances") + 
      theme_few() +
      theme(legend.position = "bottom")
    
    if (nrow(mlus()) != 0 && !"All" %in% input$children_to_plot) {
      p <- p + facet_wrap(~target_child_name)
    }
    
    p
  })
  
  # DATA TABLE
  output$trajectory_table <- renderDataTable({mlus()})
}
