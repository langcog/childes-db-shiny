# input <- list(collection = "Eng-NA", corpus = "Clark", child = "Shem", word = "dog")

# MAIN SHINY SERVER
server <- function(input, output, session) {
  
  # --------------------- DATA FOR SELECTORS ---------------------
  
  # CORPORA IN COLLECTION
  corpora <- reactive({
    req(input$collection)
    
    corpora_df %>%
      filter(collection_name == input$collection) %>% 
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
    
    print(unique(result$name))
    
    result %>%
      filter(role == "Target_Child", 
             !is.na(name)) %>%
      pull(name) %>%
      append("All", after = 0)
  })
  
  # collections <- append(collections, "All", after = 0)
  
  # ROLES USED IN DATA
  # note, other matches are by ID but roles are duplicated across corpora and so 
  # we want to match e.g. all "Mother"s
  roles <- reactive({
    req(input$children_to_plot)
    
    speaker_stats()$speaker_role %>%
      unique %>%
      na.omit
  })
  
  # AGE MIN AND MAX FROM DATA
  age_min <- reactive({
    req(input$children_to_plot)
    
    ages_in_days <- types()$target_child_age %>%
      na.omit()
    
    ifelse(is.null(ages_in_days), 1, min(ages_in_days))/DAYS_PER_YEAR
  })
  
  age_max <- reactive({
    req(input$children_to_plot)
    
    ages_in_days <- types()$target_child_age %>%
      na.omit()
    
    ifelse(is.null(ages_in_days), 1, max(ages_in_days))/DAYS_PER_YEAR
  })
  
  # --------------------- ACTUAL DATA LOADING ---------------------
  
  # ALL TYPES AND THEIR COUNTS FROM CHILD
  types <- reactive({
    req(input$children_to_plot)
    req(input$word)
    
    get_types(collection = input$collection, 
              corpus = if("All" %in% input$corpus) NULL else input$corpus,
              child = if("All" %in% input$children_to_plot) NULL else input$children_to_plot,
              type = input$word)
    
    
  })
  
  speaker_stats <- reactive({
    req(input$children_to_plot)
    
    get_speaker_statistics(collection = input$collection,
                           corpus = if ("All" %in% input$corpus) NULL else input$corpus,
                           child = if ("All" %in% input$children_to_plot) NULL else input$children_to_plot)
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
  
  # --------------------- COMPUTATION OF FREQSs ---------------------
  
  # COMPUTE FREQS
  freqs <- reactive({
    req(input$roles_to_plot)
    req(input$age_range)
    req(input$children_to_plot)
    req(types())
    req(speaker_stats())
    
    # TODO use quosures
    
    if("All" %in% input$children_to_plot) {
      group_by_data <- types() %>%
        drop_na(speaker_role, target_child_age) %>%
        group_by(speaker_role, target_child_age) %>%
        summarise(n = sum(count))
    } else {
      group_by_data <- types() %>% 
        drop_na(target_child_name, speaker_role, target_child_age) %>%
        group_by(target_child_name, speaker_role, target_child_age) %>%
        summarise(n = sum(count))
    }
    
    print("computing")
    filtered_data <- inner_join(group_by_data, 
                               speaker_stats() %>%
                                 group_by(speaker_role, target_child_age) %>%
                                 summarise(num_tokens = sum(num_tokens))) %>%
      mutate(ppm = 1e6 * n / num_tokens)  %>%
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
        summarise(ppm = signif(mean(ppm), digits = 2)) 
    } else {
      filtered_data %>%
        group_by(target_child_name, speaker_role, age_y) %>%
        summarise(ppm = signif(mean(ppm), digits = 2)) 
    }
  })
  
  # --------------------- DISPLAY ---------------------
  
  # DOWNLOAD BUTTON
  output$download_table <- downloadHandler(
    filename = function() paste("frequency_counts_table_v", version, ".csv", sep=""),
    content = function(file) {
      write.csv(freqs(), file, row.names = FALSE)
    })
  
  # TRAJECTORY
  output$trajectory_plot <- renderPlot({
    req(freqs())
    
    p <- ggplot(freqs(), 
           aes(x = age_y,
               y = ppm,
               col = speaker_role)) +
      geom_point() +
      geom_smooth(se=FALSE, method = "loess", span=1) + 
      ylab("Frequency (parts per million words)") + 
      xlab("Target Child Age (years)") + 
      xlim(input$age_range[1], input$age_range[2]) +
      scale_colour_solarized(name = "Speaker Role") + 
      theme_few() +
      theme(legend.position = "bottom") 
    
    if (nrow(freqs()) != 0 && !"All" %in% input$children_to_plot) {
      p <- p + facet_wrap(~target_child_name)
    }
    
    p
  })
  
  # DATA TABLE
  output$trajectory_table <- renderDataTable({freqs()})}
