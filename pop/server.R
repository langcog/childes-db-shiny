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
    req(input$collection)
    req(input$corpus)
    
    result <- children_df %>%
      filter(collection_name == input$collection)
    
    if (!"All" %in% input$corpus) {
      result %<>% filter(corpus_name %in% input$corpus)
    }
    
    result %>%
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
  
  # --------------------- ACTUAL DATA LOADING ---------------------
  
  # DATA
  data <- reactive({
    req(input$children_to_plot)
    
    print("data loading")
    get_speaker_statistics(collection = input$collection, 
                           corpus = if("All" %in% input$corpus) NULL else input$corpus,
                           child = if("All" %in% input$children_to_plot) NULL else input$children_to_plot)
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
  
  # --------------------- UI ELEMENTS FOR SELECTORS ---------------------
  
  # SELECTOR FOR COLLECTIONS
  output$collection_selector <- renderUI({
    selectizeInput(inputId = "collection",
                   label = "Collection", 
                   choices = collections,
                   selected = "Eng-NA", 
                   multiple = FALSE)
  })
  
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
                   multiple = FALSE)
  })
  
  # SELECTOR FOR MEASURE
  output$measure_selector <- renderUI({
    selectizeInput(inputId = "measure",
                   label = "Measure", 
                   choices = c("Utterances", "Tokens (Words)"), 
                   selected = "Utterances", 
                   multiple = FALSE)
  })
  
  # WORD SELECTOR
  output$word_selector <- renderUI({
    selectizeInput("word", 
                   label = "Word", 
                   selected = all_words()[1],
                   choices = all_words(), 
                   multiple = FALSE)
  })
  
  # SLIDER FOR AGE RANGE (NOT IN USE)
  output$age_range <- renderUI({
    sliderInput("age_range", 
                label="Ages to include (years)", 
                value=c(age_min(), age_max()), 
                step=.25, min=floor(age_min()), max=ceiling(age_max()))
  })
  
  # SLIDER FOR AGE BINWIDTH
  output$age_binwidth_selector <- renderUI({
    sliderInput("age_binwidth", 
                label="Bin size (months)", 
                value=2, step=2,
                min=0, max=12)
  })
  
  # DB VERSION NUMBER
  output$db_version_number <- renderUI({
    paste("Using database version", get_database_version())
  })
  
  # --------------------- COMPUTATION OF POPULATION STATS ---------------------
  
  
  # COMPUTE MLUS
  pop_data <- reactive({
    req(data())
    
    filtered_data <- data() 
    # %>%
    #   filter(target_child_age >= input$age_range[1] * DAYS_PER_YEAR,
    #          target_child_age <= input$age_range[2] * DAYS_PER_YEAR) 
    
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
      pop_data <- filtered_data %>%
        group_by(age_y) %>%
        summarise(n_utts = sum(num_utterances), 
                  n_words = sum(num_tokens)) 
    } else {
      pop_data <- filtered_data %>%
        group_by(target_child_name, age_y) %>%
        summarise(n_utts = sum(num_utterances), 
                  n_words = sum(num_tokens)) 
    }
    
    print("computed")
    
    if (input$measure == "Utterances") {
      pop_data %>%
        mutate(n = n_utts)
    } else {
      pop_data %>%
        mutate(n = n_words)
    }
  })
  
  # --------------------- DISPLAY ---------------------
  
  # DOWNLOAD BUTTON
  output$download_table <- downloadHandler(
    filename = function() paste("population_table_v", version, ".csv", sep=""),
    content = function(file) {
      write.csv(pop_data(), file, row.names = FALSE)
    })
  
  
  # TRAJECTORY
  output$pop_plot <- renderPlot({
    req(pop_data())
    
    if ("All" %in% input$children_to_plot) {
      aesthetic <- aes(x = age_y,
                       y = n)
    } else {
      aesthetic <- aes(x = age_y,
                       y = n, 
                       col = target_child_name)
    }
    
    p <- ggplot(pop_data(), aesthetic) +
      geom_point() +
      geom_smooth(se=FALSE, method = "loess", span=1) + 
      xlab("Target Child Age (years)") + 
      ylim(0, max(pop_data()$n)) +
      # xlim(input$age_range[1], input$age_range[2]) +
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
  output$pop_table <- renderDataTable({pop_data()})
}
