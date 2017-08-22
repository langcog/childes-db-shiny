server <- function(input, output, session) {
  
  # --------------------- DATA FOR SELECTORS ---------------------
  
  # CORPORA IN COLLECTION
  corpora <- reactive({
    req(input$collection)
    
    corpora_df %>%
      filter(collection_name == input$collection) %>%
      pull(corpus_name)
  })
  
  # CHILDREN IN CORPUS
  children <- reactive({
    req(input$corpus)
    
    participants_df %>%
      filter(corpus_name %in% input$corpus, 
             role == "Target_Child", 
             !is.na(name)) %>%
      pull(name)
  })
  
  # ROLES USED IN DATA
  # note, other matches are by ID but roles are duplicated across corpora and so 
  # we want to match e.g. all "Mother"s
  roles <- reactive({
    req(input$children_to_plot)
    
    # workaround related to issue #6 in childesr repo
    target_children_ids <- participants_df %>%
      filter(corpus_name %in% input$corpus,
             name %in% input$children_to_plot) %>%
      pull(target_child_id)
    
    participants_df %>%
      filter(target_child_id %in% target_children_ids,
             corpus_name %in% input$corpus,
             !is.na(role)) %>%
      pull(role) %>%
      unique
  })
  
  # --------------------- ACTUAL DATA LOADING ---------------------
  
  # DATA
  data <- eventReactive(input$goButton, {
    req(input$children_to_plot)
    
    print("data loading")
    get_utterances(collection = input$collection, 
                   corpus = input$corpus,
                   child = input$children_to_plot)
  })
  
  # AGE MIN AND MAX FROM DATA
  age_min <- reactive({
    req(input$children_to_plot)
    
    ifelse(is.null(data()$target_child_age), 1, min(data()$target_child_age))/DAYS_PER_YEAR
  })
  
  age_max <- reactive({
    req(input$children_to_plot)
    
    ifelse(is.null(data()$target_child_age), 1, max(data()$target_child_age))/DAYS_PER_YEAR
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
  
  # SLIDER FOR AGE RANGE
  output$age_range <- renderUI({
    # print(age_min())
    sliderInput("age_range", 
                label="Ages to include (years)", 
                value=c(age_min, age_max), 
                step=.25, min=floor(age_min()), max=ceiling(age_max()))
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
    
    pop_data <- filtered_data %>%
      group_by(target_child_name, age_y) %>%
      summarise(n_utts = n(), 
                n_words = sum(length)) 
    
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
  
  
  # TRAJECTORY
  output$pop_plot <- renderPlot({
    req(pop_data())
    
    p <- ggplot(pop_data(), 
                aes(x = age_y,
                    y = n, 
                    col = target_child_name)) +
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
  output$pop_table <- renderDataTable({mlus()})
}
