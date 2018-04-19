#input <- list(collection = "Eng-NA", corpus = "Clark", child = "Shem", word = "dog")

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
              type = input$word %>% strsplit(split=",") %>% unlist %>% trimws) %>%
      mutate(gloss = tolower(gloss))

    
  })
  
  speaker_stats <- reactive({
    req(input$children_to_plot)
    
    get_speaker_statistics(collection = input$collection,
                           corpus = if ("All" %in% input$corpus) NULL else input$corpus,
                           child = if ("All" %in% input$children_to_plot) NULL else input$children_to_plot)
  })
  
  # --------------------- UI ELEMENTS FOR SELECTORS ---------------------
  
  # SELECTOR FOR COLLECTION
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
                   selected = roles()[1], 
                   multiple = TRUE)
  })
  
  # TEXT INPUT FOR WORDS
  output$word_selector <- renderUI({
    textInput(inputId = "word",
              label = "Word",
              value = "ball,the")
  })
  
  # SLIDER FOR AGE RANGE
  # TODO: revert this
  output$age_range_selector <- renderUI({
    # Don't adjust slider if age range has been altered by user
    # selected_min_age <- input$age_range[1]
    # selected_max_age <- input$age_range[2]
    # 
    # value <- c(
    #   if (!is.null(selected_min_age)) selected_min_age else age_min,
    #   if (!is.null(selected_max_age)) selected_max_age else age_max
    # )
    # 
    # sliderInput("age_range", 
    #             label="Ages to include (years)", 
    #             value=value, 
    #             step=.5, min=floor(age_min()), max=ceiling(age_max()))
    
    sliderInput("age_range", 
                label="Ages to include (years)", 
                value=c(age_min(), age_max()), 
                step=.5, min=floor(age_min()), max=ceiling(age_max()))
  })
  
  # SLIDER FOR AGE BINWIDTH
  output$age_binwidth_selector <- renderUI({
    sliderInput("age_binwidth", 
                label="Bin size (months)", 
                value=2, step=2,
                min=0, max=24)
  })
  
  # DB VERSION NUMBER
  output$db_version_number <- renderUI({
    paste("Using database version", get_database_version())
  })
  
  
  # --------------------- COMPUTATION OF FREQSs ---------------------
  
  # COMPUTE FREQS
  freqs <- reactive({
    req(input$roles_to_plot)
    req(input$age_range)
    req(input$children_to_plot)
    req(types())
    req(speaker_stats())
    
    print(unique(types()$gloss))
    
    # TODO use quosures
    
    if("All" %in% input$children_to_plot) {
      group_by_data <- types() %>%
        drop_na(speaker_role, target_child_age) %>%
        group_by(gloss, speaker_role, target_child_age) %>%
        summarise(n = sum(count))
    } else {
      group_by_data <- types() %>% 
        drop_na(target_child_name, speaker_role, target_child_age) %>%
        group_by(gloss, target_child_name, speaker_role, target_child_age) %>%
        summarise(n = sum(count))
    }
    
    print("computing")
    filtered_data <- left_join(group_by_data, 
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
        group_by(gloss, speaker_role, age_y) %>%
        summarise(ppm = signif(mean(ppm), digits = 2)) 
    } else {
      filtered_data %>%
        group_by(gloss, target_child_name, speaker_role, age_y) %>%
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
    
    x_lim_spacer <- input$age_range[2] * .1
    nudge_val <- input$age_range[2] * .06
    
    p <- ggplot(freqs(), 
           aes(x = age_y,
               y = ppm,
               linetype = speaker_role,
               color = gloss)) +
      geom_point(size = 5, alpha = 0.4) +
      geom_smooth(se=FALSE, method = "loess", span=1, size = 2) + 
      geom_label_repel(
        data = filter(freqs(), age_y == max(age_y)),
        aes(label = gloss, fill = gloss),
        size = 10,
        fontface = 'bold', color = 'white',
        nudge_x = nudge_val,
        segment.color = NA
      ) +
      guides(color=F, fill =F) +
      labs(x = "Target Child Age (years)",
           y = "Frequency (parts per million words)",
           linetype = "Speaker role:") +
      xlim(input$age_range[1], input$age_range[2] + x_lim_spacer) +
      scale_colour_solarized(name = "gloss") + 
      scale_fill_solarized(name = "gloss") + 
      theme_few() +
      theme(text = element_text(size = 20),
            legend.position = "top")
    
    if (nrow(freqs()) != 0 && !"All" %in% input$children_to_plot) {
      p <- p + facet_wrap(~target_child_name)
    }
    
    p
  })
  
  # DATA TABLE
  output$trajectory_table <- renderDataTable({freqs()})}
