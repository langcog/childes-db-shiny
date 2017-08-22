# MAIN SHINY SERVER
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
  
  # AGE MIN AND MAX FROM DATA
  age_min <- reactive({
    req(input$children_to_plot)
    
    ifelse(is.null(data()$target_child_age), 1, min(data()$target_child_age))/DAYS_PER_YEAR
  })
  
  age_max <- reactive({
    req(input$children_to_plot)
    
    ifelse(is.null(data()$target_child_age), 1, max(data()$target_child_age))/DAYS_PER_YEAR
  })
  
  # --------------------- ACTUAL DATA LOADING ---------------------
  
  # DATA
  data <- eventReactive(input$goButton, {
    req(input$children_to_plot)
    
    if (!is.null(input$collection) &
        !is.null(input$corpus)) {
      get_utterances(collection = input$collection, 
                     corpus = input$corpus,
                     child = input$children_to_plot)
    }
  })
  
  # WORDS IN DATA
  all_words <- reactive({
    req(data())
    
    data() %>%
      select(gloss) %>% 
      unnest_tokens(input = "gloss", 
                    output = "word") %>% 
      group_by(word) %>%
      count %>%
      filter(n > MIN_N_FOR_BIGRAMS) %>%
      pull(word)
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
  
  # --------------------- COMPUTATION OF CONTEXTS ---------------------

  # COMPUTE CONTEXTS
  contexts <- reactive({
    req(input$roles_to_plot)
    req(input$word)
    req(data())
    
    data() %>%
      select(gloss, speaker_role) %>%
      filter(str_detect(gloss, input$word)) %>%
      unnest_tokens(output = "context", input = "gloss", token = "ngrams", n = 2) %>%
      filter(str_detect(context, input$word)) %>%
      group_by(context, speaker_role) %>%
      count %>%
      ungroup %>%
      mutate(context = fct_reorder(factor(context), n, desc=FALSE))
  })
  
  # --------------------- DISPLAY ---------------------
  
  # TRAJECTORY
  output$context_plot <- renderPlot({
    req(contexts())
    
    ggplot(top_n(contexts(), 
                 20, 
                 wt = n), 
           aes(x = context,
               y = n)) +
      geom_bar(stat="identity") +
      ylab("Count") + 
      xlab("Context") + 
      theme_few() + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
  })
  
  # DATA TABLE
  output$context_table <- renderDataTable({contexts()})
}
