# walk(DBI::dbListConnections(RMySQL::MySQL()), DBI::dbDisconnect)

library(shiny)
library(tidyverse)
library(feather)
library(ggthemes)
library(magrittr)
library(childesr)
library(tidytext)
library(stringr)
library(forcats)

brown <- read_feather("../data/brown_utts.feather")

# word <- "dog"
# contexts <- brown %>%
#   select(gloss, speaker_role) %>%
#   filter(str_detect(gloss, word)) %>%
#   unnest_tokens(output = "context", input = "gloss", token = "ngrams", n = 2) %>%
#   filter(str_detect(context, word)) %>%
#   group_by(context, speaker_role) %>%
#   count %>%
#   arrange(desc(n))

DAYS_PER_YEAR <- 365.25
DAYS_PER_MONTH <- DAYS_PER_YEAR / 12
MONTHS_PER_YEAR <- 12

# CHILDES DATA
collections_df <- get_collections() %>% tbl_df() 
collections <- as.list(collections_df$collection_id)
names(collections) <- collections_df$collection_name

corpora_df <- get_corpora() %>% tbl_df()
participants_df <- get_participants() %>% 
  tbl_df()

# MAIN SHINY SERVER
server <- function(input, output, session) {
  
  # --------------------- DATA ---------------------
  
  
  # CORPORA IN COLLECTION
  corpora <- reactive({
    cdf <- corpora_df %>%
      filter(collection_id == input$collection) 
      
    corpora <- as.list(cdf$corpus_id)
    names(corpora) <- cdf$corpus_name
    
    return(corpora)
  })
  
  # CHILDREN IN CORPUS
  children <- reactive({
    print(input$corpus)
    if (is.null(input$corpus)) {
      print("not filtering corpus")
      pdf <- participants_df %>%
        filter(role == "Target_Child", 
               !is.na(name))
    } else {
      print("filtering corpus")
      pdf <- participants_df %>%
        filter(corpus_id == input$corpus, 
               role == "Target_Child", 
               !is.na(name))
    }
    
    children <- as.list(pdf$target_child_id)
    names(children) <- pdf$name
    print(children)
    
    return(children)
  })
  
  # ROLES USED IN DATA
  # note, other matches are by ID but roles are duplicated across corpora and so 
  # we want to match e.g. all "Mother"s
  roles <- reactive({
    if (is.null(input$children_to_plot)) {
      pdf <- participants_df %>%
        filter(!is.na(role))
    } else {
      pdf <- participants_df %>%
        filter(id == input$children_to_plot,
               !is.na(role)) 
    }
    
    roles <- unique(pdf$role)
    
    return(roles)
  })
  
  # AGE MIN AND MAX FROM DATA
  age_min <- reactive({
    ifelse(is.null(data()$target_child_age), 1, min(data()$target_child_age))/DAYS_PER_YEAR
  })
  
  age_max <- reactive({
    ifelse(is.null(data()$target_child_age), 1, max(data()$target_child_age))/DAYS_PER_YEAR
  })
  
  # DATA
  data <- reactive({
    get_utterances(collection = input$collection, 
                   corpus = input$corpus,
                   child = input$child_to_plot, 
                   role = input$role_to_plot)
  })
  
  # WORDS IN DATA
  all_words <- reactive({
    data() %>%
      select(gloss) %>% 
      unnest_tokens(input = "gloss", output = "word") %>% 
      pull(word) %>% 
      unique
  })
  
  # --------------------- UI ELEMENTS ---------------------
  
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
                   selected = "dog",
                   choices = all_words(), 
                   multiple = FALSE)
  })
  
  # --------------------- COMPUTATION OF CONTEXTS ---------------------
  
  
  # COMPUTE CONTEXTS
  contexts <- reactive({
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
    ggplot(top_n(contexts(), 20), 
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
