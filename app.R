library(shiny)
library(magrittr)
library(dbplyr)
library(dplyr)
library(ggplot2)
library(RMySQL)
library(readr)
library(ggthemes)
library(grid)
library(directlabels)
# library(tidyverse)

# all_cons <- DBI::dbListConnections(MySQL())
# print(all_cons)
# for (con in all_cons){
#   DBI::dbDisconnect(con)
# }
# print(paste(length(all_cons), " connections killed."))

mycss <- "
#plot-container {
position: relative;
}
#loading-spinner {
position: absolute;
left: 40%;
top: 40%;
z-index: -1;
margin-top: -33px;  /* half of the spinner's height */
margin-left: -33px; /* half of the spinner's width */
}
#plot.recalculating {
z-index: -2;
"

ui <- fluidPage(
   titlePanel("Word Frequency from CHILDES"),
   
   sidebarLayout(
      sidebarPanel(
         selectizeInput('words', 'Words', choices=NULL, multiple=TRUE),
         textInput("regex", "RegEx", "need"), # TODO check if valid regex
         sliderInput("slider", label="Age Range (months)", value=c(12, 60), step=6, min=0, max=96),
         sliderInput("slider2", label="Group Size (months)", value=12, step=3, min=3, max=24)
         # selectInput("min_age", label = "Min Age (months)", 
         #             choices = seq(0, 90, 6), selected = 12),
         # selectInput("max_age", label = "Max Age (months)", 
         #             choices = seq(5, 95, 6), selected = 65),
         # selectInput("group_size", label = "Group Size (months)", 
         #             choices = seq(3, 24, 3), selected = 12)
      ),

      mainPanel(
        tags$head(tags$style(HTML(mycss))),
        div(id = "plot-container",
            tags$img(src = "https://media.giphy.com/media/3oEjI6SIIHBdRxXI40/giphy.gif",
                     id = "loading-spinner"),
            plotOutput("plot1")
        )
      )
   )
)

server <- function(input, output, session) {
  
  # DB stuff
  
  get_con <- function () {
    print('opening connection to DB')
    host = "ec2-54-68-171-132.us-west-2.compute.amazonaws.com"
    dbname = "childesdb"
    user = "childesdb"
    password = "uy5z4hf7ihBjf"
    DBI::dbConnect(RMySQL::MySQL(), host = host, user = user, dbname = dbname, password = password)
  }
  
  tbl_from_db <- reactive({
    con <- get_con()
    token_tbl <- dplyr::tbl(con, "token_frequency")
    result <- tbl_df(token_tbl)
    DBI::dbDisconnect(con)
    result
  })
  
  # Load CSV for now
  
  original_tbl <- reactive({
    read_csv("~/Documents/R/data/childfreq2.csv", col_types=cols(target_child_age="d"))
  })
  
  # Filter original table by role and ages / group size
  
  filtered_tbl <- reactive({
    word = input$regex # remove after
    min_age = as.double(input$slider[1])
    max_age = as.double(input$slider[2]) - 1 # this is to match childfreq; helps debugging
    groupsize = as.double(input$slider2)
    
    print(min_age)
    print(max_age)
    print(groupsize)
    
    # min_age = 12
    # max_age = 60
    # groupsize = 12
    # word = 'eat'
    
    original_tbl() %>%
      filter(speaker_role=="Target_Child", between(target_child_age, min_age*30, max_age*30)) %>%
      mutate(target_child_age = floor((target_child_age/30 - min_age) / groupsize) * groupsize + min_age) %>%
      group_by(speaker_role, target_child_age) %>%
      mutate(total_tokens = sum(total_occurences)) %>% # TODO somehow use summarise for these 3 lines?
      ungroup()
      #summarise(total_tokens = sum(total_occurences), gloss_counts = sum(total_occurences[gloss==!!word])) %>%
      #mutate(occurences_per_one_million = (1000000 / (total_tokens)) * gloss_counts)
  })
  
  # TODO exclude uncommon words from this list
  
  input_words <- reactive({
    input$words
    #if (is.null(input$words)) word_options()[1] else input$words
  })
  
  trajectory_data <- reactive({
    print("REGEX!")
    # print(regexpr(input$regex, "dog", perl=TRUE)[1])
    # print(input$regex)
    #words <- input$words
    #word_options <- word_options() # just so it can load at the same time
    #print(words)
    
    # TODO
    # if (length(words) == 0) {
    #   words = c("dog", "cat")
    # }
    
    #words <- ifelse(length(input$words) == 0, c("age", "fun"), input_words())
    
    print("FILTERED TABLE")
    print(filtered_tbl())
    result0 <- filtered_tbl() %>%
      select(-target_child_sex)
      # filter(regexpr(input$regex, gloss, perl=TRUE)[1] != -1)
      # filter(gloss %in% words | grepl(input$regex, gloss))
      # filter(gloss == input$regex)
    
    if (input$regex != "") {
      result0 <- filter(result0, gloss %in% input_words() | grepl(input$regex, gloss))
    } else {
      result0 <- filter(result0, gloss %in% input_words())
    }
      
    print("POST REGEX")
    print(result0)
    
    result <- result0 %>%
      # filter(gloss %in% words | regexpr(input$regex, gloss, perl=TRUE)[1] != -1) %>%
      group_by(speaker_role, target_child_age, gloss) %>%
      mutate(gloss_counts = sum(total_occurences))
    
    print(result)
    result1 <- result %>%
      distinct()
    
    print(result1)
    
    result2 <- result1 %>%
      group_by(speaker_role, gloss) %>% # filter out...
      filter(n_distinct(target_child_age) == length(range_seq())) %>% #  ...low occurence...
      ungroup() %>% # tokens...
      mutate(occurences_per_one_million = (1000000 / (total_tokens)) * gloss_counts) %>%
      select(-total_occurences, -total_tokens, -gloss_counts) %>%
      distinct()
    print(result2)
    result2
  })
  
  
  trajectory_plot <- reactive({
    print('plotting')
    #traj <- filtered_tbl()
    traj <- trajectory_data()
    print(traj)
    p <- ggplot(traj, aes(x=target_child_age, y=occurences_per_one_million, color=gloss)) + # col=gloss
      # geom_point(aes(col=speaker_role)) +
      # geom_line(aes(col=speaker_role)) +
      geom_point() + geom_line() +
      # facet_wrap(~target_child_sex) +
      xlab("Age") +
      ylab("Occurences / 1,000,000 Words") +
      scale_colour_solarized(guide = FALSE) +
      scale_fill_solarized(guide = FALSE) +
      geom_dl(aes(label=gloss), method = list(dl.trans(y = y + 0.5, x = x - 0.5), "last.qp", cex=1)) +
      scale_x_continuous(breaks = range_seq()) 
    p
  })
  
  
  # utils
  range_seq <- reactive({
    seq(as.double(input$slider[1]), as.double(input$slider[2]) - as.numeric(input$slider2), as.numeric(input$slider2))
  })
  
  observe({
    words <- word_options()
    updateSelectInput(session, "words", choices=words)
  })
  
  word_options <- reactive({
    filtered_tbl() %>%
      group_by(speaker_role, gloss) %>%
      filter(n_distinct(target_child_age) == length(range_seq())) %>%
      ungroup() %>%
      distinct(gloss) %>%
      mutate(gloss = gsub("[^[:alnum:]///' ]", "", gloss)) # handle 1 special case.. "merge"
  })
  
  output$plot1<- renderPlot({
    print('rendering plot')
    trajectory_plot()
  })
}

shinyApp(ui = ui, server = server)

