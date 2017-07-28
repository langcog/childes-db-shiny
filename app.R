library(shiny)
library(magrittr)
library(dbplyr)
library(dplyr)
library(ggplot2)
library(RMySQL)

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
         textInput("word", "Word", "eat"),
         selectInput("min_age", label = "Min Age (months)", 
                     choices = seq(0, 90, 6), selected = 12),
         selectInput("max_age", label = "Max Age (months)", 
                     choices = seq(5, 95, 6), selected = 65),
         selectInput("group_size", label = "Group Size (months)", 
                     choices = seq(3, 24, 3), selected = 12),
         submitButton("Submit")
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

server <- function(input, output) {
  
  # data <- reactive({
  #   validate(
  #     need(as.numeric(input$max_age) > as.numeric(input$min_age), "Max age must not be less than Min age")
  #   )
  # })
  
  get_con <- function () {
    print('opening connection to DB')
    host = "ec2-35-161-14-116.us-west-2.compute.amazonaws.com"
    dbname = "childesdb"
    user = "childesdb"
    password = "uy5z4hf7ihBjf"
    con <- DBI::dbConnect(RMySQL::MySQL(), 
                          host = host,
                          user = user,
                          dbname = dbname,
                          password = password
    )
    con
  }

  
  df3 <- reactive({
    print('running query')
    
    word = input$word
    min_age = input$min_age
    max_age = input$max_age
    group_size = input$group_size
    
    con <- get_con()
    token_tbl <- dplyr::tbl(con, "token")
    
    df0 <- token_tbl %>%
      filter(speaker_role == "Target_Child", between(speaker_age, min_age*30, max_age*30)) %>%
      group_by(speaker_age = floor((speaker_age/30 - min_age) / group_size) * group_size + min_age) %>%
      summarise(total_tokens = n(), total_occurences = sum(gloss == !!word)) %>%
      mutate(occurences_per_one_million = (1000000 / (total_tokens)) * total_occurences)
    
    df2 <- tbl_df(df0)
    print('closing connection to DB')
    DBI::dbDisconnect(con)
    df2
  })
  
  plot0 <- reactive({
    print('plotting')
    p <- ggplot(df3(), aes(x=speaker_age, y=occurences_per_one_million)) +
      geom_point() +
      geom_line() +
      xlab("Age") +
      ylab("Occurences / 1,000,000 Words") +
      scale_x_continuous(breaks = seq(min(df3()$speaker_age), max(df3()$speaker_age), as.numeric(input$group_size))) 
    p
  })
  
  output$plot1<- renderPlot({
    print('rendering plot')
    validate(
      need(input$max_age < input$min_age, "Max age must not be less than Min age")
    )
    plot0()
  })
}

shinyApp(ui = ui, server = server)

