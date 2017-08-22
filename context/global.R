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
MIN_N_FOR_BIGRAMS <- 50

# CHILDES DATA
collections_df <- get_collections() %>% tbl_df() 
corpora_df <- get_corpora() %>% tbl_df()
participants_df <- get_participants() %>% 
  tbl_df()
collections <- collections_df$collection_name
