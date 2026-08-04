library(RSQLite)
library(dplyr)
library(glue)
library(stringr)

##### -- EXPORTED FUNCTIONS -- ####

create_database <- function() {
  
}


ingest_candidates_table <- function() {
  
}

ingest_election_stats_table <- function() {
  
}



















##### -- UTILITY FUNCTIONS -- ####

db_get_data_from_excel <- function(sheet, fl) {
  suppressMessages(
    df <- read_excel(fl, sheet, skip = 18) 
  )
  wrong_names <- c("Name", "NAME", "Surname")
  for(wn in wrong_names) {
    if(wn %in% names(df)) {
      df$`Candidate surname` <- df[[wn]]
      
    }
    
  }
  df <- df %>% 
    filter(!is.na(`Candidate surname`)) %>% 
    mutate(`Votes recorded` = str_replace_all(as.character(`Votes recorded`), "[,]", "")) %>% 
    mutate(`Votes recorded` = as.numeric(str_trim(`Votes recorded`))) %>% 
    mutate(Party = str_trim(Party, side = "both")) %>% 
    mutate(Party = str_replace_all(Party, "\u200B", "")) %>% 
    arrange(desc(`Votes recorded`))
}

match_geography <- function(sheet, fl) {
  
}
