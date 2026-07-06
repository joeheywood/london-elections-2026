library(RSQLite)
library(dplyr)
library(readr)
library(stringr)
library(readxl)
library(purrr)

dbfl <- "~/Downloads/elections.sqlite"

cn <- RSQLite::dbConnect(SQLite(),dbfl)
dbListTables(cn)

dbGetQuery(cn, "select * from election_candidates limit 10")
dbGetQuery(cn, "select * from election_stats limit 10")

lkp <- dbGetQuery(cn, "select * from party_lookup")



calculate_ward_level_averages <- function(wrd) {
  
}


write_csv(lkp, "lookup.csv")
lkp <- read_csv("lookup.csv")


apply_lookup_to_borough <- function(fl) {
  print(fl)
  wrds <- excel_sheets(fl)
  boro_df <- map_df(wrds, get_data_from_excel, fl = fl)
  parties <- unique(boro_df$Party)
  # ll <- 
  mss <- parties[which(!parties %in% lkp$ward_party_name)]
  if(length(mss) > 0 ) {
    data.frame(boro = basename(fl), mss = mss, stringsAsFactors = FALSE)
    
  } else {
    data.frame()
  }
  
}

df <- map_df(dir("data/Completed Forms/", full.names = TRUE), apply_lookup_to_borough)

get_data_from_excel <- function(sheet, fl) {
  # print(sheet)
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
  
  df
}



