library(dplyr)
library(readr)
library(glue)
library(stringr)
library(purrr)

geo_lkp <- readRDS("data/geo_lkp.RDS")
party_data <- read_csv("lookup.csv") 
party_lkp <- party_data$official_party_name
names(party_lkp) <- party_data$ward_party_name

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
    arrange(desc(`Votes recorded`))  |> 
    select(Party, sname = 2, v = 6, el = 7)
  
  suppressMessages( mt <- read_excel(fl, sheet, skip = 1) )
  num_c <- as.numeric(mt[[7]][4])
  xs <- c("X", "x", "Elected")
  
  ### TEST 1: elected == expected number of councillors
  t1 <- length(which(df$el %in% xs)) == num_c
  
  ### TEST 2: elected councillors have more votes than rest of candidates
  t2 <- max(df$v[which(!df$el %in% xs)]) < min(df$v[which(df$el %in% xs)])
  i <- which(geo_lkp$f == fl & geo_lkp$tabname == sheet)
  if(length(i) != 1) {
    print(glue("ERROR!! failed to get geo data for {fl}"))
    NULL
  } else {
    geo <- geo_lkp[i,]
    print(glue("{t1} numc: {num_c} num_elected: {length(which(df$el %in% xs))} T2: {t2} WARD: {geo$LAD22NM[1]}-{geo$ward[1]}"))
    df$elected <- df$el %in% xs
    df$party <- party_lkp[df$Party]
    df$borough <- geo$LAD22NM[1]
    df$ward <- geo$ward[1]
    df
    
  }
}



get_data_for_borough <- function(fl) {
  sheets <- excel_sheets(fl)
  map_df(sheets, get_data_from_excel, fl = fl)
  
}

get_data_for_borough("data/Completed Forms/Newham local results form.xlsx") |> 
  filter(elected == TRUE)  |> 
  count(party)
  
# ERRORS 
# - CAMDEN. Number of Councillors is incorrect.
# - HACKNEY. x is marked on the wrong candidate
# - HILLINGDON. x is marked on the wrong candidate
# - LAMBETH. Missing an x in Knight's Hill
# - Lewisham

nwm <- get_data_for_borough("data/Completed Forms/Lambeth local results form for GLA.xlsx") |> 
  filter(elected == TRUE)  
