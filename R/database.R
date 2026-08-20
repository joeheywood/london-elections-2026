# This file builds the local elections database by reading candidate results and
# election statistics from Excel workbooks, standardizing their fields, matching
# worksheets to ward geography, loading geography and party lookups, writing the
# resulting tables to SQLite, and creating an enriched candidate-results view.

library(RSQLite)
library(dplyr)
library(glue)
library(stringr)
library(readxl)
library(readr)

##### -- EXPORTED/PUBLIC FUNCTIONS -- ####

create_database <- function() {
  file.remove("data/elections_2026.sqlite")
  ingest_candidates_table()
  ingest_election_stats_table()
  ingest_geo()
  ingest_party_lkp()
}

# Read and standardize every candidate-results workbook in the completed-forms
# directory, then write the combined data to the SQLite candidates table.
ingest_candidates_table <- function() {
  fls <- list.files(
    "data/Completed Forms",
    pattern = "\\.xlsx$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  cn <- RSQLite::dbConnect(SQLite(), "data/elections_2026.sqlite")
  all_data <- map_df(fls, run_for_file)
  dbWriteTable(cn, "candidates", all_data)
  dbDisconnect(cn)
  
}

# Extract ward-level election statistics from every file in the global `fls`
# vector and write the combined data to the SQLite election_stats table.
ingest_election_stats_table <- function() {
  stats <- map_df(fls, run_stats_for_file)
  cn <- RSQLite::dbConnect(SQLite(), "data/elections_2026.sqlite")
  dbWriteTable(cn, "election_stats", stats)
  dbDisconnect(cn)
}

# Load the saved ward geography lookup and write it to the SQLite geo_lkp table.
ingest_geo <- function() {
  geo <- readRDS("data/geo_lkp.RDS")
  cn <- RSQLite::dbConnect(SQLite(), "data/elections_2026.sqlite")
  dbWriteTable(cn, "geo_lkp", geo)
  dbDisconnect(cn)
}

# Load the party-name lookup CSV and write it to the SQLite party_lookup table.
ingest_party_lkp <- function() {
  prty <- read_csv("data/party_lookup.csv")
  cn <- RSQLite::dbConnect(SQLite(), "data/elections_2026.sqlite")
  dbWriteTable(cn, "party_lookup", prty, overwrite = TRUE)
  dbDisconnect(cn)
}

# Create a reusable SQLite view that enriches candidate results with
# standardized party names and ward geography from the lookup tables.
create_candidates_view <- function() {
  cn <- RSQLite::dbConnect(SQLite(), "data/elections_2026.sqlite")
  dbSendQuery(cn, "DROP VIEW IF EXISTS candidates_all;")
  dbSendQuery(
    cn,
    "CREATE VIEW IF NOT EXISTS candidates_all 
      AS 
        SELECT c.wd22cd, ward, LAD22NM, LAD22CD, official_party_name, party_code,
        surname, 
               firstname, votes, elected 
        FROM candidates c 
           LEFT JOIN party_lookup p ON c.party = p.ward_party_name 
           LEFT JOIN geo_lkp g ON c.wd22cd = g.wd22cd;"
    
    
  )
  
  dbDisconnect(cn)
}


##### -- UTILITY FUNCTIONS -- ####

# Prepare the geography lookup used to match source filenames and sheet names.
geo <- readRDS("data/geo_lkp.RDS") |> 
  mutate(f = basename(f))

# Read one worksheet's candidate rows, accommodate known column-name variants,
# clean the fields, and return a standardized candidate data frame.
# `sheet` identifies the worksheet and `fl` is the source Excel file path.
db_get_data_from_excel <- function(sheet, fl) {
  print(sheet)
  suppressMessages(
    df <- read_excel(fl, sheet, skip = 18) 
  )
  wrong_names <- c("Name", "NAME", "Surname")
  for(wn in wrong_names) {
    if(wn %in% names(df)) {
      df$`Candidate surname` <- df[[wn]]
      
    }
    
  }
  if(!"First name" %in% names(df)) {
    df$`First name` <- ""
  }
  if(!"Whether Female (if known)" %in% names(df)) {
    print(glue("NO FEMALE COLUMN IN {basename(fl)} {sheet}"))
    df$`Whether Female (if known)` <- ""
  }
  

  wrong_f_names <- c("Name", "NAME", "Surname")
  df <- df %>% 
    filter(!is.na(`Candidate surname`)) %>% 
    mutate(`Votes recorded` = str_replace_all(as.character(`Votes recorded`), "[,]", "")) %>% 
    mutate(`Votes recorded` = as.numeric(str_trim(`Votes recorded`))) %>% 
    mutate(Party = str_trim(Party, side = "both")) %>% 
    mutate(Party = str_replace_all(Party, "\u200B", "")) %>% 
    arrange(desc(`Votes recorded`)) |> 
    select(surname = `Candidate surname`, firstname = `First name`, party = Party, female = `Whether Female (if known)`,
           votes = `Votes recorded`, elected = `If candidate was elected, mark with an 'X'` )
  
  xs <- c("X", "x", "Elected")
  df$elected <- df$elected %in% xs
  df |> 
    mutate(sheetname = sheet, fl = basename(fl)) |> 
    select(sheetname, fl, surname, firstname, party, female, votes, elected)
}


# Match candidate rows to ward codes using their source filename and sheet name;
# stop with an error if any row cannot be matched to the geography lookup.
match_geography <- function(df) {
  gdf <- left_join(df, geo, by = c(fl = "f", sheetname = "tabname")) 
  if(any(is.na(gdf$WD22CD))) {
    stop(glue("GEO MATCH ERROR {df$fl[1]"))
    data.frame()
  } else {
    gdf |> 
      select(wd22cd = WD22CD, surname, firstname, party, female, votes, elected)
  }
}

# Process every worksheet in one candidate-results workbook, combine the rows,
# and attach the corresponding ward code through the geography lookup.
run_for_file <- function(fl) {
  print(basename(fl))
  purrr::map_df(excel_sheets(fl), db_get_data_from_excel, fl = fl) |> 
    match_geography()
}


# Read the summary section of one worksheet and extract election statistics from
# the fixed cells used by the source workbook template.
run_election_stats <- function(sheet, fl) {
  suppressMessages(
    df <- read_excel(fl, sheet, skip = 2) 
  )
  stats <- df[1:14, c(1:2,7)]
  names(stats) <- c("main", "second", "values")
  data.frame(
    f = basename(fl),
    sheet = sheet,
    num_councillors = stats$values[3],
    entitled_electors = stats$values[4],
    valid_ballots = stats$values[5],
    ballots_polling = stats$values[6],
    ballots_postal = stats$values[7],
    turnout = stats$values[8],
    rejected_ballots = stats$values[9],
    rb_official_mark = stats$values[10],
    rb_more_candidates = stats$values[11],
    rb_voter_defined = stats$values[12],
    rb_unmarked_void = stats$values[13],
    rb_rejected_part = stats$values[14]
  )
}

# Extract statistics from every worksheet in one workbook, match each worksheet
# to its ward code, and return one standardized row per ward.
run_stats_for_file <- function(fl) {
  print(basename(fl))
  
  map_df(excel_sheets(fl), run_election_stats, fl = fl) |> 
    left_join(geo, by = c(f = "f", sheet = "tabname")) |> 
    select(wd22cd = WD22CD, num_councillors:rb_rejected_part)
}

