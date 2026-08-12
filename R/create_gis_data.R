# Prepare data for two maps: Elected Candidates by Ward and Borough Political
# Control. Each function starts with the complete candidates_all view so that
# the map-specific processing can be added separately.

library(DBI)
library(RSQLite)
library(readr)
library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(glue)

library(sf)
library(ggplot2)

# Prepare data for the Elected Candidates by Ward map.
create_elected_candidates_by_ward <- function(outfl = "data/gis/ward_results_2026.csv" ) {
  con <- dbConnect(SQLite(), "data/elections_2026.sqlite")
  on.exit(dbDisconnect(con), add = TRUE)

  candidates_all <- dbGetQuery(con, "SELECT * FROM candidates_all WHERE elected == 1")

  out <- candidates_all |> 
    summarise(.by = c(wd22cd, official_party_name, party_code, ward, LAD22NM), n = n())
  
  write_csv(out, file = outfl)
}

# Prepare data for the Borough Political Control map.
create_borough_political_control <- function(
  database_path = "data/elections_2026.sqlite"
) {
  con <- dbConnect(SQLite(), database_path)
  on.exit(dbDisconnect(con), add = TRUE)

  candidates_all <- dbGetQuery(con, "SELECT * FROM candidates_all") 
  

  b_cntrl <- map_df(
    unique(candidates_all$LAD22NM), 
         calculate_borough_control, 
         candidates_all = candidates_all) |> 
    arrange(LAD22NM)
}

# Prepare data for the turnout map.
create_turnout_map <- function(
  database_path = "data/elections_2026.sqlite"
) {
  con <- dbConnect(SQLite(), database_path)
  on.exit(dbDisconnect(con), add = TRUE)

  election_stats <- dbGetQuery(con, "SELECT * FROM election_stats")
  x <- election_stats$turnout
  xx <- str_replace_all(election_stats$turnout, "\\%", "")
  xy <- as.numeric(xx)
  xy[which(xy > 1)] <- xy[which(xy > 1)] / 100
  election_stats$adj_turnout <- xy

  # Add turnout-specific processing here.
  write_csv(election_stats |> select(wd22cd, adj_turnout), 
            file = "data/gis/turnout.csv")
}

##### -- UTILITY FUNCTIONS -- ####

# Calculate political control for one borough.
# Intended for use with purrr::map_df(), with `borough` supplied as `.x`.
calculate_borough_control <- function(borough, candidates_all) {
  
  lad_cd <- candidates_all$LAD22CD[which(candidates_all$LAD22NM == borough)]
  print(glue("{borough} found {length(unique(lad_cd))} codes: {unique(lad_cd)[1]}"))
  
  boro <- candidates_all |> 
      filter(LAD22NM == borough, elected == 1) |> 
      summarise(.by = c(official_party_name, party_code), n = n())  
  
  
  
  maj <- floor((sum(boro$n)+1)/2)
  
  w <- which.max(boro$n)
  
  most_seats <- boro$n[w]
  rest_seats <- sum(boro$n) - most_seats
  if(most_seats > rest_seats) {
    data.frame(LAD22CD = unique(lad_cd)[1], LAD22NM =borough, 
               party = boro$official_party_name[w], party_code = boro$party_code[w])
  } else {
    data.frame(LAD22CD = unique(lad_cd)[1], LAD22NM = borough, party = "No Overall Control", 
               party_code = "NOC")
    
  }
  
}

## Quick job for Jamie
save_postal_to_csv <- function() {
  con <- dbConnect(SQLite(), database_path)
  
  election_stats <- dbGetQuery(con, "SELECT e.wd22cd, entitled_electors, ballots_polling, ballots_postal, ward, LAD22NM FROM election_stats e LEFT JOIN geo_lkp g ON e.wd22cd = g.wd22cd;")
  dbDisconnect(con)
  election_stats$entitled_electors <- as.integer(election_stats$entitled_electors)
  election_stats$ballots_postal <- as.integer(election_stats$ballots_postal)
  election_stats$ballots_polling <- as.integer(election_stats$ballots_polling)
  write_csv(election_stats, "data/postal_other.csv")
  
}

calculate_to_ward <- function() {
  con <- dbConnect(SQLite(), database_path)
  candidates_all <- dbGetQuery(con, "SELECT * FROM candidates_all")
  dat <- candidates_all |> 
    summarise(.by = c(wd22cd, ward, LAD22NM, official_party_name,party_code),
              n = n(), val = mean(votes, na.rm = TRUE)) |> 
    ungroup()
  
  xdat <- dat |> 
    summarise(.by = c(wd22cd), tots = sum(val))
  
  ydat <- left_join(dat, xdat) |> 
    mutate(perc = val/tots)
  
  
}


test_map <- function() {
  x <- readRDS("data/gis/boroughs.rds")
  
  wards <- readRDS("data/gis/wards.rds") # |> 
  left_join(ydat |> filter(party_code == "LAB"))
  
  ggplot(wards) + geom_sf(aes(fill = adj_turnout)) 
  ggplot(wards) + geom_sf(aes(fill = val)) 
  
  ggplot(wards |> 
           left_join(ydat |> filter(party_code == "RUK"))) + 
    geom_sf(aes(fill = perc)) + 
    scale_fill_gradientn(colours = c("#FFFFFF","blue"), na.value = "#FFF") 
  
}

