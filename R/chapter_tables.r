library(DBI)
library(RSQLite)

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(glue)

create_table_1 <- function() {
  con <- dbConnect(SQLite(), "data/elections_2026.sqlite")
  on.exit(dbDisconnect(con), add = TRUE)
  
  elected <- dbGetQuery( con, "SELECT lad22cd, lad22nm, party_code FROM candidates_all WHERE elected == 1" ) |> 
    arrange(LAD22CD)
  
  
  elected |> 
    summarise(.by = c(LAD22NM, party_code), el = n()) |> 
    pivot_wider(names_from = "party_code", values_from = el, values_fill = 0) |> 
    select(LAD22NM, LAB, CON, LD, GRE, RUK, ASP, REA, IND) 
  
}



create_table_2 <- function() {
  
  # Connect to the database.
  #
  # NOTE: As with `save_postal_to_csv()`, `database_path` is currently assumed
  #       to exist outside the function.
  con <- dbConnect(SQLite(), database_path)
  
  # Retrieve all candidate results.
  candidates_all <- dbGetQuery(
    con,
    "SELECT * FROM candidates_all"
  )
  
  # Calculate the mean number of votes for each party in each ward.
  #
  # `n` records how many candidates are represented in the group.
  #
  # `val` is the mean vote count. The use of mean() rather than sum() appears
  # deliberate, but is worth checking depending on what the intended map is
  # meant to represent.
  dat <- candidates_all |>
    summarise(
      .by = c(
        wd22cd,
        ward,
        LAD22NM,
        official_party_name,
        party_code
      ),
      n = n(),
      val = mean(votes, na.rm = TRUE)
    ) |>
    ungroup() |> 
    rename(borough = LAD22NM)
  
  
  codes <- c("LAB", "CON", "LD", "GRE", "REA", "IND", "RUK")
  dat$party_code[which(!dat$party_code %in% codes)] <- "OTH"
  
  br_dat <- summarise(dat, .by = c(borough), x = sum(val)) 
  
  # dat is returned implicitly.
  dat |> 
    summarise(.by = c(borough, party_code), v = sum(val)) |> 
    left_join(br_dat, by = "borough") |> 
    mutate(p = ((v / x)*100))|>
    mutate(p = round(p, 1))|>
    select(borough, party_code, p) |> 
    pivot_wider(names_from = party_code, values_from = p) |> 
    select(borough, CON, LAB, LD, GRE,RUK, REA, IND, OTH) |> 
    arrange(borough)
}


add_col_to_table_3 <- function() {
  con <- dbConnect(SQLite(), database_path)
  
  # Retrieve all candidate results.
  candidates_all <- dbGetQuery(
    con,
    "SELECT * FROM candidates_all"
  )
  
  # if there isn't a matching row in `geo_lkp`.
  election_stats <- dbGetQuery(
    con,
    paste0(
      "SELECT e.wd22cd, entitled_electors, ballots_polling, valid_ballots, num_councillors, ",
      "ballots_postal, ward, LAD22NM ",
      "FROM election_stats e ",
      "LEFT JOIN geo_lkp g ON e.wd22cd = g.wd22cd;"
    )
  )
  
  election_stats$vb <- as.numeric(election_stats$valid_ballots)
  
  ww <- which(is.na(as.numeric(election_stats$valid_ballots)))
  election_stats$vb[ww] <- as.numeric(election_stats$ballots_polling[ww]) + as.numeric(election_stats$ballots_postal[ww]) 
  
  # Calculate the mean number of votes for each party in each ward.
  #
  # `n` records how many candidates are represented in the group.
  #
  # `val` is the mean vote count. The use of mean() rather than sum() appears
  # deliberate, but is worth checking depending on what the intended map is
  # meant to represent.
  dat <- candidates_all |>
    summarise(
      .by = c(
        wd22cd,
        ward,
        LAD22NM,
        official_party_name,
        party_code
      ),
      n = n(),
      val = mean(votes, na.rm = TRUE)
    ) |>
    ungroup() |> 
    rename(borough = LAD22NM)
  
  d22 <- read_csv("data/ward_level_cleaned_2022.csv")
  
  
  
  
}

calculate_to_ward_2022 <- function() {
  cnd22 <- read_csv("data/candidate_level_cleaned_2022.csv")
  prty22 <- read_csv("data/party_lookup.csv")
  prty26 <- read_csv("data/party_codes.csv")
  cnd22 <- cnd22 |> left_join(prty26, by = c(party = "ward_party_name"))
  
  dat22 <- cnd22 |>
    summarise(
      .by = c(
        WD22CD,
        ward,
        borough,
        official_party_name,
        party_code
      ),
      n = n(),
      val = mean(votes, na.rm = TRUE),
      tot = sum(votes, na.rm = TRUE)
    ) |>
    ungroup()
  
  
  # Calculate the total of the party-level values for each ward.
  #
  # This gives us the denominator used to calculate each party's share.
  
  unique(ydat22$party_code)
  codes <- c("LAB", "CON", "LD", "GRE", "REA", "IND")
  ydat22$party_code[which(!ydat22$party_code %in% codes)] <- "OTH"
  
  br_dat <- summarise(ydat22, .by = c(borough), x = sum(val)) 
  
  # ydat is returned implicitly.
  ydat22 |> 
    summarise(.by = c(borough, party_code), v = sum(val)) |> 
    left_join(br_dat, by = "borough") |> 
    mutate(p = ((v / x)*100))|>
    mutate(p = round(p, 1))|>
    select(borough, party_code, p) |> 
    pivot_wider(names_from = party_code, values_from = p) |> 
    select(borough, CON, LAB, LD, GRE, REA, IND, OTH)
  
}
