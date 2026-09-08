library(DBI)
library(RSQLite)

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(glue)

source("R/create_gis_data.R")

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
      "SELECT e.wd22cd, entitled_electors, ballots_polling, valid_ballots, num_councillors, turnout, ",
      "ballots_postal, ward, LAD22NM, rb_official_mark, rb_more_candidates, rb_voter_defined, ",
      "rb_unmarked_void, rb_rejected_part, rejected_ballots ",
      "FROM election_stats e ",
      "LEFT JOIN geo_lkp g ON e.wd22cd = g.wd22cd;"
    )
  )
  dbDisconnect(con)
  
  election_stats$entitled_electors_nm <- convert_to_numeric_integer(election_stats$entitled_electors)
  total_electorate <- sum(election_stats$entitled_electors_nm)
  
  election_stats$ballots_polling_nm <- convert_to_numeric_integer(election_stats$ballots_polling)
  election_stats$ballots_postal_nm <- convert_to_numeric_integer(election_stats$ballots_postal)
  election_stats$number_voting <- election_stats$ballots_polling_nm + election_stats$ballots_postal_nm
  num_voting <- sum(election_stats$number_voting)
  
  
  election_stats$lack_mark <- convert_to_numeric_integer(election_stats$rb_official_mark)
  election_stats$more_candidates <- convert_to_numeric_integer(election_stats$rb_more_candidates)
  election_stats$voter_identified <- convert_to_numeric_integer(election_stats$rb_voter_defined)
  election_stats$unmarked <- convert_to_numeric_integer(election_stats$rb_unmarked_void)
  
  turnout <- num_voting / total_electorate
  
  election_stats$total_rejected <- election_stats$lack_mark + election_stats$more_candidates + 
    election_stats$voter_identified + election_stats$unmarked
  
  rejected <- sum(election_stats$total_rejected)
  
  
  valid_ballots <- num_voting - rejected
  
  
  
  # election_stats$ballots_polling_clean <- as.numeric( 
  #   str_replace_all(election_stats$ballots_polling, "\\.([0-9]+)", "")
  # )
  # 
  # election_stats$ballots_postal_clean <- as.numeric( 
  #   str_replace_all(election_stats$ballots_postal, "\\.([0-9]+)", "")
  # )
  # 
  # 
  # num_at_polling_st <- sum(as.numeric(election_stats$ballots_polling))
  # num_postal_votes <- sum(as.numeric(election_stats$ballots_postal))
  # election_stats$number_voting <- election_stats$ballots_polling_clean + election_stats$ballots_postal_clean
  # 
  # 
  # 
  # election_stats$vb_clean <- str_replace_all(election_stats$valid_ballots, "\\.([0-9]+)", "")
  # election_stats$vb <- as.numeric( str_replace_all(election_stats$vb_clean, "[^0-9]", "")) 
  # 
  # election_stats$els_clean <- str_replace_all(election_stats$entitled_electors, "\\.([0-9]+)", "")
  # 
  # 
  # election_stats$ee <- as.numeric( str_replace_all(election_stats$els_clean, "[^0-9]", "")) 
  # election_stats$pol <- as.numeric( str_replace_all(election_stats$ballots_polling, "[^0-9]", "")) 
  # 
  # 
  # ww <- which(is.na(election_stats$vb))
  # election_stats$vb[ww] <- as.numeric(election_stats$ballots_polling[ww]) + as.numeric(election_stats$ballots_postal[ww]) 
  # sum(election_stats$vb)
  # election_stats$tt <- election_stats$vb / election_stats$ee
  
  num_candidates <- nrow(candidates_all)
  num_seats <- sum(candidates_all$elected)
  sum(as.numeric(election_stats$num_councillors))
  candidates_per_seat <- num_candidates / num_seats
  
  

  
  
  
  
  
  
}

add_number_councillors_row <- function() {
  con <- dbConnect(SQLite(), database_path)
  
  # Retrieve all candidate results.
  candidates_all <- dbGetQuery(
    con,
    "SELECT * FROM candidates_all WHERE elected = 1"
  )
  dbDisconnect(con)
  con <- length(which(candidates_all$party_code == "CON"))
  lab <- length(which(candidates_all$party_code == "LAB"))
  ld <- length(which(candidates_all$party_code == "LD"))
  oth <- length(which(!candidates_all$party_code %in% c("CON", "LAB", "LD")))
  data.frame(con, lab, ld, oth)
  
}

add_percentage_councillors_row <- function() {
  norm <- calculate_to_ward()
  total_votes <- sum(norm$val)
  
  lab_perc <- sum(norm$val[which(norm$party_code == "LAB")]) / total_votes
  con_perc <- sum(norm$val[which(norm$party_code == "CON")]) / total_votes
  ld_perc <- sum(norm$val[which(norm$party_code == "LD")]) / total_votes
  oth_perc <- sum(norm$val[which(!norm$party_code %in% c("CON", "LAB", "LD"))]) / total_votes
  data.frame(lab_perc, con_perc, ld_perc, oth_perc)
  
}

number_candidates_votes_table <- function() {
  con <- dbConnect(SQLite(), database_path)
  # Retrieve all candidate results.
  candidates_all <- dbGetQuery(
    con,
    "SELECT * FROM candidates_all"
  )
  dbDisconnect(con)
  
  candidates_all$party_code[which(!candidates_all$party_code %in% c("LAB", "CON", "LD"))] <- "OTH"
  
  votes <- candidates_all |> 
    summarise(.by = c(party_code, LAD22NM), votes = sum(votes)) |> 
    pivot_wider(names_from = party_code, values_from = votes, names_prefix = "v_") |> 
    arrange(LAD22NM)
  
  num_cands <- candidates_all |> 
    summarise(.by = c(party_code, LAD22NM), candidates = n()) |> 
    pivot_wider(names_from = party_code, values_from = candidates, names_prefix = "n_") |> 
    arrange(LAD22NM)
  
  left_join(votes, num_cands) |> 
    mutate(total_cands = n_CON + n_LAB + n_LD + n_OTH,
           total_votes = v_CON + v_LAB + v_LD + v_OTH) |> 
    select(n_CON, n_LAB, n_LD, n_OTH, total_cands, v_CON, v_LAB, v_LD, v_OTH, total_votes) 
  
}

share_votes_table <- function() {
  
}





convert_to_numeric_integer <- function(x) {
  x[which(is.na(x))] <- 0
  x <- str_replace_all(x, "\\.([0-9]+)", "")
  as.numeric( str_replace_all(x, "[^0-9]", "")) 
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
