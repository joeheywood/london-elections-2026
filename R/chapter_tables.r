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
  
  
  elected <- elected |> 
    summarise(.by = c(LAD22NM, party_code), el = n()) |> 
    pivot_wider(names_from = "party_code", values_from = el, values_fill = 0) |> 
    select(LAD22NM, LAB, CON, LD, GRE, RUK, ASP, REA, IND) 
  
  save(elected, file = "data/elected_table.RData")
  
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
  share_votes1 <- dat |> 
    summarise(.by = c(borough, party_code), v = sum(val)) |> 
    left_join(br_dat, by = "borough") |> 
    mutate(p = ((v / x)*100))|>
    mutate(p = round(p, 1))|>
    select(borough, party_code, p) |> 
    pivot_wider(names_from = party_code, values_from = p) |> 
    select(borough, CON, LAB, LD, GRE,RUK, REA, IND, OTH) |> 
    arrange(borough)
  save(share_votes1, file = "data/share_votes_1.RData")
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
  
  
  
  
  nm_cnd <- nrow(candidates_all)
  nm_sts <- sum(candidates_all$elected)
  cnd_p_st <- nm_cnd / nm_sts
  elct <- total_electorate
  nm_vt <- num_voting
  tnt <- (nm_vt / elct)*100
  
  poll <- sum(election_stats$ballots_polling_nm)
  post <- sum(election_stats$ballots_postal_nm)
  
  l_mrk <- sum(election_stats$lack_mark)
  mr_cnd <- sum(election_stats$more_candidates)
  vtr_id <- sum(election_stats$voter_identified)
  unmkd <- sum(election_stats$unmarked)
  rej <- l_mrk + mr_cnd + vtr_id + unmkd
  vb <- nm_vt - rej
  
  summ <- tribble(
    ~stat,                                     ~`2026`, ~`2022`, ~`2018`, ~`2014`, ~`2010`,
    "Total number of candidates",              nm_cnd,  6191,    6554,    6951,    6828,
    "Total number of seats",                   nm_sts,  1817,    1833,    1851,    1861,
    "Total number of seats contested",         nm_sts,  1817,    1833,    1851,    1861,
    "Average number of candidates per seat",   cnd_p_st,3.1,     3.6,     3.8,     3.7,
    "Total electorate",                        elct,    6042390, 5962160, 5878824, 5689223, 
    "Number voting",                           nm_vt,   2146159, 2315166, 2284882, 3524643,
    "Percentage voting",                       tnt,     35.5,    38.8,    38.9,    62.0,
    "Number at polling stations",              poll,    1445312, 1722464, 1754441, 2945839, 
    "Number of postal votes",                  post,    700847,  592702,  530441,  578803,
    "Lacking the official mark",               l_mrk,   192,     82,      201,     64,
    "Voting for more candidates than entitled",mr_cnd,  3154,    2746,    3020,    3874, 
    "Marked so that voter could be identified",vtr_id,  161,     374,     164,     756,
    "Unmarked or wholly void",                 unmkd,   8070,    4978,    9678,    16869,
    "Total rejected in whole",                 rej,     11577,    8158,   13208,   22371, 
    "Total number of valid ballot papers",     vb,      2134582, 2307008, 2271723, 3502273
  )
  save(summ, file = "data/summary_table.RData")
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
  tot <- con + lab + ld + oth
  
  
  num_councillors <- tribble(
    ~Year,  ~CON, ~LAB, ~LD,  ~Other, ~Total,
    "2026", con,  lab,  ld,   oth,    tot,
    "2022", 404,  1156, 180,  77,     1817,
    "2018", 511,  1123, 154,  45,     1833,
    "2014", 612,  1060, 116,  63,     1851,
    "2010", 717,  875,  246,  23,     1861,
    "2006", 785,  684,  317,  75,     1861,
    "2002", 653,  866,  309,  33,     1861,
    "1998", 538,  1050, 301,  28,     1917,
    "1994", 519,  1044, 323,  31,     1917,
    "1990", 731,  925,  229,  29,     1914,
    "1986", 685,  957,  249,  23,     1914,
    "1982", 980,  781,  124,  29,     1914,
    "1978", 960,  882,  30,   36,     1908,
    "1974", 713,  1090, 27,   37,     1867,
    "1971", 597,  1221, 9,    36,     1863,
    "1968", 1438, 350,  10,   65,     1863,
    "1964", 668,  1112, 13,   66,     1859
  )
  
}



add_percentage_councillors_row <- function() {
  norm <- calculate_to_ward()
  
  con <- dbConnect(SQLite(), database_path)
  
  election_stats <- dbGetQuery(
    con,
    paste0(
      "SELECT wd22cd, entitled_electors, ballots_polling, ballots_postal ",
      "FROM election_stats;"
    )
  )
  dbDisconnect(con)
  
  election_stats$entitled_electors_nm <- convert_to_numeric_integer(election_stats$entitled_electors)
  total_electorate <- sum(election_stats$entitled_electors_nm)
  
  election_stats$ballots_polling_nm <- convert_to_numeric_integer(election_stats$ballots_polling)
  election_stats$ballots_postal_nm <- convert_to_numeric_integer(election_stats$ballots_postal)
  election_stats$number_voting <- election_stats$ballots_polling_nm + election_stats$ballots_postal_nm
  num_voting <- sum(election_stats$number_voting)
  
  total_votes <- sum(norm$val)
  
  lab_perc <- (sum(norm$val[which(norm$party_code == "LAB")]) / total_votes)*100
  con_perc <- (sum(norm$val[which(norm$party_code == "CON")]) / total_votes)*100
  ld_perc <- (sum(norm$val[which(norm$party_code == "LD")]) / total_votes)*100
  oth_perc <- (sum(norm$val[which(!norm$party_code %in% c("CON", "LAB", "LD"))]) / total_votes)*100
  data.frame(lab_perc, con_perc, ld_perc, oth_perc)
  tnt26 <- (num_voting / total_electorate) * 100
  
  tribble(
  ~year, ~`% poll`, ~CON, ~LAB, ~LD, ~Other,
  "2026", tnt26, con_perc, lab_perc, ld_perc, oth_perc,
  "2022",35.5,26.2,42.2,14.1,17.5,
  "2018",38.8,29.0,44.1,12.7,14.2,
  "2014",38.9,26.1,37.4,10.2,26.3,
  "2010",62.0,32.0,32.6,22.0,13.4,
  "2006",37.9,35.1,27.6,20.3,17.0,
  "2002",31.8,34.4,33.8,20.3,11.6,
  "1998",34.7,32.3,40.5,20.6,6.6,
  "1994",46.1,31.3,41.5,21.8,5.4,
  "1990",48.1,37.8,38.3,14.1,9.7,
  "1986",45.4,35.8,37.4,23.8,3.1,
  "1982",43.8,43.0,30.4,24.6,2.0,
  "1978",42.9,49.6,39.6,6.4,4.4,
  "1974",36.3,41.7,42.9,12.3,3.1,
  "1971",38.7,39.4,53.1,4.2,3.3,
  "1968",35.8,60.1,28.3,7.2,4.4
  ) 
  
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

num_votes_table <- function() {
  norm <- calculate_to_ward()
  norm$party_code[which(!norm$party_code %in% c("LAB", "CON", "LD", "GRE", "RUK"))] <- "OTH"
  total_votes <- sum(norm$val)
  n_votes <- norm |> 
    summarise(.by = c(LAD22NM, party_code), v = sum(val)) |> 
    pivot_wider(names_from = party_code, values_from = v) |> 
    arrange(LAD22NM) |> 
    select(LAD22NM, CON, LAB, LD, GRE, RUK, OTH)
  save(n_votes, file = "data/num_votes_table.RData")
}


share_votes_table <- function(n_votes) {
  n_votes$total <- n_votes$CON + n_votes$LAB + n_votes$LD + n_votes$GRE + n_votes$RUK + n_votes$OTH
  share_votes <- n_votes |> select(LAD22NM, total)
  share_votes$LAB <- n_votes$LAB / n_votes$total
  share_votes$CON <- n_votes$CON / n_votes$total
  share_votes$LD <- n_votes$LD / n_votes$total
  share_votes$GRE <- n_votes$GRE / n_votes$total
  share_votes$RUK <- n_votes$RUK / n_votes$total
  share_votes$OTH <- n_votes$OTH / n_votes$total
  
  save(share_votes, file = "data/share_votes_table.RData")

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
