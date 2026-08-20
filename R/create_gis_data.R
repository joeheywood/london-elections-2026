# =============================================================================
# PREPARE DATA FOR GIS MAPS
# =============================================================================
#
# This script prepares data from the 2026 election SQLite database for use in
# GIS maps.
#
# There are currently three main map datasets being prepared:
#
#   1. Elected candidates by ward
#      -> Which parties won how many seats in each ward?
#
#   2. Borough political control
#      -> Which party controls each borough, or is it under No Overall Control?
#
#   3. Turnout by ward
#      -> What was the turnout in each ward?
#
# The general approach is to keep the database as the source of truth and
# create relatively small, map-specific datasets from it. This means that the
# GIS files don't need to contain all of the underlying election data.
#
# The main tables/views used here are:
#
#   candidates_all
#       Candidate-level election results. This is the main source for the
#       elected-candidate and political-control maps.
#
#   election_stats
#       Ward-level election statistics, including turnout.
#
#   geo_lkp
#       Geographic lookup information used to associate election data with
#       wards/boroughs.
# =============================================================================


# -----------------------------------------------------------------------------
# Packages
# -----------------------------------------------------------------------------

library(DBI)
library(RSQLite)

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(glue)

library(sf)
library(ggplot2)


# =============================================================================
# MAP DATA PREPARATION FUNCTIONS
# =============================================================================


# -----------------------------------------------------------------------------
# Elected candidates by ward
# -----------------------------------------------------------------------------
#
# Creates a CSV containing the number of elected candidates from each party
# in each ward.
#
# The output is intended to be joined to the ward geometry using `wd22cd`.
#
# For example, a resulting row might represent:
#
#   ward       = "Example Ward"
#   LAB         = 2
#   CON         = 1
#
# The data starts from `candidates_all`, but we immediately restrict it to
# elected candidates. This means each row in the source data represents a
# winning candidate rather than every candidate who stood.
# -----------------------------------------------------------------------------

create_elected_candidates_by_ward <- function(
    outfl = "data/gis/ward_results_2026.csv"
) {
  
  # Open the election database.
  #
  # `on.exit()` ensures that the connection is closed even if something goes
  # wrong later in the function.
  con <- dbConnect(SQLite(), "data/elections_2026.sqlite")
  on.exit(dbDisconnect(con), add = TRUE)
  
  # Only retrieve elected candidates.
  #
  # `elected == 1` is important here because the map is about seats won,
  # rather than votes/candidates who stood.
  candidates_all <- dbGetQuery(
    con,
    "SELECT * FROM candidates_all WHERE elected == 1"
  )
  
 
  # ---------------------------------------------------------------------------
  # Convert from "long" party data to "wide" party data.
  # ---------------------------------------------------------------------------
  #
  # At this point we still have one row per ward/party combination.
  #
  # `pivot_wider()` turns the party codes into columns, which makes the output
  # much easier to use in a map.
  #
  # For example:
  #
  #   wd22cd   ward        party_code   n
  #   E01001   Example     LAB          2
  #   E01001   Example     CON          1
  #
  # becomes something like:
  #
  #   wd22cd   ward        LAB   CON
  #   E01001   Example      2     1
  #
  # Missing party/ward combinations become NA rather than zero.
  # ---------------------------------------------------------------------------
  
  out <- candidates_all |>
    summarise(
      .by = c(
        wd22cd,
        party_code,
        ward,
        LAD22NM
      ),
      n = n()
    ) |>
    pivot_wider(
      names_from = "party_code",
      values_from = n
    )
  
  # Write the map-ready dataset to CSV.
  write_csv(out, file = outfl)
}


# -----------------------------------------------------------------------------
# Borough political control
# -----------------------------------------------------------------------------
#
# Creates a dataset containing one row per London borough indicating:
#
#   - the borough code
#   - the borough name
#   - the party controlling the borough
#   - the party code
#
# The actual calculation of control is handled by `calculate_borough_control()`
# below. This function is mainly responsible for:
#
#   1. loading the database data;
#   2. identifying each borough;
#   3. running the control calculation for every borough;
#   4. combining the results.
# -----------------------------------------------------------------------------

create_borough_political_control <- function(
    database_path = "data/elections_2026.sqlite"
) {
  
  # Connect to the election database and make sure the connection is closed
  # when the function exits.
  con <- dbConnect(SQLite(), database_path)
  on.exit(dbDisconnect(con), add = TRUE)
  
  # Unlike the elected-candidate map, we need all candidates here because
  # `calculate_borough_control()` performs the filtering for elected
  # candidates itself.
  candidates_all <- dbGetQuery(
    con,
    "SELECT * FROM candidates_all"
  )
  
  # Run the borough-control calculation once for each unique borough.
  #
  # `map_df()`:
  #   - takes each borough name as `.x`;
  #   - passes that borough to `calculate_borough_control()`;
  #   - combines the resulting data frames into one data frame.
  #
  # The result should therefore contain one row per borough.
  b_cntrl <- map_df(
    unique(candidates_all$LAD22NM),
    calculate_borough_control,
    candidates_all = candidates_all
  ) |>
    arrange(LAD22NM)
  
  # The object is currently returned implicitly by the function.
  b_cntrl
}


# -----------------------------------------------------------------------------
# Turnout map
# -----------------------------------------------------------------------------
#
# Creates a ward-level turnout dataset.
#
# The source `election_stats$turnout` is not necessarily stored consistently:
# it may contain percentages such as "42%" or decimal values such as "0.42".
#
# This function standardises those values into a single decimal representation
# between 0 and 1.
#
# For example:
#
#   "42%"  -> 0.42
#   "0.42" -> 0.42
#   "42"   -> 0.42
#
# Where turnout is missing, it is calculated from:
#
#   valid ballots / entitled electors
#
# The resulting value is called `adj_turnout`.
# -----------------------------------------------------------------------------

create_turnout_map <- function(
    database_path = "data/elections_2026.sqlite"
) {
  
  # Connect to the database.
  con <- dbConnect(SQLite(), database_path)
  on.exit(dbDisconnect(con), add = TRUE)
  
  # Retrieve the ward-level election statistics.
  election_stats <- dbGetQuery(
    con,
    "SELECT * FROM election_stats"
  )
  
  # Keep the original turnout value temporarily for debugging/reference.
  x <- election_stats$turnout
  
  # Remove percentage signs.
  #
  # For example:
  #   "45%" -> "45"
  #
  # The escaped `%` isn't actually necessary in a regex, but the expression
  # explicitly looks for a literal percentage sign.
  xx <- str_replace_all(
    election_stats$turnout,
    "\\%",
    ""
  )
  
  # Convert the cleaned values to numeric.
  xy <- as.numeric(xx)
  
  # Standardise values greater than 1.
  #
  # Values such as 45 represent 45%, whereas the desired representation is
  # 0.45. Values already expressed as decimals (e.g. 0.45) are left alone.
  xy[which(xy > 1)] <- xy[which(xy > 1)] / 100
  
  election_stats$adj_turnout <- xy
  
  # Find any rows where turnout could not be converted to a number.
  mss <- which(is.na(election_stats$adj_turnout))
  
  print(glue("Missing {length(mss)} rows"))
  
  # For missing turnout values, calculate turnout directly from the ballot
  # and elector counts.
  #
  # This provides a fallback where the supplied turnout field is missing or
  # couldn't be parsed.
  election_stats$adj_turnout[mss] <-
    as.numeric(election_stats$valid_ballots[mss]) /
    as.numeric(election_stats$entitled_electors[mss])
  
  # Write only the fields required by the GIS layer.
  #
  # `wd22cd` is the key that allows this data to be joined to the ward
  # geometry, while `adj_turnout` is the value used to colour the map.
  write_csv(
    election_stats |>
      select(wd22cd, adj_turnout),
    file = "data/gis/turnout.csv"
  )
}


# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================


# -----------------------------------------------------------------------------
# Calculate political control for one borough
# -----------------------------------------------------------------------------
#
# This function calculates political control for a SINGLE borough.
#
# It is designed to be called by `purrr::map_df()` from
# `create_borough_political_control()`.
#
# The logic is:
#
#   1. Find the borough's LAD code.
#   2. Keep only elected candidates.
#   3. Count seats won by each party.
#   4. Find the party with the most seats.
#   5. Compare its seats with all other parties combined.
#   6. If it has more than all other parties combined, it has overall control.
#   7. Otherwise classify the borough as No Overall Control (NOC).
#
# `.x` from map_df() is passed in as `borough`.
# -----------------------------------------------------------------------------

calculate_borough_control <- function(
    borough,
    candidates_all
) {
  
  # Find the local authority code associated with this borough.
  #
  # There should normally be one unique LAD22CD per borough. The diagnostic
  # output below is useful for spotting unexpected duplicates or inconsistent
  # geographic data.
  lad_cd <- candidates_all$LAD22CD[
    which(candidates_all$LAD22NM == borough)
  ]
  
  print(
    glue(
      "{borough} found {length(unique(lad_cd))} codes: {unique(lad_cd)[1]}"
    )
  )
  
  # Keep only elected candidates for this borough and count the number of
  # seats won by each party.
  #
  # The resulting data has one row per party, e.g.:
  #
  #   party       party_code    n
  #   Labour      LAB          32
  #   Conservative CON          18
  #   Green       GRN           4
  #
  boro <- candidates_all |>
    filter(
      LAD22NM == borough,
      elected == 1
    ) |>
    summarise(
      .by = c(official_party_name, party_code),
      n = n()
    )
  
  # Calculate the number of seats required for an outright majority.
  #
  # For example:
  #
  #   63 seats -> 32 required
  #   64 seats -> 33 required
  #
  # NOTE: `maj` is currently calculated but isn't used below. The actual
  #       control test compares the largest party's seats with all other
  #       parties combined, which is mathematically equivalent to requiring
  #       more than half the seats.
  maj <- floor((sum(boro$n) + 1) / 2)
  
  # Identify the party with the largest number of elected seats.
  w <- which.max(boro$n)
  
  most_seats <- boro$n[w]
  
  # Calculate all seats held by parties other than the largest party.
  rest_seats <- sum(boro$n) - most_seats
  
  # A party has overall control if it has more seats than every other party
  # combined.
  if (most_seats > rest_seats) {
    
    # Return one row describing the controlling party.
    data.frame(
      LAD22CD = unique(lad_cd)[1],
      LAD22NM = borough,
      party = boro$official_party_name[w],
      party_code = boro$party_code[w]
    )
    
  } else {
    
    # If the largest party does not have more seats than all other parties
    # combined, classify the borough as No Overall Control.
    data.frame(
      LAD22CD = unique(lad_cd)[1],
      LAD22NM = borough,
      party = "No Overall Control",
      party_code = "NOC"
    )
  }
}


# =============================================================================
# ADDITIONAL / TEMPORARY DATA EXPORTS
# =============================================================================


# -----------------------------------------------------------------------------
# Save postal voting data to CSV
# -----------------------------------------------------------------------------
#
# This appears to be a one-off export prepared for Jamie.
#
# It combines election statistics with the geographic lookup so that the
# resulting CSV contains both the ward/borough information and the number of
# postal/polling ballots.
# -----------------------------------------------------------------------------

save_postal_to_csv <- function() {
  
  # Connect to the database.
  #
  # NOTE: `database_path` is not defined inside this function. It therefore
  #       needs to exist in the global environment for this function to work.
  con <- dbConnect(SQLite(), database_path)
  
  # Join election statistics to the geographic lookup using the ward code.
  #
  # The LEFT JOIN means that every row in `election_stats` is retained, even
  # if there isn't a matching row in `geo_lkp`.
  election_stats <- dbGetQuery(
    con,
    paste0(
      "SELECT e.wd22cd, entitled_electors, ballots_polling, ",
      "ballots_postal, ward, LAD22NM ",
      "FROM election_stats e ",
      "LEFT JOIN geo_lkp g ON e.wd22cd = g.wd22cd;"
    )
  )
  
  dbDisconnect(con)
  
  # Convert the relevant fields explicitly to integers before exporting.
  election_stats$entitled_electors <-
    as.integer(election_stats$entitled_electors)
  
  election_stats$ballots_postal <-
    as.integer(election_stats$ballots_postal)
  
  election_stats$ballots_polling <-
    as.integer(election_stats$ballots_polling)
  
  # Write the resulting dataset to CSV.
  write_csv(
    election_stats,
    "data/postal_other.csv"
  )
}


# -----------------------------------------------------------------------------
# Calculate vote share at ward level
# -----------------------------------------------------------------------------
#
# This appears to be an exploratory calculation for producing party vote-share
# information at ward level.
#
# The calculation happens in two stages:
#
#   1. Calculate the mean votes for each party within each ward.
#   2. Calculate the total of those party means for each ward.
#   3. Divide each party's mean by the ward total to get `perc`.
#
# `perc` can then be used as a map fill value.
# -----------------------------------------------------------------------------

calculate_to_ward <- function() {
  
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
    ungroup()
  
  # Calculate the total of the party-level values for each ward.
  #
  # This gives us the denominator used to calculate each party's share.
  xdat <- dat |>
    summarise(
      .by = c(wd22cd),
      tots = sum(val)
    )
  
  # Join the ward totals back onto the party-level data and calculate the
  # party's proportion of the ward total.
  ydat <- left_join(dat, xdat) |>
    mutate(
      perc = val / tots
    )
  
  # ydat is returned implicitly.
  ydat
}


# =============================================================================
# QUICK MAP TESTING
# =============================================================================


# -----------------------------------------------------------------------------
# Test the GIS data
# -----------------------------------------------------------------------------
#
# This is an exploratory function for checking that the prepared data can be
# successfully joined to the ward/borough geometries and plotted with ggplot2.
#
# It currently contains examples for:
#
#   - turnout
#   - party vote totals
#   - Reform UK vote share
#
# This is not part of the production data-preparation pipeline; it is mainly
# useful for quickly checking the output visually.
# -----------------------------------------------------------------------------

test_map <- function() {
  
  # Load the borough geometry.
  x <- readRDS("data/gis/boroughs.rds")
  
  # Load the ward geometry.
  wards <- readRDS("data/gis/wards.rds")
  
  # ---------------------------------------------------------------------------
  # Turnout map
  # ---------------------------------------------------------------------------
  #
  # Assumes `wards` has already been joined to the turnout data and therefore
  # contains `adj_turnout`.
  #
  # NOTE: As currently written, the turnout data is not actually joined here,
  #       so this will only work if `adj_turnout` is already present in
  #       `wards`.
  # ---------------------------------------------------------------------------
  
  ggplot(wards) +
    geom_sf(aes(fill = adj_turnout))
  
  
  # ---------------------------------------------------------------------------
  # Party vote/value map
  # ---------------------------------------------------------------------------
  #
  # Assumes `wards` contains a variable called `val`.
  ggplot(wards) +
    geom_sf(aes(fill = val))
  
  
  # ---------------------------------------------------------------------------
  # Reform UK map
  # ---------------------------------------------------------------------------
  #
  # Join the ward-level data for RUK onto the ward geometry using the ward
  # identifier.
  #
  # `perc` is then used to colour the wards according to Reform UK's share.
  #
  # White is used for wards where there is no matching RUK value.
  # ---------------------------------------------------------------------------
  
  ggplot(
    wards |>
      left_join(
        ydat |> filter(party_code == "RUK")
      )
  ) +
    geom_sf(aes(fill = perc)) +
    scale_fill_gradientn(
      colours = c("#FFFFFF", "blue"),
      na.value = "#FFF"
    )
}