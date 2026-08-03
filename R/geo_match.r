library(readr)
library(readxl)
library(dplyr)
library(purrr)
library(glue)
library(stringr)

x <- read_csv("data/Wards_December_2022_Boundaries_UK_BSC_680501922530048981.csv") |> 
  select(WD22CD, WD22NM, LAD22CD, LAD22NM) |> 
  mutate(WD22NM = str_replace_all(WD22NM, " & ", " and ")) |> 
  mutate(WD22NM = str_replace_all(WD22NM, "^St. ", "St ")) 

x$WD22NM[which(x$WD22CD == "E05009331")] <- "Bethnal Green West"
x$WD22NM[which(x$WD22CD == "E05009317")] <- "Bethnal Green East"

fls <- dir("data/Completed Forms/", full.names = TRUE)
fl <- fls[1]


extract_wards_from_form <- function(fl) {
  map_df(excel_sheets(fl), function(s) {
    
    suppressMessages({
      df <- read_excel(fl, s)
    })
    wrd <- as.character(df[4,7])
    data.frame(f = fl, ward = wrd, tabname = s, row.names = FALSE) |>  
      mutate(ward = str_replace_all(ward, " & ", " and ")) |> 
      mutate(ward = str_replace_all(ward, "`", "'")) 
  })
  
}


match_geo <- function(fl, boro) {
  print(glue("MATCHING {boro} for {basename(fl)}"))
  coll_df <- extract_wards_from_form(fl) 
  filt_df <- x |> filter(LAD22NM == boro) 
  
  if(all(coll_df$ward %in% filt_df$WD22NM) == TRUE) {
    dfx <- left_join(coll_df, filt_df, by = c(ward = "WD22NM"))
  } else {
    print(glue("Not working for {fl}"))
    save(fl, boro, coll_df ,filt_df, file = glue("debug_{boro}.RDA"))
    data.frame()
  }
  
}

a <- list(
  match_geo("data/Completed Forms/LBBD Results Form (for GLA).xlsx", "Barking and Dagenham"),
  match_geo("data/Completed Forms/BarnetLocalStats26_forGLA.xlsx", "Barnet"),
  match_geo("data/Completed Forms/Bexley local results form.xlsx", "Bexley"),
  match_geo("data/Completed Forms/Brent local results form GLA.xlsx", "Brent"),
  match_geo("data/Completed Forms/Bromley local results form - LOCALS 2026.xlsx", "Bromley"),
  match_geo("data/Completed Forms/GLA Camden local results form.xlsx", "Camden"),
  match_geo("data/Completed Forms/Croydon local results for GLA.xlsx", "Croydon"),
  match_geo("data/Completed Forms/Ealing local results form (1).xlsx", "Ealing"),
  match_geo("data/Completed Forms/Enfield local results form.xlsx", "Enfield"),
  match_geo("data/Completed Forms/Greenwich local results form.xlsx", "Greenwich"),
  match_geo("data/Completed Forms/GLA - Hammersmith and Fulham local results form.xlsx", "Hammersmith and Fulham"),
  match_geo("data/Completed Forms/Hackney local results form (1).xlsx", "Hackney"),
  match_geo("data/Completed Forms/Haringey Locals 2026 - Report of election for GLA.xlsx", "Haringey"),
  match_geo("data/Completed Forms/Harrow local results form (1).xlsx", "Harrow"),
  match_geo("data/Completed Forms/Havering local results form (1).xlsx", "Havering"),
  match_geo("data/Completed Forms/Hillingdon local results form.xlsx", "Hillingdon"),
  match_geo("data/Completed Forms/Hounslow Local Stats GLA.xlsx", "Hounslow"),
  match_geo("data/Completed Forms/Islington local results form (1).xlsx", "Islington"),
  match_geo("data/Completed Forms/Kensington and Chelsea local results form.xlsx", "Kensington and Chelsea"),
  match_geo("data/Completed Forms/Kingston upon Thames local results form.xlsx", "Kingston upon Thames"),
  match_geo("data/Completed Forms/Lambeth local results form for GLA.xlsx", "Lambeth"),
  match_geo("data/Completed Forms/Lewisham local results form - GLA.xlsx", "Lewisham"),
  match_geo("data/Completed Forms/Merton local results form.xlsx", "Merton"),
  match_geo("data/Completed Forms/Newham local results form.xlsx", "Newham"),
  match_geo("data/Completed Forms/Redbridge local results form.xlsx", "Redbridge"),
  match_geo("data/Completed Forms/Richmond upon Thames local results form for GLA.xlsx", "Richmond upon Thames"),
  match_geo("data/Completed Forms/Southwark local results form.xlsx", "Southwark"),
  match_geo("data/Completed Forms/Sutton local results form - GLA.xlsx", "Sutton"),
  match_geo("data/Completed Forms/Tower Hamlets local results form - for the GLA.xlsx", "Tower Hamlets"),
  match_geo("data/Completed Forms/Wandsworth local results form (1).xlsx", "Wandsworth"),
  match_geo("data/Completed Forms/Waltham Forest local results form (1).xlsx", "Waltham Forest"),
  match_geo("data/Completed Forms/Westminster local results form (1).xlsx", "Westminster")
)


df <- bind_rows(a)
unique(df$LAD22NM)

saveRDS(df, file = "data/geo_lkp.RDS")
