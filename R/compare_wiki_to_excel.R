library(readr)
library(readxl)
library(purrr)
library(glue)
library(stringr)

# w <- "~/Projects/GLA/Local elections/data/Completed Forms/Hackney local results form (1).xlsx"
# w <- "~/Projects/GLA/Local elections/data/Completed Forms/Ealing local results form (1).xlsx"
# w <- "~/Projects/GLA/Local elections/data/Completed Forms/Harrow local results form (1).xlsx"
# w <- "~/Projects/GLA/Local elections/data/Completed Forms/Havering local results form (1).xlsx"
# w <- "~/Projects/GLA/Local elections/data/Completed Forms/Islington local results form (1).xlsx"
# w <- "~/Projects/GLA/Local elections/data/Completed Forms/LBBD Results Form (for GLA).xlsx"
# w <- "~/Projects/GLA/Local elections/data/Completed Forms/Brent local results form GLA.xlsx"
# w <- "~/Projects/GLA/Local elections/data/Completed Forms/Bromley local results form - LOCALS 2026.xlsx"
w <- "~/Projects/GLA/Local elections/data/Completed Forms/Croydon local results for GLA.xlsx"

wrds <- excel_sheets(w)  

# ward <- "Brownswood"
# boro <- "Ealing_"

compare_wards_wikipedia <- function(ward, boro) {
  tryCatch({
    suppressMessages(
      b2 <- read_excel(w, ward, skip = 18) %>% 
        filter(!is.na(`Candidate surname`)) %>% 
        mutate(`Votes recorded` = str_replace_all(as.character(`Votes recorded`), "[,]", "")) %>% 
        mutate(`Votes recorded` = as.numeric(str_trim(`Votes recorded`))) %>% 
        arrange(desc(`Votes recorded`))
    )
    
    suppressMessages( mt <- read_excel(w, ward, skip = 1) )
    
  }, error = function(e) {
    print(glue("UNABLE TO GET EXCEL DATA FOR {ward}"))
    return(FALSE)
  })
  
  ward <- str_replace(mt[[7]][3], " & ", " and ")
  
  b <- all_results[which(all_results$ward == ward & all_results$borough == boro),] 
  
  bx <- b %>% 
    filter(Votes != "Swing", !Party %in% c("Turnout", "Majority", "Registered electors") ) %>% 
    mutate(Votes = as.numeric(str_replace_all(Votes, "[,]", ""))) %>% 
    filter(!is.na(Votes)) %>% 
    arrange(desc(Votes))
  
  t <- b %>% 
    filter(Party %in% c("Turnout")) %>% 
    mutate(Votes = str_replace_all(Votes, "[,]", ""))
  
  if(nrow(bx) != nrow(b2)) {
    print(glue("Different # of candidates in {ward} b2: {nrow(b2)} bz: {nrow(bx)}"))
    return(FALSE)
  }
  
  
  correct <- all(as.numeric(bx$Votes) == b2$`Votes recorded`)
  
  if(correct == FALSE) {
    wx <- which(as.numeric(bx$Votes) != b2$`Votes recorded`)
    print(glue("Difference {ward} {paste(wx, collapse = ' ')}"))
    print(glue("Sum from boro: {sum(b2$`Votes recorded`)} Sum from wiki: {sum(as.numeric(bx$Votes))} T: {t$Votes} "))
  } else {
    print(glue("All good for {ward}"))
  }
  
  
  # bmain <- read_excel(w, "Balham", range = "A2:G16") %>%
  #   select(info = 1, info2 = 2, x = 7)
  TRUE
}


m <- map(wrds, compare_wards_wikipedia, boro = "Croydon_")

