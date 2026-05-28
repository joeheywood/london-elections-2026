library(rvest)
library(xml2)
library(dplyr)
library(stringr)
library(purrr)
library(tibble)

url <- "https://en.wikipedia.org/wiki/List_of_electoral_wards_in_Greater_London"

page <- read_html(url)

# Get all relevant nodes in document order
nodes <- page %>%
  html_elements(xpath = "//h3 | //p | //ol")

current_borough <- NA_character_
current_period <- NA_character_

results <- list()

for(i in seq_along(nodes)) {
  
  node <- nodes[[i]]
  node_type <- xml_name(node)
  txt <- html_text2(node)
  
  # ---------------------------
  # Borough headings (h3)
  # ---------------------------
  if(node_type == "h3") {
    
    # borough <- node %>%
    #   html_element(".mw-headline") %>%
    #   html_text2()
    borough <- txt
    
    current_borough <- borough
  }
  
  # ---------------------------
  # Ward period paragraphs
  # ---------------------------
  if(node_type == "p") {
    
    if(str_detect(txt, "^Wards from")) {
      current_period <- txt
    }
  }
  
  # ---------------------------
  # Ordered lists of wards
  # ---------------------------
  if(node_type == "ol" &&
     !is.na(current_borough) &&
     !is.na(current_period)) {
    
    ward_text <- node %>%
      html_elements("li") %>%
      html_text2()
    
    if(length(ward_text) > 0) {
      
      ward_df <- tibble(
        borough = current_borough,
        period = current_period,
        ward_raw = ward_text
      ) %>%
        mutate(
          ward = str_remove(ward_raw, "\\s*\\(\\d+\\)$"),
          councillors = str_extract(ward_raw, "(?<=\\()\\d+(?=\\)$)") %>%
            as.integer()
        )
      
      results[[length(results) + 1]] <- ward_df
    }
  }
}

wards_df <- bind_rows(results)

# Extract year from "Wards from ..."
wards_df <- wards_df %>%
  mutate(
    year = str_extract(period, "\\d{4}") %>% as.integer()
  )

# Keep only latest ward set per borough
latest_wards <- wards_df %>%
  group_by(borough) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  ungroup()

latest_wards %>% 
  filter(!borough %in% c("City of London", "Greater London Council", "London Assembly"))
         
         
library(glue)
library(openxlsx)

for(br in unique(latest_wards$borough)) {
  print(glue("Doing {br}"))
  wb <- loadWorkbook("borough_template.xlsx")
  bdat <- latest_wards %>% filter(borough == br)
  sheets <- c()
  
  for(i in 1:nrow(bdat)) {
    sheetnm <- str_trunc(bdat$ward[i], 14, side = "right")
    
    if(sheetnm %in% sheets) {
      sheetnm <- str_trunc(bdat$ward[i], 19, side = "right")
    } 
    sheets <- c(sheets, sheetnm)
    
    cloneWorksheet(wb, sheetnm, "1")
    writeData(wb, sheetnm, br, startCol = 1, startRow = 3)
    writeData(wb, sheetnm, br, startCol = 1, startRow = 3)
    writeData(wb, sheetnm, c("7 May 2026", bdat$ward[i], bdat$councillors[i]), 
              startCol = 8, startRow = 4)
    setColWidths(wb, sheetnm, 8, "auto")
    showGridLines(wb, sheetnm, FALSE)
  }
  
  removeWorksheet(wb, 1)
  activeSheet(wb) <- 1
  saveWorkbook(wb, glue("Ward Forms/{br} local results form.xlsx"), overwrite = TRUE)
}
