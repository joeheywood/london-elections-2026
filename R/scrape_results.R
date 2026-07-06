library(rvest)
library(xml2)
library(dplyr)
library(purrr)
library(stringr)
library(glue)

url <- "https://en.wikipedia.org/wiki/2026_Barking_and_Dagenham_London_Borough_Council_election"
url <- "https://en.wikipedia.org/wiki/2026_Harrow_London_Borough_Council_election"

scrape_borough_page <- function(url) {
  page <- read_html(url)
  
  ward_headings <- page %>%
    html_elements("h3")
  
  
  
  ward_results <- map_df(ward_headings, function(h) {
    # print(html_text2(h) )
    
    table <- xml_find_first(
      xml_parent(h),
      "following-sibling::table[1]"
    )
    
    # table <- xml_find_first(
    #   h,
    #   "following-sibling::table[1]"
    # )
    
    
    if(inherits(table, "xml_missing"))
      return(NULL)
    
    df <- html_table(table) 
    if(!is.data.frame(df) ) {
      if(is.data.frame(df[[1]])) {
        df <- df[[1]]
        
      }
    }
    
    if(!"Candidate" %in% names(df)) {
      return(NULL) 
    } else {
      print(ncol(df))
      if(ncol(df) == 5) df$chng <- ""
      df$ward <- html_text2(h) 
      
      df <- df %>% select(Party = 2, Candidate = 3, Votes = 4, prc = 5, chng = 6, ward = 7)
      df$Votes <- as.character(df$Votes)
      df$prc <- as.character(df$prc)
      df$chng <- as.character(df$chng)
      return(df)
    }
    
  })
  ward_results
}


index_page <- read_html(
  "https://en.wikipedia.org/wiki/2026_London_local_elections"
)

links <- index_page %>%
  html_elements("a") %>%
  html_attr("href")

borough_urls <- links %>%
  .[str_detect(., "2026_\\w*(London_Borough|Westminster_City)_Council_election$")] %>%
  unique() %>%
  paste0("https://en.wikipedia.org", .)

length(borough_urls)

all_results <- map_df(borough_urls, function(url) {
  tryCatch({
    message(url)
    boro <- str_replace_all(url, "https://en.wikipedia.org/wiki/2026_(.*)(London_Borough|City)_Council_election", "\\1")
    scrape_borough_page(url) %>% mutate(borough = boro)
  }, error = function(e) {
    print(glue("ERROR FOR {url}"))
    data.frame()
  })
})

