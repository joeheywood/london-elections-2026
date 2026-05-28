# London Electoral Wards

Scrapes the latest electoral wards for each London borough from Wikipedia.

Source:
https://en.wikipedia.org/wiki/List_of_electoral_wards_in_Greater_London

## Output

The scraper produces:

- `data/latest_wards.csv`

Columns:

- `borough`
- `ward`
- `councillors`

## Requirements

R packages:

- rvest
- xml2
- dplyr
- stringr
- tibble

## Run

```r
source("R/scrape_wards.R")
```

## Notes

The number in parentheses after each ward name represents the number of councillors elected for that ward.
