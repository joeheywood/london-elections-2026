for(br in unique(dat$borough)) {
  print(glue("Doing {br}"))
  wb <- loadWorkbook("borough_template.xlsx")
  bdat <- dat %>% filter(borough == br)
  sheets <- c()
  
  for(i in 1:nrow(bdat)) {
    sheetnm <- str_trunc(bdat$ward[i], 14, side = "right")
    
    if(sheetnm %in% sheets) {
      sheetnm <- str_trunc(bdat$ward[i], 19, side = "right")
    } 
    sheets <- c(sheets, sheetnm)
    
    cloneWorksheet(wb, sheetnm, "1")
    writeData(wb, sheetnm, br, startCol = 1, startRow = 3)
    writeData(wb, sheetnm, c(bdat$ward[i], bdat$num[i]), 
              startCol = 8, startRow = 5)
    showGridLines(wb, sheetnm, FALSE)
  }
  
  removeWorksheet(wb, 1)
  activeSheet(wb) <- 1
  saveWorkbook(wb, glue("Ward Forms/{br} local results form.xlsx"), overwrite = TRUE)
}
