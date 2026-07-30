library(readr)

x <- read_csv("data/Wards_December_2022_Boundaries_UK_BSC_680501922530048981.csv")

unique(latest_wards$borough) %in% x$LAD22NM

bb <- unique(latest_wards$borough)


b1 <- bb[2]

wrds <- unique(latest_wards$ward[which(latest_wards$borough == b1)])

xwrds <- x$WD22NM[which(x$LAD22NM == b1)]

all(wrds %in% xwrds)


