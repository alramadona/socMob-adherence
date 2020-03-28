library(googledrive)

## store token in an object and then to file
# ttt <- drive_auth()
# saveRDS(ttt, "socMed.rds")

## load a pre-existing token
drive_auth("socMed.rds",
           use_oob = TRUE)

drive_find(n_max = 30)