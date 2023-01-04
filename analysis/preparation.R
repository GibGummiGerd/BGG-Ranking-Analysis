## This file is for the preparation of the ranking file and subsequent the file which is used to download/update the csv files

# PACKAGES
source("setup.R")

### Start of preparation before downloading ###

# Load new rating history and clean it up, needs to be only done once for every rating file
input_csv <- load_csv(RANKING_DATE, CSV_RANKING_FOLDER, add_csv_ending = TRUE)
all_top <- clean_and_save_ranking(input_csv, min_no_of_ratings = 2000)

# order list by game id
r <- r[order(r$ID),]

# get total number of ratings
total_sum <- 0
for (i in 1:nrow(r)) {
  total_sum <- total_sum + r[i, "Users.rated"]
}

#create vars
row1 <- 0
row2 <- 0
row3 <- 0
ids1 <- c()
ids2 <- c()
ids3 <- c()
ids4 <- c()

# get rows which split the list into fourths
current_sum <- 0  # create a variable to store the current sum
for (i in 1:nrow(r)) {
  current_sum <- current_sum + r[i, "Users.rated"]
  if ((current_sum >= total_sum / 4) & (row1 == 0)) {
    row1 <- i
    next
  }
  if ((current_sum >= total_sum / 2) & (row2 == 0)) {
    row2 <- i
    next
  }
  if ((current_sum >= (total_sum / 4) * 3) & (row3 == 0)) {
    row3 <- i
    next
  }
}

# get ids of the rows in the quarters
for (i in 1:row1) {
  ids1 <- c(ids1, r[i, "ID"])
}
for (i in (row1+1):row2) {
  ids2 <- c(ids2, r[i, "ID"])
}
for (i in (row2+1):row3) {
  ids3 <- c(ids3, r[i, "ID"])
}
for (i in (row3+1):nrow(r)) {
  ids4 <- c(ids4, r[i, "ID"])
}

# save in txt file
writeLines(as.character(ids1), "ids1.txt")
writeLines(as.character(ids2), "ids2.txt")
writeLines(as.character(ids3), "ids3.txt")
writeLines(as.character(ids4), "ids4.txt")

### End of preparation before downloading ###