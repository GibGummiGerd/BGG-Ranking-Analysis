source("const.R")
library(tidyverse)

# Loads CSV with given BGG ID
load_csv_with_id <- function(id) {
  filename <- list.files(path = CSV_FOLDER, pattern = id_regex(id))[1]
  if (is.na(filename)) {
    print(paste("Did not find csv for id", toString(id), sep = " "))
    return (NA)
  }
  filepath <- paste(CSV_FOLDER, filename, sep = "/")
  return(load_csv(filepath))
}

# Loads the csv with given filepath
load_csv <- function(filepath) {
  tryCatch(
    expr = {
      df <- read.csv(filepath)
      return(df)
    },
    error = function(err) {
      print(paste("Did not find csv at path", filepath, sep = " "))
      return (NA)
    }
  )
}

# Creates regex to find csv file
id_regex <- function(id) {
  return (paste("^", id, "_",sep = ""))
}


save_csv <- function(df, filename, filepath = "") {
  write.csv(df, file = paste(filepath, filename, ".csv", sep = ""))
}