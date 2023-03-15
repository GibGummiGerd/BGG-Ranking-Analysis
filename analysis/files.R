source("const.R")
library(tidyverse)

# Loads CSV with given BGG ID
load_csv_with_id <- function(id, csv_path = CSV_GAMES_MONTHLY_FOLDER) {
  filename <- list.files(path = csv_path, pattern = id_regex(id))[1]
  if (is.na(filename)) {
    print(paste("Did not find csv for id", toString(id), "in folder ", csv_path, sep = " "))
    return(NA)
  }
  return(load_csv(filename, csv_path))
}

# Loads the csv with given filepath
load_csv <- function(filename, filepath = "", add_csv_ending = FALSE) {
  if (add_csv_ending) {
    filename <- add_csv_to_path(filename)
  }
  tryCatch(
    expr = {
      if (filepath == "") {
        df <- read.csv(filename)
        return(df)
      }

      df <- read.csv(paste(filepath, filename, sep = ""))
      return(df)
    },
    error = function(err) {
      print(paste("Did not find csv at path", filepath, sep = " "))
      return(NA)
    }
  )
}

# Creates regex to find csv file
id_regex <- function(id) {
  return(paste("^", id, "_", sep = ""))
}

add_csv_to_path <- function(file_name) {
  file_name <- paste(file_name, "csv", sep = ".")
}


save_csv <- function(df, filename, filepath = "", add_csv_ending = TRUE) {
  if (!dir.exists(filepath) & filepath != "") {
    dir.create(filepath)
  }
  file <- paste(filepath, filename, sep = "")
  if (add_csv_ending) {
    file <- add_csv_to_path(file)
    write.csv(df, file)
  } else {
    write.csv(df, file)
  }
}

does_file_with_regex_exist <- function(file_path, regex) {
  matching_files <- list.files(path = file_path, pattern = regex)
  if (length(matching_files) > 1) {
    print(paste("More than one file found for regex: ", regex, sep = ""))
    quit()
  }
  if (is.na(matching_files)) {
    return(FALSE)
  }
  return(TRUE)
}
