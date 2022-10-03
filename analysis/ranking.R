library(tidyverse)
source("const.R")
source("files.R")

clean_and_save_ranking <- function(ranking, no_of_games = 0, min_no_of_ratings = 0, ranking_date = RANKING_DATE, folder = CSV_RANKING_FOLDER) {
  
  ranking <- clean_ranking(ranking, no_of_games, min_no_of_ratings)
  save_ranking(ranking, no_of_games, min_no_of_ratings, ranking_date, folder)
  return(ranking)
}


clean_ranking <- function(ranking, no_of_games = 0, min_no_of_ratings = 0) {
  
  ranking <- ranking %>%
    select(!(URL | Thumbnail))
  
  if (no_of_games > 0) {
    ranking <- ranking %>%
      filter(Rank <= no_of_games)
  }
  
  if (min_no_of_ratings > 0) {
    ranking <- ranking %>%
      filter(Users.rated >= min_no_of_ratings)
  }
  
  for (key in 1:nrow(ranking)) {
    if (ranking$Year[key] > CURRENT_YEAR) {
      ranking$Year[key] <- ranking$Year[key] * (-1)
    }
  }

  return(ranking)
}

# 
save_ranking <- function(ranking, no_of_games, min_no_of_ratings, ranking_date, folder) {
  file_name <- create_ranking_file_name(no_of_games, min_no_of_ratings, ranking_date, folder)
  save_csv(ranking, file_name)
}


create_ranking_file_name <- function(no_of_games = 0, min_no_of_ratings = 0, ranking_date, folder, add_csv_ending = TRUE) {
  print(ranking_date)
  print(min_no_of_ratings)
  filename <- paste(
    folder,
    paste(
      "ranking",
      paste(
        "top",
        no_of_games,
        "min_ratings",
        min_no_of_ratings,
        sep = "_"
      ),
      ranking_date,
      sep = "_"
    ),
    sep = ""
  )
  print("HIIIIIIIER")
  print(filename)
  if (add_csv_ending) {
    filename <- add_csv_to_path(filename)
  }
  return(filename)
}
