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
  filename <- create_ranking_file_name(
    folder = folder,
    ranking_date = ranking_date,
    top_games = no_of_games,
    min_no_of_ratings = min_no_of_ratings,
    add_csv_ending = FALSE
  )
  save_csv(ranking, filename)
}

# Create a ranking file name with path
# contains top games, a minimum number of ratings or both
create_ranking_file_name <- function(folder = CSV_RANKING_FOLDER,
                                     ranking_date = RANKING_DATE,
                                     min_no_of_ratings = 0,
                                     top_games = 0,
                                     add_csv_ending = FALSE) {
  # Name which should be added besides the filepath and date
  naming <- create_name_no_ratings_top_games(
    top_games = top_games,
    min_no_of_ratings = min_no_of_ratings)
  
  filename <- paste(
    folder,
    paste(ranking_date, naming, sep = "_"),
    sep = ""
  )
  cat("created filename for ranking: ", filename)
  if (add_csv_ending) {
    filename <- add_csv_to_path(filename)
  }
  return(filename)
}

# Creates name dependent if number of ratings and top games
create_name_no_ratings_top_games <- function(top_games = 0, 
                                             min_no_of_ratings = 0) {
  # Name which should be added besides the filepath and date
  naming <- ""
  if (min_no_of_ratings == 0 && top_games == 0) {
    print("with no rating or top games the ranking will not be smaller than the original")
    return
  } else if (min_no_of_ratings == 0) {
    naming <- paste("top_", top_games, sep = "_")
  } else if (top_games == 0) {
    naming <- paste("min_ratings", min_no_of_ratings, sep = "_")
  } else {
    naming <- paste("top_", top_games, "min_ratings", min_no_of_ratings, sep = "_")
  }
  return (naming)
}
