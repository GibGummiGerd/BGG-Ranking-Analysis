library("tidyverse")
source("const.R")
source("files.R")

clean_ranking <- function(ranking, no_of_games) {
  ranking <- ranking %>%
    select(!(URL | Thumbnail)) %>%
    filter(
      Rank <= no_of_games
    )
  for (key in 1:nrow(ranking)) {
    if (ranking$Year[key] > CURRENT_YEAR) {
      ranking$Year[key] = ranking$Year[key] * (-1)
    }
  }
  return (ranking)
}

save_ranking <- function(ranking, no_of_games, ranking_date) {
  filename <- paste(
    CSV_RANKING_FOLDER,
    paste(
      "ranking", 
      paste(
        "top", 
        no_of_games, 
        sep = "-"
      ), 
      ranking_date, 
      sep = "_"
    ),
    sep = ""
  )
  save_csv(ranking, filename)
}

clean_and_save_ranking <- function(ranking, no_of_games, ranking_date) {
  ranking <- clean_ranking(ranking, no_of_games)
  save_ranking(ranking, no_of_games, ranking_date)
  return(ranking)
}