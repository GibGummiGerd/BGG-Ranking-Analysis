library(tidyverse)
source("const.R")
source("files.R")

calc_cum_avg_rating <- function(
    avg_rating_column,
    no_ratings_column,
    cum_no_ratings_column
) {
  
  # set NA to 0 for later formulas
  # avg_rating_column[is.na(avg_rating_column)] <- 0
  # no_ratings_column[is.na(no_ratings_column)] <- 0
  # cum_no_ratings_column[is.na(cum_no_ratings_column)] <- 0
    
    
  # Create data frame which will be returned
  cum_avg_rating_data_frame <- setNames(data.frame(matrix(ncol = 1, nrow = 0)), c("cum_avg_rating"))
  
  last_avg_rating = 0
  last_cum_avg_rating = 0
  last_cum_no_ratings = 0
  found = FALSE
  
  for (key in 1:nrow(avg_rating_column)) {
    if (found == FALSE & is.na(avg_rating_column[key,])) {
      cum_avg_rating = NA
    } else if (is.na(avg_rating_column[key,])){
      cum_avg_rating = last_cum_avg_rating
    } else {
      found <- TRUE

      cum_avg_rating = (last_cum_avg_rating * last_cum_no_ratings + avg_rating_column[key,] * no_ratings_column[key,]) / cum_no_ratings_column[key,]
      last_cum_avg_rating = cum_avg_rating
      last_avg_rating = avg_rating_column[key,]
      last_cum_no_ratings = cum_no_ratings_column[key,]
    }
    cum_avg_rating_data_frame[key,] = c(cum_avg_rating)
  }
  
  return (cum_avg_rating_data_frame)
}

# durchschnittliches Rating pro Monat
calc_ratings_per_month <- function(
    data,
    year_official
) {
  ratings_per_month <- data %>%
    group_by(year, month) %>%
    summarise(
      avg_rating = mean(rating),
      no_ratings = n(),
      avg_rating_owned = mean(rating[own == "True"]),
      avg_rating_not_owned = mean(rating[own == "False"]),
    )
  
  # owned grouping
  temp <- data %>%
    filter(own == "True") %>%
    group_by(year, month) %>%
    summarise(
      no_ratings_owned = n(),
    )
  ratings_per_month <- merge(ratings_per_month, temp, all = TRUE)
  
  # not owned grouping
  temp <- data %>%
    filter(own == "False") %>%
    group_by(year, month) %>%
    summarise(
      no_ratings_not_owned = n()
    )
  ratings_per_month <- merge(ratings_per_month, temp, all = TRUE)
  
  ratings_per_month <- ratings_per_month %>%
    replace_na(list(no_ratings_owned = 0,
                    no_ratings_not_owned = 0)) %>%
    mutate(
      cum_no_ratings = cumsum(no_ratings),
      cum_no_ratings_owned = cumsum(no_ratings_owned),
      cum_no_ratings_not_owned = cumsum(no_ratings_not_owned)
    )
  
  ratings_per_month <- ratings_per_month %>%
    mutate(
      cum_avg_rating = calc_cum_avg_rating(
        ratings_per_month["avg_rating"],
        ratings_per_month["no_ratings"],
        ratings_per_month["cum_no_ratings"]
      )[,1],
      cum_avg_rating_owned = calc_cum_avg_rating(
        ratings_per_month["avg_rating_owned"],
        ratings_per_month["no_ratings_owned"],
        ratings_per_month["cum_no_ratings_owned"]
      )[,1],
      cum_avg_rating_not_owned = calc_cum_avg_rating(
        ratings_per_month["avg_rating_not_owned"],
        ratings_per_month["no_ratings_not_owned"],
        ratings_per_month["cum_no_ratings_not_owned"]
      )[,1]
    )
  
  release <- get_release_year_and_month(ratings_per_month, year_official)
  print(release)

  ratings_per_month <- ratings_per_month %>%
    filter(
      ratings_per_month$year > release$year | (ratings_per_month$year == release$year & ratings_per_month$month >= release$month)
      ) %>%
    mutate(
      month_after_release = 12*(year - release$year) + (month - release$month)
      )

  return(ratings_per_month)
}


#
get_release_year_and_month <- function(ratings_per_month, year_official) {
  
  start_month <- 5
  start_year <- 2001
  valid_months <- 0
  year_reached <- FALSE
  current_months_in_a_row <- 0
  
  vector_no_ratings <- ratings_per_month$no_ratings
  vector_years <- ratings_per_month$year
  vector_months <- ratings_per_month$month
  
  if (year_official <= 2000) {
    months_in_a_row <- 1
    minimum_ratings_per_month <- 1
  } else if (year_official <= 2004) {
    months_in_a_row <- 3
    minimum_ratings_per_month <- 3
  } else if (year_official <= 2009) {
    months_in_a_row <- 3
    minimum_ratings_per_month <- 3
  } else if (year_official <= 2016) {
    months_in_a_row <- 6
    minimum_ratings_per_month <- 10
  } else {
    months_in_a_row <- 6
    minimum_ratings_per_month <- 10
  }
  
  for (key in 1:length(vector_years)) {
    
    if (year_reached == FALSE) {
      if (vector_years[key] >= year_official) {
        year_reached <- TRUE
      } 
    } else {
      next
    }
    
    
    if (vector_no_ratings[key] >= minimum_ratings_per_month) {
      current_months_in_a_row <- current_months_in_a_row + 1
    } else {
      current_months_in_a_row <- 0
      next
    }
    
    if (current_months_in_a_row == 1) {
      start_month = vector_months[key]
      start_year = vector_years[key]
    } else if ((start_month + current_months_in_a_row - 1)%% 12 != vector_months[key]) {
      current_months_in_a_row <- 0
      next
    }
    
    if (current_months_in_a_row == months_in_a_row) {
      return (list("year" = start_year, "month" = start_month))
    }
    
  }
  
  return (list("year" = start_year, "month" = start_month))
  
}


#
prepare_data <- function(data) {
  
  # Add month and year after release
  data$month <- as.numeric(format(as.Date(data$rating_tstamp), format="%m"))
  data$year <- as.numeric(format(as.Date(data$rating_tstamp), format="%Y"))
  
  # Remove entries without rating
  data <- subset(data, data$rating>0)
  
  return(data)
}

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
  filename = paste("ranking", paste("top", no_of_games, sep = "-"), ranking_date, sep = "_")
  save_csv(ranking, filename)
}

clean_and_save_ranking <- function(ranking, no_of_games, ranking_date) {
  ranking <- clean_ranking(ranking, no_of_games)
  save_ranking(ranking, no_of_games, ranking_date)
  return(ranking)
}
