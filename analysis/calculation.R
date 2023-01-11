library(tidyverse)
source("const.R")
source("ranking.R")
source("files.R")


#
add_date <- function(data, ranking_year = RANKING_YEAR, ranking_month = RANKING_MONTH) {
  # Add month and year after release
  data$month <- as.numeric(format(as.Date(data$rating_tstamp), format = "%m"))
  data$year <- as.numeric(format(as.Date(data$rating_tstamp), format = "%Y"))

  # Remove entries without rating
  data <- subset(data, data$rating > 0)
  data <- subset(data, data$year < ranking_year | (data$month < ranking_month & data$year == ranking_year))

  return(data)
}


# durchschnittliches Rating pro Monat
calc_ratings_per_month <- function(data,
                                   year_official) {
  ratings_per_month <- data %>%
    group_by(year, month) %>%
    summarise(
      avg_rating = mean(rating),
      no_ratings = n(),
      avg_rating_owned = mean(rating[own == "True"]),
      avg_rating_not_owned = mean(rating[own == "False"]),
      avg_rating_prev_owned = mean(rating[prevowned == "True"]),
      avg_rating_comment = mean(rating[comment != ""]),
      avg_rating_no_comment = mean(rating[comment == ""]),
      .groups = "drop",
    ) # %>%
  # ungroup replaced by .groups
  # ungroup() # Important to ungroup, otherwise it will be later applied

  # owned grouping
  temp <- data %>%
    filter(own == "True") %>%
    group_by(year, month) %>%
    summarise(
      no_ratings_owned = n(),
      .groups = "drop",
    )

  ratings_per_month <- full_join(ratings_per_month, temp, by = c("year", "month"))


  # not owned grouping
  temp <- data %>%
    filter(own == "False") %>%
    group_by(year, month) %>%
    summarise(
      no_ratings_not_owned = n(),
      .groups = "drop",
    )
  ratings_per_month <- full_join(ratings_per_month, temp, by = c("year", "month"))


  # prev owned grouping
  temp <- data %>%
    filter(prevowned == "True") %>%
    group_by(year, month) %>%
    summarise(
      no_ratings_prev_owned = n(),
      .groups = "drop",
    )
  ratings_per_month <- full_join(ratings_per_month, temp, by = c("year", "month"))


  # comment grouping
  temp <- data %>%
    filter(comment != "") %>%
    group_by(year, month) %>%
    summarise(
      no_ratings_comment = n(),
      .groups = "drop",
    )
  ratings_per_month <- full_join(ratings_per_month, temp, by = c("year", "month"))

  # no comment grouping
  temp <- data %>%
    filter(comment == "") %>%
    group_by(year, month) %>%
    summarise(
      no_ratings_no_comment = n(),
      .groups = "drop",
    )
  ratings_per_month <- full_join(ratings_per_month, temp, by = c("year", "month"))


  ratings_per_month <- ratings_per_month %>%
    replace_na(list(
      no_ratings_owned = 0,
      no_ratings_not_owned = 0,
      no_ratings_prev_owned = 0,
      no_ratings_comment = 0,
      no_ratings_no_comment = 0
    )) %>%
    mutate(
      cum_no_ratings = cumsum(no_ratings),
      cum_no_ratings_owned = cumsum(no_ratings_owned),
      cum_no_ratings_not_owned = cumsum(no_ratings_not_owned),
      cum_no_ratings_prev_owned = cumsum(no_ratings_prev_owned),
      cum_no_ratings_comment = cumsum(no_ratings_comment),
      cum_no_ratings_no_comment = cumsum(no_ratings_no_comment)
    )

  vector_year <- ratings_per_month$year
  vector_month <- ratings_per_month$month


  cum_avg_rating <- calc_cum_avg_rating(
    ratings_per_month$avg_rating,
    ratings_per_month$no_ratings,
    ratings_per_month$cum_no_ratings,
    vector_year,
    vector_month,
    "cum_avg_rating"
  )
  ratings_per_month <- ratings_per_month %>% left_join(cum_avg_rating, by = c("year", "month"))

  cum_avg_rating_owned <- calc_cum_avg_rating(
    ratings_per_month$avg_rating_owned,
    ratings_per_month$no_ratings_owned,
    ratings_per_month$cum_no_ratings_owned,
    vector_year,
    vector_month,
    "cum_avg_rating_owned"
  )
  ratings_per_month <- ratings_per_month %>% left_join(cum_avg_rating_owned, by = c("year", "month"))

  cum_avg_rating_not_owned <- calc_cum_avg_rating(
    ratings_per_month$avg_rating_not_owned,
    ratings_per_month$no_ratings_not_owned,
    ratings_per_month$cum_no_ratings_not_owned,
    vector_year,
    vector_month,
    "cum_avg_rating_not_owned"
  )
  ratings_per_month <- ratings_per_month %>% left_join(cum_avg_rating_not_owned, by = c("year", "month"))

  cum_avg_rating_prev_owned <- calc_cum_avg_rating(
    ratings_per_month$avg_rating_prev_owned,
    ratings_per_month$no_ratings_prev_owned,
    ratings_per_month$cum_no_ratings_prev_owned,
    vector_year,
    vector_month,
    "cum_avg_rating_prev_owned"
  )
  ratings_per_month <- ratings_per_month %>% left_join(cum_avg_rating_prev_owned, by = c("year", "month"))

  cum_avg_rating_comment <- calc_cum_avg_rating(
    ratings_per_month$avg_rating_comment,
    ratings_per_month$no_ratings_comment,
    ratings_per_month$cum_no_ratings_comment,
    vector_year,
    vector_month,
    "cum_avg_rating_comment"
  )
  ratings_per_month <- ratings_per_month %>% left_join(cum_avg_rating_comment, by = c("year", "month"))

  cum_avg_rating_no_comment <- calc_cum_avg_rating(
    ratings_per_month$avg_rating_no_comment,
    ratings_per_month$no_ratings_no_comment,
    ratings_per_month$cum_no_ratings_no_comment,
    vector_year,
    vector_month,
    "cum_avg_rating_no_comment"
  )
  ratings_per_month <- ratings_per_month %>% left_join(cum_avg_rating_no_comment, by = c("year", "month"))

  release <- get_release_year_and_month(ratings_per_month, year_official)

  ratings_per_month <- ratings_per_month %>%
    mutate(.,
      month_after_release = 12 * (year - release$start_year) + (month - release$start_month) # add + 1 if first month should be 1
    )

  return(ratings_per_month)
}


calc_cum_avg_rating <- function(avg_rating_column,
                                no_ratings_column,
                                cum_no_ratings_column,
                                year_column,
                                month_column,
                                name_of_column) {
  # Create data frame which will be returned
  cum_avg_rating_data_frame <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c(name_of_column, "year", "month"))
  last_avg_rating <- 0
  last_cum_avg_rating <- 0
  last_cum_no_ratings <- 0
  first_entry_found <- FALSE

  for (key in 1:length(avg_rating_column)) {
    if (first_entry_found == FALSE & is.na(avg_rating_column[key])) {
      cum_avg_rating <- NA
    } else if (is.na(avg_rating_column[key])) {
      cum_avg_rating <- last_cum_avg_rating
    } else {
      first_entry_found <- TRUE

      cum_avg_rating <- (last_cum_avg_rating * last_cum_no_ratings + avg_rating_column[key] * no_ratings_column[key]) / cum_no_ratings_column[key]

      last_cum_avg_rating <- cum_avg_rating
      last_avg_rating <- avg_rating_column[key]
      last_cum_no_ratings <- cum_no_ratings_column[key]
    }

    cum_avg_rating_data_frame[key, ] <- list(cum_avg_rating, year_column[key], month_column[key])
  }


  return(cum_avg_rating_data_frame)
}


#
get_release_year_and_month <- function(ratings_per_month, year_official) {
  start_month <- 5
  start_year <- 2001
  prior_month <- NA
  prior_year <- NA
  valid_months <- 0
  year_reached <- FALSE
  current_months_in_a_row <- 0
  start_is_first <- TRUE

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
      } else {
        start_is_first <- FALSE
        prior_month <- vector_months[key]
        prior_year <- vector_years[key]
        next
      }
    }


    if (vector_no_ratings[key] >= minimum_ratings_per_month) {
      current_months_in_a_row <- current_months_in_a_row + 1
    } else {
      current_months_in_a_row <- 0
      start_is_first <- FALSE
      prior_month <- vector_months[key]
      prior_year <- vector_years[key]
      next
    }

    if (current_months_in_a_row == 1) {
      start_month <- vector_months[key]
      start_year <- vector_years[key]
      # Check if a month was skipped, as in had no ratings and is not in list
    } else if (
      # previous month is not month - 1
      ((start_month + current_months_in_a_row - 1) %% 12 != (vector_months[key]) %% 12) |
        # month not 1, but previous month in different year
        (vector_months[key] != 1 & vector_years[key] != vector_years[key - 1]) |
        # month is 1, but year is not previous year
        (vector_years[key] == 1 & vector_years[key] - 1 != vector_years[key - 1])
    ) {
      current_months_in_a_row <- 0
      start_is_first <- FALSE
      prior_month <- vector_months[key]
      prior_year <- vector_years[key]
      next
    }

    if (current_months_in_a_row == months_in_a_row) {
      return(list(
        "start_month" = start_month,
        "start_year" = start_year,
        "start_is_first" = start_is_first,
        "prior_month" = prior_month,
        "prior_year" = prior_year
      ))
    }
  }

  return(list(
    "start_month" = start_month,
    "start_year" = start_year,
    "start_is_first" = start_is_first,
    "prior_month" = prior_month,
    "prior_year" = prior_year
  ))
}


create_and_save_monthly_data <- function(game_rank, game_id, game_name, release_year, ranking_year, ranking_month) {
  data <- load_csv_with_id(game_id, CSV_INPUT_FOLDER)
  data <- add_date(data, ranking_year, ranking_month)
  ratings_per_month <- calc_ratings_per_month(data, release_year)
  save_csv(ratings_per_month, paste(game_id, RANKING_DATE, sep = "_"), CSV_GAMES_MONTHLY_FOLDER)
  # id_of_last_month <-
  #   which(
  #     ratings_per_month$year == LAST_VALID_YEAR &
  #       ratings_per_month$month == LAST_VALID_MONTH
  #   )
  #
  # last_month_stat <- ratings_per_month[id_of_last_month, ] %>%
  data <- load_csv_with_id(174430, CSV_INPUT_FOLDER)
  data <- add_date(data, 2023, 1)

  return(ratings_per_month)
}

get_monthly_data <- function() {
  data <- load_csv_with_id(game_id, CSV_GAMES_MONTHLY_FOLDER)
}

# Sums up all the last months columns
sum_up_last_months <- function(all_last_month_stats) {
  summed_up <- all_last_month_stats %>%
    summarize(
      no_ratings = sum(cum_no_ratings),
      avg_rating = (sum(cum_no_ratings * cum_avg_rating)) / sum(cum_no_ratings),
      no_ratings_owned = sum(cum_no_ratings_owned),
      avg_rating_owned = (sum(cum_no_ratings_owned * cum_avg_rating_owned)) / sum(cum_no_ratings_owned),
      no_ratings_not_owned = sum(cum_no_ratings_not_owned),
      avg_rating_not_owned = (sum(cum_no_ratings_not_owned * cum_avg_rating_not_owned)) / sum(cum_no_ratings_not_owned),
      no_rating_prev_owneds = sum(cum_no_ratings_prev_owned),
      avg_rating_prev_owned = (sum(cum_no_ratings_prev_owned * cum_avg_rating_prev_owned)) / sum(cum_no_ratings_prev_owned),
      no_ratings_comment = sum(cum_no_ratings_comment),
      avg_rating_comment = (sum(cum_no_ratings_comment * cum_avg_rating_comment)) / sum(cum_no_ratings_comment),
      no_ratings_no_comment = sum(cum_no_ratings_no_comment),
      avg_rating_no_comment = (sum(cum_no_ratings_no_comment * cum_avg_rating_no_comment)) / sum(cum_no_ratings_no_comment)
    )

  return(summed_up)
}

# searches for cleaned ranking file
# creates it, if it doesn't exist
# goes through list of games and searches if list with monthly stats of game is available
# if not, creates them
# gets last month of every game
# collects them and saves them together
collect_last_months <- function(no_top_games = NUMBER_TOP_GAMES, min_no_ratings = MINIMUM_NUMBER_RATINGS) {
  no_top_games <- NUMBER_TOP_GAMES
  min_no_ratings <- MINIMUM_NUMBER_RATINGS
  ranking_file_path <- create_ranking_file_name(
    folder = CSV_RANKING_FOLDER,
    ranking_date = RANKING_DATE,
    top_games = no_top_games,
    min_no_of_ratings = min_no_ratings,
    add_csv_ending = FALSE
  )
  if (file.exists(ranking_file_path)) {
    ranking <- load_csv(ranking_file_path)
  } else {
    ranking <- load_csv(paste(RANKING_DATE, ".csv", sep = ""), CSV_RANKING_FOLDER)
    ranking <- clean_and_save_ranking(ranking, no_top_games, min_no_ratings, RANKING_DATE, CSV_RANKING_FOLDER)
  }

  # Create vectors from ranking
  vector_id <- c(ranking$ID)
  vector_rank <- c(ranking$Rank)
  vector_release_year <- c(ranking$Year)
  vector_name <- c(ranking$Name)

  for (i in 1:length(vector_rank)) {
    print(paste("Current rank:", vector_rank[i], "i:", i, "from:", length(vector_rank)))

    monthly_stats <- load_csv_with_id(vector_id[i], CSV_GAMES_MONTHLY_FOLDER)
    if (!is.list(monthly_stats)) {
      monthly_stats <- create_and_save_monthly_data(
        vector_rank[i],
        vector_id[i],
        vector_name[i],
        vector_release_year[i],
        ranking_year = RANKING_YEAR,
        ranking_month = RANKING_MONTH
      )
    }

    last_month <- monthly_stats[nrow(monthly_stats), ]
    last_month <- add_column(
      last_month,
      "rank" = vector_rank[i],
      "id" = vector_id[i],
      "name" = vector_name[i],
      .before = 1
    )

    # ID of original data frame
    if ("X" %in% colnames(last_month)) {
      last_month <- subset(last_month, select = -c(X))
    }

    if (i == 1) {
      all_last_month_stats <- as.data.frame(last_month)
    } else {
      all_last_month_stats <- all_last_month_stats %>%
        add_row(last_month)
    }
  }

  all_last_month_stats <- subset(all_last_month_stats, select = -c(
    year,
    month,
    avg_rating,
    no_ratings,
    avg_rating_owned,
    avg_rating_not_owned,
    avg_rating_prev_owned,
    avg_rating_comment,
    avg_rating_no_comment,
    no_ratings_owned,
    no_ratings_not_owned,
    no_ratings_prev_owned,
    no_ratings_comment,
    no_ratings_no_comment
  ))

  naming <- create_name_no_ratings_top_games(min_no_of_ratings = min_no_ratings, top_games = no_top_games)

  save_csv(all_last_month_stats,
    
    filename = paste(RANKING_DATE, naming, sep = "_"),
    filepath = CSV_LAST_MONTHS_FOLDER,
    add_csv_ending = TRUE
  )

  print(all_last_month_stats)

  # return(all_last_month_stats)
}
