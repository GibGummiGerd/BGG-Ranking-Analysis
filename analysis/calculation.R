library(tidyverse)
source("const.R")
source("files.R")

#
add_date <- function(data) {
  
  # Add month and year after release
  data$month <- as.numeric(format(as.Date(data$rating_tstamp), format="%m"))
  data$year <- as.numeric(format(as.Date(data$rating_tstamp), format="%Y"))
  
  # Remove entries without rating
  data <- subset(data, data$rating>0)
  
  return(data)
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
      avg_rating_prev_owned = mean(rating[prevowned == "True"]),
      avg_rating_comment = mean(rating[textfield != ""]),
      avg_rating_no_comment = mean(rating[textfield == ""]),
    ) %>%
    ungroup()
  
  # owned grouping
  temp <- data %>%
    filter(own == "True") %>%
    group_by(year, month) %>%
    summarise(
      no_ratings_owned = n(),
    ) %>%
    ungroup()
    
  ratings_per_month <- merge(ratings_per_month, temp, all = TRUE)
  
  # not owned grouping
  temp <- data %>%
    filter(own == "False") %>%
    group_by(year, month) %>%
    summarise(
      no_ratings_not_owned = n()
    )
  ratings_per_month <- merge(ratings_per_month, temp, all = TRUE)
  
  # prev owned grouping
  temp <- data %>%
    filter(prevowned == "True") %>%
    group_by(year, month) %>%
    summarise(
      no_ratings_prev_owned = n()
    )
  ratings_per_month <- merge(ratings_per_month, temp, all = TRUE)
  
  # comment grouping
  temp <- data %>%
    filter(textfield != "") %>%
    group_by(year, month) %>%
    summarise(
      no_ratings_comment = n()
    )
  ratings_per_month <- merge(ratings_per_month, temp, all = TRUE)
  
  # no comment grouping
  temp <- data %>%
    filter(textfield == "") %>%
    group_by(year, month) %>%
    summarise(
      no_ratings_no_comment = n()
    )
  ratings_per_month <- merge(ratings_per_month, temp, all = TRUE)
  
  ratings_per_month <- ratings_per_month %>%
    replace_na(list(no_ratings_owned = 0,
                    no_ratings_not_owned = 0,
                    no_ratings_prev_owned = 0,
                    no_ratings_comment = 0,
                    no_ratings_no_comment = 0)) %>%
    mutate(
      cum_no_ratings = cumsum(no_ratings),
      cum_no_ratings_owned = cumsum(no_ratings_owned),
      cum_no_ratings_not_owned = cumsum(no_ratings_not_owned),
      cum_no_ratings_prev_owned = cumsum(no_ratings_prev_owned),
      cum_no_ratings_comment = cumsum(no_ratings_comment),
      cum_no_ratings_no_comment = cumsum(no_ratings_no_comment)
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
      )[,1],
      cum_avg_rating_prev_owned = calc_cum_avg_rating(
        ratings_per_month["avg_rating_prev_owned"],
        ratings_per_month["no_ratings_prev_owned"],
        ratings_per_month["cum_no_ratings_prev_owned"]
      )[,1],
      cum_avg_rating_comment = calc_cum_avg_rating(
        ratings_per_month["avg_rating_comment"],
        ratings_per_month["no_ratings_comment"],
        ratings_per_month["cum_no_ratings_comment"]
      )[,1],
      cum_avg_rating_no_comment = calc_cum_avg_rating(
        ratings_per_month["avg_rating_no_comment"],
        ratings_per_month["no_ratings_no_comment"],
        ratings_per_month["cum_no_ratings_no_comment"]
      )[,1]
    )
  
  release <- get_release_year_and_month(ratings_per_month, year_official)
  print("RELEASE")
  print(release)
  
  
  
  # #here
  # if (isFALSE(release$start_is_first)) {
  #   prior_year <- release$prior_year
  #   prior_month <- release$prior_month
  #   # if there is a gap
  #   if ((release$prior_month + 1)%% 12 != release$start_month) {
  #     prior_row <- filter(
  #       ratings_per_month,
  #       year == prior_year,
  #       month == prior_month
  #       )
  #     prior_row <- prior_row[,1]
  #     ratings_per_month %>% add_row(prior_row)
  #   }
  # } else {
  #   release_year <- release$prior_year
  #   release_month <- release$prior_month
  # }
  # 
  # if (isFALSE(release$start_is_first)) {
  #   ratings_per_month <- ratings_per_month %>%
  #     filter(
  #       ratings_per_month$year > release$prior_year | 
  #         (ratings_per_month$year == release$prior_year & ratings_per_month$month >= release$prior_month)
  #     ) %>%
  #     mutate(
  #       month_after_release = 12*(year - release$prior_year) + (month - release$prior_month)
  #     )
  # }
  # # here

  ratings_per_month <- ratings_per_month %>%
    # {
    #   if (isFALSE(release$start_is_first)) {
    #     filter( .,
    #       ratings_per_month$year > release$prior_year | 
    #         (ratings_per_month$year == release$prior_year & ratings_per_month$month >= release$prior_month)
    #       )
    #   } else {
    #     filter( .,
    #       ratings_per_month$year > release$start_year | 
    #         (ratings_per_month$year == release$start_year & ratings_per_month$month >= release$start_month)
    #     )
    #   }
    # } %>%
    mutate( .,
            month_after_release = 12*(year - release$start_year) + (month - release$start_month) # add + 1 if first month should be 1
    )
    # {
    #   if (isTRUE(release$start_is_first)) {
    #     print("if")
    #     mutate( .,
    #             month_after_release = 12*(year - release$start_year) + (month - release$start_month) + 1 # + 1 because first month = 1
    #     )
    #   } else {
    #     print("else")
    #     # Find
    #     print(release$prior_year)
    #     print(release$prior_month)
    #     mutate( .,
    #             month_after_release = 12*(year - release$prior_year) + (month - release$prior_month)
    #     )
    #   }
    # }

  return(ratings_per_month)
}


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
  first_entry_found = FALSE
  
  for (key in 1:nrow(avg_rating_column)) {
    if (first_entry_found == FALSE & is.na(avg_rating_column[key,])) {
      cum_avg_rating = NA
    } else if (is.na(avg_rating_column[key,])){
      cum_avg_rating = last_cum_avg_rating
    } else {
      first_entry_found <- TRUE
      
      cum_avg_rating = (last_cum_avg_rating * last_cum_no_ratings + avg_rating_column[key,] * no_ratings_column[key,]) / cum_no_ratings_column[key,]
      last_cum_avg_rating = cum_avg_rating
      last_avg_rating = avg_rating_column[key,]
      last_cum_no_ratings = cum_no_ratings_column[key,]
    }
    cum_avg_rating_data_frame[key,] = c(cum_avg_rating)
  }
  
  return (cum_avg_rating_data_frame)
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
    
    print("")
    print("number of ratings:")
    print(vector_no_ratings[key])
    
    if (year_reached == FALSE) {
      if (vector_years[key] >= year_official) {
        print("year reached")
        year_reached <- TRUE
      } else {
        print("year not reached")
        start_is_first <- FALSE
        prior_month = vector_months[key]
        prior_year = vector_years[key]
        next
      }
    } 
    
    
    if (vector_no_ratings[key] >= minimum_ratings_per_month) {
      print("fulfilled")
      current_months_in_a_row <- current_months_in_a_row + 1
    } else {
      current_months_in_a_row <- 0
      start_is_first <- FALSE
      prior_month = vector_months[key]
      prior_year = vector_years[key]
      next
    }
    
    if (current_months_in_a_row == 1) {
      print("new streak started")
      start_month = vector_months[key]
      start_year = vector_years[key]
      # Check if a month was skipped, as in had no ratings and is not in list
    } else if (
      # previous month is not month - 1
      ((start_month + current_months_in_a_row - 1)%% 12 != (vector_months[key])%% 12) |
      # month not 1, but previous month in different year
      (vector_months[key] != 1 & vector_years[key] != vector_years[key-1]) |
      # month is 1, but year is not previous year
      (vector_years[key] == 1 & vector_years[key] - 1 != vector_years[key-1])
      ) 
      {
        print("missing month between this and the last one in the streak")
        print(vector_months[key])
        print(vector_years[key] != vector_years[key-1])
        current_months_in_a_row <- 0
        start_is_first <- FALSE
        prior_month = vector_months[key]
        prior_year = vector_years[key]
        next
    }
    
    if (current_months_in_a_row == months_in_a_row) {
      return (list(
        "start_month" = start_month,
        "start_year" = start_year, 
        "start_is_first" = start_is_first,
        "prior_month" = prior_month,
        "prior_year" = prior_year))
    }
    
  }
  
  return (list(
    "start_month" = start_month,
    "start_year" = start_year, 
    "start_is_first" = start_is_first,
    "prior_month" = prior_month,
    "prior_year" = prior_year))
  
}



