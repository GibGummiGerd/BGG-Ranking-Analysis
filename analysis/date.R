source("const.R")
source("files.R")
source("ranking.R")

#
create_date_file <- function() {
    ranking <- load_csv(filename = create_ranking_file_name(), add_csv_ending = TRUE)
    print(ranking)
    date_file <- tibble(
        id = integer(),
        name = character(),
        year_official = integer(),
        month_first_rating = integer(),
        year_first_rating = integer(),
        month_available = integer(),
        year_available = integer()
    )
    # colnames(date_file) <- c("ID", "Name", "year_official", "month_first_rating", "year_first_rating", "month_available", "year_available")
    for (key in seq_along(ranking$X)) {
        print(key)
        ratings_per_month <- load_csv_with_id(ranking$ID[key])
        calculated_dates <- calculate_date_available_and_first_rating(ratings_per_month, ranking$Year[key])
        date_file <- add_row(date_file,
            id = ranking$ID[key],
            name = ranking$Name[key],
            year_official = calculated_dates$year_official,
            month_first_rating = calculated_dates$month_first_rating,
            year_first_rating = calculated_dates$year_first_rating,
            month_available = calculated_dates$month_available,
            year_available = calculated_dates$year_available
        )
    }
    save_csv(
        df = date_file,
        filename = paste("date", RANKING_DATE, create_name_no_ratings_top_games(), sep = "_"),
        filepath = FOLDER_DATE_AVAILABILITY,
    )
}


# Determine a release date for a game
# Tries to find a date when a game was available, because officially only a year is given, which not always represents real availability
# Dependent on the official year different amount of ratings are needed to determine release date
calculate_date_available_and_first_rating <- function(ratings_per_month, year_official) {
    # Start date when BGG first allowed ratings
    month_available <- 5
    year_available <- 2001
    current_months_in_a_row <- 0

    vector_no_ratings <- ratings_per_month$no_ratings
    vector_years <- ratings_per_month$year
    vector_months <- ratings_per_month$month

    max_no_ratings_tenth <- max(vector_no_ratings) %/% 10

    # Set how many month in a row with how many ratings
    # Games 2000 and before will be set to date of first ratings possible
    if (year_official <= 2000) {
        return(list(
            "year_official" = year_official,
            "month_first_rating" = vector_months[1],
            "year_first_rating" = vector_years[1],
            "month_available" = month_available,
            "year_available" = year_available
        ))
    } else if (year_official <= 2006) {
        months_in_a_row <- 3
        minimum_ratings_per_month <- min(3, max_no_ratings_tenth)
    } else if (year_official <= 2009) {
        months_in_a_row <- 3
        minimum_ratings_per_month <- min(5, max_no_ratings_tenth)
    } else if (year_official <= 2016) {
        months_in_a_row <- 6
        minimum_ratings_per_month <- min(10, max_no_ratings_tenth)
    } else if (year_official <= 2019) {
        months_in_a_row <- 6
        minimum_ratings_per_month <- min(12, max_no_ratings_tenth)
    } else {
        months_in_a_row <- 6
        minimum_ratings_per_month <- min(20, max_no_ratings_tenth)
    }

    for (key in seq_along(vector_years)) {
        # Check if minimum rating of month was reached
        # Otherwise reset counter
        if (vector_no_ratings[key] >= minimum_ratings_per_month) {
            current_months_in_a_row <- current_months_in_a_row + 1
        } else {
            current_months_in_a_row <- 0
            next
        }

        # Reset start month and year
        if (current_months_in_a_row == 1) {
            month_available <- vector_months[key]
            year_available <- vector_years[key]
            # Check if a month was skipped, as in had no ratings and is not in list
        } else if (
            # previous month is not month - 1
            ((month_available + current_months_in_a_row - 1) %% 12 != (vector_months[key]) %% 12) |
                # month not 1, but previous month in different year
                (vector_months[key] != 1 & vector_years[key] != vector_years[key - 1]) |
                # month is 1, but year is not previous year
                (vector_months[key] == 1 & vector_years[key] - 1 != vector_years[key - 1])
        ) {
            current_months_in_a_row <- 0
            next
        }

        # Month streak wanted is met
        if (current_months_in_a_row == months_in_a_row) {
            break
        }
    }

    return(list(
        "year_official" = year_official,
        "month_first_rating" = vector_months[1],
        "year_first_rating" = vector_years[1],
        "month_available" = month_available,
        "year_available" = year_available
    ))
}
