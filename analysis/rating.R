# PACKAGES
source("calculation.R")
source("const.R")
source("files.R")
source("ranking.R")
library(tidyverse)


# Ranking
ranking <- load_csv(paste(RANKING_DATE, ".csv", sep = ""), CSV_RANKING_FOLDER)
ranking <- clean_and_save_ranking(ranking, NO_TOP_GAMES, RANKING_DATE)


data <- load_csv_with_id(167355)
data <- add_date(data)
ratings_per_month <- calc_ratings_per_month(data, 2018)
get_release_year_and_month(ratings_per_month, 2002)

vectorID <- c(ranking$ID)
vectorRank <- c(ranking$Rank)
vectorYear <- c(ranking$Year)


mapply(flow, vectorID, vectorYear)



flow <- function(id, year) {
  data <- load_csv_with_id(id)
  data <- group_ratings_by_month(data)
  ratings_per_month <- calc_ratings_per_month(data, year)
  save_csv(ratings_per_month, paste(id, RANKING_DATE, sep = "_"), CSV_GAMES_FOLDER)
  
}

ggplot(data=ratings_per_month, mapping = aes(x = month_after_release)) +
  geom_point(aes(y = avg_rating, color="cum_avg_rating", size=no_ratings)) +
  geom_line(aes(y = cum_avg_rating, color="cum_avg_rating")) +
  geom_smooth(aes(y = avg_rating, color="cum_avg_rating"), method = "gam", formula = y ~ s(x, bs = "cs")) +
  
  geom_point(aes(y = avg_rating_owned, color="cum_avg_rating_owned", size=no_ratings_owned)) +
  geom_smooth(aes(y = avg_rating_owned, color="cum_avg_rating_owned"), method = "gam", formula = y ~ s(x, bs = "cs")) +
  geom_line(aes(y = cum_avg_rating_owned, color="cum_avg_rating_owned")) +
  
  geom_point(aes(y = avg_rating_not_owned, color="cum_avg_rating_not_owned", size=no_ratings_not_owned)) +
  
  geom_smooth(aes(y = avg_rating_not_owned, color="cum_avg_rating_not_owned"), method = "gam", formula = y ~ s(x, bs = "cs")) +
  geom_line(aes(y = cum_avg_rating_not_owned, color="cum_avg_rating_not_owned"))


ggplot(data=ratings_per_month, mapping = aes(x = month_after_release, y = avg_rating_owned)) + 
  geom_point() + 
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))

