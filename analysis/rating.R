# PACKAGES
source("calculation.R")
source("const.R")
source("files.R")
library(tidyverse)


data <- prepare_data(throne)

# Ranking
ranking <- load_csv(paste(RANKING_DATE, ".csv", sep = ""))
ranking <- clean_and_save_ranking(ranking, NO_TOP_GAMES, RANKING_DATE)



ratings_per_month <- calc_ratings_per_month(data)
get_release_year_and_month(ratings_per_month, 2002)

vectorID <- c(ranking$ID)
vectorRank <- c(ranking$Rank)
vectorYear <- c(ranking$Year)
vv <- vectorRank * vectorYear
sapply(vectorID, yolo)

vectorYear * vectorRank
mapply(flow, vectorID, vectorYear)

rm(RANKING_DATE)

flow <- function(id, year) {
  data <- load_csv_with_id(id)
  data <- prepare_data(data)
  ratings_per_month <- calc_ratings_per_month(data, year)
  
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

