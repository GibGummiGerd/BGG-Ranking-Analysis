# PACKAGES
source("setup.R")

all_top <- clean_and_save_ranking(jo, min_no_of_ratings = 2000)



# Only needs to be run once for every RANKING_DATE - NUMBER_OF_RATINGS combo
collect_last_months()
all_last_month_stats <- collect_last_months(1000, 0)

all_last_month_stats <- load_csv(paste(RANKING_DATE, "last_months_top", NUMBER_TOP_GAMES, sep = "_"), CSV_LAST_MONTHS_FOLDER, add_csv_ending = TRUE)

summed_up_last_months <- sum_up_last_months(all_last_month_stats)

qqnorm(all_last_month_stats$cum_avg_rating)
qqline(all_last_month_stats$cum_avg_rating)


str(all_last_month_stats)

ggplot(all_last_month_stats, aes()) +

  scale_x_discrete("Subset of ratings",
                   labels = c(
                     "cum_avg_rating" = "overall",
                     "cum_avg_rating_owned" = "Game owned",
                     "cum_avg_rating_not_owned" = "Game not owned",
                     "cum_avg_rating_prev_owned" = "Game previously owned",
                     "cum_avg_rating_comment" = "Rating commented",
                     "cum_avg_rating_no_comment" = "Rating not commented"
                   )) +
  
  scale_y_continuous(
    "Rating",
    breaks = seq(1, 10, 1),
    limits = c(1,10)
  ) +
  
  # geom_violin(aes(x="cum_avg_rating",y = cum_avg_rating, fill = "violin"), adjust = 0.3) +
  # geom_boxplot(aes(x="cum_avg_rating",y = cum_avg_rating), varwidth = TRUE, width = 0.15, outlier.shape = NA) +
  
  geom_violin(aes(x="cum_avg_rating_owned",y = cum_avg_rating_owned, fill = "violin"), adjust = 0.3) +
  geom_boxplot(aes(x="cum_avg_rating_owned",y = cum_avg_rating_owned), varwidth = TRUE, width = 0.15, outlier.shape = NA) +
  
  # geom_dotplot(binaxis = "y", method = "histodot", dotsize = 0.3, binwidth = 0.05, stackratio = 1, stackdir = "center",
  #              aes(x="cum_avg_rating_not_owned",y = cum_avg_rating_not_owned)) +
  geom_violin(aes(x="cum_avg_rating_not_owned",y = cum_avg_rating_not_owned, fill = "violin"), adjust = 0.3) +
  geom_boxplot(aes(x="cum_avg_rating_not_owned",y = cum_avg_rating_not_owned), varwidth = TRUE, width = 0.15, outlier.shape = NA) +
  
  geom_violin(aes(x="cum_avg_rating_prev_owned",y = cum_avg_rating_prev_owned, fill = "violin"), adjust = 0.3) +
  geom_boxplot(aes(x="cum_avg_rating_prev_owned",y = cum_avg_rating_prev_owned), varwidth = TRUE, width = 0.15, outlier.shape = NA) +
  
  geom_violin(aes(x="cum_avg_rating_comment",y = cum_avg_rating_comment, fill = "violin"), adjust = 0.3) +
  # geom_dotplot(binaxis = "y", method = "histodot", dotsize = 0.3, binwidth = 0.05, stackratio = 1, stackdir = "center",
  #              aes(x="cum_avg_rating_comment",y = cum_avg_rating_comment)) +
  geom_boxplot(aes(x="cum_avg_rating_comment",y = cum_avg_rating_comment), varwidth = TRUE, width = 0.15, outlier.shape = NA) +
  
  geom_violin(aes(x="cum_avg_rating_no_comment",y = cum_avg_rating_no_comment, fill = "violin"), adjust = 0.3) +
  geom_boxplot(aes(x="cum_avg_rating_no_comment",y = cum_avg_rating_no_comment), varwidth = TRUE, width = 0.15, outlier.shape = NA) +
  
  geom_dotplot(binaxis = "y", method = "histodot", dotsize = 0.3, binwidth = 0.05, stackratio = 1, stackdir = "down",
               aes(x="cum_avg_rating",y = cum_avg_rating_not_owned)) +
  geom_dotplot(binaxis = "y", method = "histodot", dotsize = 0.3, binwidth = 0.05, stackratio = 1, stackdir = "up",
               aes(x="cum_avg_rating",y = cum_avg_rating_owned)) +
  
  theme_bw() +
  theme(panel.border = element_rect(colour = "white"),
        axis.line = element_line(colour = "grey80")) +
  coord_flip()

ggplot(all_last_month_stats, aes()) + 
  scale_y_continuous(
    "Rating",
    breaks = seq(1, 10, 1),
    limits = c(1,10)
  ) +
  geom_dotplot(binaxis = "y", method = "histodot", dotsize = 0.5, binwidth = 0.1, stackratio = 1, stackdir = "up",
               aes(x="cum_avg_rating_owned",y = cum_avg_rating_prev_owned, fill = "1")) +
  geom_dotplot(binaxis = "y", method = "histodot", dotsize = 0.5, binwidth = 0.1, stackratio = 1, stackdir = "down",
               aes(x="cum_avg_rating_owned",y = cum_avg_rating_not_owned, fill = "2")) 

df <- data.frame(uno  = c(summed_up_last_months$no_ratings_not_owned, summed_up_last_months$no_ratings_owned), 
                 dos = c("no_ratings_not_owned", "no_ratings_owned")
                 )


ggplot(df, aes(x = uno, fill=dos, y ="323")) +
  geom_col(position="stack", width = 1)



ggplot(all_last_month_stats, aes(sample = cum_avg_rating)) +
  stat_qq() + stat_qq_line()


t_data <- data.frame(r_comment_no_comment = c(all_last_month_stats$cum_avg_rating_comment, all_last_month_stats$cum_avg_rating_no_comment),
                        half = c(rep.int(TRUE, nrow(all_last_month_stats)), rep.int(FALSE ,nrow(all_last_month_stats))),
                        r_comment = c(all_last_month_stats$cum_avg_rating, all_last_month_stats$cum_avg_rating_comment),
                        avg_other = c(rep.int(FALSE, nrow(all_last_month_stats)), rep.int(TRUE ,nrow(all_last_month_stats))),
                        r_no_comment = c(all_last_month_stats$cum_avg_rating, all_last_month_stats$cum_avg_rating_no_comment),
                        r_not_owned_prev = c(all_last_month_stats$cum_avg_rating_not_owned, all_last_month_stats$cum_avg_rating_prev_owned),
                     r_owned_not_owned = c(all_last_month_stats$cum_avg_rating_owned, all_last_month_stats$cum_avg_rating_not_owned)
                        )

# influence comment, no comment
t.test(data = t_data, r_comment_no_comment ~ half, var.equal = FALSE)

# influence comment, average
t.test(data = t_data, r_comment ~ avg_other, var.equal = TRUE)
# influence no comment, average
t.test(data = t_data, r_no_comment ~ avg_other, alternative = "greater")

t.test(data = t_data, r_not_owned_prev ~ half, var.equal = TRUE)
