# PACKAGES
source("setup.R")



# Only needs to be run once for every RANKING_DATE - NUMBER_OF_RATINGS combo
collect_last_months()
all_last_month_stats <- collect_last_months(1000, 0)


# Load file with the last month stat of each game
all_last_month_stats <- load_csv(paste(RANKING_DATE, "last_months_top", 0, sep = "_"), CSV_LAST_MONTHS_FOLDER, add_csv_ending = TRUE)

# If you need the summed up statistics
summed_up_last_months <- sum_up_last_months(all_last_month_stats)

# Differences between average rating and conidtional ratings
all_last_month_stats <- all_last_month_stats %>%
  mutate(
    diff_avg_owned = cum_avg_rating_owned - cum_avg_rating,
    diff_avg_not_owned = cum_avg_rating_not_owned - cum_avg_rating,
    diff_avg_prev_owned = cum_avg_rating_prev_owned - cum_avg_rating,
    diff_avg_comment = cum_avg_rating_comment - cum_avg_rating,
    diff_avg_no_comment = cum_avg_rating_no_comment - cum_avg_rating,
    perc_diff_avg_owned = ((cum_avg_rating_owned / cum_avg_rating) - 1) * 100,
    perc_diff_avg_not_owned = ((cum_avg_rating_not_owned / cum_avg_rating) - 1) * 100,
    perc_diff_avg_prev_owned = ((cum_avg_rating_prev_owned / cum_avg_rating) - 1) * 100,
    perc_diff_avg_comment = ((cum_avg_rating_comment / cum_avg_rating) - 1) * 100,
    perc_diff_avg_no_comment = ((cum_avg_rating_no_comment / cum_avg_rating) - 1) * 100
  )

ownage_influence <- all_last_month_stats %>%
  select(
    name,
    rank,
    cum_no_ratings,
    cum_no_ratings_owned,
    cum_no_ratings_not_owned,
    cum_no_ratings_prev_owned,
    cum_avg_rating,
    cum_avg_rating_owned,
    cum_avg_rating_not_owned,
    cum_avg_rating_prev_owned,
    diff_avg_owned,
    diff_avg_not_owned,
    diff_avg_prev_owned,
    perc_diff_avg_owned,
    perc_diff_avg_not_owned,
    perc_diff_avg_prev_owned,
  )

# Graph to see if normal distribution
qqnorm(all_last_month_stats$cum_avg_rating)
qqline(all_last_month_stats$cum_avg_rating)


reactable(
  best_owned_influence,
  columns = list(
    name = colDef(name = "Name"),
    rank = colDef(name = "Rank"),
    cum_no_ratings = colDef(align = "center", name = "Total number of ratings"),
    cum_no_ratings_owned = colDef(name = "Number of ratings of owners"),
    cum_avg_rating = colDef(name = "Overall rating"),
    cum_avg_rating_owned = colDef(name = "Rating when game is owned"),
    diff_avg_owned = colDef(name = "Rating difference"),
    perc_diff_avg_owned = colDef(name = "Percentage difference")
  )
)

reactable(
  ownage_influence,
  theme = fivethirtyeight(),
  defaultPageSize = 25,
  
  ### add column group header
  columnGroups = list(
    colGroup(name = "Number of Ratings", columns = c("cum_no_ratings", "cum_no_ratings_owned", "cum_no_ratings_not_owned", "cum_no_ratings_prev_owned")),
    colGroup(name = "Average rating", columns = c("cum_avg_rating", "cum_avg_rating_owned", "cum_avg_rating_not_owned", "cum_avg_rating_prev_owned")),
    colGroup(name = "Rating Difference from avg when", columns = c("diff_avg_owned", "diff_avg_not_owned", "diff_avg_prev_owned")),
    colGroup(name = "Percentage difference from avg when", columns = c("perc_diff_avg_owned", "perc_diff_avg_not_owned", "perc_diff_avg_prev_owned"))
  ),
  columns = list(
    name = colDef(name = "Name", maxWidth = 120),
    rank = colDef(
      name = "BGG Rank", 
      maxWidth = 60, 
      align = "center",
      style = list(borderRight = "1px dashed rgba(0, 0, 0, 0.3)")),
    
    cum_no_ratings = colDef(name = "Overall", maxWidth = 60, align = "center"),
    cum_no_ratings_owned = colDef(name = "Owned", maxWidth = 60, align = "center"),
    cum_no_ratings_not_owned = colDef(name = "Not owned", maxWidth = 60, align = "center"),
    cum_no_ratings_prev_owned = colDef(name = "Prev. owned", maxWidth = 60, align = "center"),
    diff_avg_owned = colDef(
      name = "Owned",
      maxWidth = 60,
      align = "center",
      cell = function(x) {
        sprintf("%+0.2f", x)
      },
      style = color_scales(ownage_influence, colors = c("#fd84a9", "white", "#42c2ca"))
    ),
    diff_avg_not_owned = colDef(
      name = "Not owned",
      maxWidth = 60,
      align = "center",
      cell = function(x) {
        sprintf("%+0.2f", x)
      },
      style = color_scales(ownage_influence, colors = c("#fd84a9", "white", "#42c2ca"))
    ),
    diff_avg_prev_owned = colDef(
      name = "Prev. owned",
      maxWidth = 60,
      align = "center",
      cell = function(x) {
        sprintf("%+0.2f", x)
      },
      style = color_scales(ownage_influence, colors = c("#fd84a9", "white", "#42c2ca"))
    ),
    perc_diff_avg_owned = colDef(
      name = "owned",
      maxWidth = 65,
      align = "center",
      cell = function(x) {
        sprintf("%+0.2f %%", x)
      },
      style = color_scales(ownage_influence, colors = c("#fd84a9", "white", "#42c2ca"))
    ),
    perc_diff_avg_not_owned = colDef(
      name = "not owned",
      maxWidth = 65,
      align = "center",
      cell = function(x) {
        sprintf("%+0.2f %%", x)
      },
      style = color_scales(ownage_influence, colors = c("#fd84a9", "white", "#42c2ca"))
    ),
    perc_diff_avg_prev_owned = colDef(
      name = "prev. owned",
      maxWidth = 65,
      align = "center",
      cell = function(x) {
        sprintf("%+0.2f %%", x)
      },
      style = color_scales(ownage_influence, colors = c("#fd84a9", "white", "#42c2ca"))
    ),
    cum_avg_rating = colDef(
      name = "Overall",
      maxWidth = 60,
      align = "center",
      cell = function(x) {
        sprintf("%0.2f", x)
      },
      style = color_scales(ownage_influence, colors = c("#fd84a9", "white", "#42c2ca"))
    ),
    cum_avg_rating_owned = colDef(
      name = "Owned",
      maxWidth = 60,
      align = "center",
      cell = function(x) {
        sprintf("%0.2f", x)
      },
      style = color_scales(ownage_influence, colors = c("#fd84a9", "white", "#42c2ca"))
    ),
    cum_avg_rating_not_owned = colDef(
      name = " Not Owned",
      maxWidth = 60,
      align = "center",
      cell = function(x) {
        sprintf("%0.2f", x)
      },
      style = color_scales(ownage_influence, colors = c("#fd84a9", "white", "#42c2ca"))
    ),
    cum_avg_rating_prev_owned = colDef(
      name = "Prev. Owned",
      maxWidth = 60,
      align = "center",
      cell = function(x) {
        sprintf("%0.2f", x)
      },
      style = color_scales(ownage_influence, colors = c("#fd84a9", "white", "#42c2ca"))
    )
  ),
)

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
    )
  ) +
  scale_y_continuous(
    "Rating",
    breaks = seq(1, 10, 1),
    limits = c(1, 10)
  ) +

  # geom_violin(aes(x="cum_avg_rating",y = cum_avg_rating, fill = "violin"), adjust = 0.3) +
  # geom_boxplot(aes(x="cum_avg_rating",y = cum_avg_rating), varwidth = TRUE, width = 0.15, outlier.shape = NA) +

  geom_violin(aes(x = "cum_avg_rating_owned", y = cum_avg_rating_owned, fill = "violin"), adjust = 0.3) +
  geom_boxplot(aes(x = "cum_avg_rating_owned", y = cum_avg_rating_owned), varwidth = TRUE, width = 0.15, outlier.shape = NA) +

  # geom_dotplot(binaxis = "y", method = "histodot", dotsize = 0.3, binwidth = 0.05, stackratio = 1, stackdir = "center",
  #              aes(x="cum_avg_rating_not_owned",y = cum_avg_rating_not_owned)) +
  geom_violin(aes(x = "cum_avg_rating_not_owned", y = cum_avg_rating_not_owned, fill = "violin"), adjust = 0.3) +
  geom_boxplot(aes(x = "cum_avg_rating_not_owned", y = cum_avg_rating_not_owned), varwidth = TRUE, width = 0.15, outlier.shape = NA) +
  geom_violin(aes(x = "cum_avg_rating_prev_owned", y = cum_avg_rating_prev_owned, fill = "violin"), adjust = 0.3) +
  geom_boxplot(aes(x = "cum_avg_rating_prev_owned", y = cum_avg_rating_prev_owned), varwidth = TRUE, width = 0.15, outlier.shape = NA) +
  geom_violin(aes(x = "cum_avg_rating_comment", y = cum_avg_rating_comment, fill = "violin"), adjust = 0.3) +
  # geom_dotplot(binaxis = "y", method = "histodot", dotsize = 0.3, binwidth = 0.05, stackratio = 1, stackdir = "center",
  #              aes(x="cum_avg_rating_comment",y = cum_avg_rating_comment)) +
  geom_boxplot(aes(x = "cum_avg_rating_comment", y = cum_avg_rating_comment), varwidth = TRUE, width = 0.15, outlier.shape = NA) +
  geom_violin(aes(x = "cum_avg_rating_no_comment", y = cum_avg_rating_no_comment, fill = "violin"), adjust = 0.3) +
  geom_boxplot(aes(x = "cum_avg_rating_no_comment", y = cum_avg_rating_no_comment), varwidth = TRUE, width = 0.15, outlier.shape = NA) +
  geom_dotplot(
    binaxis = "y", method = "histodot", dotsize = 0.3, binwidth = 0.05, stackratio = 1, stackdir = "down",
    aes(x = "cum_avg_rating", y = cum_avg_rating_not_owned)
  ) +
  geom_dotplot(
    binaxis = "y", method = "histodot", dotsize = 0.3, binwidth = 0.05, stackratio = 1, stackdir = "up",
    aes(x = "cum_avg_rating", y = cum_avg_rating_owned)
  ) +
  theme_bw() +
  theme(
    panel.border = element_rect(colour = "white"),
    axis.line = element_line(colour = "grey80")
  ) +
  coord_flip()

ggplot(all_last_month_stats, aes()) +
  scale_y_continuous(
    "Rating",
    breaks = seq(1, 10, 1),
    limits = c(1, 10)
  ) +
  geom_dotplot(
    binaxis = "y", method = "histodot", dotsize = 0.5, binwidth = 0.1, stackratio = 1, stackdir = "up",
    aes(x = "cum_avg_rating_owned", y = cum_avg_rating_prev_owned, fill = "1")
  ) +
  geom_dotplot(
    binaxis = "y", method = "histodot", dotsize = 0.5, binwidth = 0.1, stackratio = 1, stackdir = "down",
    aes(x = "cum_avg_rating_owned", y = cum_avg_rating_not_owned, fill = "2")
  )

df <- data.frame(
  uno = c(summed_up_last_months$no_ratings_not_owned, summed_up_last_months$no_ratings_owned),
  dos = c("no_ratings_not_owned", "no_ratings_owned")
)


ggplot(df, aes(x = uno, fill = dos, y = "323")) +
  geom_col(position = "stack", width = 1)



ggplot(all_last_month_stats, aes(sample = cum_avg_rating)) +
  stat_qq() +
  stat_qq_line()


t_data <- data.frame(
  r_comment_no_comment = c(all_last_month_stats$cum_avg_rating_comment, all_last_month_stats$cum_avg_rating_no_comment),
  half = c(rep.int(TRUE, nrow(all_last_month_stats)), rep.int(FALSE, nrow(all_last_month_stats))),
  r_comment = c(all_last_month_stats$cum_avg_rating, all_last_month_stats$cum_avg_rating_comment),
  avg_other = c(rep.int(FALSE, nrow(all_last_month_stats)), rep.int(TRUE, nrow(all_last_month_stats))),
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
