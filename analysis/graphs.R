save_reactable_test(interactive_table, "iris_table.html")
write.csv(summed_up_last_months, "123.csv", row.names=FALSE)
# Graph about average rating
interactive_table <- reactable(
# reactable(
  summed_up_last_months,
  theme = fivethirtyeight(),
  
  columns = list(
    type_rating = colDef(
      name = "Type of rating", 
      style = list(borderRight = "1px solid #777")
    ),
    no_ratings = colDef(
      name = "Number of ratings",
      align = "right",
      cell = function(x) {
        formatC(x, format="f", big.mark=",", digits=0)
      },
      style = color_scales(summed_up_last_months, colors = c("#56B4E9", "white", "#FFB715"))
    ),
    percentage = colDef(name = "Percentage of all ratings", 
                        cell = function(x) {
                          sprintf("%0.2f %%", x)
                        },
                        style = color_scales(summed_up_last_months, colors = c("#56B4E9", "white", "#FFB715"))
                        ),
    avg_rating = colDef(
      name = "Average Rating",
      cell = function(x) {
        sprintf("%0.2f", x)
      },
      style = color_scales(summed_up_last_months, colors = c("#56B4E9", "white", "#FFB715"))
    )
  )
  
)


# Show distribution of game ratings based on ownership and comment as violin plots
a <-ggplot(all_last_month_stats, aes()) +
  scale_x_discrete("Rating subset",
                   limits = c(
                     "cum_avg_rating_no_comment",
                     "cum_avg_rating_comment",
                     "cum_avg_rating_prev_owned",
                     "cum_avg_rating_not_owned",
                     "cum_avg_rating_owned",
                     "cum_avg_rating"
                     ),
                   labels = c(
                     "cum_avg_rating" = "All ratings",
                     "cum_avg_rating_owned" = "Game owned",
                     "cum_avg_rating_not_owned" = "Game not owned",
                     "cum_avg_rating_prev_owned" = "Game previously owned",
                     "cum_avg_rating_comment" = "Rating with comment",
                     "cum_avg_rating_no_comment" = "Rating without comment"
                   )
  ) +
  scale_y_continuous(
    "Rating",
    breaks = seq(1, 10, 1),
    limits = c(1, 10)
  ) +
  ggtitle("Game rating distribution") +
  
  geom_violin(aes(x = "cum_avg_rating", y = cum_avg_rating), adjust = 0.3, fill = "#56B4E9") +
  stat_boxplot(aes(x = "cum_avg_rating", y = cum_avg_rating),geom = "errorbar", width = 0.2) +
  geom_boxplot(aes(x = "cum_avg_rating", y = cum_avg_rating), varwidth = TRUE, width = 0.15, outlier.shape = NA) +

  geom_violin(aes(x = "cum_avg_rating_owned", y = cum_avg_rating_owned), adjust = 0.3, fill = "#56B4E9") +
  stat_boxplot(aes(x = "cum_avg_rating_owned", y = cum_avg_rating_owned),geom = "errorbar", width = 0.2) +
  geom_boxplot(aes(x = "cum_avg_rating_owned", y = cum_avg_rating_owned), varwidth = TRUE, width = 0.15, outlier.shape = NA) +

  geom_violin(aes(x = "cum_avg_rating_not_owned", y = cum_avg_rating_not_owned), adjust = 0.3, fill = "#56B4E9") +
  stat_boxplot(aes(x = "cum_avg_rating_not_owned", y = cum_avg_rating_not_owned),geom = "errorbar", width = 0.2) +
  geom_boxplot(aes(x = "cum_avg_rating_not_owned", y = cum_avg_rating_not_owned), varwidth = TRUE, width = 0.15, outlier.shape = NA) +
  
  geom_violin(aes(x = "cum_avg_rating_prev_owned", y = cum_avg_rating_prev_owned), adjust = 0.3, fill = "#56B4E9") +
  stat_boxplot(aes(x = "cum_avg_rating_prev_owned", y = cum_avg_rating_prev_owned),geom = "errorbar", width = 0.2) +
  geom_boxplot(aes(x = "cum_avg_rating_prev_owned", y = cum_avg_rating_prev_owned), varwidth = TRUE, width = 0.15, outlier.shape = NA) +
  
  geom_violin(aes(x = "cum_avg_rating_comment", y = cum_avg_rating_comment), adjust = 0.3, fill = "#56B4E9") +
  stat_boxplot(aes(x = "cum_avg_rating_comment", y = cum_avg_rating_comment),geom = "errorbar", width = 0.2) +
  geom_boxplot(aes(x = "cum_avg_rating_comment", y = cum_avg_rating_comment), varwidth = TRUE, width = 0.15, outlier.shape = NA) +
  
  geom_violin(aes(x = "cum_avg_rating_no_comment", y = cum_avg_rating_no_comment), adjust = 0.3, fill = "#56B4E9") +
  stat_boxplot(aes(x = "cum_avg_rating_no_comment", y = cum_avg_rating_no_comment),geom = "errorbar", width = 0.2) +
  geom_boxplot(aes(x = "cum_avg_rating_no_comment", y = cum_avg_rating_no_comment), varwidth = TRUE, width = 0.15, outlier.shape = NA) +
  
  theme_bw() +
  theme(
    panel.border = element_rect(colour = "white"),
    axis.line = element_line(colour = "grey60"),
    plot.title = element_text(size=16, face="bold", vjust = 1, hjust = 0),
    axis.title.x = element_text(size=14, vjust = 0),
    axis.title.y = element_text(size=14, vjust = 1, hjust = 0.6)
  ) +
  coord_flip()

ggplot(all_last_month_stats, aes(x=cum_avg_rating, y=diff_avg_not_owned))  + stat_density_2d(color = "gray", size = 0.4, binwidth = 0.02)
ggplot(all_last_month_stats, aes(x=cum_avg_rating, y=diff_avg_not_owned)) + geom_point()
ggplot(all_last_month_stats, aes(x=cum_avg_rating, y=diff_avg_not_owned)) + geom_hex()
ggplot(all_last_month_stats, aes(x=cum_avg_rating, y=diff_avg_not_owned)) + geom_bin2d()
ggplot(all_last_month_stats, aes(x=cum_avg_rating, y=diff_avg_not_owned)) + geom_density_2d()

ggplot(all_last_month_stats, aes(diff_avg_not_owned)) + geom_histogram(bins = 80)
ggplot(all_last_month_stats, aes(perc_diff_avg_not_owned)) + geom_histogram(bins = 80)

ggplot(all_last_month_stats, aes()) +
  scale_x_discrete("Subset of ratings",
                   labels = c(
                     "diff_avg_not_owned" = "not owned",
                     "diff_avg_owned" = "Game owned"
                   )
  ) +
  geom_violin(aes(x = "diff_avg_not_owned", y = diff_avg_not_owned, fill = "violin"), adjust = 0.3) +
  stat_boxplot(aes(x = "diff_avg_not_owned", y = diff_avg_not_owned),geom = "errorbar", width = 0.1) +  
  geom_boxplot(aes(x = "diff_avg_not_owned", y = diff_avg_not_owned), varwidth = TRUE, width = 0.15, outlier.shape = NA) +
  geom_violin(aes(x = "diff_avg_owned", y = diff_avg_owned, fill = "violin"), adjust = 0.3) +
  stat_boxplot(aes(x = "diff_avg_owned", y = diff_avg_owned),geom = "errorbar", width = 0.1) +  
  geom_boxplot(aes(x = "diff_avg_owned", y = diff_avg_owned), varwidth = TRUE, width = 0.15, outlier.shape = NA) +
  coord_flip()


ggsave(file="test.svg", plot=a)
