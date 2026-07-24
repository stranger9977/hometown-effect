suppressMessages({ library(dplyr); library(ggplot2) })
source("R/lib/theme_hometown.R")

# Published data, Aune et al. (2018), Frontiers in Psychology 9:1091,
# "Relative Age Effects and Gender Differences in the National Test of Numeracy",
# Norway, n = 175,760 (grades 5, 8, 9 combined). Table 2, high-score quartile
# (top 25%): share of each birth-quarter group scoring in the top quartile.
d <- tibble::tribble(
  ~sex,    ~grp,                       ~pct_top,
  "Boys",  "Born Jan-Mar (oldest)",    33.5,
  "Boys",  "Born Oct-Dec (youngest)",  25.8,
  "Girls", "Born Jan-Mar (oldest)",    25.5,
  "Girls", "Born Oct-Dec (youngest)",  18.0
) |>
  mutate(grp = factor(grp, levels = c("Born Jan-Mar (oldest)", "Born Oct-Dec (youngest)")),
         sex = factor(sex, levels = c("Boys", "Girls")))

grp_col <- c("Born Jan-Mar (oldest)" = "#2B8CBE", "Born Oct-Dec (youngest)" = "#A6BDDB")

p <- ggplot(d, aes(sex, pct_top, fill = grp)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.62) +
  geom_text(aes(label = paste0(pct_top, "%")),
            position = position_dodge(width = 0.72), vjust = -0.5,
            size = 4.6, fontface = "bold", color = "grey25") +
  # in-panel key instead of a legend box, placed over the empty space above the Girls bars
  annotate("text", x = 1.62, y = 38.5, hjust = 0, size = 4.3, fontface = "bold",
           color = "#2B8CBE", label = "Oldest in grade") +
  annotate("text", x = 1.62, y = 35.3, hjust = 0, size = 4.3, fontface = "bold",
           color = "#8fb4d0", label = "Youngest in grade") +
  scale_fill_manual(values = grp_col, guide = "none") +
  scale_y_continuous(limits = c(0, 40), expand = expansion(mult = c(0, 0.02))) +
  labs(
    title = "In the classroom too, the oldest kids are overrepresented among top scorers",
    subtitle = "Share of students scoring in the top quarter of a national numeracy test, oldest vs youngest in the grade, by sex",
    x = NULL, y = "Share in the top quarter of scores",
    caption = paste0(
      "From published research, not this project's data: Aune et al. (2018), Frontiers in Psychology, Norwegian national numeracy test,\n",
      "175,760 students in grades 5, 8, and 9. The oldest-in-grade edge is about 7 to 8 points for both sexes; boys score higher overall on this test.\n",
      "The gender gap the wider literature keeps finding is longer-run, not here: young-for-grade boys fare somewhat worse on finishing school and adult earnings."
    )
  ) +
  theme_hometown(grid = "y")

save_fig("docs/figures/ba_classroom_sex.png", p, w = 11, h = 6.4)
cat("wrote ba_classroom_sex.png\n")
