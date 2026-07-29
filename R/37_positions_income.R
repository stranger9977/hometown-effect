# =============================================================================
# 37_positions_income.R -- which NFL positions come from the richest
# hometowns? Median hometown household income by position group.
# =============================================================================

suppressMessages({
  library(dplyr); library(arrow); library(ggplot2); library(stringr)
})
source("R/lib/theme_hometown.R")

df <- read_parquet("data/processed/hometown.parquet")

cat("=== raw position codes ===\n")
print(sort(unique(df$position)))

# --- map raw codes into standard position groups -----------------------------
group_of <- c(
  QB = "QB",
  RB = "RB", HB = "RB", FB = "RB",
  WR = "WR",
  TE = "TE",
  T = "OL", OT = "OL", G = "OL", OG = "OL", C = "OL", OL = "OL",
  DE = "DL", DT = "DL", NT = "DL", DL = "DL", EDGE = "DL",
  LB = "LB", ILB = "LB", OLB = "LB", MLB = "LB",
  CB = "DB", S = "DB", FS = "DB", SS = "DB", DB = "DB", SAF = "DB",
  K = "ST", P = "ST", LS = "ST"
)

pos_tbl <- df |>
  filter(!is.na(position), !is.na(matched_income_now)) |>
  mutate(pos_group = unname(group_of[position]))

stopifnot(!any(is.na(pos_tbl$pos_group)))   # every raw code must map somewhere

overall_median <- median(pos_tbl$matched_income_now)

group_tbl <- pos_tbl |>
  group_by(pos_group) |>
  summarise(median_income = median(matched_income_now), n = n(), .groups = "drop") |>
  arrange(desc(median_income)) |>
  mutate(pos_group = factor(pos_group, levels = rev(pos_group)),
         is_qb = as.character(pos_group) == "QB",
         label = sprintf("$%.0fk", median_income / 1000))

cat("\n=== median hometown household income by position group ===\n")
print(group_tbl |>
        mutate(median_income = round(median_income)) |>
        select(pos_group, median_income, n) |>
        as.data.frame())
cat(sprintf("\noverall median (all positions): $%.0f\n", overall_median))

# --- chart --------------------------------------------------------------
p <- ggplot(group_tbl, aes(pos_group, median_income, fill = is_qb)) +
  geom_hline(yintercept = overall_median, linetype = "dashed",
             colour = ink_baseline, linewidth = 0.4) +
  geom_col(width = 0.7) +
  geom_text(aes(label = label), hjust = -0.15, size = 3.6,
            fontface = "bold", colour = ink_body) +
  annotate("text", x = nrow(group_tbl) + 0.55, y = overall_median, vjust = 0, hjust = 0,
           label = sprintf("all positions median $%.0fk", overall_median / 1000),
           size = 3.1, colour = ink_body) +
  scale_fill_manual(values = c(`TRUE` = pal_sport[["NFL"]], `FALSE` = "grey70")) +
  scale_y_continuous(labels = scales::label_dollar(scale = 1e-3, suffix = "k"),
                     expand = expansion(mult = c(0, 0.14))) +
  scale_x_discrete(expand = expansion(add = c(0.6, 1.3))) +
  coord_flip() +
  labs(
    title = "NFL kickers and punters come from richer hometowns than quarterbacks do",
    subtitle = "Median hometown household income by position group",
    x = NULL, y = "Median hometown household income",
    caption = fig_caption(
      "nflverse + ESPN + Sleeper rosters matched to Census/ACS place income",
      sprintf("\nHometown household income at high school location where known, else birthplace; n=%d players with a matched hometown.",
              sum(group_tbl$n)),
      "\nDashed line is the median across all positions combined.")) +
  theme_hometown(grid = "none")

save_fig("docs/figures/ba_positions_qb.png", p, w = 11, h = 6.75)

cat("\ndone\n")
