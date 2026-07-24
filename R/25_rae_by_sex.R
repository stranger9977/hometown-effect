suppressMessages({ library(dplyr); library(readr); library(lubridate); library(ggplot2); library(tidyr) })
source("R/lib/theme_hometown.R")

dim <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
exp_q <- c(sum(dim[1:3]), sum(dim[4:6]), sum(dim[7:9]), sum(dim[10:12])) / sum(dim)

load_sport <- function(f, label) {
  read_csv(f, show_col_types = FALSE) |>
    mutate(sex = case_when(grepl("Q6581097", sex) ~ "Men",
                           grepl("Q6581072", sex) ~ "Women", TRUE ~ NA_character_),
           mo = month(as.Date(substr(dob, 1, 10))),
           q = ceiling(mo / 3)) |>
    filter(!is.na(q), !is.na(sex)) |>
    count(sport = label, sex, q) |>
    group_by(sport, sex) |>
    mutate(share = n / sum(n), n_sex = sum(n)) |>
    ungroup() |>
    mutate(ratio = share / exp_q[q])
}

d <- bind_rows(
  load_sport("data/raw/wikidata/soccer_sex.csv", "Soccer"),
  load_sport("data/raw/wikidata/swim_sex.csv", "Swimming")
)

qlab <- c("Q1\nJan-Mar", "Q2\nApr-Jun", "Q3\nJul-Sep", "Q4\nOct-Dec")
d <- d |> mutate(qf = factor(qlab[q], levels = qlab))

# sex n for the subtitle
ns <- d |> distinct(sport, sex, n_sex) |> arrange(sport, sex)

ends <- d |> filter(q == 4)

sex_col <- c(Women = "#CC6677", Men = "#4477AA")

p <- ggplot(d, aes(qf, ratio, color = sex, group = sex)) +
  geom_baseline(1) +
  geom_line(linewidth = 1.4) +
  geom_point(size = 2.8) +
  geom_text(data = ends, aes(label = sex), hjust = -0.15, vjust = 0.4,
            size = 4.4, fontface = "bold", show.legend = FALSE) +
  facet_wrap(~sport) +
  scale_color_manual(values = sex_col) +
  scale_x_discrete(expand = expansion(add = c(0.4, 1.1))) +
  scale_y_continuous(limits = c(0.6, 1.35)) +
  labs(
    title = "In sports, the birthday edge is the same for women and men",
    subtitle = "How over- or under-represented each birth quarter is, US soccer players and swimmers, split by sex",
    x = NULL, y = "Representation ratio (1 = proportional)",
    caption = paste0(
      "Data: Wikidata (query.wikidata.org), US soccer players and swimmers with a recorded birth date and sex. Our-computed data.\n",
      "Soccer: ", ns$n_sex[ns$sport == "Soccer" & ns$sex == "Women"], " women, ",
      ns$n_sex[ns$sport == "Soccer" & ns$sex == "Men"], " men. Swimming: ",
      ns$n_sex[ns$sport == "Swimming" & ns$sex == "Women"], " women, ",
      ns$n_sex[ns$sport == "Swimming" & ns$sex == "Men"], " men.\n",
      "Q1 is January to March, Q4 is October to December. Baseline: days-in-month adjusted uniform (Feb = 28.25)."
    )
  ) +
  theme_hometown() +
  theme(legend.position = "none")

save_fig("docs/figures/ba_rae_by_sex.png", p, w = 12, h = 6.2)
cat("wrote ba_rae_by_sex.png\n")
print(d |> select(sport, sex, q, ratio) |> pivot_wider(names_from = q, values_from = ratio) |> as.data.frame(), digits = 3)
