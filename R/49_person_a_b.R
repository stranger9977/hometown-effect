# =============================================================================
# 49_person_a_b.R -- Michael's quantified Person A vs Person B example. One
# family covers the three big early-adult bills; the other does not. Put the
# gift into the market instead and it compounds into a fortune.
#
# Michael's exact inputs: college help $50k + down payment $100k + wedding $25k
# = a $175k head start, invested at 8% a year from early adulthood to age 65.
# This is an ILLUSTRATIVE projection, not measured data, and the assumptions are
# stated on its face:
#   - head start invested at age 20, compounded 45 years to 65 at 8% nominal
#   - "today's dollars" discounts the age-65 value back at 2.5% inflation
# Those assumptions reproduce Michael's own figures ($5-6M at 65, ~$2M today).
# =============================================================================

suppressMessages({
  library(dplyr); library(ggplot2); library(tibble)
})
source("R/lib/theme_hometown.R")

head_start <- 50000 + 100000 + 25000   # 175,000
rate <- 0.08; start_age <- 20; end_age <- 65; inflation <- 0.025

curve <- tibble(age = start_age:end_age,
                t = age - start_age,
                value = head_start * (1 + rate)^t)
fv <- curve$value[curve$age == end_age]
today <- fv / (1 + inflation)^(end_age - start_age)
cat(sprintf("head start $%s | age-65 value $%.2fM | today's dollars $%.2fM\n",
            format(head_start, big.mark = ","), fv/1e6, today/1e6))

marks <- curve |> filter(age %in% c(20, 40, 65)) |>
  mutate(lab = sprintf("$%.1fM", value/1e6),
         lab = ifelse(age == 20, sprintf("$%.0fk", value/1e3), lab))

p <- ggplot(curve, aes(age, value/1e6)) +
  geom_area(fill = pal_sport[["NFL"]], alpha = 0.12) +
  geom_line(colour = pal_sport[["NFL"]], linewidth = 1.1) +
  geom_point(data = marks, aes(age, value/1e6), colour = pal_sport[["NFL"]], size = 2.8) +
  geom_text(data = marks, aes(age, value/1e6, label = lab), vjust = -0.9, hjust = c(0, 0.5, 1),
            size = 3.7, fontface = "bold", colour = ink_body) +
  annotate("text", x = 21, y = 4.55, hjust = 0, lineheight = 0.98, size = 3.5, colour = ink_body,
           label = "Person A's family covers three bills:\ncollege help $50k, a down payment $100k,\nand a wedding $25k. A $175k head start.") +
  annotate("text", x = 21, y = 3.5, hjust = 0, lineheight = 0.98, size = 3.5, colour = "#1c6fa8",
           fontface = "bold",
           label = "Person B pays their own way,\nand starts adult life at zero or in debt.") +
  annotate("segment", x = 63.2, xend = 64.8, y = 5.0, yend = 5.45,
           colour = "grey55", linewidth = 0.4) +
  annotate("text", x = 61, y = 4.75, hjust = 1, size = 3.3, colour = ink_subtitle, lineheight = 0.98,
           label = "about $2.0M in\ntoday's dollars") +
  scale_x_continuous(breaks = seq(20, 65, 5)) +
  scale_y_continuous(labels = function(x) paste0("$", x, "M"),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "The same three gifts, compounded: a $175,000 head start becomes $5.6 million by 65",
    subtitle = "Person A's family pays for college, a down payment, and a wedding. Person B does not. Invest the difference at 8 percent and this is the gap it opens.",
    x = "Age", y = NULL,
    caption = fig_caption(
      "Illustration, not measured data. Michael's inputs: $50k + $100k + $25k = $175k, invested at 8 percent a year",
      "\nCompounded from age 20 to 65 (45 years). Age-65 value is $5.6M; in today's dollars, discounting at 2.5 percent inflation, about $2.0M.",
      "\nThis is the income thread made literal: the head start is not talent or effort, it is a balance sheet that quietly turns three gifts into a fortune.")) +
  theme_hometown(grid = "y")
save_fig("docs/figures/ba_person_a_b.png", p, w = 12, h = 6.4)
cat("\ndone\n")
