# =============================================================================
# 43_kid_a_b.R -- Michael's "Kid A vs Kid B" device: the first three big bills
# of adult life (college, a first home, a wedding) and what family money does to
# them. This is an ILLUSTRATION of the mechanism, not measured data, and it says
# so on its face. It makes the income thread concrete: the same three costs, two
# very different starts, which is how a hometown-income edge turns into a life.
# =============================================================================

suppressMessages({
  library(ggplot2); library(tibble); library(dplyr)
})
source("R/lib/theme_hometown.R")

rows <- tibble(
  expense = c("Paying for college", "A first home", "A wedding"),
  y = c(3, 2, 1),
  kidA = c("Family covers it.\nStarts with no debt.",
           "Family helps the down payment.\nStarts building equity early.",
           "Family pays.\nNo debt added."),
  kidB = c("Loans.\nStarts adult life owing.",
           "Saves for years first,\nor keeps renting.",
           "Pays out of pocket,\nor goes into debt."))

long <- bind_rows(
  rows |> transmute(y, kid = "Kid A", x = 1, text = kidA, tone = "ahead"),
  rows |> transmute(y, kid = "Kid B", x = 2, text = kidB, tone = "behind"))

labels_left <- rows |> transmute(y, expense)

tone_fill <- c(ahead = "#E4F1E8", behind = "#F3E7E1")
tone_line <- c(ahead = "#2E8B57", behind = "#C1663B")

p <- ggplot(long, aes(x, y)) +
  geom_tile(aes(fill = tone), colour = "white", width = 0.94, height = 0.82) +
  geom_text(aes(label = text, colour = tone), size = 3.5, lineheight = 0.95,
            fontface = "plain") +
  # expense labels down the left
  geom_text(data = labels_left, aes(x = 0.3, y = y, label = expense),
            hjust = 1, size = 3.7, fontface = "bold", colour = ink_title,
            inherit.aes = FALSE) +
  # column headers
  annotate("text", x = 1, y = 3.7, label = "KID A", fontface = "bold",
           size = 4.6, colour = tone_line[["ahead"]]) +
  annotate("text", x = 2, y = 3.7, label = "KID B", fontface = "bold",
           size = 4.6, colour = tone_line[["behind"]]) +
  annotate("text", x = 1, y = 3.45, label = "family can help", size = 3,
           colour = ink_body) +
  annotate("text", x = 2, y = 3.45, label = "family cannot", size = 3,
           colour = ink_body) +
  # bottom line
  annotate("text", x = 1, y = 0.35, label = "Enters their 30s with savings\nand a foothold",
           size = 3.3, colour = tone_line[["ahead"]], fontface = "bold", lineheight = 0.95) +
  annotate("text", x = 2, y = 0.35, label = "Enters their 30s paying off\nthe same three things",
           size = 3.3, colour = tone_line[["behind"]], fontface = "bold", lineheight = 0.95) +
  scale_fill_manual(values = tone_fill, guide = "none") +
  scale_colour_manual(values = tone_line, guide = "none") +
  scale_x_continuous(limits = c(-0.75, 2.6)) +
  scale_y_continuous(limits = c(0, 4.0)) +
  labs(
    title = "Kid A vs Kid B: the same three bills, two very different starts",
    subtitle = "The first big costs of adult life, and what a family's help does to them. An illustration of the mechanism, not measured data.",
    caption = paste0(
      "Illustration, not data. It makes the income thread concrete: the family-income edge behind who reaches the pros is the same edge that pays\n",
      "for college, a down payment, and a wedding. The head start is not talent, it is a balance sheet. See the hometown-income work on the main site.")) +
  theme_hometown(grid = "none") +
  theme(axis.text = element_blank(), axis.title = element_blank(),
        plot.caption = element_text(colour = ink_caption, size = rel(0.7), hjust = 0,
                                    lineheight = 1.25, margin = margin(t = 10)))
save_fig("docs/figures/ba_kid_a_b.png", p, w = 11, h = 6.2)
cat("done\n")
