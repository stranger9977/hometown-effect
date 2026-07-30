# =============================================================================
# 48_wealth_gradient.R -- Michael's "by wealth" cut of the beyond-sports idea.
# Bin US counties into equal-population income fifths and show how general life
# outcomes rise with local wealth: college degrees, employment, and LIFE
# EXPECTANCY. The gradient is the point: how rich your place is tracks your
# whole life, right down to how long you live.
#
# Honest exception, reported in the caption: home ownership does NOT follow
# wealth (expensive places have more renters), so it is noted, not plotted.
#
# Income + education + employment: Census ACS 5-year 2023, county (keyed).
# Life expectancy: County Health Rankings 2024 (data/raw/chr_2024.csv), which
# carries the CDC/NCHS small-area life-expectancy estimate per county.
# =============================================================================

suppressMessages({
  library(jsonlite); library(curl); library(dplyr); library(tidyr)
  library(ggplot2); library(tibble); library(readr)
})
source("R/lib/theme_hometown.R")

KEY <- Sys.getenv("CENSUS_API_KEY"); stopifnot(nzchar(KEY))
vars <- c("B19013_001E", "B01003_001E",
          "B15003_001E", "B15003_022E", "B15003_023E", "B15003_024E", "B15003_025E",
          "B25003_001E", "B25003_002E",
          "B23025_001E", "B23025_004E")
url <- sprintf("https://api.census.gov/data/2023/acs/acs5?get=%s&for=county:*&in=state:*&key=%s",
               paste(vars, collapse = ","), KEY)
res <- curl_fetch_memory(url); stopifnot(res$status_code == 200)
m <- fromJSON(rawToChar(res$content))
df <- as.data.frame(m[-1, , drop = FALSE], stringsAsFactors = FALSE); names(df) <- m[1, ]
df[vars] <- lapply(df[vars], as.numeric)

le <- read_csv("data/raw/chr_2024.csv", col_types = cols(.default = col_character()))[-1, ] |>
  transmute(fips = `5-digit FIPS Code`, life_exp = as.numeric(`Life Expectancy raw value`)) |>
  filter(!is.na(life_exp))

co <- df |>
  mutate(fips = paste0(state, county)) |>
  filter(!is.na(B19013_001E), B19013_001E > 0, B01003_001E > 0) |>
  left_join(le, by = "fips") |>
  transmute(pop = B01003_001E, income = B19013_001E, life_exp,
            pop25 = B15003_001E,
            baplus = B15003_022E + B15003_023E + B15003_024E + B15003_025E,
            hh = B25003_001E, owner = B25003_002E,
            pop16 = B23025_001E, employed = B23025_004E) |>
  arrange(income) |>
  mutate(fifth = cut(cumsum(pop), breaks = c(0, seq(0.2, 1, 0.2) * sum(pop)),
                     labels = c("Poorest\n20%", "Lower\nmiddle", "Middle", "Upper\nmiddle", "Richest\n20%"),
                     include.lowest = TRUE))

grad <- co |>
  group_by(fifth) |>
  summarise(income = weighted.mean(income, pop),
            ba_rate = 100 * sum(baplus) / sum(pop25),
            emp_rate = 100 * sum(employed) / sum(pop16),
            own_rate = 100 * sum(owner) / sum(hh),
            life_exp = weighted.mean(life_exp, pop, na.rm = TRUE),
            .groups = "drop")
cat("=== outcomes by county income fifth (equal population) ===\n")
print(as.data.frame(grad |> mutate(income = round(income), across(c(ba_rate, emp_rate, own_rate), ~round(.x,1)), life_exp = round(life_exp,1))))
le_gap <- round(grad$life_exp[5] - grad$life_exp[1], 1)
cat(sprintf("life-expectancy gap richest vs poorest fifth: %.1f years\n", le_gap))
cat(sprintf("home ownership across fifths (the flat exception): %s\n", paste0(round(grad$own_rate,0), "%", collapse = " ")))

plot_df <- grad |>
  transmute(fifth,
            `Bachelor's degree or higher (%)` = ba_rate,
            `Working, age 16 and up (%)` = emp_rate,
            `Life expectancy (years)` = life_exp) |>
  pivot_longer(-fifth, names_to = "outcome", values_to = "value") |>
  mutate(outcome = factor(outcome, levels = c("Bachelor's degree or higher (%)",
                                             "Working, age 16 and up (%)", "Life expectancy (years)")),
         lab = ifelse(outcome == "Life expectancy (years)", sprintf("%.1f", value), sprintf("%.0f%%", value)))

p <- ggplot(plot_df, aes(fifth, value, group = 1)) +
  geom_line(colour = pal_sport[["NFL"]], linewidth = 0.9) +
  geom_point(colour = pal_sport[["NFL"]], size = 2.6) +
  geom_text(aes(label = lab), vjust = -1.0, size = 3.2, fontface = "bold", colour = ink_body) +
  facet_wrap(~outcome, scales = "free_y", nrow = 1) +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.2))) +
  labs(
    title = "The wealth gradient: richer places have more degrees, more work, and years more life",
    subtitle = "US counties sorted into equal-population income fifths, from the poorest 20 percent of the country by local income to the richest.",
    x = NULL, y = NULL,
    caption = fig_caption(
      "Census ACS 5-year 2023 (income, education, employment) + County Health Rankings 2024 (CDC small-area life expectancy), county level",
      sprintf("\nEach fifth holds about a fifth of the US population (poorest places average $%dk median income, richest $%dk). Rates pooled, life expectancy population-weighted.",
              round(grad$income[1]/1000), round(grad$income[5]/1000)),
      sprintf("\nThe richest fifth of places lives about %.1f years longer than the poorest. The one outcome that does NOT follow wealth is home ownership (flat near\ntwo-thirds across every fifth, since expensive places have more renters). No sports anywhere here: this is the birthplace story for the whole population.", le_gap))) +
  theme_hometown(grid = "y") +
  theme(strip.text = element_text(hjust = 0.5, size = rel(0.88)),
        panel.spacing = unit(1.5, "lines"),
        axis.text.x = element_text(size = rel(0.72), lineheight = 0.9))
save_fig("docs/figures/ba_wealth_gradient.png", p, w = 12, h = 5.0)
cat("\ndone\n")
