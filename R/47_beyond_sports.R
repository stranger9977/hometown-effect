# =============================================================================
# 47_beyond_sports.R -- Michael's "final piece": take the whole where-you-are-
# born-shapes-your-life idea and point it at GENERAL life outcomes, nothing to
# do with sports. This is the flagship proof of the direction: three outcomes
# (median household income, share with a bachelor's degree or more, and home
# ownership) by US region, straight from the Census.
#
# Data: Census ACS 5-year 2023, state level, aggregated to the four Census
# regions. Rates aggregate cleanly (sum numerators over denominators). Median
# income is a household-weighted average of state medians (labeled as such,
# since medians do not sum). Nick's CENSUS_API_KEY (~/.Renviron) is used; never
# printed.
# =============================================================================

suppressMessages({
  library(jsonlite); library(curl); library(dplyr); library(tidyr)
  library(ggplot2); library(tibble)
})
source("R/lib/theme_hometown.R")

KEY <- Sys.getenv("CENSUS_API_KEY")
stopifnot("CENSUS_API_KEY missing" = nzchar(KEY))

vars <- c("NAME", "B19013_001E",              # median household income
          "B25003_001E", "B25003_002E",       # occupied units, owner-occupied
          "B15003_001E", "B15003_022E", "B15003_023E", "B15003_024E", "B15003_025E") # edu 25+
url <- sprintf("https://api.census.gov/data/2023/acs/acs5?get=%s&for=state:*&key=%s",
               paste(vars, collapse = ","), KEY)
res <- curl_fetch_memory(url)
stopifnot(res$status_code == 200)
m <- fromJSON(rawToChar(res$content))
df <- as.data.frame(m[-1, , drop = FALSE], stringsAsFactors = FALSE)
names(df) <- m[1, ]
num <- setdiff(vars, "NAME")
df[num] <- lapply(df[num], as.numeric)

region_of <- setNames(as.character(state.region), state.name)
region_of["District of Columbia"] <- "South"

st <- df |>
  mutate(region = recode(region_of[NAME], "North Central" = "Midwest")) |>
  filter(!is.na(region)) |>                       # drops Puerto Rico
  transmute(region,
            hh = B25003_001E, owner = B25003_002E,
            pop25 = B15003_001E,
            baplus = B15003_022E + B15003_023E + B15003_024E + B15003_025E,
            med_income = B19013_001E)

region_levels <- c("Northeast", "Midwest", "South", "West")
reg <- st |>
  group_by(region) |>
  summarise(income = weighted.mean(med_income, hh),
            ba_rate = 100 * sum(baplus) / sum(pop25),
            own_rate = 100 * sum(owner) / sum(hh),
            .groups = "drop") |>
  mutate(region = factor(region, levels = region_levels))

cat("=== life outcomes by region (ACS 2023) ===\n")
print(as.data.frame(reg |> mutate(income = round(income), ba_rate = round(ba_rate,1), own_rate = round(own_rate,1))))

plot_df <- reg |>
  transmute(region,
            `Median household income` = income,
            `Bachelor's degree or higher (%)` = ba_rate,
            `Home ownership (%)` = own_rate) |>
  pivot_longer(-region, names_to = "outcome", values_to = "value") |>
  mutate(outcome = factor(outcome, levels = c("Median household income",
                                             "Bachelor's degree or higher (%)",
                                             "Home ownership (%)")),
         lab = ifelse(outcome == "Median household income",
                      sprintf("$%.0fk", value/1000), sprintf("%.0f%%", value)))

p <- ggplot(plot_df, aes(region, value)) +
  geom_col(fill = pal_sport[["NFL"]], width = 0.7) +
  geom_text(aes(label = lab), vjust = -0.5, size = 3.4, fontface = "bold", colour = ink_body) +
  facet_wrap(~outcome, scales = "free_y", nrow = 1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
  labs(
    title = "The same geography that sorts athletes sorts everything else too",
    subtitle = "Median household income, college attainment, and home ownership by US region. Nothing to do with sports.",
    x = NULL, y = NULL,
    caption = fig_caption(
      "US Census ACS 5-year 2023, state level aggregated to the four Census regions",
      "\nRates are pooled (numerators over denominators). Median income is a household-weighted average of state medians, since medians do not sum.",
      "\nThis is the general-population version of the birthplace story: where you are born shapes income, degrees, and whether you own a home, long before any sport.")) +
  theme_hometown(grid = "y") +
  theme(strip.text = element_text(hjust = 0.5, size = rel(0.9)),
        panel.spacing = unit(1.5, "lines"),
        axis.text.x = element_text(size = rel(0.85)))
save_fig("docs/figures/ba_beyond_sports_region.png", p, w = 12, h = 4.8)
cat("\ndone\n")
