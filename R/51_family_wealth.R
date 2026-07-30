# =============================================================================
# 51_family_wealth.R -- Michael's "family wealth (net worth)" outcome, handled
# as honestly as the map allows. True household NET WORTH does not exist at the
# county or zip level (the Fed's Survey of Consumer Finances is national only).
# The one large, place-measurable slice of family wealth is HOME VALUE: home
# equity is roughly two-thirds of the typical US family's net worth. So we chart
# median home value by county income fifth, clearly labeled as the wealth proxy
# it is, not true net worth. Census ACS 5-year 2023, county level (keyed).
# =============================================================================

suppressMessages({
  library(jsonlite); library(curl); library(dplyr); library(ggplot2); library(tibble)
})
source("R/lib/theme_hometown.R")

KEY <- Sys.getenv("CENSUS_API_KEY"); stopifnot(nzchar(KEY))
vars <- c("B19013_001E", "B01003_001E", "B25077_001E", "B25003_002E")
url <- sprintf("https://api.census.gov/data/2023/acs/acs5?get=%s&for=county:*&in=state:*&key=%s",
               paste(vars, collapse = ","), KEY)
res <- curl_fetch_memory(url); stopifnot(res$status_code == 200)
m <- fromJSON(rawToChar(res$content))
df <- as.data.frame(m[-1, , drop = FALSE], stringsAsFactors = FALSE); names(df) <- m[1, ]
df[vars] <- lapply(df[vars], as.numeric)

co <- df |>
  filter(!is.na(B19013_001E), B19013_001E > 0, B01003_001E > 0,
         !is.na(B25077_001E), B25077_001E > 0, B25003_002E > 0) |>
  transmute(pop = B01003_001E, income = B19013_001E,
            home_value = B25077_001E, owners = B25003_002E) |>
  arrange(income) |>
  mutate(fifth = cut(cumsum(pop), breaks = c(0, seq(0.2, 1, 0.2) * sum(pop)),
                     labels = c("Poorest\n20%", "Lower\nmiddle", "Middle", "Upper\nmiddle", "Richest\n20%"),
                     include.lowest = TRUE))

grad <- co |>
  group_by(fifth) |>
  summarise(home_value = weighted.mean(home_value, owners), .groups = "drop")
ratio <- round(grad$home_value[5] / grad$home_value[1], 1)
cat("=== median home value (wealth proxy) by county income fifth ===\n")
print(as.data.frame(grad |> mutate(home_value = round(home_value))))
cat(sprintf("richest fifth homes are worth %.1fx the poorest fifth's\n", ratio))

p <- ggplot(grad, aes(fifth, home_value/1000)) +
  geom_col(fill = pal_sport[["NFL"]], width = 0.7) +
  geom_text(aes(label = sprintf("$%.0fk", home_value/1000)), vjust = -0.5, size = 3.6,
            fontface = "bold", colour = ink_body) +
  scale_y_continuous(labels = function(x) paste0("$", x, "k"),
                     expand = expansion(mult = c(0, 0.14))) +
  labs(
    title = "Family wealth by place: a home in the richest fifth is worth 3.5 times one in the poorest",
    subtitle = "Median home value by county income fifth. Home equity is most of a typical family's wealth, and the only piece measurable by place.",
    x = NULL, y = NULL,
    caption = fig_caption(
      "US Census ACS 5-year 2023, median value of owner-occupied homes, county level, binned into equal-population income fifths",
      sprintf("\nA proxy, not true net worth. Household net worth does not exist at the county or zip level (the Fed's wealth survey is national only), so we use home value,\nabout two-thirds of the typical family's wealth. Homes in the richest fifth of places are worth %.1f times those in the poorest.", ratio),
      "\nSame story as the rest of the wealth gradient: where you are born stocks the family balance sheet, long before any sport.")) +
  theme_hometown(grid = "y") +
  theme(axis.text.x = element_text(size = rel(0.85), lineheight = 0.9))
save_fig("docs/figures/ba_family_wealth.png", p, w = 11, h = 5.4)
cat("\ndone\n")
