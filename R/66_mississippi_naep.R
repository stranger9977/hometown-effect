# =============================================================================
# 66_mississippi_naep.R -- Michael's "Mississippi miracle" thread, double-checked.
#
# THE ASK. Michael sent a clip claiming that among LOW-INCOME 4th graders,
# Mississippi is 24 percent proficient in reading versus Oregon's 11 percent
# (2024 NAEP), and asked us to verify it. His wider thread: after adjusting for
# demographics Mississippi ranks 1st in 4th-grade reading and math; the state
# reformed reading a little over a decade ago; New York spends the most per
# pupil while Mississippi spends near the least.
#
# WHAT WE FIND. His clip checks out to the decimal. In 2024, 24.4 percent of
# Mississippi's economically disadvantaged 4th graders scored at or above NAEP
# Proficient in reading; Oregon's figure was 11.2 percent -- the lowest of any
# state. Mississippi ranks 3rd among the states on that measure. The trend and
# spending claims hold too (details in each figure's caption below).
#
# SOURCES (all verified from primary data, not typed from memory):
#   * NAEP, National Assessment of Educational Progress (NCES), 2024 and trend.
#     Pulled from the NAEP Data Service API, GetAdhocData endpoint:
#       subject=reading, grade=4, subscale=RRPCM (reading composite),
#       stattype=ALC:AP (cumulative achievement level, at or above Proficient),
#       variable=ECONDIS (economically disadvantaged status, the measure that
#         replaced National School Lunch Program eligibility beginning in 2024),
#       variable=TOTAL for the trend, Year=2024 (and 1998-2024 for the trend).
#     Cross-checked against the NAEP 2024 State Snapshot Reports for MS and OR
#     (grade 4 reading), which print 24 and 11 for the same group.
#     Saved API responses live in data/naep/*.json.
#   * Per-pupil spending: U.S. Census Bureau, Annual Survey of School System
#     Finances (F-33), Public Elementary-Secondary Education Finance, Fiscal
#     Year 2024, Summary Table 8 "Per Pupil Amounts for Current Spending."
#     Saved as data/naep/perpupil_2024.csv (parsed from elsec24_sumtables.xlsx).
#   * Demographically adjusted rank (cited, not charted): Urban Institute,
#     "States' Demographically Adjusted Performance on the 2024 NAEP" -- MS
#     ranks 1st in adjusted grade-4 reading and math, 1st in grade-8 math.
#
# NOTE ON LANGUAGE. NAEP's own guidance is that "Proficient" is a demanding
# standard, not the same thing as reading "at grade level"; NAEP Basic is the
# nearer marker for grade level. We report the group as "proficient readers"
# throughout and flag this in the captions.
# =============================================================================

suppressMessages({
  library(dplyr); library(ggplot2); library(jsonlite); library(readr); library(tidyr)
})
source("R/lib/theme_hometown.R")

blue   <- pal_sport[["NFL"]]   # Mississippi, the reform state
orange <- pal_sport[["MLB"]]   # Oregon, the clip's comparison
grey_bar <- "grey78"

state_abbr <- c(AL="Alabama",AK="Alaska",AZ="Arizona",AR="Arkansas",CA="California",
  CO="Colorado",CT="Connecticut",DE="Delaware",FL="Florida",GA="Georgia",HI="Hawaii",
  ID="Idaho",IL="Illinois",IN="Indiana",IA="Iowa",KS="Kansas",KY="Kentucky",LA="Louisiana",
  ME="Maine",MD="Maryland",MA="Massachusetts",MI="Michigan",MN="Minnesota",MS="Mississippi",
  MO="Missouri",MT="Montana",NE="Nebraska",NV="Nevada",NH="New Hampshire",NJ="New Jersey",
  NM="New Mexico",NY="New York",NC="North Carolina",ND="North Dakota",OH="Ohio",OK="Oklahoma",
  OR="Oregon",PA="Pennsylvania",RI="Rhode Island",SC="South Carolina",SD="South Dakota",
  TN="Tennessee",TX="Texas",UT="Utah",VT="Vermont",VA="Virginia",WA="Washington",
  WV="West Virginia",WI="Wisconsin",WY="Wyoming",DC="District of Columbia")

# =============================================================================
# FIGURE 1 -- low-income 4th-grade reading proficiency by state, 2024.
# =============================================================================
ed <- fromJSON("data/naep/econdis_g4read_2024.json")$result
ed <- ed |>
  filter(varValue == "1", value < 900) |>          # varValue 1 = economically disadvantaged
  transmute(abbr = jurisdiction, pct = value)

natl <- ed$pct[ed$abbr == "NT"]                     # national public reference

f1 <- ed |>
  filter(abbr %in% names(state_abbr), abbr != "DC") |>   # 50 states, DC noted in caption
  mutate(state = state_abbr[abbr],
         hl = case_when(abbr == "MS" ~ "MS", abbr == "OR" ~ "OR", TRUE ~ "other"),
         state = factor(state, levels = state[order(pct)]))

ms_v  <- round(f1$pct[f1$abbr == "MS"], 1)
or_v  <- round(f1$pct[f1$abbr == "OR"], 1)
ms_rk <- sum(f1$pct > f1$pct[f1$abbr == "MS"]) + 1
or_rk <- nrow(f1) - sum(f1$pct > f1$pct[f1$abbr == "OR"])   # rank from the bottom check

cat(sprintf("FIG1: MS=%.1f%% (rank %d of %d)  OR=%.1f%% (lowest of the 50)  national=%.1f%%\n",
            ms_v, ms_rk, nrow(f1), or_v, round(natl,1)))

lab1 <- f1 |> filter(abbr %in% c("MS","OR"))

p1 <- ggplot(f1, aes(pct, state, fill = hl)) +
  geom_col(width = 0.72) +
  geom_vline(xintercept = natl, linetype = "dashed", colour = ink_baseline, linewidth = 0.4) +
  annotate("text", x = natl, y = 2.2, label = sprintf("National: %.0f%%", natl),
           hjust = -0.06, size = 3.1, colour = ink_body) +
  geom_text(data = lab1, aes(label = sprintf("%.0f%%", pct), colour = hl),
            hjust = -0.25, size = 3.6, fontface = "bold") +
  scale_fill_manual(values = c(MS = blue, OR = orange, other = grey_bar)) +
  scale_colour_manual(values = c(MS = blue, OR = orange)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.10)),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title = "Among low-income 4th graders, Mississippi is more than twice\nas likely as Oregon to read at a proficient level",
    subtitle = "Share of economically disadvantaged 4th graders scoring at or above NAEP Proficient in reading, 2024, by state.\nMichael's clip said 24 vs 11; the data say 24 vs 11.",
    x = "Percent proficient in reading, economically disadvantaged 4th graders", y = NULL,
    caption = fig_caption(
      "NAEP 2024 (NCES), reading grade 4, variable ECONDIS (economically disadvantaged), stattype ALC:AP (at or above Proficient);\ncross-checked to the MS and OR State Snapshot Reports",
      "\nEconomically disadvantaged 4th graders only. In 2024 this measure replaced National School Lunch Program eligibility, so it is not identical to the pre-2024 low-income group.",
      sprintf("\nMississippi %.1f%% ranks 3rd of 50 states (Nevada and Louisiana are a shade higher); Oregon %.1f%% is the lowest of the 50. NAEP Proficient is a high bar, above grade level.\nDistrict of Columbia (a city district) is left out of the 50-state ranking; its figure was the lowest overall. National reference is the public-school average.", ms_v, or_v))) +
  theme_hometown(grid = "none") +
  theme(axis.text.y = element_text(size = rel(0.72)),
        plot.margin = margin(10, 30, 8, 10))
save_fig("docs/figures/ba_mississippi_lowincome.png", p1, w = 10.5, h = 10.5)

# =============================================================================
# FIGURE 2 -- Mississippi vs national 4th-grade reading over time, 1998-2024.
# =============================================================================
tr <- fromJSON("data/naep/trend_g4read_prof.json")$result |>
  filter(value < 900) |>
  transmute(year, who = ifelse(jurisdiction == "MS", "Mississippi", "National (public)"),
            pct = value)

end2 <- tr |> group_by(who) |> filter(year == max(year)) |> ungroup()
ms02 <- tr$pct[tr$who == "Mississippi" & tr$year == 2002]
ms24 <- tr$pct[tr$who == "Mississippi" & tr$year == 2024]
nt24 <- tr$pct[tr$who == "National (public)" & tr$year == 2024]
cat(sprintf("FIG2: MS 2002=%.1f%% -> 2024=%.1f%%; national 2024=%.1f%% (MS now above national)\n",
            ms02, ms24, nt24))

p2 <- ggplot(tr, aes(year, pct, colour = who)) +
  geom_vline(xintercept = 2013, linetype = "dotted", colour = ink_baseline, linewidth = 0.4) +
  annotate("text", x = 2013, y = 12.5, label = "2013: Literacy-Based\nPromotion Act",
           hjust = -0.05, vjust = 0, size = 3.0, colour = ink_body, lineheight = 0.95) +
  geom_line(linewidth = 1.05) +
  geom_point(size = 2.1) +
  geom_text(data = end2, aes(label = who), hjust = 0, nudge_x = 0.6,
            size = 3.5, fontface = "bold") +
  geom_text(data = end2, aes(label = sprintf("%.0f%%", pct)), hjust = 0, nudge_x = 0.6,
            nudge_y = 2.1, size = 3.4, fontface = "bold", show.legend = FALSE) +
  scale_colour_manual(values = c("Mississippi" = blue, "National (public)" = ink_baseline)) +
  scale_x_continuous(breaks = c(1998, 2002, 2005, 2009, 2013, 2017, 2019, 2022, 2024),
                     limits = c(1998, 2030), expand = expansion(mult = c(0.02, 0))) +
  scale_y_continuous(limits = c(10, 40), breaks = seq(10, 40, 10),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title = "After its 2013 reading reform, Mississippi climbed from half the national rate to above it",
    subtitle = "Share of all public-school 4th graders scoring at or above NAEP Proficient in reading, Mississippi vs the national average.",
    x = NULL, y = "Percent of 4th graders proficient in reading",
    caption = fig_caption(
      "NAEP main reading assessment (NCES), grade 4, variable TOTAL, stattype ALC:AP (at or above Proficient); national figure is the public-school average",
      "\nMississippi went from 16 percent proficient in 2002 (national 31) to 32 percent in 2024 (national 31) as the national average slipped after 2019.",
      "\nMississippi's 2013 law pairs early-grade literacy coaching with a 3rd-grade reading gate; because some weak readers repeat 3rd grade, part of the\n4th-grade gain reflects who sits the test, a caveat researchers have flagged. NAEP Proficient is a high bar, above grade level.")) +
  theme_hometown(grid = "y") +
  theme(legend.position = "none", plot.margin = margin(10, 30, 8, 10))
save_fig("docs/figures/ba_mississippi_trend.png", p2, w = 11, h = 6.8)

# =============================================================================
# FIGURE 3 -- per-pupil spending vs low-income reading outcome, 2024.
# =============================================================================
sc <- read_csv("data/naep/scatter_2024.csv", show_col_types = FALSE)  # 50 states, DC excluded
rr <- cor(sc$ppe, sc$lowinc)
ny_sp <- sc$ppe[sc$abbr == "NY"]; ms_sp <- sc$ppe[sc$abbr == "MS"]
cat(sprintf("FIG3: cor(spend, low-income proficiency) = %+.2f; NY spends $%s (most), MS $%s\n",
            rr, format(ny_sp, big.mark=","), format(ms_sp, big.mark=",")))

hl3 <- c("MS","OR","NY","LA","AL")
sc <- sc |> mutate(grp = ifelse(abbr %in% hl3, abbr, "other"))
lab3 <- sc |> filter(abbr %in% hl3) |>
  mutate(nm = recode(abbr, MS="Mississippi", OR="Oregon", NY="New York",
                     LA="Louisiana", AL="Alabama"))

p3 <- ggplot(sc, aes(ppe, lowinc)) +
  geom_smooth(method = "lm", se = FALSE, colour = ink_grid, linewidth = 0.8) +
  geom_point(data = filter(sc, grp == "other"), colour = grey_bar, size = 2.6) +
  geom_point(data = filter(sc, grp != "other"), aes(colour = grp), size = 3.4) +
  geom_text(data = lab3, aes(label = nm, colour = grp),
            hjust = c(MS=1.15, OR=-0.15, NY=1.15, LA=-0.15, AL=1.15)[lab3$abbr],
            vjust = c(MS=0.4, OR=0.4, NY=0.4, LA=0.4, AL=1.5)[lab3$abbr],
            size = 3.5, fontface = "bold", show.legend = FALSE) +
  scale_colour_manual(values = c(MS = blue, OR = orange, NY = ink_body,
                                 LA = "#5B9BD5", AL = "#5B9BD5")) +
  scale_x_continuous(labels = scales::dollar_format(),
                     expand = expansion(mult = c(0.06, 0.08))) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.08, 0.10))) +
  labs(
    title = "Spending more per pupil buys no edge in low-income reading:\nMississippi spends near the least and scores near the top",
    subtitle = "Each dot is a state. Horizontal: current spending per pupil (2024).\nVertical: share of economically disadvantaged 4th graders proficient in reading (2024).",
    x = "Current spending per pupil, 2024", y = "Low-income 4th graders proficient in reading",
    caption = fig_caption(
      "Spending: U.S. Census Bureau, Annual Survey of School System Finances (F-33), FY2024, Summary Table 8 (current spending per pupil).\nReading: NAEP 2024 (NCES), ECONDIS, ALC:AP",
      sprintf("\nAcross the 50 states the two are essentially unrelated (correlation %+.2f). New York spends the most ($%s per pupil); Mississippi spends $%s, among the lowest.",
              rr, format(ny_sp, big.mark=","), format(ms_sp, big.mark=",")),
      "\nDistrict of Columbia excluded as a non-state outlier. Louisiana and Alabama, two states that later copied Mississippi's reading law, are marked in light blue.")) +
  theme_hometown(grid = "y") +
  theme(plot.margin = margin(10, 16, 8, 10))
save_fig("docs/figures/ba_mississippi_spending.png", p3, w = 11, h = 7)

cat("\ndone\n")
