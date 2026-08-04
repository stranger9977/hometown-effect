# ==============================================================================
# Extend Chetty et al. (2017) "Fading American Dream" absolute-mobility series
# past the 1984 birth cohort, using IPUMS CPS ASEC marginals + Chetty's released
# copula and method. Validate on cohorts 1980-1983 (overlap with published
# series), then compute new cohorts 1985-1994.
#   Method: absmob = frac0 + (1-frac0)*mean_i sum_k copula[k,i]*1(kid_min[k] >= par[i])
#   Income = (person + spouse) INCTOT minus INCWELFR, real 2014$ (CPI-U-RS,
#   spliced to CPI-U after 2014). Parent = couple w/ newborn, richer parent 25-35.
#   Kid = age-30 couple income, immigrants (BPL>=15000) dropped.
# Data: IPUMS CPS (Flood et al. 2025, v13.0). Copula: Chetty et al. 2017 Dataverse.
# ==============================================================================
suppressMessages({library(dplyr); library(readr); library(tidyr)})
setwd("/private/tmp/claude-501/-Users-nick/894d474a-0a68-42e6-877b-804c54b296b4/scratchpad/fad")

d <- readRDS("cps.rds")
d <- d[!is.na(d$ASECWT) & d$ASECWT > 0, ]
d$INCTOT[d$INCTOT >= 99999999] <- NA
d$INCWELFR[is.na(d$INCWELFR) | d$INCWELFR >= 9999999] <- 0
d$inc <- d$INCTOT - d$INCWELFR              # incmix component (person)

# --- deflator: CPI-U-RS to 2014 for y<=2014; splice CPI-U after ---------------
cpirs <- read_tsv("cpirs.tab", show_col_types = FALSE)
cpiu <- c(`2014`=236.736,`2015`=237.017,`2016`=240.007,`2017`=245.120,`2018`=251.107,
          `2019`=255.657,`2020`=258.811,`2021`=270.970,`2022`=292.655,`2023`=304.702,`2024`=313.689)
rs2014 <- cpirs$rate[cpirs$year==2014]      # 347.8
rs <- function(y) ifelse(y <= 2014, cpirs$rate[match(y, cpirs$year)],
                          rs2014 * cpiu[as.character(y)] / cpiu["2014"])
to2014 <- function(nom, iyear) nom * rs2014 / rs(iyear)

# --- copula (rows kid_bin, cols parent pctile) --------------------------------
cop <- read_tsv("copula_base_adjusted.tab", show_col_types = FALSE)
K <- as.matrix(cop[, paste0("p", 1:100)])

# --- weighted 100-bin marginal: returns mean and min income per bin -----------
wbins <- function(income, w) {
  o <- order(income); income <- income[o]; w <- w[o]
  bin <- pmin(100L, findInterval(cumsum(w)/sum(w), (1:99)/100) + 1L)
  tibble(income, w, bin) |> group_by(bin) |>
    summarise(mean = weighted.mean(income, w), min = min(income), .groups="drop") |>
    complete(bin = 1:100) |> arrange(bin) |>
    mutate(mean = zoo::na.locf(ifelse(bin<=2 & is.na(mean), 0, mean), na.rm=FALSE),
           min  = zoo::na.locf(ifelse(bin<=2 & is.na(min), 0, min), na.rm=FALSE))
}

# --- parent marginal for cohort c (newborn in survey year c) ------------------
par_marginal <- function(c) {
  yr <- d[d$YEAR == c, c("SERIAL","PERNUM","AGE","inc","MOMLOC","POPLOC","ASECWT")]
  look <- yr |> select(SERIAL, PERNUM, p_inc = inc, p_age = AGE)
  nb <- yr[yr$AGE == 0, c("SERIAL","MOMLOC","POPLOC","ASECWT")]
  nb <- nb |>
    left_join(look, by = c("SERIAL","MOMLOC"="PERNUM")) |> rename(mom_inc=p_inc, mom_age=p_age) |>
    left_join(look, by = c("SERIAL","POPLOC"="PERNUM")) |> rename(pop_inc=p_inc, pop_age=p_age)
  nb <- nb |> filter(!(is.na(mom_inc) & is.na(pop_inc))) |>
    mutate(couple = rowSums(cbind(mom_inc, pop_inc), na.rm=TRUE),
           rep_age = ifelse(coalesce(mom_inc,-Inf) >= coalesce(pop_inc,-Inf), mom_age, pop_age)) |>
    filter(!is.na(rep_age), rep_age >= 25, rep_age <= 35) |>
    mutate(couple = to2014(couple, c - 1))
  frac0 <- with(nb, sum(ASECWT[couple <= 0]) / sum(ASECWT))
  pos <- nb |> filter(couple > 0)
  list(par = wbins(pos$couple, pos$ASECWT)$mean, frac0 = frac0, n = nrow(nb))
}

# --- kid marginal for cohort c (age 30 in survey year c+30) -------------------
kid_marginal <- function(c) {
  yr <- d[d$YEAR == c + 30, c("SERIAL","PERNUM","AGE","inc","SPLOC","BPL","ASECWT")]
  look <- yr |> select(SERIAL, PERNUM, s_inc = inc)
  k <- yr[yr$AGE == 30, ] |>
    left_join(look, by = c("SERIAL","SPLOC"="PERNUM")) |>
    mutate(couple = inc + coalesce(s_inc, 0)) |>
    filter(BPL < 15000, !is.na(couple)) |>                 # drop immigrants (foreign-born)
    mutate(couple = to2014(couple, c + 29))
  wbins(k$couple, k$ASECWT)$min
}

# --- absolute mobility for a cohort -------------------------------------------
absmob <- function(c) {
  pm <- par_marginal(c); km <- kid_marginal(c)
  amp <- sapply(1:100, function(i) sum(K[, i] * (km >= pm$par[i])))
  list(am = pm$frac0 * 1 + (1 - pm$frac0) * mean(amp), frac0 = pm$frac0, n = pm$n)
}

t1 <- read_tsv("table1_national_absmob_by_cohort_parpctile.tab", show_col_types=FALSE)
pub  <- setNames(round(t1$cohort_mean,3), round(t1$cohort))
pubf <- setNames(round(t1$par_frac0,3),  round(t1$cohort))
cat("=== VALIDATION (my rebuild vs Chetty published) ===\n")
cat("cohort   mine    pub     my_frac0  pub_frac0   my_n\n")
voff <- c()
for (c in 1980:1983) { r<-absmob(c); voff<-c(voff, r$am-pub[as.character(c)])
  cat(sprintf("%d    %.3f   %.3f    %.3f     %.3f      %d\n", c, r$am, pub[as.character(c)], r$frac0, pubf[as.character(c)], r$n)) }
offset <- mean(voff)
cat(sprintf("\nmean validation offset (mine - Chetty): %+.3f -> calibrate extension by %+.3f\n", offset, -offset))

cat("\n=== EXTENSION (new cohorts) ===\n")
cat("cohort   raw     calibrated\n")
ext <- sapply(1985:1994, function(c) absmob(c)$am)
cal <- ext - offset
for (i in seq_along(ext)) cat(sprintf("%d    %.3f   %.3f\n", 1984+i, ext[i], cal[i]))
# 3-cohort moving average of calibrated to show trend through the noise
ma <- stats::filter(cal, rep(1/3,3))
cat("\ncalibrated 3-cohort moving avg (1986-1993):", round(ma[!is.na(ma)],3), "\n")
saveRDS(data.frame(cohort=1985:1994, raw=ext, calibrated=cal), "extension.rds")
