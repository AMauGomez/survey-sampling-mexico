# ENVI 2020: households that need to rent, buy or build a separate home ---------------
#
# Design (INEGI): probabilistic, two-stage, stratified and clustered.
#   EST_DIS = design stratum (552), UPM_DIS = primary sampling unit (8,278),
#   FACTOR  = expansion factor (household weight).
# Question P3A1_1: "Does anyone in this household need, or plan, to rent, buy or
# build a home, apart from the one they live in now?" (1 = yes, 2 = no, 9 = don't know)
#
# Estimates: number and percentage of households with a housing need, national
# and by state, with 90% confidence intervals (the level INEGI publishes).

options(survey.lonely.psu = "adjust")

envi <- readr::read_csv("data/raw/envi_2020_thogar.csv", col_types = readr::cols(.default = "c"),
                        progress = FALSE) |>
  mutate(FACTOR = as.numeric(FACTOR),
         need = as.numeric(P3A1_1 == "1"),
         one = 1,
         ent = as.integer(ENT))

des <- svydesign(ids = ~UPM_DIS, strata = ~EST_DIS, weights = ~FACTOR, data = envi, nest = TRUE)

state_names <- c("Aguascalientes", "Baja California", "Baja California Sur", "Campeche",
  "Coahuila de Zaragoza", "Colima", "Chiapas", "Chihuahua", "Ciudad de México", "Durango",
  "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "México", "Michoacán de Ocampo", "Morelos",
  "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí",
  "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz de Ignacio de la Llave",
  "Yucatán", "Zacatecas")

z90 <- qnorm(0.95)
estimate <- function(d) {
  tot <- svytotal(~one + need, d)
  pr  <- svymean(~need, d, deff = TRUE)
  tibble(
    households = coef(tot)[["one"]],
    need = coef(tot)[["need"]], need_se = SE(tot)[["need"]],
    pct = 100 * coef(pr)[[1]], pct_se = 100 * SE(pr)[[1]],
    deff = deff(pr)[[1]],
    sample_households = nrow(d$variables)
  )
}

national <- estimate(des) |> mutate(ent = 0L, state = "Mexico (national)")
states <- bind_rows(lapply(1:32, function(e) estimate(subset(des, ent == e)) |>
                             mutate(ent = e, state = state_names[e])))

results <- bind_rows(national, states) |>
  mutate(need_lo = need - z90 * need_se, need_hi = need + z90 * need_se,
         pct_lo = pct - z90 * pct_se, pct_hi = pct + z90 * pct_se,
         cv_pct = 100 * pct_se / pct,
         quality = cut(cv_pct, c(0, 15, 30, Inf), labels = c("High", "Moderate", "Low"), right = FALSE)) |>
  select(ent, state, sample_households, households, need, need_se, need_lo, need_hi,
         pct, pct_se, pct_lo, pct_hi, cv_pct, quality, deff)

# ---- Check against INEGI's official tabulation (Cuadro 5.1) ---------------------
tab <- tempfile(); dir.create(tab)
unzip("data/raw/envi_2020_tabulados_tema05.zip", exdir = tab)
read_sheet <- function(prefix) {
  f <- list.files(tab, pattern = paste0("^", prefix), full.names = TRUE)
  x <- readxl::read_excel(f, sheet = "Cuadro 5.1", col_names = FALSE, col_types = "text",
                          .name_repair = "minimal")
  # Keep the non-empty cells of each row; data rows are: name, total, yes (abs), yes (%), ...
  rows <- lapply(seq_len(nrow(x)), function(i) { v <- unlist(x[i, ]); v[!is.na(v)] })
  rows <- Filter(function(v) length(v) >= 4 && !is.na(suppressWarnings(as.numeric(v[2]))) &&
                   is.na(suppressWarnings(as.numeric(v[1]))), rows)
  tibble(households = as.numeric(sapply(rows, `[`, 2)),
         need = as.numeric(sapply(rows, `[`, 3)),
         pct = as.numeric(sapply(rows, `[`, 4)))
}
official <- read_sheet("01EST") |> mutate(ent = 0:32)
official_se <- read_sheet("03EES") |> transmute(ent = 0:32, need_se = need, pct_se = pct)

check <- results |>
  select(ent, state, need, pct, need_se, pct_se) |>
  left_join(official |> rename(need_inegi = need, pct_inegi = pct, households_inegi = households), by = "ent") |>
  left_join(official_se |> rename(need_se_inegi = need_se, pct_se_inegi = pct_se), by = "ent") |>
  mutate(diff_need = need - need_inegi, diff_pct_pp = pct - pct_inegi,
         se_ratio_pct = pct_se / pct_se_inegi)

readr::write_csv(results, "results/envi_housing_need.csv")
readr::write_csv(check, "results/envi_check_vs_inegi.csv")

design_info <- tibble(
  households_in_sample = nrow(envi),
  strata = n_distinct(envi$EST_DIS),
  psus = n_distinct(envi$UPM_DIS),
  max_abs_diff_households = max(abs(check$diff_need)),
  max_abs_diff_pct_pp = max(abs(check$diff_pct_pp)),
  se_pct_ratio_min = min(check$se_ratio_pct), se_pct_ratio_max = max(check$se_ratio_pct)
)
readr::write_csv(design_info, "results/envi_design_info.csv")
print(design_info, width = Inf)
print(results |> filter(ent == 0) |> select(need, need_lo, need_hi, pct, pct_lo, pct_hi, deff))
