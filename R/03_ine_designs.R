# Three sampling designs for a quick count, each with 900 polling stations ---------
#
#   I.   SRS:        simple random sample without replacement of 900 stations.
#   II.  STRAT:      stratified by the 300 federal districts, SRS of 3 stations each.
#   III. TWO-STAGE:  stratified by the 5 circumscriptions; in each, SRS of 6 districts
#                    (primary units), then SRS of 30 stations in each selected district.
#
# Estimator: the ratio estimator R_hat = t_y_hat / t_z_hat, both totals estimated
# with Horvitz–Thompson weights (1 / inclusion probability).
#
# Variance: Taylor linearisation. R_hat − R ≈ t_u_hat with u_k = (y_k − R z_k) / t_z,
# so the variance of R_hat is the design variance of the HT estimator of the total
# of u. This gives:
#   * the exact (population) variance of each design, computed from the frame;
#   * an estimated variance from each sample, used for 95% confidence intervals.

N  <- nrow(frame)
u  <- (frame$y - R_TRUE * frame$z) / sum(frame$z)
by_district <- split(seq_len(N), frame$district)
N_i  <- lengths(by_district)
circ_of_district <- tapply(frame$circ, frame$district, `[`, 1)[names(by_district)]
districts_by_circ <- split(names(by_district), circ_of_district)
M_c <- lengths(districts_by_circ)

n_srs <- 900; n_h <- 3; m_c <- 6; n_2 <- 30

# ---- 1. Population variances (linearised) --------------------------------------
var_u_district <- vapply(by_district, function(ix) var(u[ix]), numeric(1))
T_district     <- vapply(by_district, function(ix) sum(u[ix]), numeric(1))

V_srs   <- N^2 * (1 - n_srs / N) / n_srs * var(u)
V_strat <- sum(N_i^2 * (1 - n_h / N_i) / n_h * var_u_district)
V_two   <- sum(vapply(names(districts_by_circ), function(c) {
  d  <- districts_by_circ[[c]]; M <- length(d)
  first  <- M^2 * (1 - m_c / M) / m_c * var(T_district[d])
  second <- (M / m_c) * sum(N_i[d]^2 * (1 - n_2 / N_i[d]) / n_2 * var_u_district[d])
  first + second
}, numeric(1)))

# Same 900 stations, spread differently across the two stages
two_stage_var <- function(m, n) sum(vapply(names(districts_by_circ), function(c) {
  d <- districts_by_circ[[c]]; M <- length(d)
  M^2 * (1 - m / M) / m * var(T_district[d]) +
    (M / m) * sum(N_i[d]^2 * (1 - n / N_i[d]) / n * var_u_district[d])
}, numeric(1)))
allocation <- tibble(districts_per_circ = c(6, 10, 15, 30, 45, 60),
                     stations_per_district = 900 / (5 * districts_per_circ)) |>
  mutate(se_pp = 100 * sqrt(mapply(two_stage_var, districts_per_circ, stations_per_district)),
         deff = (se_pp / (100 * sqrt(V_srs)))^2)
readr::write_csv(allocation, "results/ine_two_stage_allocation.csv")

# ---- 2. One sample from each design: estimate and estimated variance -----------
ratio_parts <- function(s, w) {
  R_hat  <- sum(w * frame$y[s]) / sum(w * frame$z[s])
  tz_hat <- sum(w * frame$z[s])
  list(R_hat = R_hat, u_hat = (frame$y[s] - R_hat * frame$z[s]) / tz_hat)
}

draw_srs <- function() {
  s <- sample.int(N, n_srs)
  p <- ratio_parts(s, rep(N / n_srs, n_srs))
  c(p$R_hat, N^2 * (1 - n_srs / N) / n_srs * var(p$u_hat))
}

draw_strat <- function() {
  s <- unlist(lapply(by_district, function(ix) ix[sample.int(length(ix), n_h)]), use.names = FALSE)
  h <- rep(seq_along(by_district), each = n_h)
  p <- ratio_parts(s, rep(N_i / n_h, each = n_h))
  s2 <- (rowsum(p$u_hat^2, h)[, 1] - rowsum(p$u_hat, h)[, 1]^2 / n_h) / (n_h - 1)
  c(p$R_hat, sum(N_i^2 * (1 - n_h / N_i) / n_h * s2))
}

draw_two <- function() {
  sel <- unlist(lapply(districts_by_circ, function(d) d[sample.int(length(d), m_c)]), use.names = FALSE)
  s   <- unlist(lapply(by_district[sel], function(ix) ix[sample.int(length(ix), n_2)]), use.names = FALSE)
  dist <- rep(sel, each = n_2)
  circ <- circ_of_district[dist]
  M <- M_c[as.character(circ)]
  p <- ratio_parts(s, (M / m_c) * (N_i[dist] / n_2))
  # Unbiased two-stage variance estimator
  T_hat <- (N_i[sel] / n_2) * rowsum(p$u_hat, dist)[sel, 1]
  s2_within <- (rowsum(p$u_hat^2, dist)[sel, 1] - rowsum(p$u_hat, dist)[sel, 1]^2 / n_2) / (n_2 - 1)
  c_sel <- circ_of_district[sel]; M_sel <- M_c[as.character(c_sel)]
  first  <- sum(tapply(seq_along(sel), c_sel, function(j)
    M_sel[j[1]]^2 * (1 - m_c / M_sel[j[1]]) / m_c * var(T_hat[j])))
  second <- sum((M_sel / m_c) * N_i[sel]^2 * (1 - n_2 / N_i[sel]) / n_2 * s2_within)
  c(p$R_hat, first + second)
}

# ---- 3. Monte Carlo: B samples per design --------------------------------------
set.seed(SEED)
t0 <- Sys.time()
sims <- list(
  SRS        = t(replicate(B, draw_srs())),
  Stratified = t(replicate(B, draw_strat())),
  `Two-stage` = t(replicate(B, draw_two()))
)
cat("Simulation time:", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), "s\n")

V_pop <- c(SRS = V_srs, Stratified = V_strat, `Two-stage` = V_two)
design_label <- c(SRS = "I. Simple random sample (900 stations)",
                  Stratified = "II. Stratified by district (300 × 3)",
                  `Two-stage` = "III. Two-stage by circumscription (5 × 6 districts × 30)")

summary_tbl <- bind_rows(lapply(names(sims), function(d) {
  est <- sims[[d]][, 1]; v <- sims[[d]][, 2]
  tibble(
    design = d, label = design_label[[d]],
    mean_estimate_pct = 100 * mean(est),
    bias_pp = 100 * (mean(est) - R_TRUE),
    se_simulated_pp = 100 * sd(est),
    se_theoretical_pp = 100 * sqrt(V_pop[[d]]),
    se_estimated_mean_pp = 100 * mean(sqrt(pmax(v, 0))),
    rmse_pp = 100 * sqrt(mean((est - R_TRUE)^2)),
    ci95_coverage_pct = 100 * mean(abs(est - R_TRUE) <= 1.96 * sqrt(pmax(v, 0))),
    deff = V_pop[[d]] / V_srs,
    effective_n = 900 / (V_pop[[d]] / V_srs)
  )
}))

draws <- bind_rows(lapply(names(sims), function(d)
  tibble(design = d, estimate_pct = 100 * sims[[d]][, 1])))

# The first 100 samples of each design, as an analyst would report them on election night
ci_examples <- bind_rows(lapply(names(sims), function(d) {
  x <- sims[[d]][1:100, , drop = FALSE]
  tibble(design = d, sample = 1:100, estimate_pct = 100 * x[, 1],
         lo_pct = 100 * (x[, 1] - 1.96 * sqrt(pmax(x[, 2], 0))),
         hi_pct = 100 * (x[, 1] + 1.96 * sqrt(pmax(x[, 2], 0))))
})) |> mutate(covers = lo_pct <= 100 * R_TRUE & hi_pct >= 100 * R_TRUE)
readr::write_csv(ci_examples, "results/ine_ci_examples.csv")

summary_tbl <- summary_tbl |> mutate(margin95_pp = 1.96 * se_theoretical_pp)
readr::write_csv(summary_tbl, "results/ine_design_comparison.csv")
readr::write_csv(draws, "results/ine_simulated_estimates.csv")
print(summary_tbl |> select(-label), width = Inf)
