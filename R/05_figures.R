# Figures ---------------------------------------------------------------------------
theme_set(theme_minimal(base_size = 12))
pal <- c(SRS = "#5f6368", Stratified = "#1a73e8", `Two-stage` = "#e37400")

cmp   <- readr::read_csv("results/ine_design_comparison.csv", show_col_types = FALSE)
draws <- readr::read_csv("results/ine_simulated_estimates.csv", show_col_types = FALSE)
R_pct <- readr::read_csv("results/ine_frame_summary.csv", show_col_types = FALSE)$coalition_share_pct
lev <- c("SRS", "Stratified", "Two-stage")
draws$design <- factor(draws$design, lev); cmp$design <- factor(cmp$design, lev)

# 1. Sampling distributions of the estimator under each design
p1 <- ggplot(draws, aes(estimate_pct, fill = design, colour = design)) +
  geom_density(alpha = 0.25, linewidth = 0.6, adjust = 1.2) +
  geom_vline(xintercept = R_pct, linetype = 2) +
  annotate("label", x = R_pct, y = Inf, vjust = 1.3, size = 3.3,
           label = sprintf("true share %.2f%%", R_pct)) +
  scale_fill_manual(values = pal) + scale_colour_manual(values = pal) +
  coord_cartesian(xlim = R_pct + c(-7, 7)) +
  labs(x = "Estimated coalition share of valid votes (%)", y = NULL, fill = NULL, colour = NULL,
       title = "Same sample size, very different precision",
       subtitle = sprintf("%s samples of 900 polling stations per design, 2021 federal deputies", format(nrow(draws) / 3, big.mark = ","))) +
  theme(legend.position = "top", axis.text.y = element_blank())
ggsave("figures/ine_sampling_distributions.png", p1, width = 8, height = 4.4, dpi = 150)

# 2. Standard errors: theory vs simulation vs what each sample estimates
se_long <- cmp |>
  select(design, `Theoretical (linearisation)` = se_theoretical_pp,
         `Simulated (5,000 samples)` = se_simulated_pp,
         `Estimated from one sample (mean)` = se_estimated_mean_pp) |>
  tidyr::pivot_longer(-design, names_to = "source", values_to = "se")
p2 <- ggplot(se_long, aes(se, design, colour = source, shape = source)) +
  geom_point(size = 3.2, position = position_dodge(width = 0.5)) +
  geom_text(data = cmp, aes(x = se_theoretical_pp, y = design,
                            label = sprintf("deff = %.2f  ·  effective n ≈ %s", deff, trimws(format(round(effective_n), big.mark = ",")))),
            inherit.aes = FALSE, nudge_y = 0.38, size = 3.3, hjust = 0) +
  scale_colour_manual(values = c("#202124", "#1a73e8", "#e37400")) +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.45))) +
  labs(x = "Standard error of the estimated share (percentage points)", y = NULL, colour = NULL, shape = NULL,
       title = "Theory, simulation and per-sample estimates agree") +
  theme(legend.position = "top")
ggsave("figures/ine_standard_errors.png", p2, width = 8, height = 4.2, dpi = 150)

# 1b. One hundred samples per design: estimate and 95% confidence interval
ci <- readr::read_csv("results/ine_ci_examples.csv", show_col_types = FALSE) |>
  mutate(design = factor(design, lev),
         status = ifelse(covers, "Interval contains the true share", "Interval misses it"))
mg <- cmp |> mutate(lab = sprintf("margin of error ±%.2f pp", margin95_pp))
p1b <- ggplot(ci, aes(y = sample)) +
  geom_vline(xintercept = R_pct, linetype = 2) +
  geom_segment(aes(x = lo_pct, xend = hi_pct, yend = sample, colour = status), linewidth = 0.5) +
  geom_point(aes(x = estimate_pct, colour = status), size = 0.8) +
  geom_label(data = mg, aes(x = R_pct, y = 106, label = lab), size = 3.2, inherit.aes = FALSE, fill = "white", label.size = 0) +
  facet_wrap(~design, nrow = 1) +
  scale_colour_manual(values = c("Interval contains the true share" = "#5f6368", "Interval misses it" = "#d93025")) +
  scale_y_continuous(expand = expansion(add = c(1, 4))) +
  labs(x = "Estimated share with 95% confidence interval (%)", y = "Sample", colour = NULL,
       title = "What one quick count would report, 100 times over",
       subtitle = "Each line is one sample of 900 polling stations; the dashed line is the true share") +
  theme(legend.position = "top", axis.text.y = element_blank(), panel.grid.major.y = element_blank())
ggsave("figures/ine_confidence_intervals.png", p1b, width = 9, height = 5.6, dpi = 150)

# 2b. Two-stage design: how to split the same 900 stations between the stages
alloc <- readr::read_csv("results/ine_two_stage_allocation.csv", show_col_types = FALSE)
p2b <- ggplot(alloc, aes(districts_per_circ, deff)) +
  geom_hline(yintercept = 1, linetype = 2, colour = "#5f6368") +
  geom_line(colour = "#e37400", linewidth = 1) +
  geom_point(colour = "#e37400", size = 3) +
  geom_text(aes(label = sprintf("%d × %g", districts_per_circ, stations_per_district)),
            vjust = -1.1, size = 3.3) +
  annotate("text", x = 58, y = 1.25, label = "SRS", colour = "#5f6368", size = 3.3) +
  scale_y_log10(breaks = c(0.5, 1, 2, 5, 10)) +
  labs(x = "Districts sampled per circumscription", y = "Design effect (log scale)",
       title = "Sample more districts, fewer stations per district",
       subtitle = "Two-stage design with 900 stations: districts per circumscription × stations per district")
ggsave("figures/ine_two_stage_allocation.png", p2b, width = 8, height = 4.2, dpi = 150)

# 3. ENVI: housing need by state with 90% confidence intervals
envi <- readr::read_csv("results/envi_housing_need.csv", show_col_types = FALSE)
nat <- envi |> filter(ent == 0)
st  <- envi |> filter(ent != 0) |> mutate(state = reorder(state, pct))
p3 <- ggplot(st, aes(pct, state)) +
  annotate("rect", xmin = nat$pct_lo, xmax = nat$pct_hi, ymin = -Inf, ymax = Inf, fill = "#e8f0fe") +
  geom_vline(xintercept = nat$pct, colour = "#1a73e8", linetype = 2) +
  geom_errorbar(aes(xmin = pct_lo, xmax = pct_hi), width = 0, orientation = "y", colour = "#9aa0a6") +
  geom_point(colour = "#202124", size = 1.8) +
  annotate("text", x = nat$pct, y = 33.2, label = sprintf("national %.1f%%", nat$pct),
           colour = "#1a73e8", size = 3.3, hjust = -0.05) +
  scale_y_discrete(expand = expansion(add = c(0.6, 1.4))) +
  labs(x = "Households that need to rent, buy or build a home (%)", y = NULL,
       title = "Housing need by state, ENVI 2020",
       subtitle = "Survey-weighted estimates with 90% confidence intervals")
ggsave("figures/envi_need_by_state.png", p3, width = 8, height = 8.2, dpi = 150)

# 4. ENVI: tile map of Mexico (one square per state)
tiles <- tibble::tribble(
  ~ent, ~abbr, ~col, ~row,
   2, "BC", 1, 1,   26, "SON", 2, 1,   8, "CHH", 3, 1,   5, "COA", 4, 1,  19, "NL", 5, 1,
   3, "BCS", 1, 2,  25, "SIN", 2, 2,  10, "DUR", 3, 2,  32, "ZAC", 4, 2,  24, "SLP", 5, 2,  28, "TAM", 6, 2,
  18, "NAY", 3, 3,   1, "AGS", 4, 3,  11, "GTO", 5, 3,  22, "QRO", 6, 3,  13, "HGO", 7, 3,
  14, "JAL", 3, 4,  16, "MIC", 4, 4,  15, "MEX", 5, 4,   9, "CDMX", 6, 4, 29, "TLX", 7, 4, 30, "VER", 8, 4,
   6, "COL", 3, 5,  12, "GRO", 4, 5,  17, "MOR", 5, 5,  21, "PUE", 6, 5,  20, "OAX", 7, 5,  27, "TAB", 8, 5,
   4, "CAM", 9, 5,  31, "YUC", 10, 5,
   7, "CHP", 8, 6,  23, "ROO", 10, 6
)
map_df <- tiles |> left_join(envi, by = "ent")
p4 <- ggplot(map_df, aes(col, -row, fill = pct)) +
  geom_tile(colour = "white", linewidth = 1.2, width = 0.95, height = 0.95) +
  geom_text(aes(label = sprintf("%s\n%.0f%%", abbr, pct), colour = pct > 21), size = 3, lineheight = 0.9) +
  scale_colour_manual(values = c(`FALSE` = "#202124", `TRUE` = "white"), guide = "none") +
  scale_fill_gradient(low = "#d2e3fc", high = "#174ea6", name = "% of\nhouseholds") +
  coord_equal() + theme_void(base_size = 12) +
  labs(title = "Share of households with a housing need, by state",
       subtitle = "ENVI 2020; one square per state, arranged roughly by geography") +
  theme(plot.title = element_text(face = "bold"), legend.position = "right")
ggsave("figures/envi_tile_map.png", p4, width = 8, height = 5.2, dpi = 150, bg = "white")

cat("Figures saved in figures/\n")
