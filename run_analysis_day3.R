# Day 3 Analysis Script
#
# This script analyzes data from day 3 (OwnD3 and Others phases) and compares
# it to baseline data from day 0 to investigate the persistence and transfer
# of norm influence.

# Load packages
library(tidyverse)
library(glmmTMB)
library(emmeans)
library(easystats)
library(patchwork)
library(ggsignif)

# Define factor levels for consistent ordering
TIME_LEVELS <- c("Dayone", "Followup")
PHASE_LEVELS <- c("Baseline", "OwnD0", "OwnD3", "Others")
CONDITION_LEVELS <- c("Risk Averse", "Risk Seeking")
DOMAIN_LEVELS <- c("Gains", "Losses")
FRAME_LEVELS <- c("Lives", "Money")

# Load Data ---------------------------------------------------------------

# Load experiment 1 data (Lives frame)
e1_data_full <- readRDS("data/prepared/experiment1.Rds")
e1_data <- e1_data_full %>%
  filter(phase %in% PHASE_LEVELS) %>% # Keep Baseline for comparison
  mutate(
    time = factor(time, levels = TIME_LEVELS),
    condition = factor(condition, levels = CONDITION_LEVELS),
    phase = factor(phase, levels = PHASE_LEVELS),
    domain = factor(domain, levels = DOMAIN_LEVELS)
  ) %>%
  select(-age, -influence_score, -comprehensionCorrectPercentage, -gamble_value_base) # Remove unused cols

# Load experiment 2 data (Money frame)
e2_data_full <- readRDS("data/prepared/experiment2.Rds")
e2_data <- e2_data_full %>%
  filter(phase %in% PHASE_LEVELS) %>% # Keep Baseline for comparison
  mutate(
    time = factor(time, levels = TIME_LEVELS),
    condition = factor(condition, levels = CONDITION_LEVELS),
    phase = factor(phase, levels = PHASE_LEVELS),
    domain = factor(domain, levels = DOMAIN_LEVELS)
  ) %>%
  select(-age, -influence_score, -comprehensionCorrectPercentage, -gamble_value_base) # Remove unused cols

# Combine both experiments for joint analysis
combined_data <- bind_rows(e1_data, e2_data) %>%
  mutate(frame = factor(frame, levels = FRAME_LEVELS))

# Model Fitting -----------------------------------------------------------

# Create model formula for experiment 1 (Lives frame) - includes Baseline, OwnD3, Others
e1_formula_d3 <- formula(choice ~ ev * phase * condition * domain + (1|id))

# Create model formula for experiment 2 (Money frame) - includes Baseline, OwnD3, Others
e2_formula_d3 <- formula(choice ~ ev * phase * condition * domain + (1|id))

# Create model formula for combined data - includes Baseline, OwnD3, Others
combined_formula_d3 <- formula(choice ~ ev * phase * condition * domain * frame + (1|id))

# Set model control parameters
model_control <- glmmTMBControl(
  optCtrl = list(iter.max = 1e+3, eval.max = 1e+3),
  parallel = (parallel::detectCores() - 1)
)

# Check if models already exist
model_file_path <- file.path("output/models_day3.Rds")

if (!file.exists(model_file_path)) {
  # Fit model for experiment 1 (Lives frame)
  e1_model_d3 <- glmmTMB(
    e1_formula_d3, data = e1_data,
    family = binomial(link = "logit"),
    control = model_control
  )

  # Fit model for experiment 2 (Money frame)
  e2_model_d3 <- glmmTMB(
    e2_formula_d3, data = e2_data,
    family = binomial(link = "logit"),
    control = model_control
  )

  # Fit model for combined data
  combined_model_d3 <- glmmTMB(
    combined_formula_d3, data = combined_data,
    family = binomial(link = "logit"),
    control = model_control
  )

  # Save models
  models_d3 <- list(e1_model_d3, e2_model_d3, combined_model_d3)
  saveRDS(models_d3, file = model_file_path)
} else {
  models_d3 <- readRDS(file = model_file_path)
  e1_model_d3 <- models_d3[[1]]
  e2_model_d3 <- models_d3[[2]]
  combined_model_d3 <- models_d3[[3]]
}

# Post-Hoc Analyses ------------------------------------------------------

## Experiment 1 (Lives) ----

# Calculate predicted probabilities for plotting (Baseline vs OwnD3)
e1_predicted_probs_d3 <- estimate_means(
  e1_model_d3,
  by = c("phase = c('Baseline', 'OwnD3')", "condition", "domain", "ev"),
  length = 100
)

# Calculate marginal means and contrasts (OwnD3 vs Baseline)
e1_emmeans_d3 <- emmeans(
  e1_model_d3, revpairwise ~ phase | condition | domain,
  regrid = "response", infer = TRUE, adjust = "mvt",
  at = list(phase = c("Baseline", "OwnD0", "OwnD3", "Others"))
)

# Calculate contrasts of contrasts (interaction effect)
# Compare (OwnD3 - Baseline) difference between Risk Averse and Risk Seeking
e1_conds_emmeans_d3 <- emmeans(
  e1_model_d3, ~condition*phase|domain,
  regrid = "response",
  at = list(phase = c("OwnD3", "Baseline")),
)

e1_contrasts_d3 <- contrast(
  e1_conds_emmeans_d3,
  list(
    `Baseline Risk Averse - OwnD3 Risk Averse` = c(-1, 0, 1, 0),
    `OwnD3 Risk Seeking - Baseline Risk Seeking` = c(0, 1, 0, -1)
  )
)

e1_contrast_pairs_d3 <- as.data.frame(pairs(e1_contrasts_d3, infer = TRUE))
e1_contrast_pairs_d3$frame <- "Lives"
e1_contrast_pairs_d3$p_sig <- with(e1_contrast_pairs_d3, ifelse(p.value < 0.05, "*", ""))
e1_contrast_pairs_d3$p_lab <- with(
  e1_contrast_pairs_d3,
  cut(p.value, c(-Inf, 0.001, 0.01, 0.05, Inf), labels = c("***", "**", "*", "ns"))
)
e1_contrast_pairs_d3$x <- 1
e1_contrast_pairs_d3$xend <- 2
e1_contrast_pairs_d3$y <- 0.475

## Experiment 2 (Money) ----

# Calculate predicted probabilities for plotting (Baseline vs OwnD3)
e2_predicted_probs_d3 <- estimate_means(
  e2_model_d3,
  by = c("phase = c('Baseline', 'OwnD3')", "condition", "domain", "ev"),
  length = 100
)

# Calculate marginal means and contrasts (OwnD3 vs Baseline)
e2_emmeans_d3 <- emmeans(
  e2_model_d3, revpairwise ~ phase | condition | domain,
  regrid = "response", infer = TRUE, adjust = "mvt",
  at = list(phase = c("Baseline", "OwnD0", "OwnD3", "Others"))
)

# Calculate contrasts of contrasts (interaction effect)
# Compare (OwnD3 - Baseline) difference between Risk Averse and Risk Seeking
e2_conds_emmeans_d3 <- emmeans(
  e2_model_d3, ~condition*phase|domain,
  regrid = "response",
  at = list(phase = c("OwnD3", "Baseline"))
)

e2_contrasts_d3 <- contrast(
  e2_conds_emmeans_d3,
  list(
    `Baseline Risk Averse - OwnD3 Risk Averse` = c(-1, 0, 1, 0),
    `OwnD3 Risk Seeking - Baseline Risk Seeking` = c(0, 1, 0, -1)
  )
)

e2_contrast_pairs_d3 <- as.data.frame(pairs(e2_contrasts_d3, infer = TRUE))
e2_contrast_pairs_d3$frame <- "Money"
e2_contrast_pairs_d3$p_sig <- with(e2_contrast_pairs_d3, ifelse(p.value < 0.05, "*", ""))
e2_contrast_pairs_d3$p_lab <- with(
  e2_contrast_pairs_d3,
  cut(p.value, c(-Inf, 0.001, 0.01, 0.05, Inf), labels = c("***", "**", "*", "ns"))
)
e2_contrast_pairs_d3$x <- 1
e2_contrast_pairs_d3$xend <- 2
e2_contrast_pairs_d3$y <- 0.475

# Combine contrast pairs for plotting annotations
all_contrast_pairs_d3 <- rbind(e1_contrast_pairs_d3, e2_contrast_pairs_d3)

# Create Plots -----------------------------------------------------------

# Create plots for Experiment 1 (Lives) - Day 3

# Probability by EV plot (Baseline vs OwnD3)
e1_prob_data_d3 <- e1_predicted_probs_d3

e1_prob_plot_d3 <- ggplot(e1_prob_data_d3, aes(x = ev, y = Probability, color = condition, fill = condition)) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_vline(xintercept = 10, linetype = "dashed") +
  geom_line(size = 1, linetype = "solid") +
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.6, color = NA) +
  scale_color_grey() +
  scale_fill_grey() +
  coord_cartesian(ylim = c(0, 1)) +
  cowplot::theme_cowplot() +
  labs(
    title = "Gamble Acceptance by Expected Value",
    x = "Gamble Expected Value (Lives)",
    y = "Probability of Choosing the Gamble Option",
    color = "Condition",
    fill = "Condition",
    linetype = "Condition"
  ) +
  facet_grid(phase ~ domain, scales = "free_x") +
  theme(
    legend.position = "bottom",
    legend.justification = c(0.5, 0.5),
    panel.spacing = unit(2, "lines"),
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(color = "black"),
    strip.background = element_rect(fill = "transparent")
  )

# Difference plot by EV (OwnD3 - Baseline)
e1_baseline_data_d3 <- e1_predicted_probs_d3[e1_predicted_probs_d3$phase == "Baseline", ]
e1_own3_data_d3 <- e1_predicted_probs_d3[e1_predicted_probs_d3$phase == "OwnD3", ]

e1_diffs_d3 <- e1_own3_data_d3
e1_diffs_d3$predicted <- e1_own3_data_d3$Probability - e1_baseline_data_d3$Probability
e1_diffs_d3$conf.low <- e1_own3_data_d3$CI_low - e1_baseline_data_d3$CI_low # Approximate CI
e1_diffs_d3$conf.high <- e1_own3_data_d3$CI_high - e1_baseline_data_d3$CI_high # Approximate CI
e1_diffs_d3$contrast <- "OwnD3 - Baseline"

e1_prob_diffs_plot_d3 <- ggplot(e1_diffs_d3, aes(x = ev, y = predicted, color = condition, fill = condition)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 10, linetype = "dashed") +
  geom_line(aes(linetype = condition), size = 1, color = "black") +
  scale_linetype_manual(values = c("solid", "dotted")) +
  scale_color_grey() +
  scale_fill_grey() +
  coord_cartesian(ylim = c(-0.5, 0.5)) +
  cowplot::theme_cowplot() +
  labs(
    title = "Influence by Expected Value",
    x = "Gamble Expected Value (Lives)",
    y = "Difference in Probabilities of Gambling\n(OwnD3 - Baseline)",
    color = "Condition",
    fill = "Condition",
    linetype = "Condition"
  ) +
  facet_grid(contrast ~ domain) +
  theme(
    legend.position = "bottom",
    legend.justification = c(0.5, 0.5),
    panel.spacing = unit(2, "lines"),
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(color = "black"),
    strip.background = element_rect(fill = "transparent")
  )

# Group probability plot (Baseline vs OwnD3)
e1_emmeans_df_d3 <- as.data.frame(e1_emmeans_d3$emmeans)

e1_group_plot_d3 <- ggplot(e1_emmeans_df_d3, aes(x = phase, y = prob, fill = condition)) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_col(width = 0.75, position = position_dodge(width = 0.75), color = "black", alpha = 0.8) +
  geom_errorbar(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    width = 0.1, position = position_dodge(width = 0.75)
  ) +
  scale_fill_grey() +
  coord_cartesian(ylim = c(0, 1)) +
  cowplot::theme_cowplot() +
  labs(
    title = "Gamble Acceptance by Phase",
    x = "Phase",
    y = "Probability of Choosing the Gamble Option",
    fill = "Condition"
  ) +
  geom_vline(xintercept = 2.5, linetype = "dashed") +
  annotate("text", x = 1.5, y = 0.85, label = "Day 0") +
  annotate("text", x = 3.5, y = 0.85, label = "Day 3") +
  facet_wrap(~domain) +
  theme(
    legend.position = "bottom",
    legend.justification = c(0.5, 0.5),
    panel.spacing = unit(1, "lines"),
    plot.title = element_text(hjust = 0.5),
    strip.background = element_rect(fill = "transparent")
  )

# Group difference plot (OwnD3 - Baseline)
e1_contrasts_df_d3 <- as.data.frame(e1_emmeans_d3$contrasts) %>%
  mutate(p_lab = cut(p.value, c(-Inf, 0.001, 0.01, 0.05, Inf), labels = c("***", "**", "*", "ns")))

e1_contrasts_subset_d3 <- e1_contrasts_df_d3[grepl("OwnD3 - Baseline", e1_contrasts_df_d3$contrast), ]

e1_group_diffs_plot_d3 <- ggplot(e1_contrasts_subset_d3, aes(x = condition, y = estimate, fill = condition)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_col(width = 0.35, position = position_dodge(width = 0.35), color = "black", alpha = 0.8) +
  geom_errorbar(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    width = 0.1, position = position_dodge(width = 0.35)
  ) +
  geom_text( # Add significance labels
    aes(label = p_lab, y = ifelse(estimate > 0, (asymp.UCL + 0.05), (asymp.LCL - 0.1))),
    position = position_dodge(width = 0.35)
  ) +
  scale_fill_grey() +
  coord_cartesian(ylim = c(-0.5, 0.5)) +
  cowplot::theme_cowplot() +
  geom_signif(
    stat = "identity", data = all_contrast_pairs_d3[all_contrast_pairs_d3$frame == "Lives", ],
    aes(x = x, xend = xend, y = y, yend = y, annotation = p_lab),
    comparisons = list("Risk Averse", "Risk Seeking"),
    inherit.aes = FALSE
  ) +
  labs(
    title = "Influence by Condition",
    subtitle = "Positive values indicate more risky choices compared to baseline\nNegative values indicate more risk averse choices compared to baseline",
    x = "Condition",
    y = "Difference in Probabilities of Gambling\n(OwnD3 - Baseline)",
    fill = "Difference"
  ) +
  facet_wrap(~domain) +
  theme(
    legend.position = "none",
    legend.justification = 0.5,
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.spacing = unit(1, "lines"),
    strip.background = element_rect(fill = "transparent")
  )

# Create plots for Experiment 2 (Money) - Day 3

# Probability by EV plot (Baseline vs OwnD3)
e2_prob_data_d3 <- e2_predicted_probs_d3

e2_prob_plot_d3 <- ggplot(e2_prob_data_d3, aes(x = ev, y = Probability, color = condition, fill = condition)) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_vline(xintercept = 10, linetype = "dashed") +
  geom_line(size = 1, linetype = "solid") +
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.6, color = NA) +
  scale_color_grey() +
  scale_fill_grey() +
  coord_cartesian(ylim = c(0, 1)) +
  cowplot::theme_cowplot() +
  labs(
    title = "Gamble Acceptance by Expected Value",
    x = "Gamble Expected Value (Money)",
    y = "Probability of Choosing the Gamble Option",
    color = "Condition",
    fill = "Condition",
    linetype = "Condition"
  ) +
  facet_grid(phase ~ domain, scales = "free_x") +
  theme(
    legend.position = "bottom",
    legend.justification = c(0.5, 0.5),
    panel.spacing = unit(2, "lines"),
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(color = "black"),
    strip.background = element_rect(fill = "transparent")
  )

# Difference plot by EV (OwnD3 - Baseline)
e2_baseline_data_d3 <- e2_predicted_probs_d3[e2_predicted_probs_d3$phase == "Baseline", ]
e2_own3_data_d3 <- e2_predicted_probs_d3[e2_predicted_probs_d3$phase == "OwnD3", ]

e2_diffs_d3 <- e2_own3_data_d3
e2_diffs_d3$predicted <- e2_own3_data_d3$Probability - e2_baseline_data_d3$Probability
e2_diffs_d3$conf.low <- e2_own3_data_d3$CI_low - e2_baseline_data_d3$CI_low # Approximate CI
e2_diffs_d3$conf.high <- e2_own3_data_d3$CI_high - e2_baseline_data_d3$CI_high # Approximate CI
e2_diffs_d3$contrast <- "OwnD3 - Baseline"

e2_prob_diffs_plot_d3 <- ggplot(e2_diffs_d3, aes(x = ev, y = predicted, color = condition, fill = condition)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 10, linetype = "dashed") +
  geom_line(aes(linetype = condition), size = 1, color = "black") +
  scale_linetype_manual(values = c("solid", "dotted")) +
  scale_color_grey() +
  scale_fill_grey() +
  coord_cartesian(ylim = c(-0.5, 0.5)) +
  cowplot::theme_cowplot() +
  labs(
    title = "Influence by Expected Value",
    x = "Gamble Expected Value (Money)",
    y = "Difference in Probabilities of Gambling\n(OwnD3 - Baseline)",
    color = "Condition",
    fill = "Condition",
    linetype = "Condition"
  ) +
  facet_grid(contrast ~ domain) +
  theme(
    legend.position = "bottom",
    legend.justification = c(0.5, 0.5),
    panel.spacing = unit(2, "lines"),
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(color = "black"),
    strip.background = element_rect(fill = "transparent")
  )

# Group probability plot (Baseline vs OwnD3)
e2_emmeans_df_d3 <- as.data.frame(e2_emmeans_d3$emmeans)

e2_group_plot_d3 <- ggplot(e2_emmeans_df_d3, aes(x = phase, y = prob, fill = condition)) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_col(width = 0.75, position = position_dodge(width = 0.75), color = "black", alpha = 0.8) +
  geom_errorbar(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    width = 0.1, position = position_dodge(width = 0.75)
  ) +
  scale_fill_grey() +
  coord_cartesian(ylim = c(0, 1)) +
  cowplot::theme_cowplot() +
  labs(
    title = "Gamble Acceptance by Phase",
    x = "Phase",
    y = "Probability of Choosing the Gamble Option",
    fill = "Condition"
  ) +
  geom_vline(xintercept = 2.5, linetype = "dashed") +
  annotate("text", x = 1.5, y = 0.85, label = "Day 0") +
  annotate("text", x = 3.5, y = 0.85, label = "Day 3") +
  facet_wrap(~domain) +
  theme(
    legend.position = "bottom",
    legend.justification = c(0.5, 0.5),
    panel.spacing = unit(1, "lines"),
    plot.title = element_text(hjust = 0.5),
    strip.background = element_rect(fill = "transparent")
  )

# Group difference plot (OwnD3 - Baseline)
e2_contrasts_df_d3 <- as.data.frame(e2_emmeans_d3$contrasts) %>%
  mutate(p_lab = cut(p.value, c(-Inf, 0.001, 0.01, 0.05, Inf), labels = c("***", "**", "*", "ns")))

e2_contrasts_subset_d3 <- e2_contrasts_df_d3[grepl("OwnD3 - Baseline", e2_contrasts_df_d3$contrast), ]

e2_group_diffs_plot_d3 <- ggplot(e2_contrasts_subset_d3, aes(x = condition, y = estimate, fill = condition)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_col(width = 0.35, position = position_dodge(width = 0.35), color = "black", alpha = 0.8) +
  geom_errorbar(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    width = 0.1, position = position_dodge(width = 0.35)
  ) +
  geom_text( # Add significance labels
    aes(label = p_lab, y = ifelse(estimate > 0, (asymp.UCL + 0.05), (asymp.LCL - 0.1))),
    position = position_dodge(width = 0.35)
  ) +
  scale_fill_grey() +
  coord_cartesian(ylim = c(-0.5, 0.5)) +
  cowplot::theme_cowplot() +
  geom_signif(
    stat = "identity", data = all_contrast_pairs_d3[all_contrast_pairs_d3$frame == "Money", ],
    aes(x = x, xend = xend, y = y, yend = y, annotation = p_lab),
    comparisons = list("Risk Averse", "Risk Seeking"),
    inherit.aes = FALSE
  ) +
  labs(
    title = "Influence by Condition",
    subtitle = "Positive values indicate more risky choices compared to baseline\nNegative values indicate more risk averse choices compared to baseline",
    x = "Condition",
    y = "Difference in Probabilities of Gambling\n(OwnD3 - Baseline)",
    fill = "Difference"
  ) +
  facet_grid(~domain) +
  theme(
    legend.position = "none",
    legend.justification = 0.5,
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.spacing = unit(1, "lines"),
    strip.background = element_rect(fill = "transparent")
  )

# Create Figures ----------------------------------------------------------

# Define layout for figures with subplots
layout <- "
AACCC
AACCC
BBDDD
"

# Figure 3: Experiment 1 (Lives frame) - Day 3 Comparison
fig3 <- e1_prob_plot_d3 + e1_prob_diffs_plot_d3 + e1_group_plot_d3 + e1_group_diffs_plot_d3 +
  plot_layout(design = layout) +
  plot_annotation(tag_levels = list(c("a", "c", "b", "d")))

# Figure 5: Experiment 2 (Money frame) - Day 3 Comparison
fig5 <- e2_prob_plot_d3 + e2_prob_diffs_plot_d3 + e2_group_plot_d3 + e2_group_diffs_plot_d3 +
  plot_layout(design = layout) +
  plot_annotation(tag_levels = list(c("a", "c", "b", "d")))

fig3
fig5

# Save figures
ggsave("output/fig3.pdf", fig3, scale = 1.5, width = 10, height = 7)
ggsave("output/fig5.pdf", fig5, scale = 1.5, width = 10, height = 7)
