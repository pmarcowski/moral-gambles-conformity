# Day 0 Analysis Script
#
# This script analyzes data from day 0 (baseline and OwnD0 phases) to investigate
# how participants' choices are influenced by the experimental conditions.

# Load packages
library(tidyverse)
library(glmmTMB)
library(emmeans)
library(easystats)
library(patchwork)

# Define factor levels for consistent ordering
PHASE_LEVELS <- c("Baseline", "OwnD0")
CONDITION_LEVELS <- c("Risk Averse", "Risk Seeking")
DOMAIN_LEVELS <- c("Gains", "Losses")
FRAME_LEVELS <- c("Lives", "Money")

# Load Data ---------------------------------------------------------------

# Load experiment 1 data (Lives frame)
e1_data <- readRDS("data/prepared/experiment1.Rds") %>%
  filter(time == "Dayone", phase %in% PHASE_LEVELS) %>%
  mutate(
    condition = factor(condition, levels = CONDITION_LEVELS),
    phase = factor(phase, levels = PHASE_LEVELS),
    domain = factor(domain, levels = DOMAIN_LEVELS)
  ) %>%
  select(-age) # Remove unused cols

# Load experiment 2 data (Money frame)
e2_data <- readRDS("data/prepared/experiment2.Rds") %>%
  filter(time == "Dayone", phase %in% PHASE_LEVELS) %>%
  mutate(
    condition = factor(condition, levels = CONDITION_LEVELS),
    phase = factor(phase, levels = PHASE_LEVELS),
    domain = factor(domain, levels = DOMAIN_LEVELS)
  ) %>%
  select(-age) # Remove unused cols

# Combine both experiments for joint analysis
combined_data <- bind_rows(e1_data, e2_data) %>%
  mutate(frame = factor(frame, levels = FRAME_LEVELS))

# Get sample size by frame
sample_size_by_frame <- combined_data %>%
  group_by(frame) %>%
  summarize(n = n_distinct(id))

sample_size_by_frame

# Model Fitting -----------------------------------------------------------

# Create model formula for experiment 1 (Lives frame)
e1_formula <- formula(choice ~ ev * phase * condition * domain + (1|id))

# Create model formula for experiment 2 (Money frame)
e2_formula <- formula(choice ~ ev * phase * condition * domain + (1|id))

# Create model formula for combined data
combined_formula <- formula(choice ~ ev * phase * condition * domain * frame + (1|id))

# Set model control parameters
model_control <- glmmTMBControl(
  optCtrl = list(iter.max = 1e+3, eval.max = 1e+3),
  parallel = (parallel::detectCores() - 1)
)

# Check if models already exist
model_file_path <- file.path("output/models_day0.Rds")

if (!file.exists(model_file_path)) {
  # Fit model for experiment 1 (Lives frame)
  e1_model <- glmmTMB(
    e1_formula, data = e1_data,
    family = binomial(link = "logit"),
    control = model_control
  )

  # Fit model for experiment 2 (Money frame)
  e2_model <- glmmTMB(
    e2_formula, data = e2_data,
    family = binomial(link = "logit"),
    control = model_control
  )

  # Fit model for combined data
  combined_model <- glmmTMB(
    combined_formula, data = combined_data,
    family = binomial(link = "logit"),
    control = model_control
  )

  # Save models
  models <- list(e1_model, e2_model, combined_model)
  saveRDS(models, file = model_file_path)
} else {
  models <- readRDS(file = model_file_path)
  e1_model <- models[[1]]
  e2_model <- models[[2]]
  combined_model <- models[[3]]
}

# Post-Hoc Analyses ------------------------------------------------------

## Experiment 1 (Lives) ----

# Calculate model slopes
e1_slopes <- standardize(estimate_slopes(e1_model, trend = "ev", at = "domain"))

# Pairwise comparisons between conditions for each phase and domain
e1_condition_comparisons <- as.data.frame(
  emmeans(e1_model, pairwise ~ condition | phase | domain,
          regrid = "response", infer = TRUE, adjust = "mvt",
          data = e1_data)$contrasts
)

# Calculate slopes for each domain
e1_domain_slopes <- standardize(estimate_slopes(e1_model, trend = "ev", at = "domain"))

# Test overall gambling behavior at baseline
e1_baseline_gambling <- binom.test(
  sum(e1_data[e1_data$phase == "Baseline", "choice"]),
  nrow(e1_data[e1_data$phase == "Baseline", "choice"])
)

# Pairwise comparisons between domains for each phase
e1_domain_comparisons <- emmeans(
  e1_model, revpairwise ~ domain | phase,
  regrid = "response", infer = TRUE, adjust = "mvt"
)

# Calculate marginal means and contrasts
e1_emmeans <- emmeans(
  e1_model, revpairwise ~ phase | condition | domain,
  regrid = "response", infer = TRUE, adjust = "mvt"
)

# Calculate predicted probabilities for each experiment
e1_predicted_probs <- estimate_means(
  e1_model,
  at = c("phase", "condition", "domain", "ev"),
  length = 100
)

# Calculate contrasts of contrasts
e1_conds_emmeans <- emmeans(
  e1_model, ~condition*phase|domain,
  regrid = "response",
  at = list(phase = c("OwnD0", "Baseline"))
)

e1_contrasts <- contrast(
  e1_conds_emmeans,
  list(
    `Baseline Risk Averse - OwnD0 Risk Averse` = c(-1, 0, 1, 0),
    `OwnD0 Risk Seeking - Baseline Risk Seeking` = c(0, 1, 0, -1)
  )
)

e1_contrast_pairs <- as.data.frame(pairs(e1_contrasts, infer = TRUE))
e1_contrast_pairs$frame <- "Lives"
e1_contrast_pairs$p_sig <- with(e1_contrast_pairs, ifelse(p.value < 0.05, "*", ""))
e1_contrast_pairs$p_lab <- with(
  e1_contrast_pairs,
  cut(p.value, c(-Inf, 0.001, 0.01, 0.05, Inf), labels = c("***", "**", "*", "ns"))
)
e1_contrast_pairs$x <- 1
e1_contrast_pairs$xend <- 2
e1_contrast_pairs$y <- 0.4

## Experiment 2 (Money) ----

# Calculate slopes for each domain
e2_domain_slopes <- standardize(estimate_slopes(e2_model, trend = "ev", at = "domain"))

# Test overall gambling behavior at baseline
e2_baseline_gambling <- binom.test(
  sum(e2_data[e2_data$phase == "Baseline", "choice"]),
  nrow(e2_data[e2_data$phase == "Baseline", "choice"])
)

# Pairwise comparisons between conditions for each phase and domain
e2_condition_comparisons <- as.data.frame(
  emmeans(e2_model, pairwise ~ condition | phase | domain,
          regrid = "response", infer = TRUE, adjust = "mvt")$contrasts
)

# Pairwise comparisons between domains for each phase
e2_domain_comparisons <- emmeans(
  e2_model, revpairwise ~ domain | phase,
  regrid = "response", infer = TRUE, adjust = "mvt"
)

# Calculate marginal means and contrasts
e2_emmeans <- emmeans(
  e2_model, revpairwise ~ phase | condition | domain,
  regrid = "response", infer = TRUE, adjust = "mvt"
)

# Calculate predicted probabilities for each experiment
e2_predicted_probs <- estimate_means(
  e2_model,
  at = c("phase", "condition", "domain", "ev"),
  length = 100
)

# Calculate contrasts of contrasts
e2_conds_emmeans <- emmeans(
  e2_model, ~condition*phase|domain,
  regrid = "response",
  at = list(phase = c("OwnD0", "Baseline"))
)

e2_contrasts <- contrast(
  e2_conds_emmeans,
  list(
    `Baseline Risk Averse - OwnD0 Risk Averse` = c(-1, 0, 1, 0),
    `OwnD0 Risk Seeking - Baseline Risk Seeking` = c(0, 1, 0, -1)
  )
)

e2_contrast_pairs <- as.data.frame(pairs(e2_contrasts, infer = TRUE))
e2_contrast_pairs$frame <- "Money"
e2_contrast_pairs$p_sig <- with(e2_contrast_pairs, ifelse(p.value < 0.05, "*", ""))
e2_contrast_pairs$p_lab <- with(
  e2_contrast_pairs,
  cut(p.value, c(-Inf, 0.001, 0.01, 0.05, Inf), labels = c("***", "**", "*", "ns"))
)
e2_contrast_pairs$x <- 1
e2_contrast_pairs$xend <- 2
e2_contrast_pairs$y <- 0.475

# Save predicted probabilities for use in Day 3 script
day0_probs <- list(e1 = e1_predicted_probs, e2 = e2_predicted_probs)

# Combine contrast pairs
all_contrast_pairs <- rbind(e1_contrast_pairs, e2_contrast_pairs)

# Calculate custom contrasts for condition influence
# Lists to store custom contrasts
condition_influence_contrasts <- list()

# For Experiment 1 (Lives frame)
condition_influence_contrasts[[1]] <- as.data.frame(e1_contrast_pairs)
condition_influence_contrasts[[1]]$frame <- "Lives"
condition_influence_contrasts[[1]]$p_sig <- with(
  condition_influence_contrasts[[1]],
  ifelse(p.value < 0.05, "*", "")
)
condition_influence_contrasts[[1]]$p_lab <- with(
  condition_influence_contrasts[[1]],
  cut(p.value, c(-Inf, 0.001, 0.01, 0.05, Inf), labels = c("***", "**", "*", "ns"))
)

# For Experiment 2 (Money frame)
condition_influence_contrasts[[2]] <- as.data.frame(e2_contrast_pairs)
condition_influence_contrasts[[2]]$frame <- "Money"
condition_influence_contrasts[[2]]$p_sig <- with(
  condition_influence_contrasts[[2]],
  ifelse(p.value < 0.05, "*", "")
)
condition_influence_contrasts[[2]]$p_lab <- with(
  condition_influence_contrasts[[2]],
  cut(p.value, c(-Inf, 0.001, 0.01, 0.05, Inf), labels = c("***", "**", "*", "ns"))
)

# Combine condition influence contrasts
all_condition_influence <- do.call(rbind, condition_influence_contrasts)

## Combined Data ----

# Calculate model slopes
combined_slopes <- standardize(estimate_slopes(combined_model, trend = "ev", at = "domain"))

# Combined data marginal means and contrasts
combined_emmeans <- emmeans(
  combined_model, revpairwise ~ frame | phase | condition | domain,
  regrid = "response", infer = TRUE, adjust = "mvt",
  at = list(phase = "OwnD0")
)

# Combined data
combined_predicted_probs <- estimate_means(
  combined_model,
  at = c("condition", "domain", "frame", "ev"),
  length = 100
)

# Looking at frame*phase*condition interactions
combined_conds_emmeans <- emmeans(
  combined_model, ~frame*phase*condition|domain,
  regrid = "response",
  at = list(phase = c("OwnD0", "Baseline"))
)

# Contrast for phase differences by frame and condition
combined_contrasts <- contrast(
  combined_conds_emmeans,
  list(
    `Baseline Risk Averse [Lives] - OwnD0 Risk Averse [Lives]` = c(-1, 0, 1, 0, 0, 0, 0, 0),
    `Baseline Risk Averse [Money] - OwnD0 Risk Averse [Money]` = c(0, -1, 0, 1, 0, 0, 0, 0),
    `OwnD0 Risk Seeking [Lives] - Baseline Risk Seeking [Lives]` = c(0, 0, 0, 0, 1, 0, -1, 0),
    `OwnD0 Risk Seeking [Money] - Baseline Risk Seeking [Money]` = c(0, 0, 0, 0, 0, 1, 0, -1)
  )
)

# Contrast for frame differences by condition
frame_by_condition_contrast <- contrast(
  combined_contrasts,
  list(
    `Money - Lives [Risk Averse]` = c(-1, 1, 0, 0),
    `Money - Lives [Risk Seeking]` = c(0, 0, -1, 1)
  ),
  infer = TRUE
)

frame_condition_pairs <- as.data.frame(frame_by_condition_contrast)
frame_condition_pairs$condition <- with(
  frame_condition_pairs,
  str_remove_all(str_extract(contrast, "\\[.+\\]"), "\\[|\\]")
)
frame_condition_pairs$p_sig <- with(
  frame_condition_pairs,
  ifelse(p.value < 0.05, "*", "")
)
frame_condition_pairs$p_lab <- with(
  frame_condition_pairs,
  cut(p.value, c(-Inf, 0.001, 0.01, 0.05, Inf), labels = c("***", "**", "*", "ns"))
)

# Contrast for condition differences by frame
condition_by_frame_contrast <- contrast(
  combined_contrasts,
  list(
    `Risk Averse [Lives] - Risk Seeking [Lives]` = c(1, 0, -1, 0),
    `Risk Averse [Money] - Risk Seeking [Money]` = c(0, 1, 0, -1)
  ),
  infer = TRUE
)

condition_frame_pairs <- pairs(condition_by_frame_contrast, infer = TRUE)
condition_frame_pairs_df <- as.data.frame(condition_frame_pairs)
condition_frame_pairs_df$p_sig <- with(
  condition_frame_pairs_df,
  ifelse(p.value < 0.05, "*", "")
)
condition_frame_pairs_df$p_lab <- with(
  condition_frame_pairs_df,
  cut(p.value, c(-Inf, 0.001, 0.01, 0.05, Inf), labels = c("***", "**", "*", "ns"))
)
condition_frame_pairs_df$x <- 1
condition_frame_pairs_df$xend <- 2
condition_frame_pairs_df$y <- c(-0.4, 0.4)

# Domain by frame contrast at baseline
domain_frame_contrast <- emmeans(
  combined_model, ~domain*frame,
  regrid = "response",
  at = list(phase = c("Baseline"))
)

domain_frame_contrast_pairs <- contrast(
  domain_frame_contrast,
  list(
    `Gains - Losses [Lives]` = c(1, -1, 0, 0),
    `Gains - Losses [Money]` = c(0, 0, 1, -1)
  ),
  infer = TRUE
)

domain_frame_pairs <- pairs(domain_frame_contrast_pairs, infer = TRUE)

# Create Plots -----------------------------------------------------------

# Create plots for Experiment 1 (Lives)

# Probability by EV plot
# Filter and prepare data
e1_prob_data <- e1_predicted_probs

# Create the continuous plot with lines and ribbons for confidence intervals
e1_prob_plot <- ggplot(e1_prob_data, aes(x = ev, y = Probability, color = condition, fill = condition)) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_vline(xintercept = 10, linetype = "dashed") +
  geom_line(size = 1, linetype = "solid") +
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.6, color = NA) +
  scale_color_grey() +
  scale_fill_grey() +
  coord_cartesian(ylim = c(0, 1)) +
  cowplot::theme_cowplot() +
  theme(legend.position = "bottom") +
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

# Difference plot by EV
# Filter data for the specified contrast phases
e1_baseline_data <- e1_predicted_probs[e1_predicted_probs$phase == "Baseline", ]
e1_own0_data <- e1_predicted_probs[e1_predicted_probs$phase == "OwnD0", ]

# Compute phase differences
e1_diffs <- e1_own0_data
e1_diffs$predicted <- e1_own0_data$Probability - e1_baseline_data$Probability
e1_diffs$conf.low <- e1_own0_data$CI_low - e1_baseline_data$CI_low
e1_diffs$conf.high <- e1_own0_data$CI_high - e1_baseline_data$CI_high
e1_diffs$contrast <- "OwnD0 - Baseline"

# Create the difference plot
e1_prob_diffs_plot <- ggplot(e1_diffs, aes(x = ev, y = predicted, color = condition, fill = condition)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 10, linetype = "dashed") +
  geom_line(aes(linetype = condition), size = 1, color = "black") +
  scale_linetype_manual(values = c("solid", "dotted")) +
  scale_color_grey() +
  scale_fill_grey() +
  coord_cartesian(ylim = c(-0.5, 0.5)) +
  cowplot::theme_cowplot() +
  theme(legend.position = "bottom") +
  labs(
    title = "Influence by Expected Value",
    x = "Gamble Expected Value (Lives)",
    y = "Difference in Probabilities of Gambling\n(OwnD0 - Baseline)",
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

# Group probability plot
e1_emmeans_df <- as.data.frame(e1_emmeans$emmeans)

# Create the categorical plot with bars and error bars
e1_group_plot <- ggplot(e1_emmeans_df, aes(x = phase, y = prob, fill = condition)) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_col(width = 0.75, position = position_dodge(width = 0.75), color = "black", alpha = 0.8) +
  geom_errorbar(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    width = 0.1, position = position_dodge(width = 0.75)
  ) +
  scale_fill_grey() +
  coord_cartesian(ylim = c(0, 1)) +
  cowplot::theme_cowplot() +
  theme(legend.position = "bottom") +
  labs(
    title = "Gamble Acceptance by Phase",
    x = "Phase",
    y = "Probability of Choosing the Gamble Option",
    fill = "Condition"
  ) +
  facet_wrap(~domain) +
  theme(
    legend.position = "bottom",
    legend.justification = c(0.5, 0.5),
    panel.spacing = unit(1, "lines"),
    plot.title = element_text(hjust = 0.5),
    strip.background = element_rect(fill = "transparent")
  )

# Group difference plot
e1_contrasts_df <- as.data.frame(e1_emmeans$contrasts) %>%
  mutate(p_lab = cut(p.value, c(-Inf, 0.001, 0.01, 0.05, Inf), labels = c("***", "**", "*", "ns")))

# Filter contrasts for the specific phase comparison
e1_contrasts_subset <- e1_contrasts_df[grepl("OwnD0 - Baseline", e1_contrasts_df$contrast), ]

# Create the difference plot
e1_group_diffs_plot <- ggplot(e1_contrasts_subset, aes(x = condition, y = estimate, fill = condition)) +
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
  ggsignif::geom_signif(
    stat = "identity", data = e1_contrast_pairs,
    aes(x = x, xend = xend, y = y, yend = y, annotation = p_lab),
    comparisons = list("Risk Averse", "Risk Seeking"),
    inherit.aes = FALSE
  ) +
  labs(
    title = "Influence by Condition",
    subtitle = "Positive values indicate more risky choices compared to baseline\nNegative values indicate more risk averse choices compared to baseline",
    x = "Condition",
    y = "Difference in Probabilities of Gambling\n(OwnD0 - Baseline)",
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

# Create plots for Experiment 2 (Money)

# Probability by EV plot
# Filter and prepare data
e2_prob_data <- e2_predicted_probs

# Create the continuous plot with lines and ribbons for confidence intervals
e2_prob_plot <- ggplot(e2_prob_data, aes(x = ev, y = Probability, color = condition, fill = condition)) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_vline(xintercept = 10, linetype = "dashed") +
  geom_line(size = 1, linetype = "solid") +
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.6, color = NA) +
  scale_color_grey() +
  scale_fill_grey() +
  coord_cartesian(ylim = c(0, 1)) +
  cowplot::theme_cowplot() +
  theme(legend.position = "bottom") +
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

# Difference plot by EV
# Filter data for the specified contrast phases
e2_baseline_data <- e2_predicted_probs[e2_predicted_probs$phase == "Baseline", ]
e2_own0_data <- e2_predicted_probs[e2_predicted_probs$phase == "OwnD0", ]

# Compute phase differences
e2_diffs <- e2_own0_data
e2_diffs$predicted <- e2_own0_data$Probability - e2_baseline_data$Probability
e2_diffs$conf.low <- e2_own0_data$CI_low - e2_baseline_data$CI_low
e2_diffs$conf.high <- e2_own0_data$CI_high - e2_baseline_data$CI_high
e2_diffs$contrast <- "OwnD0 - Baseline"

# Create the difference plot
e2_prob_diffs_plot <- ggplot(e2_diffs, aes(x = ev, y = predicted, color = condition, fill = condition)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 10, linetype = "dashed") +
  geom_line(aes(linetype = condition), size = 1, color = "black") +
  scale_linetype_manual(values = c("solid", "dotted")) +
  scale_color_grey() +
  scale_fill_grey() +
  coord_cartesian(ylim = c(-0.5, 0.5)) +
  cowplot::theme_cowplot() +
  theme(legend.position = "bottom") +
  labs(
    title = "Influence by Expected Value",
    x = "Gamble Expected Value (Money)",
    y = "Difference in Probabilities of Gambling\n(OwnD0 - Baseline)",
    color = "Condition",
    fill = "Condition",
    linetype = "Condition"
  ) +
  facet_grid(fct_recode(contrast, `OwnD0 - Baseline` = "OwnD0 - Baseline") ~ domain) +
  theme(
    legend.position = "bottom",
    legend.justification = c(0.5, 0.5),
    panel.spacing = unit(2, "lines"),
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(color = "black"),
    strip.background = element_rect(fill = "transparent")
  )

# Group probability plot
e2_emmeans_df <- as.data.frame(e2_emmeans$emmeans)

# Create the categorical plot with bars and error bars
e2_group_plot <- ggplot(e2_emmeans_df, aes(x = phase, y = prob, fill = condition)) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_col(width = 0.75, position = position_dodge(width = 0.75), color = "black", alpha = 0.8) +
  geom_errorbar(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    width = 0.1, position = position_dodge(width = 0.75)
  ) +
  scale_fill_grey() +
  coord_cartesian(ylim = c(0, 1)) +
  cowplot::theme_cowplot() +
  theme(legend.position = "bottom") +
  labs(
    title = "Gamble Acceptance by Phase",
    x = "Phase",
    y = "Probability of Choosing the Gamble Option",
    fill = "Condition"
  ) +
  facet_wrap(~domain) +
  theme(
    legend.position = "bottom",
    legend.justification = c(0.5, 0.5),
    panel.spacing = unit(1, "lines"),
    plot.title = element_text(hjust = 0.5),
    strip.background = element_rect(fill = "transparent")
  )

# Group difference plot
e2_contrasts_df <- as.data.frame(e2_emmeans$contrasts) %>%
  mutate(p_lab = cut(p.value, c(-Inf, 0.001, 0.01, 0.05, Inf), labels = c("***", "**", "*", "ns")))

# Filter contrasts for the specific phase comparison
e2_contrasts_subset <- e2_contrasts_df[grepl("OwnD0 - Baseline", e2_contrasts_df$contrast), ]

# Create the difference plot
e2_group_diffs_plot <- ggplot(e2_contrasts_subset, aes(x = condition, y = estimate, fill = condition)) +
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
  ggsignif::geom_signif(
    stat = "identity", data = all_contrast_pairs[all_contrast_pairs$frame == "Money", ],
    aes(x = x, xend = xend, y = y, yend = y, annotation = p_lab),
    comparisons = list("Risk Averse", "Risk Seeking"),
    inherit.aes = FALSE
  ) +
  labs(
    title = "Influence by Condition",
    subtitle = "Positive values indicate more risky choices compared to baseline\nNegative values indicate more risk averse choices compared to baseline",
    x = "Condition",
    y = "Difference in Probabilities of Gambling\n(OwnD0 - Baseline)",
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

# Create plots for Combined Data

# Group probability plot
combined_emmeans_df <- as.data.frame(combined_emmeans$emmeans)

# Create the categorical plot with bars and error bars
combined_group_plot <- ggplot(combined_emmeans_df, aes(x = condition, y = prob, fill = frame)) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_col(width = 0.75, position = position_dodge(width = 0.75), color = "black", alpha = 0.8) +
  geom_errorbar(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    width = 0.1, position = position_dodge(width = 0.75)
  ) +
  scale_fill_manual(values = c("black", "white")) +
  coord_cartesian(ylim = c(0, 1)) +
  cowplot::theme_cowplot() +
  labs(
    title = "Gamble Acceptance by Domain",
    x = "Condition",
    y = "Probability of Choosing the Gamble Option (OwnD0)",
    fill = "Version"
  ) +
  facet_wrap(~domain) +
  theme(
    legend.position = "bottom",
    legend.justification = 0.5,
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(1, "lines"),
    strip.background = element_rect(fill = "transparent"),
  )

# Group difference plot
combined_contrasts_df <- as.data.frame(combined_emmeans$contrasts) %>%
  mutate(p_lab = cut(p.value, c(-Inf, 0.001, 0.01, 0.05, Inf), labels = c("***", "**", "*", "ns")))

# Filter contrasts for the specific comparison
combined_contrasts_subset <- combined_contrasts_df[grepl("Money - Lives", combined_contrasts_df$contrast), ]


# Create the difference plot
combined_group_diffs_plot <- ggplot(combined_contrasts_subset, aes(x = condition, y = estimate, fill = condition)) +
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
  labs(
    title = "Difference in Gamble Acceptance",
    subtitle = "Positive values indicate greater acceptance of gambles in monetary domain",
    x = "Condition",
    y = "Difference in Probabilities of Gambling\n(OwnD0: Money - Lives)",
    fill = "Difference"
  ) +
  facet_wrap(~domain) +
  theme(
    legend.position = "none",
    legend.justification = 0.5,
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.spacing = unit(1, "lines"),
    strip.background = element_rect(fill = "transparent"),
  )

# Plot differences in influence between conditions
condition_influence_plot <- all_condition_influence %>%
  mutate(
    estimate_abs = if_else(estimate < 0, abs(estimate), NA_real_),
    lower.CL_abs = if_else(asymp.LCL < 0, abs(asymp.LCL), NA_real_),
    upper.CL_abs = if_else(asymp.UCL < 0, abs(asymp.UCL), NA_real_),
  ) %>%
  ggplot(aes(x = frame, y = estimate, fill = frame)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_col(width = 0.35, position = position_dodge(width = 0.35), color = "black", alpha = 0.8) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.1, position = position_dodge(width = 0.35)) +
  geom_text(aes(label = p_lab, y = ifelse(estimate > 0, (asymp.UCL + 0.05), (asymp.LCL - 0.05))),
            position = position_dodge(width = 0.35)) +
  ggsignif::geom_signif(
    stat = "identity", data = condition_frame_pairs_df,
    aes(x = x, xend = xend, y = y, yend = y, annotation = p_lab),
    comparisons = list("Lives", "Money"),
    inherit.aes = FALSE
  ) +
  labs(
    title = "Relative Difference in Influence Between Conditions",
    subtitle = "Positive value indicate risk averse norms are more influential\nNegative values indicate risk seeking norms are more influential",
    x = "Domain",
    y = "Relative Influence\n(D0: Risk Averse - Risk Seeking)",
    fill = "Version"
  ) +
  coord_cartesian(ylim = c(-0.5, 0.5)) +
  scale_fill_manual(values = c("black", "white")) +
  cowplot::theme_cowplot() +
  facet_wrap(~domain) +
  theme(
    legend.position = "none",
    legend.justification = c(0.5, 0.5),
    panel.spacing = unit(1, "lines"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    strip.background = element_rect(fill = "transparent")
  )

# Plot differences in influence between domains
domain_influence_plot <- frame_condition_pairs %>%
  mutate(
    estimate_abs = if_else(estimate < 0, abs(estimate), NA_real_),
    lower.CL_abs = if_else(asymp.LCL < 0, abs(asymp.LCL), NA_real_),
    upper.CL_abs = if_else(asymp.UCL < 0, abs(asymp.UCL), NA_real_),
  ) %>%
  ggplot(aes(x = condition, y = estimate, fill = condition)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_col(width = 0.35, position = position_dodge(width = 0.35), color = "black", alpha = 0.8) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.1, position = position_dodge(width = 0.35)) +
  geom_text(aes(label = p_lab, y = ifelse(estimate > 0, (asymp.UCL + 0.05), (asymp.LCL - 0.05))),
            position = position_dodge(width = 0.35)) +
  labs(
    title = "Relative Difference in Influence Between Domains",
    subtitle = "Positive values indicate greater norm influence in monetary domain\nNegative values indicate greater norm influence in moral domain",
    x = "Condition",
    y = "Relative Influence\n(D0: Money - Lives)",
    fill = "Version"
  ) +
  coord_cartesian(ylim = c(-0.5, 0.5)) +
  scale_fill_grey() +
  cowplot::theme_cowplot() +
  facet_wrap(~domain) +
  theme(
    legend.position = "none",
    legend.justification = c(0.5, 0.5),
    panel.spacing = unit(1, "lines"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    strip.background = element_rect(fill = "transparent")
  )

# Create Figures ----------------------------------------------------------

# Define layout for figures with subplots
layout <- "
AACCC
AACCC
BBDDD
"

# Figure 2: Experiment 1 (Lives frame)
fig2 <- e1_prob_plot + e1_prob_diffs_plot + e1_group_plot + e1_group_diffs_plot +
  plot_layout(design = layout) +
  plot_annotation(tag_levels = list(c("a", "c", "b", "d")))

# Figure 4: Experiment 2 (Money frame)
fig4 <- e2_prob_plot + e2_prob_diffs_plot + e2_group_plot + e2_group_diffs_plot +
  plot_layout(design = layout) +
  plot_annotation(tag_levels = list(c("a", "c", "b", "d")))

# Figure 6: Combined analysis
fig6 <- combined_group_plot + combined_group_diffs_plot +
  condition_influence_plot + domain_influence_plot +
  plot_annotation(tag_levels = "a")

fig2
fig4
fig6

# Save figures
ggsave("output/fig2.pdf", fig2, scale = 1.5, width = 10, height = 7)
ggsave("output/fig4.pdf", fig4, scale = 1.5, width = 10, height = 7)
ggsave("output/fig6.pdf", fig6, scale = 1.5, width = 10, height = 7)
