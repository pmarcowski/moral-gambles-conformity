# Risk Norms Data Preparation
#
# This script loads raw data from two experiments, performs data cleaning,
# applies exclusion criteria, and saves the processed datasets for analysis.

# Load packages
library(tidyverse)
library(haven)

# Define factor levels
TIME_LEVELS <- c("Dayone", "Followup")
PHASE_LEVELS <- c("Baseline", "OwnD0", "OwnD3", "Others")
CONDITION_LEVELS <- c("Risk Averse", "Risk Seeking")
DOMAIN_LEVELS <- c("Gains", "Losses")
FRAME_LEVELS <- c("Lives", "Money")

# Process Experiment 1 data -----------------------------------------------

# List of participants to exclude based on low-variance responses
e1_low_variance_exclusions <- c(
  "G-H-_e7z9i2jzh", "G-L-_fljodvudf",
  "L-H-_d6kg1dmdi", "L-H-_o6jpy1jof",
  "L-L-_appqo1dlu", "L-L-_rwgwuh993",
  "L-L-_t8o1cz7vn"
)

# Load and prepare day one data for Experiment 1
e1_day_one <- read_csv("data/raw/experiment1_day0.csv") %>%
  rename("id" = "uniqueCode", "influence_score" = "influenceScore") %>%
  mutate(
    time = "Dayone",
    condition = case_when(
      condition == "low" ~ "Risk Averse",
      condition == "high" ~ "Risk Seeking"
    ),
    domain = case_when(
      version == "save" ~ "Gains",
      version == "loss" ~ "Losses"
    ),
    phase = case_when(
      !is.na(outcome_base) ~ "Baseline",
      !is.na(outcome_learning) ~ "Learning",
      !is.na(outcome_transfer) ~ "OwnD0"
    ),
    gender = case_when(
      gender == "M" ~ "Male",
      gender == "F" ~ "Female",
      TRUE ~ gender
    ),
    across(where(is.character), ~if_else(is.na(.x), "nonspec", .x)),
    choice = coalesce(outcome_base, outcome_learning, outcome_transfer),
    ev = coalesce(gamble_value_base, gamble_value_learning, gamble_value_transfer) / 2
  ) %>%
  group_by(id) %>%
  mutate(trial = 1:n()) %>%
  ungroup() %>%
  select(
    id, influence_score, time, gender, age, domain, condition, phase,
    trial, ev, choice,
    comprehensionCorrectPercentage, gamble_value_base
  )

# Load and prepare follow-up data for Experiment 1
e1_followup <- read_csv("data/raw/experiment1_day3.csv") %>%
  rename(
    "id" = "uniqueCode",
    "condition" = "version",
    "version" = "condition"
  ) %>%
  mutate(
    time = "Followup",
    condition = case_when(
      condition == "low" ~ "Risk Averse",
      condition == "high" ~ "Risk Seeking"
    ),
    domain = case_when(
      version == "save" ~ "Gains",
      version == "loss" ~ "Losses"
    ),
    phase = case_when(
      !is.na(outcome_base) ~ "OwnD3",
      !is.na(outcome_learning) ~ "Others"
    ),
    choice = coalesce(outcome_base, outcome_learning),
    ev = coalesce(gamble_value_base, gamble_value_learning) / 2,
    across(where(is.character), ~if_else(is.na(.x), "nonspec", .x))
  ) %>%
  group_by(id) %>%
  mutate(trial = 1:n()) %>%
  ungroup() %>%
  select(id, time, domain, condition, phase, trial, ev, choice,
         comprehensionCorrectPercentage, gamble_value_base)

# Combine day one and follow-up data for Experiment 1
e1_combined <- bind_rows(e1_day_one, e1_followup) %>%
  mutate(frame = "Lives") %>%
  group_by(id) %>%
  fill(gender, age, influence_score, .direction = "down") %>% # fill gender and age
  ungroup() %>%
  distinct()

# Apply exclusion criteria (Experiment 1) --------------------------------
# 1. Exclude participants who completed only follow-up but not day one
extra_followup_ids <- setdiff(
  unique(e1_combined$id[e1_combined$time == "Followup"]),
  unique(e1_combined$id[e1_combined$time == "Dayone"])
)
e1_filtered <- e1_combined[!e1_combined$id %in% extra_followup_ids, ]

# 2. Exclude participants who failed comprehension checks on day one
participants_day_one <- n_distinct(e1_filtered$id[e1_filtered$time == "Dayone"])
passed_checks_day_one <- unique(
  e1_filtered$id[e1_filtered$comprehensionCorrectPercentage > 50 &
                   e1_filtered$time == "Dayone"]
)
failed_checks_day_one <- participants_day_one - length(passed_checks_day_one)
e1_filtered <- e1_filtered[e1_filtered$id %in% passed_checks_day_one, ]

# 3. Exclude participants with low-variance responses
e1_filtered <- e1_filtered[!e1_filtered$id %in% e1_low_variance_exclusions, ]

# 4. Exclude participants who failed comprehension checks on follow-up
participants_followup <- n_distinct(e1_filtered$id[e1_filtered$time == "Followup"])
passed_checks_followup <- unique(
  e1_filtered$id[e1_filtered$comprehensionCorrectPercentage > 50 &
                   e1_filtered$time == "Followup"]
)
failed_checks_followup <- participants_followup - length(passed_checks_followup)
e1_filtered <- e1_filtered[e1_filtered$id %in% passed_checks_followup, ]

# 5. Exclude participants who completed day one but not follow-up
incomplete_participants <- setdiff(
  unique(e1_filtered$id[e1_filtered$time == "Dayone"]),
  unique(e1_filtered$id[e1_filtered$time == "Followup"])
)
e1_filtered <- e1_filtered[!e1_filtered$id %in% incomplete_participants, ]

# Process experiment 2 data -----------------------------------------------

# List of participants to exclude based on low-variance responses
e2_low_variance_exclusions <- c("L-H-_rwnskks2p")

# Load and prepare day one data for experiment 2
e2_day_one <- read_csv("data/raw/experiment2_day0.csv") %>%
  rename("id" = "uniqueCode", "influence_score" = "influenceScore") %>%
  mutate(
    time = "Dayone",
    condition = case_when(
      condition == "low" ~ "Risk Averse",
      condition == "high" ~ "Risk Seeking"
    ),
    domain = case_when(
      version == "gain" ~ "Gains",
      version == "loss" ~ "Losses"
    ),
    phase = case_when(
      !is.na(outcome_base) ~ "Baseline",
      !is.na(outcome_learning) ~ "Learning",
      !is.na(outcome_transfer) ~ "OwnD0"
    ),
    gender = case_when(
      gender == "male" ~ "Male",
      gender == "female" ~ "Female",
      TRUE ~ gender
    ),
    across(where(is.character), ~if_else(is.na(.x), "nonspec", .x)),
    choice = coalesce(outcome_base, outcome_learning, outcome_transfer),
    ev = coalesce(gamble_value_base, gamble_value_learning, gamble_value_transfer) / 2
  ) %>%
  group_by(id) %>%
  mutate(trial = 1:n()) %>%
  ungroup() %>%
  select(
    id, influence_score, time, gender, age, domain, condition, phase,
    trial, ev, choice,
    comprehensionCorrectPercentage, gamble_value_base
  )

# Load and prepare follow-up data for experiment 2
e2_followup <- read_csv("data/raw/experiment2_day3.csv") %>%
  rename("id" = "uniqueCode") %>%
  left_join(unique(e2_day_one[, c("id", "domain", "condition")])) %>%
  mutate(
    time = "Followup",
    phase = case_when(
      !is.na(outcome_base) ~ "OwnD3",
      !is.na(outcome_learning) ~ "Others"
    ),
    across(where(is.character), ~if_else(is.na(.x), "nonspec", .x)),
    choice = coalesce(outcome_base, outcome_learning),
    ev = coalesce(gamble_value_base, gamble_value_learning) / 2
  ) %>%
  group_by(id) %>%
  mutate(trial = 1:n()) %>%
  ungroup() %>%
  select(id, time, domain, condition, phase, trial, ev, choice,
         comprehensionCorrectPercentage, gamble_value_base)

# Combine day one and follow-up data for experiment 2
e2_combined <- bind_rows(e2_day_one, e2_followup) %>%
  mutate(frame = "Money") %>%
  group_by(id) %>%
  fill(gender, age, influence_score, .direction = "down") %>% # fill gender and age
  ungroup() %>%
  distinct()

# Apply exclusion criteria (experiment 2) --------------------------------
# 1. Exclude participants who completed only follow-up but not day one
extra_followup_ids <- setdiff(
  unique(e2_combined$id[e2_combined$time == "Followup"]),
  unique(e2_combined$id[e2_combined$time == "Dayone"])
)
e2_filtered <- e2_combined[!e2_combined$id %in% extra_followup_ids, ]

# 2. Exclude participants who failed comprehension checks on day one
participants_day_one <- n_distinct(e2_filtered$id[e2_filtered$time == "Dayone"])
passed_checks_day_one <- unique(
  e2_filtered$id[e2_filtered$comprehensionCorrectPercentage > 50 &
                   e2_filtered$time == "Dayone"]
)
failed_checks_day_one <- participants_day_one - length(passed_checks_day_one)
e2_filtered <- e2_filtered[e2_filtered$id %in% passed_checks_day_one, ]

# 3. Exclude participants with low-variance responses
e2_filtered <- e2_filtered[!e2_filtered$id %in% e2_low_variance_exclusions, ]

# 4. Exclude participants who failed comprehension checks on follow-up
participants_followup <- n_distinct(e2_filtered$id[e2_filtered$time == "Followup"])
passed_checks_followup <- unique(
  e2_filtered$id[e2_filtered$comprehensionCorrectPercentage > 50 &
                   e2_filtered$time == "Followup"]
)
failed_checks_followup <- participants_followup - length(passed_checks_followup)
e2_filtered <- e2_filtered[e2_filtered$id %in% passed_checks_followup, ]

# 5. Exclude participants who completed day one but not follow-up
incomplete_participants <- setdiff(
  unique(e2_filtered$id[e2_filtered$time == "Dayone"]),
  unique(e2_filtered$id[e2_filtered$time == "Followup"])
)
e2_filtered <- e2_filtered[!e2_filtered$id %in% incomplete_participants, ]

# Save the final processed datasets
saveRDS(e1_filtered, "data/prepared/experiment1.Rds")
saveRDS(e2_filtered, "data/prepared/experiment2.Rds")
