## Create the data-sets used by the models for study 2
rm(list=ls())
options(stringsAsFactors = FALSE)
source("R/utils.R")
source("R/FunctionsForRWLAnalysis.R")

library("lookr")
library("magrittr")
library("stringr")
library("dplyr", warn.conflicts = FALSE)
library("lme4")
library("yaml")
library("readr")

analysis_opts <- yaml.load_file("analysis_options.yaml")
bin_width <- analysis_opts$bin_width
good_phon <- analysis_opts$targets_s2$phono
good_semy <- analysis_opts$targets_s2$semantic

# Clean up medu values, abbreviate
normalize_medu <- function(xs) {
  xs <- ifelse(xs == "4 year college degree", "College", xs)
  xs <- ifelse(xs == "", "Declined", xs)
  substr(xs, 1, 4)
}

# Look-up list for abbreviated medu values
medu_codes <- c(
  Less = "low",
  GED  = "low",
  High = "low",
  Some = "middle",
  Tech = "middle",
  Trad = "middle",
  Coll = "high",
  Grad = "high",
  Decl = "decl")

# Load the looks
s2_looks <- read_csv("data/study2/01_looking_data.csv")







# Load vocab scores and demographics. Clean up medu labels
s2_infos <- read_csv("data/study2/00_child_info.csv") %>%
  mutate(Dialect = factor(AAE, levels = c(0, 1), labels = c("MAE", "AAE")),
         medu = normalize_medu(medu)) %>%
  rename(Subj = Participant_ID) %>%
  select(-householdIncome, -pcedu) %>%
  rename(EVT_standard = EVT_Standard, PPVT_standard = PPVT_Standard)

# Convert medu types to low-mid-high levels
s2_infos$medu <- medu_codes[s2_infos$medu]

s2_binned <- standard_model_pipeline(s2_looks, bins = bin_width, draw = TRUE)
stopifnot(setdiff(s2_binned$Subj, s2_infos$Subj) == 0)

s2 <- make_model_data(s2_binned, s2_infos)
write_csv(s2, "data/study2/02_analysis1.csv")




## Bias analyses --------------------------------------------------------------

# Get the log-odds of viewing target for each trial-onset bias
s2_bias_dfs <- compute_bias_growth_curves(
  looks = s2_looks,
  infos = s2_infos,
  bins = 3,
  phon_set = good_phon,
  semy_set = good_semy)

# Visualize the bias curves

library("ggplot2")
p_four <- ggplot(data = s2_bias_dfs$fourway_bias) +
  aes(x = Time, y = plogis(elogit), color = Bias) +
  stat_summary(fun.data = "mean_se", geom = "pointrange")
p_four

# Target vs any distractor
p_four %+% s2_bias_dfs$binary_bias

# Make just one dataframe
s2_all_bias <- bind_rows(s2_bias_dfs)

# Confirm factor coding
s2_all_bias %>%
  select(BiasSet, Bias) %>%
  distinct

# Count up the trials in each bias
s2_all_bias %>%
  group_by(BiasSet, Bias) %>%
  summarise(mean(Trials), min(Trials), max(Trials)) %>%
  ungroup

n_distinct(s2_all_bias$Subj)

s2_all_bias %>%
  write_csv("data/study2/02_analysis2.csv")



#### Logistic regression models -----------------------------------------------

# Generate the datasets for phonological vs unrelated comparisons
max_phon_time <- analysis_opts$convergence$s2$phono

count_targets <- . %>%
  select(Block, Target) %>%
  distinct %>%
  count(Block)

# These analyses were run only on trials for which there was not a bias to look
# to the target at word onset
s2_not_to_target <- s2_looks %>%
  filter(Bias %in% c("SemanticFoil", "PhonologicalFoil", NA, "Unrelated"))

# Keep just trials where there was an effective phonological foil
s2_phon_looks <- s2_not_to_target %>%
  filter(Target %in% good_phon)

# Only 13 targets have a good phonological foil
trial_counts <- count_targets(s2_phon_looks)
trial_counts
stopifnot(trial_counts$n == c(13, 13))

# Verify convergence point in the raw data
target_vs_phon <- s2_phon_looks %>%
  standard_model_pipeline(bins = bin_width, flip = TRUE) %>%
  mutate(ToTarget = ToTarget / (ToTarget + ToPhonological),
         Comparison = "T/(T+P)")

target_vs_unre <- s2_phon_looks %>%
  standard_model_pipeline(bins = bin_width, flip = TRUE) %>%
  mutate(ToTarget = ToTarget / (ToTarget + ToUnrelated),
         Comparison = "T/(T+U)")

tp_tu <- bind_rows(target_vs_phon, target_vs_unre)
ggplot(tp_tu) +
  aes(x = Time, y = ToTarget, color = Comparison) +
  stat_summary(fun.data = mean_se, geom = "pointrange")

# Aggregate looking data before convergence point
s2_phon <- s2_phon_looks %>%
  standard_model_pipeline(bins = bin_width, flip = TRUE, max_bin_time = max_phon_time) %>%
  make_model_data(s2_infos)
range(s2_phon$Time)

# Dataset with the full time window in case whole trial needs to be plotted
s2_phon_for_plotting <- s2_phon_looks %>%
  standard_model_pipeline(bins = bin_width, flip = TRUE) %>%
  make_model_data(s2_infos)

# Semantic vs unrelated comparison
s2_semy_looks <- s2_not_to_target %>%
  filter(Target %in% good_semy)

# Confirm trial counts
trial_counts <- count_targets(s2_semy_looks)
trial_counts
stopifnot(trial_counts$n == c(13, 13))

# Reduce for modelling
s2_semy <- s2_semy_looks %>%
  standard_model_pipeline(bins = bin_width, flip = TRUE) %>%
  make_model_data(s2_infos)

write_csv(s2_phon, "data/study2/03_logistic_phon.csv")
write_csv(s2_semy, "data/study2/03_logistic_semy.csv")
write_csv(s2_phon_for_plotting, "data/study2/03_logistic_phon_whole_window.csv")

