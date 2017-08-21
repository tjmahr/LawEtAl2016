## Note: All 37 subjects have EVT scores. Only 36 subjects have PPVT scores.

## I use "semy" as short for "semantic" (modeled after "polysemy").

options(stringsAsFactors = FALSE)
source("./R/FunctionsForRWLAnalysis.R")

analysis_opts <- yaml.load_file("./analysis_options.yaml")
bin_width <- analysis_opts$bin_width
good_phon <- analysis_opts$targets_s1$phono
good_semy <- analysis_opts$targets_s1$semantic

# Load the looks and the vocab scores
s1_looks <- read_csv("./data/study1/02_looking_data.csv")
s1_infos <- read_csv("./data/study1/01_test_scores.csv") %>%
  rename(Subj = ParticipantID) %>%
  select(-Income, -medu)

# Confirm 33 targets
s1_looks %>% select(Target) %>% distinct %>% unlist(use.names = FALSE) %>% sort
s1_looks %>% select(Target) %>% distinct %>% nrow

# Confirm other counts
length(good_phon)
length(good_semy)

# Print descriptives by EVT thirds
s1_infos %>%
  mutate(EVTThirds = cut_by_thirds(EVT_GSV)) %>%
  select(-contains("PPVT", -EVT_raw)) %>%
  # Make a long table of scores
  gather(Variable, value, -Subj, -AAE, -female, -EVTThirds) %>%
  # For each test and third, give mean and range
  group_by(EVTThirds, Variable) %>%
  summarise(n = n(), mean = mean(value), min = min(value), max = max(value))

# 22% missing data
AggregateLooks(s1_looks, 1 ~ GazeByImageAOI)

# 2442 trials
n_trials <- s1_looks %>% select(Subj, Block, TrialNo) %>%  distinct %>% nrow

# Find trials to drop (with more than 50% missing data)
bad_trials <- find_mistracked_trials(s1_looks, analysis_opts$excessive_na)

# 21% bad trials
nrow(bad_trials) / n_trials

# 7% blank
n_blank <- bad_trials %>% filter(PropNA == 1) %>% nrow
n_blank / n_trials

# missingness not related to EVT or age
missing_by_kid <- AggregateLooks(s1_looks, Subj ~ GazeByImageAOI)
d_missing <- inner_join(missing_by_kid, s1_infos)
summary(lm(PropNA ~ Age, d_missing))
summary(lm(PropNA ~ EVT_GSV, d_missing))

# odds having a trial dropped not related to EVT or age
trial_outcomes <- s1_looks %>%
  AggregateLooks(Subj + Block + TrialNo ~ GazeByImageAOI) %>%
  mutate(Keeper = PropNA < analysis_opts$excessive_na) %>%
  group_by(Subj) %>%
  summarise(nKeeper = sum(Keeper), nDropper = sum(!Keeper))
d_outcomes <- inner_join(trial_outcomes, s1_infos)
summary(glm(cbind(nKeeper, nDropper) ~ Age, family = binomial, d_outcomes))
summary(glm(cbind(nKeeper, nDropper) ~ EVT_GSV, family = binomial, d_outcomes))

# Save normal set of trials
s1_binned <- s1_looks %>%
  standard_model_pipeline(bins = bin_width, draw_ot = TRUE) %>%
  make_model_data(s1_infos)
# write_csv(s1_binned, "./data/study1/modelling/all_trials/01_gca.csv")

# Save trimmed set of trials
s1_trimmed_looks <- s1_looks %>% anti_join(bad_trials)
s1_trimmed <- s1_trimmed_looks %>%
  standard_model_pipeline(bins = bin_width, draw_ot = TRUE) %>%
  make_model_data(s1_infos)
write_csv(s1_trimmed, "./data/study1/modelling/01_gca.csv")

# Visually inspect trimming effects
s1_compared <- bind_rows(
    mutate(s1_trimmed, Dataset = "Trimmed"),
    mutate(s1_binned, Dataset = "Not Trimmed"))

p <- ggplot(s1_compared) + aes(x = Time, y = Proportion, color = Dataset) +
  stat_summary(fun.data = mean_se, geom = "pointrange") +
  facet_grid(.~ EVTBreakThirds) +
  labs(title = "Trimming trials with 50%+ missing looks does nothing (S1, by EVT)")
p
ggsave("./figs/s1_props_with_trial_trimming.png", p, width = 7, height = 6)




#### Looking to target vs looking to distractors ------------------------------

# Above analysis collapsed across all trials. Is there a difference when the
# child is looking to the target vs. one of the distractors at target word
# onset?

# Create two bias dataframes (targ vs dist, target vs phon vs semy vs unre) for
# each dataset (trimmed vs non)
s1_biases <- compute_bias_growth_curves(
  looks = s1_looks,
  infos = s1_infos,
  bins = bin_width,
  phon_set = good_phon,
  semy_set = good_semy)

s1_biases_trimmed <- compute_bias_growth_curves(
  looks = s1_trimmed_looks,
  infos = s1_infos,
  bins = bin_width,
  phon_set = good_phon,
  semy_set = good_semy)

s1_compare_bias <- bind_rows(
  mutate(s1_biases_trimmed$fourway_bias, Trim = TRUE),
  mutate(s1_biases_trimmed$binary_bias, Trim = TRUE),
  mutate(s1_biases$fourway_bias, Trim = FALSE),
  mutate(s1_biases$binary_bias, Trim = FALSE)
)

# Count up the trials in data-set
s1_compare_bias %>%
  group_by(BiasSet, Trim, Bias) %>%
  summarise(min(Trials), mean(Trials), max(Trials))

# Trial counts follow what we might expect (there's about 2-3 times as many
# distractor-bias trials than target-bias trials)
s1_compare_bias %>%
  group_by(BiasSet, Trim, Bias) %>%
  summarise(mean(Trials), min(Trials), max(Trials)) %>%
  ungroup

s1_biases_trimmed$binary_bias %>%
  write_csv("./data/study1/modelling/02_binary_bias.csv")
s1_biases_trimmed$fourway_bias %>%
  write_csv("./data/study1/modelling/02_fourway_bias.csv")

# s1_biases$binary_bias %>%
#   write_csv("./data/study1/modelling/all_trials/02_binary_bias.csv")
# s1_biases$fourway_bias %>%
  # write_csv("./data/study1/modelling/all_trials/02_fourway_bias.csv")




#### Logistic regression models -----------------------------------------------

max_phon_time <- analysis_opts$convergence$s1$phono

count_targets <- . %>%
  select(Block, Target) %>%
  distinct %>%
  count(Block)

# "These analyses were run only on trials for which there was not a bias to look
# to the target at word onset."
s1_not_to_target <- s1_looks %>%
  filter(Bias %in% c("SemanticFoil", "PhonologicalFoil", NA, "Unrelated"))

# "We included only the subset of trials in which there was a same-onset
# phonological foil (n = 42 trials, 21 per block)."
s1_phon_looks <- s1_not_to_target %>%
  filter(Target %in% good_phon)

# Confirm above counts
trial_counts <- count_targets(s1_phon_looks)
trial_counts
stopifnot(trial_counts$n == c(21, 21))

# "For the semantic vs. unrelated analysis, all bins from 250 to 1,750 ms were
# used, as the two curves did not converge."

# "including only the subset of trials in which there was a
# same-category semantic foil (n = 32 trials, 16 per block)."
s1_semy_looks <- s1_not_to_target %>%
  filter(Target %in% good_semy)

# Confirm trial counts
trial_counts <- count_targets(s1_semy_looks)
trial_counts
stopifnot(trial_counts$n == c(16, 16))

# Reduce for modelling
s1_semy <- s1_semy_looks %>%
  standard_model_pipeline(bins = bin_width, flip = TRUE) %>%
  make_model_data(s1_infos)

# "These analyses were conducted using time bins starting 250 ms after word
# onset and ending at the point at which the curves converged. For the
# phonological vs. unrelated foils, this point was 1,050 ms."
s1_phon <- s1_phon_looks %>%
  standard_model_pipeline(bins = bin_width, flip = TRUE, max_bin_time = max_phon_time) %>%
  make_model_data(s1_infos)

s1_semy_trimmed <- s1_semy_looks %>%
  anti_join(bad_trials) %>%
  standard_model_pipeline(bins = bin_width, flip = TRUE) %>%
  make_model_data(s1_infos)

s1_phon_trimmed <- s1_phon_looks %>%
  anti_join(bad_trials) %>%
  standard_model_pipeline(bins = bin_width, flip = TRUE, max_bin_time = max_phon_time, draw = TRUE) %>%
  make_model_data(s1_infos)

# Dataset with the full time window in case whole trial needs to be plotted
s1_phon_for_plotting <- s1_phon_looks %>%
  standard_model_pipeline(bins = bin_width, flip = TRUE) %>%
  make_model_data(s1_infos)

s1_phon_for_plotting_trimmed <- s1_phon_looks %>%
  anti_join(bad_trials) %>%
  standard_model_pipeline(bins = bin_width, flip = TRUE) %>%
  make_model_data(s1_infos)

# write_csv(s1_phon, "./data/study1/modelling/all_trials/03_logistic_phon.csv")
# write_csv(s1_semy, "./data/study1/modelling/all_trials/03_logistic_semy.csv")

write_csv(s1_phon_trimmed, "./data/study1/modelling/03_logistic_phon.csv")
write_csv(s1_semy_trimmed, "./data/study1/modelling/03_logistic_semy.csv")

# write_csv(s1_phon_for_plotting, "./data/study1/modelling/all_trials/03_logistic_phon_whole_window.csv")
write_csv(s1_phon_for_plotting_trimmed, "./data/study1/modelling/03_logistic_phon_whole_window.csv")





s1_phon_to_phon <- s1_phon %>%
  mutate(elog = empirical_logit(ToTarget, ToPhonological),
         Comparison = "Target vs Phon")
s1_phon_to_unre <- s1_phon %>%
  mutate(elog = empirical_logit(ToTarget, ToUnrelated),
         Comparison = "Target vs Unre")

phon_compare_curves <- bind_rows(s1_phon_to_phon, s1_phon_to_unre) %>%
  mutate(Trials = "All")


s1_semy_to_semy <- s1_semy %>%
  mutate(elog = empirical_logit(ToTarget, ToSemantic),
         Comparison = "Target vs Semy")
s1_semy_to_unre <- s1_semy %>%
  mutate(elog = empirical_logit(ToTarget, ToUnrelated),
         Comparison = "Target vs Unre")

semy_compare_curves <- bind_rows(s1_semy_to_semy, s1_semy_to_unre) %>%
  mutate(Trials = "All")

ggplot(phon_compare_curves) +
  aes(x = Time, y = elog, color = Comparison) +
  stat_summary(fun.data = mean_se, geom = "pointrange")

last_plot() %+% semy_compare_curves


# On the trimmed trials
s1_phon_to_phon <- s1_phon_trimmed %>%
  mutate(elog = empirical_logit(ToTarget, ToPhonological),
         Comparison = "Target vs Phon")
s1_phon_to_unre <- s1_phon_trimmed %>%
  mutate(elog = empirical_logit(ToTarget, ToUnrelated),
         Comparison = "Target vs Unre")
phon_compare_curves2 <- bind_rows(s1_phon_to_phon, s1_phon_to_unre) %>%
  mutate(Trials = "Trimmed")

s1_semy_to_semy <- s1_semy_trimmed %>%
  mutate(elog = empirical_logit(ToTarget, ToSemantic),
         Comparison = "Target vs Semy")
s1_semy_to_unre <- s1_semy_trimmed %>%
  mutate(elog = empirical_logit(ToTarget, ToUnrelated),
         Comparison = "Target vs Unre")
semy_compare_curves2 <- bind_rows(s1_semy_to_semy, s1_semy_to_unre) %>%
  mutate(Trials = "Trimmed")

ggplot(phon_compare_curves2) +
  aes(x = Time, y = elog, color = Comparison) +
  stat_summary(fun.data = mean_se, geom = "pointrange")

last_plot() %+% semy_compare_curves2

# Visualize the effect of trimming trials
trimmed_compare <- bind_rows(phon_compare_curves, phon_compare_curves2)

ggplot(trimmed_compare) +
  aes(x = Time, y = elog, color = Comparison, linetype = Trials) +
  stat_summary(fun.data = mean_se, geom = "pointrange") +
  stat_summary(fun.y = mean, geom = "line")
