## Prepare Study 1 looking data for manipulation and modeling
options(stringsAsFactors = FALSE)
library("lookr")
library("magrittr")
library("stringr")
library("dplyr", warn.conflicts = FALSE)
library("ggplot2")
library("yaml")
library("readr")
load("./data/study1/01_keepers.Rdata")
analysis_opts <- yaml.load_file("./analysis_options.yaml")


# Check dates of blocks
blocks <- trials %>%
  gather_attributes(c("Subj", "Basename", "DateTime")) %>%
  distinct %>%
  arrange(Subj, DateTime) %>%
  group_by(Subj) %>%
  mutate(Block = seq_along(DateTime),
         Date = as.Date(parse_datetime(DateTime))) %>%
  ungroup %>%
  select(-Basename, -DateTime) %>%
  tidyr::spread(Block, Date) %>%
  mutate(SameDate = `1` == `2`)
table(blocks$SameDate)


# We want the bias window and the modelled data window to not overlap, so we
# calculate the bias from [0, 250), and keep the looking data from [250, 1750)
# for modelling. The length of the modelled data is divisible by 3 (for
# binning).

# Use a 250 ms window for calculating the bias
analysis_opts$windows

# Prep the trial data
trials <- trials %>%
  AddAOIData %>%
  InterpolateMissingFrames %>%
  CalculateBias(window = analysis_opts$windows$bias)

# Combine all the trials together
model_times <- analysis_opts$windows$models
s1_looks <- trials %>%
  MeltLooks(other_attrs = "Bias") %>% as.tbl %>%
  select(Subj, Block = BlockNo, TrialNo, Time, GazeByImageAOI,
         Target:Unrelated, Bias) %>%
  filter(between(Time, model_times[1], model_times[2]))

# Number of frames is divisible by 3
stopifnot(n_distinct(s1_looks$Time) %% analysis_opts$bin_width == 0)

# Sneak a peek at the bias effect. Visually confirm high proportions at start of
# Target-bias trials and low proportions in other bias trials.
looks_by_bias <- s1_looks %>%
  AggregateLooks(Subj + Bias + Time ~ GazeByImageAOI) %>%
  as.tbl()

p <- qplot(data = looks_by_bias, x = Time, y = Proportion, color = Bias) +
  facet_wrap("Subj") +
  labs(title = "Looking to target [250, 1750) by subject by bias [0, 250)",
       y = "Proportion looking to target")
ggsave(plot = p, "./figs/s1_props_by_bias_by_subj.png", width = 11, height = 8)

# Confirm that trial data is correct (33 distinct word sets)
trial_types <- s1_looks %>%
  select(Target:Unrelated) %>%
  distinct %>%
  arrange(Target, SemanticFoil)
stopifnot(nrow(trial_types) == 33)

s1_looks %>% write_csv("./data/study1/02_looking_data.csv")
