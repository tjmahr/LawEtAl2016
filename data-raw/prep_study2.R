## Load and narrow down the raw looking data

options(stringsAsFactors = FALSE)
library("lookr")
library("magrittr")
library("stringr")
library("dplyr", warn.conflicts = FALSE)
library("yaml")
library("readr")
analysis_opts <- yaml.load_file("./analysis_options.yaml")

model_times <- analysis_opts$windows$models

load("./data-raw/study2/00_raw_data.Rdata")

# list of matched subjects
matches <- read_csv("./data-raw/s2_matching_info.csv")

# Make functions for filtering out trials with certain attributes
exclude_subj <- MakeAttributeFilter(attr_name = "Subj")
exclude_subject <- MakeAttributeFilter(attr_name = "Subject")
exclude_basename <- MakeAttributeFilter(attr_name = "Basename")

# Check for discrepancies between the looking data and participant data
all_subj <- s2_trials %@% "Subj" %>% unique %>% sort
no_looks <- setdiff(matches$Participant_ID, all_subj)
no_infos <- setdiff(all_subj, matches$Participant_ID)

# Check for timepoint 2 stimuli
df <- data_frame(
  Subject = s2_trials %@% "Subject",
  Dialect = s2_trials %@% "Dialect",
  Basename = s2_trials %@% "Basename",
  Target = s2_trials %@% "Target",
  Audio = s2_trials %@% "Audio",
  TargetDur = s2_trials %@% "TargetDur")

# The duration of "spoon" should be 567ms or 816ms, depending on dialect
t2_stim <- df %>% filter(Dialect == "AAE", Target == "spoon", TargetDur != 567)
stopifnot(nrow(t2_stim) == 0)
t2_stim <- df %>% filter(Dialect == "SAE", Target == "spoon", TargetDur != 816)
stopifnot(nrow(t2_stim) == 0)

# Check IDs with ages greater than 60 months
too_old <- s2_trials %@% "Subject" %>%
  str_extract("\\d{3}\\w\\d{2}") %>%
  str_extract("\\d{2}$") %>%
  as.numeric %>%
  is_greater_than(60) %>%
  extract(s2_trials %@% "Subject", .)
stopifnot(length(too_old) == 0)

# Interpolate missing looks then calculate missing data over analysis window
na_window <- analysis_opts$windows$prop_na
s2_trials <- s2_trials %>%
  AdjustTimes %>%
  AddAOIData %>%
  InterpolateMissingFrames


# Run a couple checks on the unfiltered data-set...

# Images onscreen for 1500 ms before doing the wait-for-fixation checks
s2_trials %@% "FixationOnset" - s2_trials %@% "ImageOnset"

# Look at how long the wait-for-fixation windows lasted.
hist(x = s2_trials %@% "CarrierOnset" - s2_trials %@% "FixationOnset",
     breaks = 50,
     main = "Lengths of gaze contingency check",
     xlab = "Duration of gaze contingency checks (ms)",
     ylab = "Num. Trials")


# Need to verify the information used in the recruitment paragraphs
blocks_by_dialect_by_study <- s2_trials %>%
  gather_attributes(c("Subj", "Dialect", "Basename")) %>%
  distinct %>%
  mutate(StudyPool = str_extract(Subj, "[DL]"))

# Assert just one dialect per child
kid_with_both_dialects <- blocks_by_dialect_by_study %>%
  select(Dialect, Subj) %>%
  distinct %>%
  count(Dialect, Subj) %>%
  filter(n != 1)
stopifnot(nrow(kid_with_both_dialects) == 0)

children_per_dialect_per_pool <- blocks_by_dialect_by_study %>%
  select(StudyPool, Subj, Dialect) %>%
  distinct %>%
  count(StudyPool, Dialect) %>%
  rename(NumChildren = n)
children_per_dialect_per_pool

blocks_per_child <- blocks_by_dialect_by_study %>%
  group_by(StudyPool, Dialect, Subj) %>%
  summarise(NumBlocks = n())

# Which longitudinal child contributed just one block? (Need to check why other
# is not in dataset.)
blocks_per_child %>% filter(NumBlocks == 1, StudyPool == "L")

# Count children per num blocks contributed per dialect per study pool
kids_per_block_count <- blocks_per_child %>%
  group_by(StudyPool, Dialect, NumBlocks) %>%
  summarise(NumChildren = n())
kids_per_block_count

# Wide version of above data-frame
wide_kids_per_block_count <- kids_per_block_count %>%
  ungroup %>%
  tidyr::spread(NumBlocks, NumChildren) %>%
  rename(x1 = `1`, x2 = `2`) %>%
  # Replace NAs with 0s
  mutate(x1 = ifelse(is.na(x1), 0, x1),
         x2 = ifelse(is.na(x2), 0, x2)) %>%
  rename(Kids_w_1_Block = x1, Kids_w_2_Blocks = x2)
wide_kids_per_block_count


# Create a worksheet of information to check
blocks_by_date <- s2_trials %>%
  gather_attributes(c("Subj", "Dialect", "Basename", "DateTime")) %>%
  distinct %>%
  mutate(StudyPool = str_extract(Subj, "[DL]"),
         BlockNo = str_extract(Basename, "Block\\d")) %>%
  select(StudyPool, Dialect, Subj, BlockNo, DateTime) %>%
  tidyr::spread(BlockNo, DateTime) %T>%
  write_csv("./data/study2/blocks_by_date.csv")




# Create a long data-frame of looking data so we can aggregate and compute
# proportion of missing data per block
long_looks <- s2_trials %>%
  TimeSlice(na_window[1], na_window[2]) %>%
  MeltLooks %>%
  as.tbl

block_summary <- long_looks %>%
  AggregateLooks(Subj + Basename + DateTime ~ GazeByImageAOI) %>%
  mutate(PropNA = round(PropNA, 4)) %>%
  select(Subj:DateTime, Frames_NA = NAs, Frames_Total = Looks, PropNA) %>%
  rename(PropMissingLooks = PropNA)

# Number the blocks by presentation order within child
block_summary <- block_summary %>%
  group_by(Subj) %>%
  mutate(Order = rank(DateTime)) %>%
  ungroup

# assert max of two blocks within subj
stopifnot(range(block_summary$Order) == 1:2)

block_summary <- block_summary %>%
  mutate(BlockChron = ifelse(Order == 1, "First", "Second"))

# Which blocks have excessive missing data
bad_blocks <- block_summary %>%
  filter(analysis_opts$excessive_na <= PropMissingLooks)

# Determine number of bad trials
trial_summary <- long_looks %>%
  AggregateLooks(Subj + Basename + TrialNo ~ GazeByImageAOI) %>%
  select(Subj:TrialNo, Frames_NA = NAs, Frames_Total = Looks, PropNA) %>%
  rename(PropMissingLooks = PropNA)

# Which blocks have 50% or more bad trials
usable_trial_summary <- trial_summary %>%
  mutate(BadTrial = analysis_opts$excessive_na <= PropMissingLooks) %>%
  group_by(Subj, Basename) %>%
  summarise(PropBadTrials = round(mean(BadTrial), 4)) %>%
  ungroup

blocks_with_bad_trials <- usable_trial_summary %>%
  filter(analysis_opts$excessive_na <= PropBadTrials)

# Which blocks have too high averages or too few good trials
blocks_to_drop <- left_join(blocks_with_bad_trials, block_summary) %>%
  full_join(bad_blocks) %>%
  select(Subj, Basename, DateTime, PropMissingLooks, PropBadTrials, BlockChron)
blocks_to_drop

blocks_to_drop %>%
  write_csv("./data/study2/block_summaries/dropped_bc_missing_data.csv")

# Exclude them
good_blocks <- anti_join(block_summary, blocks_to_drop)

# Choose the first chronological good block within subj
first_good_blocks <- good_blocks %>%
  group_by(Subj) %>%
  # Negate order nums so ranking works (so 1 < 2 becomes -1 > -2)
  top_n(1, -Order) %>%
  ungroup %>%
  arrange(Subj) %>%
  select(Subj:DateTime, PropMissingLooks, BlockChron)

# Assert that kids with a better second block had their first block dropped
had_to_use_second_block <- first_good_blocks %>%
  filter(BlockChron == "Second")
stopifnot(is.element(had_to_use_second_block$Subj, blocks_to_drop$Subj))

# Attach the usable trials summary stats
first_good_blocks <- first_good_blocks %>%
  left_join(usable_trial_summary) %>%
  select(Subj:DateTime, BlockChron, everything())
first_good_blocks %>%
  write_csv("./data/study2/block_summaries/used_for_models.csv")

second_blocks <- anti_join(good_blocks, first_good_blocks) %>%
  left_join(usable_trial_summary) %>%
  select(Subj:DateTime, BlockChron, PropMissingLooks, PropBadTrials)
second_blocks %>%
  write_csv("./data/study2/block_summaries/dropped_bc_not_first_block_child_saw.csv")

# Next we exclude the blocks we are not using from the set of Trials
blocks_we_wont_use <- block_summary %>%
  anti_join(first_good_blocks, "Basename") %>%
  extract2("Basename")

s2_trials <- exclude_basename(s2_trials, blocks_we_wont_use)

# Num trials
length(s2_trials)

# Num kids contributing a block of data
s2_trials %>%
  gather_attributes(c("Subj")) %>%
  distinct %>%
  nrow




# Detour: Stats on missing data for manuscript
looks_for_first_good_blocks_in_analysis_window <- long_looks %>%
  semi_join(first_good_blocks, by = "Basename") %>%
  filter(model_times[1] <= Time, Time <= model_times[2])
range(looks_for_first_good_blocks_in_analysis_window$Time)

overall_missing <- looks_for_first_good_blocks_in_analysis_window %>%
  AggregateLooks(1 ~ GazeByImageAOI)
overall_missing$PropNA

missing_per_trial <- looks_for_first_good_blocks_in_analysis_window %>%
  AggregateLooks(Subj + Basename + TrialNo ~ GazeByImageAOI) %>%
  mutate(GoodTrial = PropNA < analysis_opts$excessive_na,
         AllMissing = PropNA == 1)

missing_per_trial %>%
  summarise(nTrials = n(),
            nGood = sum(GoodTrial),
            nBad = sum(!GoodTrial),
            PropGood = mean(GoodTrial),
            PropBad = mean(!GoodTrial),
            nAllMissing = sum(AllMissing),
            PropAllMissing = mean(AllMissing))

# check if missingness is related to evt or age
missing_by_kid <- looks_for_first_good_blocks_in_analysis_window %>%
  AggregateLooks(Subj ~ GazeByImageAOI)
s2_infos <- matches %>% rename(Subj = Participant_ID)
d_missing <- inner_join(missing_by_kid, s2_infos)

summary(lm((PropNA*100) ~ Age, d_missing))
summary(lm(PropNA ~ EVT_GSV, d_missing))
m <- lm((PropNA*100) ~ Age + EVT_GSV, d_missing)
summary(m)
range(d_missing$Age)

cor.test(d_missing$Age, d_missing$PropNA)
cor.test(d_missing$EVT_GSV, d_missing$PropNA)

# Okay a modest correlation because of the larger range of ages and scores
library("ggplot2")
qplot(data = d_missing, x = Age, y = PropNA) +
  stat_smooth(method = "lm")

qplot(data = d_missing, x = EVT_GSV, y = PropNA) +
  stat_smooth(method = "lm")

# we don't drop trials, so don't need to do a logistic regression of on odds of
# having a trial dropped



# Back to processing data: Attach the bias to the trials
s2_trials <- CalculateBias(s2_trials, window = analysis_opts$windows$bias)

# Convert list of trials in a long dataframe
looks_for_models <- s2_trials %>%
  MeltLooks(other_attrs = "Bias") %>%
  as.tbl %>%
  select(Subj, Block = BlockNo, TrialNo, Time, GazeByImageAOI,
         Target:Unrelated, Bias) %>%
  filter(model_times[1] <= Time, Time <= model_times[2])

range(looks_for_models$Time)

# 24 trials per subject
trials_per_subj <- looks_for_models %>%
  select(Subj, TrialNo) %>%
  distinct %>%
  count(Subj)
stopifnot(all(trials_per_subj$n == 24))

# Number of frames is divisible by 3
stopifnot(n_distinct(looks_for_models$Time) %% analysis_opts$bin_width == 0)

# One block per subject
looks_for_models %>%
  select(Subj, Block) %>%
  unique %>%
  group_by(Subj) %>%
  tally %>%
  filter(n != 1)

looks_for_models %>%
  write_csv("./data/study2/01_looking_data.csv")
