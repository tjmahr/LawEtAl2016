# Narrow down the participant info and raw looking data to just the subjects
# who are from mid-to-high medu families, have EVT scores, did not receive AAE
# stimuli, and have less than 50% missing data


## Load libraries and prep eyetracking dataset

message("Building Study 1 dataset ", sprintf("(%s)", date()))
options(stringsAsFactors = FALSE)
library("lookr")
library("magrittr")
library("stringr")
library("dplyr", warn.conflicts = FALSE)
library("yaml")
library("readr")

analysis_opts <- yaml.load_file("./analysis_options.yaml")

eyetracking_in <- "./data-raw/study1/00_raw_data.Rdata"
eyetracking_out <- "./data/study1/01_keepers.Rdata"
scores_in <- "./data-raw/s1_info.csv"
scores_out <- "./data/study1/01_test_scores.csv"

# Make functions for filtering out trials with certain attributes
exclude_subj <- MakeAttributeFilter(attr_name = "Subj")
exclude_subject <- MakeAttributeFilter(attr_name = "Subject")

`%nin%` <- Negate(`%in%`)

# c(1, 2, 3) => "1, 2, 3"
as_comma_sep <- function(xs) paste(xs, collapse = ", ")

load(eyetracking_in)

# Add a short-id attribute to trials
trials %@% "Subj" <- substr(trials %@% "Subject", 1, 4)

n_kids <- n_distinct(trials %@% "Subj")
message("Loaded eyetracking:\n\t", "Input file: ", eyetracking_in)

# Set a dialect attribute based on last two chars of block name
trials %@% "Dialect" <- trials %@% "Basename" %>%
  str_detect("S\\d$") %>%
  ifelse("SAE", "AAE")




## Perform block-wise and subject-level exclusions

message("Raw eyetracking data:\n\t",
        n_distinct(trials %@% "Subj"), " Subject Numbers, ",
        n_distinct(trials %@% "Basename"), " Blocks IDs, ",
        length(trials %@% "TrialNo"), " Trials")

# Drop excluded ids
trials <- exclude_subject(trials, analysis_opts$s1$excluded_ids)

message("Manually excluding children: \n\t",
        as_comma_sep(analysis_opts$s1$excluded_ids))

# Find and exclude subjects exposed to an AAE version of RWL. (Subjects "080C"
# "081C" "083C" "086C" "087C" saw both dialect versions of RWL, so we list the
# unique subj-by-dialect combinations and exclude based on short-id)
aaes <- data_frame(Subj = trials %@% "Subj", Dialect = trials %@% "Dialect") %>%
  distinct %>%
  filter(Dialect == "AAE") %>%
  extract2("Subj")
trials <- exclude_subj(trials, aaes)

message("Excluding subjects exposed to AAE version of task: \n\t",
        as_comma_sep(aaes))

# Filter out subjects with fewer than two blocks of data
blocks <- data_frame(Subj = trials %@% "Subj", Block = trials %@% "Basename")
too_few_blocks <- blocks %>%
  distinct %>%
  group_by(Subj) %>%
  tally %>%
  filter(n < 2) %>%
  extract2("Subj")
trials <- exclude_subj(trials, too_few_blocks)

message("Excluding subjects with fewer than two blocks of data: \n\t",
        as_comma_sep(too_few_blocks))




## Load and filter the participant info data

# Read in the participant info
scores <- read_csv(scores_in) %>%
  select(-contains("GFTA"), -contains("Missing")) %>%
  rename(Income = `total household income`)

message("Loading cross-sectional scores\n\t",
        "Input file: ", scores_in, "\n\t",
        nrow(scores), " children in table")

# Keep: SAE, mid-to-high medu, with usable EVT scores
scores <- scores %>%
  filter(AAE == 0, !identical(medu, "high school diploma"),
         !is.na(EVT_GSV), WaitForFixation != 1)

message("Keeping SAE, mid-to-high medu, with usable EVT scores\n\t",
        nrow(scores), " children remaining")

# Remove kids with gazedata that don't have test scores
no_scores <- setdiff(unique(trials %@% "Subj"), scores$ParticipantID)
# "045C" -- no EVT
# "090C" -- AAE according to participant info
trials <- exclude_subj(trials, no_scores)

message("Excluding participants in eyetracking data-set without scores: \n\t",
        as_comma_sep(no_scores))
message("\tNote: 045C -- no EVT")
message("\tNote: 090C -- AAE according to participant info")



## Remove kids with excessive missing data

# Make a copy of trials for calculating missing data stats
temp_trials <- trials

# Interpolate missing looks then calculate missing data over analysis window
na_window <- analysis_opts$windows$prop_na
temp_trials_df <- temp_trials %>%
  AddAOIData %>%
  InterpolateMissingFrames %>%
  TimeSlice(na_window[1], na_window[2]) %>%
  MeltLooks %>%
  as.tbl

# Calculate average missing data for various aggregations
by_trial <- temp_trials_df %>%
  AggregateLooks(Subj + DateTime + Basename + TrialNo ~ GazeByImageAOI) %>%
  select(Subj:TrialNo, PropNA)
write_csv(by_trial, "./data-raw/study1/01_missing_data_per_trial.csv")

by_block <- temp_trials_df %>%
  AggregateLooks(Subj + DateTime + Basename ~ GazeByImageAOI) %>%
  select(Subj:Basename, PropNA)
write_csv(by_block, "./data-raw/study1/02_missing_data_per_block.csv")

by_child <- temp_trials_df %>%
  AggregateLooks(Subj ~ GazeByImageAOI) %>%
  select(Subj, PropNA)
write_csv(by_child, "./data-raw/study1/03_missing_data_per_child.csv")

# Exclude children with more than 50% missing data
kids_to_drop <- by_child %>% filter(analysis_opts$excessive_na < PropNA)

message("Excluding ", nrow(kids_to_drop),
        " children with more than 50% missing data overall: \n\t",
        as_comma_sep(kids_to_drop$Subj))

trials <- exclude_subj(trials, kids_to_drop$Subj)



## Save finalized scores

# Exclude scores from kids who are not in the eyetracking dataset
no_gazedata <- scores %>%
  filter(ParticipantID %nin% unique(trials %@% "Subj"))
no_gazedata

message("Excluding scores from children who don't have eyetracking data: \n\t",
        as_comma_sep(no_gazedata$ParticipantID))

# Finalize columns
scores <- scores %>%
  anti_join(no_gazedata, by = "ParticipantID") %>%
  select(-age_2, -age_3, -keeper, -WaitForFixation) %>%
  rename(Age = age_1)

message("Saving scores for ", nrow(scores), " children \n\t",
        "Output file: ", scores_out)
write_csv(scores, scores_out)



## Save eyetracking data

message("Final eyetracking data:\n\t",
        n_distinct(trials %@% "Subj"), " Subject Numbers, ",
        n_distinct(trials %@% "Basename"), " Blocks IDs, ",
        length(trials %@% "TrialNo"), " Trials")

message("Saving eyetracking:\n\t", "Output file: ", eyetracking_out)

save(trials, file = eyetracking_out)
