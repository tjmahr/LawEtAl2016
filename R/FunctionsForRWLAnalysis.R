options(stringsAsFactors = FALSE)
source("R/utils.R")

library("readr")
library("magrittr")
library("car")
library("stringr")
library("dplyr", warn.conflicts = FALSE)
library("lme4")
library("tidyr")
library("ggplot2")
library("lookr")

# "Magic numbers" and other constants/analysis parameters live in a YAML file
library("yaml")

library("broom")
source("./R/pretty_printing.R")

get_fixed_effects <- . %>%
  tidy("fixed") %>%
  mutate(p = normal_approx(statistic) %>% format_pval)




#' Aggregate long-format raw looking data to model-ready binned elogits
#'
#' @description This function performs the usual data-reduction pipeline for
#'   this manuscript: Aggregating looks across trials, binning the data,
#'   computing empirical logits of looking the target image, and computing
#'   orthogonal time values for modeling.
#'
#' @details This function performs a number of transformation on the data. We
#'   start with raw looking data with one row per Participant x Trial-Number x
#'   Frame-of-Time.
#'
#'   First, we collapse across trials and compute the number of looks to each
#'   AOI for each frame. At this point, we have one row per Participant x
#'   Frame-of-Time.
#'
#'   Next, it down-samples the frame-by-frame totals into bins and computes the
#'   empirical logits of looking to the target images in each bin for each
#'   subject. After this final aggregation, we have one row per Participant x
#'   Bin-of-Frames-of-Time.
#'
#'   Finally, we compute linear, quadratic, and cubic orthogonal polynomials
#'   time terms, so we can use uncorrelated Time, Time^2^ and Time^3^ terms in
#'   our models.
#'
#' @param looks a data-frame of long looking data, with one row per frame of
#'   eyetracking for every trial for every subject. Expects the columns "Sub",
#'   "Time", "GazeByImageAOI"
#' @param bins how many frames to include in each bin
#' @param flip whether to flip quadratic time polynomial so that positive model
#'   coefficients reflect acceleration.
#' @param draw_ot whether to draw the orthogonal polynomial time curves
#' @param max_bin_time an optional upper limit on the binned time. This lets us
#'   aggregate the data, then exclude all bins after a certain value, and then
#'   assign orthogonal times. (Trimming bins after orthogonal polynomials have
#'   been assigned will make the polynomial non-orthogonal.
#'
#' @return a data-frame with columns for the grouping variables ["Subj", "Bin",
#'   the "Time" of the first frame in that bin], number of looks to each AOI
#'   ["ToTarget", "ToPhonological", "ToSemantic", "ToUnrelated", "ToNothing",
#'   "ToElsewhere", "ToDistractors", "ToAOIs"], total number of "Trials",
#'   "Proportion" of looks to target ("ToTarget" / "ToAOIs"), empirical logits
#'   and weights of looking to each AOI ["elogit" to target, "unelogit" to
#'   unrelated, "phonoelogit", "semanticelogit", "elogit_weights",
#'   "unelogit_weights", "phonoelogit_weights", "semanticelogit_weights"] and
#'   linear, quadratic and cubic orthogonal linear time values ["ot1", "ot2",
#'   "ot3"]
standard_model_pipeline <- function(looks, bins = NULL, flip = TRUE,
                                    draw_ot = FALSE, max_bin_time = NULL) {
  stopifnot(!is.null(bins))
  model_ready <- looks %>%
    # Count looks to each AOI
    AggregateLooks(Subj + Time ~ GazeByImageAOI) %>%
    # Bin the data
    group_by(Subj) %>%
    mutate(Bin = AssignBins(Time, bins)) %>%
    # Within each bin, compute empirical logits
    group_by(Subj, Bin) %>%
    calculate_elogits

  # Trim right-side of timing window, needed for the models that look at a
  # subset of the looking window
  if (!is.null(max_bin_time)) {
    model_ready <- filter(model_ready, Time <= max_bin_time)
  }

  model_ready <- model_ready %>%
    include_orthogonal_time(degree = 3, flip_quad = flip, draw = draw_ot)

  model_ready
}





#' Create model-ready data for each trial-onset "bias"
#'
#' @description This function wraps `standard_model_pipeline` and
#'   `make_model_data` and computes a growth curve of looking to the target
#'   image for each trial-onset "bias".
#'
#' @details Consider two kinds of trials: Ones where the child is already
#'   viewing the target image at word onset, versus ones where the child is
#'   viewing a distractor at trial onset. We want to know whether the growth
#'   curves (of proportion of looking to target over time) differ between these
#'   two kinds of trials. This functions prepares data-frames for models that
#'   can answer these kinds of questions.
#'
#'   Call the initial fixation location in a trial the "bias". We are interested
#'   in two kinds of bias. First, we are interested in "binary" bias: Trials
#'   with a bias to the target image vs. trials with a bias to any of the three
#'   distractor images. Second, we are interested in a "four-way" measure of
#'   bias: Trials with a bias to the target image vs. ...the phonological foil
#'   vs. ...the semantic foil vs.  ...the unrelated foil.
#'
#' @inheritParams standard_model_pipeline
#' @inheritParams make_model_data
#' @param phon_set a vector of target words with "good" phonological foils. Only
#'   trials with these target words will be used for the growth curve of looks
#'   with a phonological bias.
#' @param semy_set a vector of target words with "good" semantic foils. Only
#'   trials with these target words will be used for the growth curve of looks
#'   with a semantic bias.
#'
#' @return a list with two model-ready data-frames: one with a binary definition
#'   of bias and one with a fourway definition of bias.
compute_bias_growth_curves <- function(looks, infos, bins = bin_width, phon_set = good_phon, semy_set = good_semy) {

  smp <- function(...) {
    standard_model_pipeline(..., bins = bin_width, flip = TRUE, draw_ot = FALSE)
  }

  # Get the log-odds of viewing target when initial look is to an effective
  # phonological foil
  phon <- looks %>%
    filter(Bias == "PhonologicalFoil", is.element(Target, good_phon)) %>%
    smp %>%
    mutate(Bias = "Phonological")

  # ..to an effective semantic foil
  semy <- looks %>%
    filter(Bias == "SemanticFoil", is.element(Target, good_semy)) %>%
    smp %>%
    mutate(Bias = "Semantic")

  # ..to an unrelated foil
  unre <- looks %>%
    filter(Bias == "Unrelated") %>%
    smp %>%
    mutate(Bias = "Unrelated")

  # ..to the target image
  targ <- looks %>%
    filter(Bias == "Target") %>%
    smp %>%
    mutate(Bias = "Target")

  # ... to any distractor
  any_other <- looks %>%
    filter(Bias != "Target", !is.na(Bias)) %>%
    smp %>%
    mutate(Bias = "Distractor")

  # Combine the individual results together. Bundle in test scores
  all_bias <- bind_rows(phon, semy, unre, targ) %>%
    mutate(BiasSet = 4) %>%
    make_model_data(infos)

  binary_bias <- bind_rows(targ, any_other) %>%
    mutate(BiasSet = 2) %>%
    make_model_data(infos)

  list(fourway_bias = all_bias, binary_bias = binary_bias)
}






















# Compute emprical logits within bins
calculate_elogits <- function(d) {
  stopifnot(is.element("Bin", groups(d)))
  stopifnot(d$PhonologicalFoil + d$SemanticFoil + d$Unrelated == d$Others)

  # Add up the looks inside each bin
  d <- d %>%
    summarise(
      # Use the time when the bin begins, rounded nearest tenth
      Time = round(min(Time), -1),
      # Each AOI
      ToTarget = sum(Target),
      ToPhonological = sum(PhonologicalFoil),
      ToSemantic = sum(SemanticFoil),
      ToUnrelated = sum(Unrelated),
      # Looks outside those AOIs
      ToNothing = sum(NAs),
      ToElsewhere = sum(Elsewhere),
      # Aggregate
      ToDistractors = sum(Others),
      ToAOIs = ToTarget + ToDistractors,
      Trials = (ToAOIs + ToNothing + ToElsewhere) / length(Bin)) %>%
    ungroup

  # Shorthand where x is value of interest and y is everything else minus x.
  # ToAOIs will be y and x will be each AOI.
  e_log <- function(x, y) empirical_logit(x, y - x)
  e_wgt <- function(x, y) empirical_logit_weight(x, y - x)

  # Empircal logits
  d <- d %>%
    mutate(
      Proportion = ToTarget / ToAOIs,
      # empirical logits
      elogit = e_log(ToTarget, ToAOIs),
      unelogit = e_log(ToUnrelated, ToAOIs),
      phonoelogit = e_log(ToPhonological, ToAOIs),
      semanticelogit = e_log(ToSemantic, ToAOIs),
      # and their weights
      elogit_weights = e_wgt(ToTarget, ToAOIs),
      unelogit_weights = e_wgt(ToUnrelated, ToAOIs),
      phonoelogit_weights = e_wgt(ToPhonological, ToAOIs),
      semanticelogit_weights = e_wgt(ToSemantic, ToAOIs))

  d
}


# Add orthogonal polynomials for Time values to a dataframe
include_orthogonal_time <- function(binned, degree, flip_quad = TRUE, draw = TRUE) {
  ots <- orthogonal_time(binned$Time, degree)

  # Message if flipping ot2
  if (flip_quad) {
    message("Inverting quadratic polynomial. Vertex at maximum value.")
    ots <- ots %>% mutate(ot2 = ot2 * -1)
  }

  if (draw) {
    print(draw_orthogonal_times(ots))
  }

  left_join(binned, ots, by = "Time")
}

# Plot the orthogonal polynomial curves used for Time
draw_orthogonal_times <- function(ots) {
  long_times <- ots %>% gather(Degree, Value, -Time)
  p <- ggplot(long_times) +
    aes(x = Time, y = Value, color = Degree) +
    geom_point() +
    geom_line() +
    ggtitle("Orthogonal time polynomials")
  p
}




#' Combine model-ready, binned looking data and participant scores
#'
#' @description Attach child-level measurements to a data-frame of eyetracking
#'   growth curves
#'
#' @details This table-joining operation is relegated to its own function
#'   because our models include summary-based values like mean-centered
#'   vocabulary scores or grouping based on median splits. We compute these
#'   means and medians using just the children who contribute data to the model.
#'
#' @param binned a data-frame of binned eyetracking data
#' @param infos  a data-frame of child-level measurements (one row per child)
#' @return the looking data combined with child-level measurements
make_model_data <- function(binned, infos) {

  looks <- ungroup(binned)
  kids_not_in_eyetracking_set <- setdiff(infos$Subj, binned$Subj)

  if (length(kids_not_in_eyetracking_set) != 0) {
    kids <- paste(kids_not_in_eyetracking_set, collapse = ", ")
    warning("Following children have no eyetracking data for model: ", kids,
            "\nMean-centered scores and median-splits are based on the ",
            n_distinct(looks$Subj), " children with eyetracking data")
  }

  infos <- infos %>%
    # Keep only subjects in the looking data-set
    semi_join(looks, by = "Subj") %>%
    # For the remaining subjects, compute summary statistics
    summary_stats

  # Attach scores onto looking data
  left_join(looks, infos, by = "Subj")
}

summary_stats <- function(infos) {
  infos <- infos %>% ungroup %>%
    mutate(
      # Center values
      cAge = mean_center(Age),
      cEVT_GSV = mean_center(EVT_GSV),
      cPPVT_GSV = mean_center(PPVT_GSV),
      cEVT_standard = mean_center(EVT_standard),
      cPPVT_standard = mean_center(PPVT_standard),
      # Compute a few different breaks for EVT scores
      EVTBreakSD = cut_by_sd(EVT_GSV),
      EVTBreakHalf = cut_by_median(EVT_GSV),
      EVTBreakThirds = cut_by_thirds(EVT_GSV))

  infos
}


# Find trials in `d` with more than `prop_na` missing looks
find_mistracked_trials <- function(d, prop_na) {
  d %>%
    AggregateLooks(Subj + Block + TrialNo ~ GazeByImageAOI) %>%
    filter(prop_na <= PropNA) %>%
    select(Subj:TrialNo, PropNA)
}








ReportDescriptives <- function(dataFrame){

  dataFrame <- subset(dataFrame, dataFrame$Bin == 1)
  #  high <- subset(dataFrame, dataFrame$EVTBreakSD == ">1SD Above")
  #  mid <- subset(dataFrame,  dataFrame$EVTBreakSD == "Within 1 SD of Mean")
  #  low <- subset(dataFrame,  dataFrame$EVTBreakSD == ">1SD Below")

  print('Bottom Third')
  PrintDescriptives(subset(dataFrame,  dataFrame$EVTBreakThirds == "Bottom Third" & dataFrame$smedu == 1))
  PrintDescriptives(subset(dataFrame,  dataFrame$EVTBreakThirds == "Bottom Third" & dataFrame$smedu == 2))
  PrintDescriptives(subset(dataFrame,  dataFrame$EVTBreakThirds == "Bottom Third" & dataFrame$smedu == 3))

  print('Middle Third')
  PrintDescriptives(subset(dataFrame,  dataFrame$EVTBreakThirds == "Middle Third" & dataFrame$smedu == 1))
  PrintDescriptives(subset(dataFrame,  dataFrame$EVTBreakThirds == "Middle Third" & dataFrame$smedu == 2))
  PrintDescriptives(subset(dataFrame,  dataFrame$EVTBreakThirds == "Middle Third" & dataFrame$smedu == 3))

  print('Top Third')
  PrintDescriptives(subset(dataFrame,  dataFrame$EVTBreakThirds == "Top Third" & dataFrame$smedu == 1))
  PrintDescriptives(subset(dataFrame,  dataFrame$EVTBreakThirds == "Top Third" & dataFrame$smedu == 2))
  PrintDescriptives(subset(dataFrame,  dataFrame$EVTBreakThirds == "Top Third" & dataFrame$smedu == 3))

  #  return(dataFrame)
}

PrintDescriptives <- function(dataFrame) {
  print(paste0("N = ", n_distinct(dataFrame$Subj)))
  print(mean_range("Age", dataFrame$Age))
  print(mean_range("EVT(GSV)", dataFrame$EVT_GSV))
  print(mean_range("EVT(Stnd.)", dataFrame$EVT_standard))
}

# Plot a lmer model where x = Time, y = elogit
plot_model <- function(model, x_ints = 250) {
  m <- fortify(model)
  ggplot(m, aes(x = Time, y = elogit)) +
    geom_hline(yintercept = qlogis(.25), color = "gray") +
    geom_vline(xintercept = x_ints, colour = "black", linetype = "longdash") +
    stat_summary(fun.data = mean_se, geom = "pointrange", linetype = "solid", alpha = 0.7) +
    stat_summary(aes(y = .fitted), fun.y = mean, geom = "line", size = 1)
}
