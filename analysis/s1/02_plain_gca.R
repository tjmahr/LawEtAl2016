# Study 1, analysis 1

options(stringsAsFactors = FALSE)
source("./R/FunctionsForRWLAnalysis.R")
source("./R/pretty_printing.R")
library("lme4")
library("broom")

# Load appropriate data sets
data_dir <- "./data/study1/modelling"
data_in <- file.path(data_dir, "01_gca.csv")
data_out <- file.path(data_dir, "01_gca_fit_models.Rdata")

d <- read_csv(data_in)

# PPVT~EVT correlation
d_infos <- d %>% select(Subj, EVT_GSV, PPVT_GSV) %>% distinct
cor(d_infos$EVT_GSV, d_infos$PPVT_GSV, use = "pairwise.complete.obs")

gcm_base <- lmer(
  elogit ~ (ot1 + ot2 + ot3) + (ot1 + ot2 + ot3 | Subj),
  data = d,
  REML = FALSE)

get_fixed_effects <- . %>%
  tidy("fixed") %>%
  mutate(p = statistic %>% normal_approx %>% format_pval)

get_fixed_effects(gcm_base)

gcm_EVT <- lmer(
  elogit ~ (ot1 + ot2 + ot3) * cEVT_GSV + (ot1 + ot2 + ot3 | Subj),
  data = d,
  REML = FALSE)

get_fixed_effects(gcm_EVT)

gcm_Age <- lmer(
  elogit ~ (ot1 + ot2 + ot3) * cAge + (ot1 + ot2 + ot3 | Subj),
  data = d,
  REML = FALSE)

get_fixed_effects(gcm_Age)

gcm_PPVT <- lmer(
  elogit ~ (ot1 + ot2 + ot3) * cPPVT_GSV + (ot1 + ot2 + ot3 | Subj),
  data = d,
  REML = FALSE)

get_fixed_effects(gcm_PPVT)

gcm_EVTAge <- lmer(
  elogit ~ (ot1 + ot2 + ot3) * cEVT_GSV * cAge + (ot1 + ot2 + ot3 | Subj),
  data = d,
  REML = FALSE)
get_fixed_effects(gcm_EVTAge)


# what if we look at looks to phon foil (but this is all phon foils, not just
# the good ones)
gcm_phon_EVTAge <- lmer(
  phonoelogit ~ (ot1 + ot2 + ot3) * cEVT_GSV * cAge + (ot1 + ot2 + ot3 | Subj),
  data = d,
  REML = FALSE)
get_fixed_effects(gcm_phon_EVTAge)

# Bundle up the models
e1_a1 <- list(gcm_base = gcm_base,
              gcm_EVT = gcm_EVT,
              gcm_PPVT = gcm_PPVT,
              gcm_Age = gcm_Age,
              gcm_EVTAge = gcm_EVTAge,
              data = d)
save(e1_a1, file = data_out)
