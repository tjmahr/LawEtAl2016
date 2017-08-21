## Study 2, Analysis 1

options(stringsAsFactors = FALSE)
source("R/utils.R")
source("R/FunctionsForRWLAnalysis.R")

library("ggplot2")
library("lme4")
library("broom")

analysis_opts <- yaml.load_file("analysis_options.yaml")

# Includes both the linear and quadratic contrast codings of maternal education
set_factors <- . %>%
  mutate(medu = factor(medu, levels = c("low", "middle", "high"), ordered = TRUE),
         Dialect = factor(Dialect, c("MAE", "AAE")))

s2 <- read_csv("data/study2/02_analysis1.csv") %>% set_factors
s2_a2 <- read_csv("data/study2/02_analysis2.csv") %>% set_factors

glimpse(s2)
glimpse(s2_a2)


# Look at the data
p1 <- qplot(data = s2, x = Time, y = Proportion, color = Dialect) +
  geom_smooth()
p1
p1 + facet_grid(medu ~ EVTBreakThirds)


# With biases
p2 <- qplot(data = s2_a2, x = Time, y = Proportion, color = Bias,
            linetype = Dialect, geom = "smooth")
p2
p2 + facet_grid(Dialect ~ EVTBreakThirds)
p2 + facet_grid(Dialect ~ medu)


## Models
#Dialect is not significant and does not significantly interact with time parameters if EVT
#is in the model
gcm_evt_dia <- lmer(
  elogit ~ (ot1 + ot2 + ot3) + cEVT_GSV + Dialect + cEVT_GSV:ot1 + cEVT_GSV:ot2 + cEVT_GSV:ot3 +
    Dialect:ot1 + Dialect:ot2 + Dialect:ot3 +
    (ot1 + ot2 + ot3 | Subj),
  data = s2, REML = FALSE)
get_fixed_effects(gcm_evt_dia)

gcm_empty <- lmer(
  elogit ~ (ot1 + ot2 + ot3) + (ot1 + ot2 + ot3 | Subj),
  data = s2, REML = FALSE)
get_fixed_effects(gcm_empty)

gcm_age <- lmer(
  elogit ~ (ot1 + ot2 + ot3) * cAge + (ot1 + ot2 + ot3 | Subj),
  data = s2, REML = FALSE)
get_fixed_effects(gcm_age)

gcm_evt <- lmer(
  elogit ~ (ot1 + ot2 + ot3) * cEVT_GSV  + (ot1 + ot2 + ot3 | Subj),
  data = s2,
  REML = FALSE)
get_fixed_effects(gcm_evt)

gcm_age_EVT <- lmer(
  elogit ~ (ot1 + ot2 + ot3) * cEVT_GSV * cAge + (ot1 + ot2 + ot3 | Subj),
  data = s2,
  REML = FALSE)
get_fixed_effects(gcm_age_EVT)

# Final model

gcm_EVT_medu_age <- lmer(
  elogit ~ (ot1 + ot2 + ot3) * cEVT_GSV * cAge * medu +
    (ot1 + ot2 + ot3 | Subj),
  data = s2, REML = FALSE)
get_fixed_effects(gcm_EVT_medu_age)

# Bundle up the models
s2_a1 <- list(gcm_evt_dia = gcm_evt_dia,
              gcm_empty = gcm_empty,
              gcm_age = gcm_age,
              gcm_evt = gcm_evt,
              gcm_age_EVT = gcm_age_EVT,
              gcm_EVT_medu_age = gcm_EVT_medu_age,
              data = s2)
save(s2_a1, file = "data/study2/modelling/01_plain_gca.Rdata")




#### Distractor analysis ------------------------------------------------------

# Above analysis collapsed across all trials. Is there a difference when the
# child is looking to the target vs. one of the distractors at target word
# onset?


# Both bias datasets are are included. Keep just Target vs Distractor one.
s2_binary_bias <- filter(s2_a2, BiasSet == 2)
count(s2_binary_bias, Bias)

gcm_target <- lmer(
  elogit ~ (ot1 + ot2 + ot3) * cEVT_GSV * Bias +
    (ot1 + ot2 + ot3 | Subj) +
    (ot1 + ot2 + ot3 | Subj:Bias),
  data = s2_binary_bias, REML = FALSE)
get_fixed_effects(gcm_target)

# Does medu or Age improve model fit?
# answer: no
gcm_target_medu <- lmer(
  elogit ~ (ot1 + ot2 + ot3) * cEVT_GSV * Bias + medu +
    (ot1 + ot2 + ot3 | Subj) +
    (ot1 + ot2 + ot3 | Subj:Bias),
  data = s2_binary_bias, REML = FALSE)
get_fixed_effects(gcm_target_medu)

gcm_target_age <- lmer(
  elogit ~ (ot1 + ot2 + ot3) * cEVT_GSV * Bias + cAge +
    (ot1 + ot2 + ot3 | Subj) +
    (ot1 + ot2 + ot3 | Subj:Bias),
  data = s2_binary_bias, REML = FALSE)
get_fixed_effects(gcm_target_age)

anova(gcm_target, gcm_target_medu)
anova(gcm_target, gcm_target_age)

# Bundle up the models
s2_a2 <- list(gcm_target = gcm_target,
              gcm_target_medu = gcm_target_medu,
              gcm_target_age = gcm_target_age,
              data = s2_binary_bias)
save(s2_a2, file = "data/study2/modelling/02_bias_analysis.Rdata")
