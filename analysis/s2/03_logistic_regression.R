# Reference for analysis
# http://stackoverflow.com/questions/21082396/multinomial-logistic-multilevel-models-in-r

# when lmer fails to converge
# http://stats.stackexchange.com/questions/97929/lmer-model-fails-to-converge
# http://stackoverflow.com/questions/21344555/convergence-error-for-development-version-of-lme4

options(stringsAsFactors = FALSE)
source("R/FunctionsForRWLAnalysis.R")
library("lme4")

set_factors <- . %>%
  mutate(medu = factor(medu, levels = c("low", "middle", "high"), ordered = TRUE),
         Dialect = factor(Dialect, c("MAE", "AAE")))

d_phon <- read_csv("data/study2/03_logistic_phon.csv") %>% set_factors
d_semy <- read_csv("data/study2/03_logistic_semy.csv") %>% set_factors




## Comparison of Target v. Phonological and Target v. Unrelated

# Check for zeroes
d_phon %>% filter(ToTarget + ToPhonological == 0)
d_phon %>% filter(ToTarget + ToUnrelated == 0)

# Fit models
bobyqa <- glmerControl(optimizer = "bobyqa")

gcm_Target_v_Phonological <- glmer(
  cbind(ToTarget, ToPhonological) ~ (ot1 + ot2) + (ot1 + ot2 | Subj),
  data = d_phon,
  family = binomial,
  control = bobyqa)

gcm_Target_v_Unrelated <- glmer(
  cbind(ToTarget, ToUnrelated) ~ (ot1 + ot2) + (ot1 + ot2 | Subj),
  data = d_phon,
  family = binomial,
  control = bobyqa)

get_fixed_effects(gcm_Target_v_Phonological)
get_fixed_effects(gcm_Target_v_Unrelated)

# Add EVT to models
gcm_Target_v_Phonological_EVT <- update(
  gcm_Target_v_Phonological,
  . ~ cEVT_GSV * (ot1 + ot2) + (ot1 + ot2 | Subj))

gcm_Target_v_Unrelated_EVT <- update(
  gcm_Target_v_Unrelated,
  . ~ cEVT_GSV * (ot1 + ot2) + (ot1 + ot2 | Subj))

get_fixed_effects(gcm_Target_v_Phonological_EVT)
get_fixed_effects(gcm_Target_v_Unrelated_EVT)

summary(gcm_Target_v_Phonological_EVT)
summary(gcm_Target_v_Unrelated_EVT)

# The next two models don't include interaction between EVT and time terms.
gcm_Target_v_Phonological_EVT_noInter <- update(
  gcm_Target_v_Phonological,
  . ~ cEVT_GSV + (ot1 + ot2) + (ot1 + ot2 | Subj))

gcm_Target_v_Unrelated_EVT_noInter <- update(
  gcm_Target_v_Unrelated,
  . ~ cEVT_GSV + (ot1 + ot2) + (ot1 + ot2 | Subj))

get_fixed_effects(gcm_Target_v_Phonological_EVT_noInter)
get_fixed_effects(gcm_Target_v_Unrelated_EVT_noInter)

anova(gcm_Target_v_Phonological_EVT,gcm_Target_v_Phonological_EVT_noInter)
anova(gcm_Target_v_Unrelated_EVT,gcm_Target_v_Unrelated_EVT_noInter)

# Control for EVT AND Age
gcm_Target_v_Phonological_EVT_age <- update(
  gcm_Target_v_Phonological,
  . ~ cEVT_GSV * cAge * (ot1 + ot2) + (ot1 + ot2 | Subj))

gcm_Target_v_Unrelated_EVT_age <- update(
  gcm_Target_v_Unrelated,
  . ~ cEVT_GSV * cAge * (ot1 + ot2) + (ot1 + ot2 | Subj))

summary(gcm_Target_v_Phonological_EVT_age)
summary(gcm_Target_v_Unrelated_EVT_age)

anova(gcm_Target_v_Phonological_EVT, gcm_Target_v_Phonological_EVT_age)
anova(gcm_Target_v_Unrelated_EVT, gcm_Target_v_Unrelated_EVT_age)

# models did not converge either. So we're
# removing random effect covariances.
# and we are removing random quadratic terms
#because just removing random effect covariances didn't work
#models with age still do not converge

gcm_Target_v_Phonological_EVT_age_2 <- update(
  gcm_Target_v_Phonological,
  . ~ cEVT_GSV * cAge * (ot1 + ot2) + (ot1 || Subj))

gcm_Target_v_Unrelated_EVT_age_2 <- update(
  gcm_Target_v_Unrelated,
  . ~ cEVT_GSV * cAge * (ot1 + ot2) + (ot1 || Subj))

summary(gcm_Target_v_Phonological_EVT_age_2)
summary(gcm_Target_v_Unrelated_EVT_age_2)

anova(gcm_Target_v_Phonological_EVT, gcm_Target_v_Phonological_EVT_age_2)
anova(gcm_Target_v_Unrelated_EVT, gcm_Target_v_Unrelated_EVT_age_2)

#trying instead
#add age but not EVT and compare AIC/BIC
#model with EVT is better for phonological and model with age is better for unrelated

gcm_Target_v_Phonological_AgeOnly <- update(
  gcm_Target_v_Phonological,
  . ~ cAge * (ot1 + ot2) + (ot1 + ot2 | Subj))

gcm_Target_v_Unrelated_AgeOnly <- update(
  gcm_Target_v_Unrelated,
  . ~ cAge * (ot1 + ot2) + (ot1 + ot2 | Subj))

AIC(gcm_Target_v_Phonological_AgeOnly,gcm_Target_v_Phonological_EVT)
BIC(gcm_Target_v_Phonological_AgeOnly,gcm_Target_v_Phonological_EVT)

AIC(gcm_Target_v_Unrelated_AgeOnly,gcm_Target_v_Unrelated_EVT)
BIC(gcm_Target_v_Unrelated_AgeOnly,gcm_Target_v_Unrelated_EVT)

anova(gcm_Target_v_Phonological_AgeOnly,gcm_Target_v_Phonological_EVT)
anova(gcm_Target_v_Unrelated_AgeOnly,gcm_Target_v_Unrelated_EVT)


#control for EVT and medu
gcm_Target_v_Phonological_EVT_medu <- update(
  gcm_Target_v_Phonological,
  . ~ cEVT_GSV * medu * (ot1 + ot2) + (ot1 + ot2 | Subj))

gcm_Target_v_Unrelated_EVT_medu <- update(
  gcm_Target_v_Unrelated,
  . ~ cEVT_GSV * medu * (ot1 + ot2) + (ot1 + ot2 | Subj))

#models don't converge so I'm removing random effects covariance and quadratic term
#models don't converge with only one of these
#models still don't converge


gcm_Target_v_Phonological_EVT_medu_2 <- update(
  gcm_Target_v_Phonological,
  . ~ cEVT_GSV * medu * (ot1 + ot2) + (ot1 || Subj))

gcm_Target_v_Unrelated_EVT_medu_2 <- update(
  gcm_Target_v_Unrelated,
  . ~ cEVT_GSV * medu * (ot1 + ot2) + (ot1 || Subj))

summary(gcm_Target_v_Phonological_EVT_medu)
summary(gcm_Target_v_Unrelated_EVT_medu)

anova(gcm_Target_v_Phonological_EVT, gcm_Target_v_Phonological_EVT_medu)
anova(gcm_Target_v_Unrelated_EVT, gcm_Target_v_Unrelated_EVT_medu)

#try running models with medu Only and without EVT
#models with EVT are better fitting than models with medu according to AIC/BIC

gcm_Target_v_Phonological_meduOnly <- update(
  gcm_Target_v_Phonological,
  . ~ medu * (ot1 + ot2) + (ot1 || Subj))

gcm_Target_v_Unrelated_meduOnly <- update(
  gcm_Target_v_Unrelated,
  . ~ medu * (ot1 + ot2) + (ot1 || Subj))

AIC(gcm_Target_v_Phonological_meduOnly,gcm_Target_v_Phonological_EVT)
BIC(gcm_Target_v_Phonological_meduOnly,gcm_Target_v_Phonological_EVT)

AIC(gcm_Target_v_Unrelated_meduOnly,gcm_Target_v_Unrelated_EVT)
BIC(gcm_Target_v_Unrelated_meduOnly,gcm_Target_v_Unrelated_EVT)

anova(gcm_Target_v_Phonological_AgeOnly,gcm_Target_v_Phonological_EVT)
anova(gcm_Target_v_Unrelated_AgeOnly,gcm_Target_v_Unrelated_EVT)

#final models

gcm_Target_v_Phonological_EVT <- update(
  gcm_Target_v_Phonological,
  . ~ cEVT_GSV * (ot1 + ot2) + (ot1 + ot2 | Subj))

gcm_Target_v_Unrelated_EVT <- update(
  gcm_Target_v_Unrelated,
  . ~ cEVT_GSV * (ot1 + ot2) + (ot1 + ot2 | Subj))


# Bundle up models
s2_phon_unre <- list(
  gcm_Target_v_Phonological = gcm_Target_v_Phonological,
  gcm_Target_v_Unrelated = gcm_Target_v_Unrelated,
  gcm_Target_v_Phonological_EVT = gcm_Target_v_Phonological_EVT,
  gcm_Target_v_Unrelated_EVT = gcm_Target_v_Unrelated_EVT,
  d_phon = d_phon
)

save(s2_phon_unre, file = "data/study2/modelling/03_logistic_phon.Rdata")




#### Comparison of Target v. Semantic and Target v. Unrelated -----------------

# Check for no zeroes
d_semy %>% filter(ToTarget + ToSemantic == 0)
d_semy %>% filter(ToTarget + ToUnrelated == 0)

gcm_Target_v_Semantic <- glmer(
  cbind(ToTarget, ToSemantic) ~ (ot1 + ot2 + ot3) + (ot1 + ot2 + ot3 | Subj),
  data = d_semy,
  family = binomial,
  control = bobyqa)

gcm_Target_v_Unrelated2 <- glmer(
  cbind(ToTarget, ToUnrelated) ~ (ot1 + ot2 + ot3) + (ot1 + ot2 + ot3 | Subj),
  data = d_semy,
  family = binomial,
  control = bobyqa)

summary(gcm_Target_v_Semantic)
summary(gcm_Target_v_Unrelated2)

# control for EVT
#both models do not converge
gcm_Target_v_Semantic_EVT <- update(
  gcm_Target_v_Semantic,
  . ~ cEVT_GSV * (ot1 + ot2 + ot3) + (ot1 + ot2 + ot3 | Subj))

gcm_Target_v_Unrelated2_EVT <- update(
  gcm_Target_v_Unrelated2,
  . ~ cEVT_GSV * (ot1 + ot2 + ot3) + (ot1 + ot2 + ot3 | Subj))

get_fixed_effects(gcm_Target_v_Semantic_EVT)
get_fixed_effects(gcm_Target_v_Unrelated2_EVT)

#try again without interactions
gcm_Target_v_Semantic_EVT_noInter <- update(
  gcm_Target_v_Semantic,
  . ~ cEVT_GSV + (ot1 + ot2 + ot3) + (ot1 + ot2 + ot3 | Subj))

gcm_Target_v_Unrelated2_EVT_noInter <- update(
  gcm_Target_v_Unrelated2,
  . ~ cEVT_GSV + (ot1 + ot2 + ot3) + (ot1 + ot2 + ot3 | Subj))

get_fixed_effects(gcm_Target_v_Semantic_EVT_noInter)
summary(gcm_Target_v_Unrelated2_EVT_noInter)

anova(gcm_Target_v_Semantic_EVT,gcm_Target_v_Semantic_EVT_noInter)
anova(gcm_Target_v_Unrelated2,gcm_Target_v_Unrelated2_EVT_noInter)

# Removed cubic time terms from random effects to try to get the models to
# converge. this was successful so we will use these as the final models.

# gcm_Target_v_Semantic_EVT_noCubic <- update(
#   gcm_Target_v_Semantic,
#   . ~ cEVT_GSV * (ot1 + ot2 + ot3) + (ot1 + ot2 | Subj))
#
# gcm_Target_v_Unrelated2_EVT_noCubic <- update(
#   gcm_Target_v_Unrelated2,
#   . ~ cEVT_GSV * (ot1 + ot2 + ot3) + (ot1 + ot2 | Subj))
#
# get_fixed_effects(gcm_Target_v_Semantic_EVT_noCubic)
# get_fixed_effects(gcm_Target_v_Unrelated2_EVT_noCubic)

# Actually, the previous unrelated model did not converge either. So we're
# removing random effect covariances.
gcm_Target_v_Semantic_EVT_no_ranef_cov <- update(
  gcm_Target_v_Semantic,
  . ~ cEVT_GSV * (ot1 + ot2 + ot3) + (ot1 + ot2 + ot3 || Subj))

gcm_Target_v_Unrelated2_EVT_no_ranef_cov <- update(
  gcm_Target_v_Unrelated2,
  . ~ cEVT_GSV * (ot1 + ot2 + ot3) + (ot1 + ot2 + ot3 || Subj))

get_fixed_effects(gcm_Target_v_Semantic_EVT_no_ranef_cov)
get_fixed_effects(gcm_Target_v_Unrelated2_EVT_no_ranef_cov)

#Can we leave age out of the model
#models don't converge so we are leaving out cubic term also
#models still do not converge

gcm_Target_v_Semantic_EVT_Age_no_ranef_cov <- update(
  gcm_Target_v_Semantic,
  . ~ cEVT_GSV * cAge * (ot1 + ot2 + ot3) + (ot1 + ot2 || Subj))

gcm_Target_v_Unrelated2_EVT_Age_no_ranef_cov <- update(
  gcm_Target_v_Unrelated2,
  . ~ cEVT_GSV * cAge* (ot1 + ot2 + ot3) + (ot1 + ot2 || Subj))


#look at effect of age separately
#EVT better for semantic and age better for unrelated according to AIC/BIC

gcm_Target_v_Semantic_AgeOnly_no_ranef_cov <- update(
  gcm_Target_v_Semantic,
  . ~ cAge * (ot1 + ot2 + ot3) + (ot1 + ot2 + ot3 || Subj))

gcm_Target_v_Unrelated2_AgeOnly_no_ranef_cov <- update(
  gcm_Target_v_Unrelated2,
  . ~ cAge* (ot1 + ot2 + ot3) + (ot1 + ot2 + ot3 || Subj))

AIC(gcm_Target_v_Semantic_EVT_no_ranef_cov,gcm_Target_v_Semantic_AgeOnly_no_ranef_cov )
BIC(gcm_Target_v_Semantic_EVT_no_ranef_cov,gcm_Target_v_Semantic_AgeOnly_no_ranef_cov )

AIC(gcm_Target_v_Unrelated2_EVT_no_ranef_cov,gcm_Target_v_Unrelated2_AgeOnly_no_ranef_cov)
BIC(gcm_Target_v_Unrelated2_EVT_no_ranef_cov,gcm_Target_v_Unrelated2_AgeOnly_no_ranef_cov)


anova(gcm_Target_v_Semantic_EVT_no_ranef_cov,gcm_Target_v_Semantic_AgeOnly_no_ranef_cov )
anova(gcm_Target_v_Unrelated2_EVT_no_ranef_cov,gcm_Target_v_Unrelated2_AgeOnly_no_ranef_cov)

#compare medu and EVT
#EVT better for both according to AIC/BIC

gcm_Target_v_Semantic_meduOnly_no_ranef_cov <- update(
  gcm_Target_v_Semantic,
  . ~ medu * (ot1 + ot2 + ot3) + (ot1 + ot2 + ot3 || Subj))

gcm_Target_v_Unrelated2_meduOnly_no_ranef_cov <- update(
  gcm_Target_v_Unrelated2,
  . ~ medu * (ot1 + ot2 + ot3) + (ot1 + ot2 + ot3 || Subj))

AIC(gcm_Target_v_Semantic_EVT_no_ranef_cov,gcm_Target_v_Semantic_meduOnly_no_ranef_cov )
BIC(gcm_Target_v_Semantic_EVT_no_ranef_cov,gcm_Target_v_Semantic_meduOnly_no_ranef_cov )

AIC(gcm_Target_v_Unrelated2_EVT_no_ranef_cov,gcm_Target_v_Unrelated2_meduOnly_no_ranef_cov)
BIC(gcm_Target_v_Unrelated2_EVT_no_ranef_cov,gcm_Target_v_Unrelated2_meduOnly_no_ranef_cov)

anova(gcm_Target_v_Semantic_EVT_no_ranef_cov,gcm_Target_v_Semantic_meduOnly_no_ranef_cov )
anova(gcm_Target_v_Unrelated2_EVT_no_ranef_cov,gcm_Target_v_Unrelated2_meduOnly_no_ranef_cov)

# Bundle up models
s2_semy_unre <- list(
  gcm_Target_v_Semantic = gcm_Target_v_Semantic,
  d_semy = d_semy,
  gcm_Target_v_Unrelated2 = gcm_Target_v_Unrelated2,
  #gcm_Target_v_Semantic_EVT = gcm_Target_v_Semantic_EVT,
  #gcm_Target_v_Unrelated2_EVT = gcm_Target_v_Unrelated2_EVT,
  # gcm_Target_v_Semantic_EVT_noCubic = gcm_Target_v_Semantic_EVT_noCubic,
  # gcm_Target_v_Unrelated2_EVT_noCubic = gcm_Target_v_Unrelated2_EVT_noCubic,
  gcm_Target_v_Semantic_EVT_no_ranef_cov = gcm_Target_v_Semantic_EVT_no_ranef_cov,
  gcm_Target_v_Unrelated2_EVT_no_ranef_cov = gcm_Target_v_Unrelated2_EVT_no_ranef_cov
)
save(s2_semy_unre, file = "data/study2/modelling/03_logistic_semy.Rdata")
