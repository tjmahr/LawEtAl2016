# Reference for analysis
# http://stackoverflow.com/questions/21082396/multinomial-logistic-multilevel-models-in-r

# when lmer fails to converge
# http://stats.stackexchange.com/questions/97929/lmer-model-fails-to-converge
# http://stackoverflow.com/questions/21344555/convergence-error-for-development-version-of-lme4

options(stringsAsFactors = FALSE)
source("R/FunctionsForRWLAnalysis.R")
library("lme4")

# Load appropriate data sets
data_dir <- "./data/study1/modelling"

data_in_phon <- file.path(data_dir, "03_logistic_phon.csv")
data_in_semy <- file.path(data_dir, "03_logistic_semy.csv")
data_out_phon <- file.path(data_dir, "03_logistic_phon_models.Rdata")
data_out_semy <- file.path(data_dir, "03_logistic_semy_models.Rdata")

d_phon <- read_csv(data_in_phon)
d_semy <- read_csv(data_in_semy)




## Comparison of Target v. Phonological and Target v. Unrelated

# Check for no zeroes
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

summary(gcm_Target_v_Phonological)
summary(gcm_Target_v_Unrelated)

# Add EVT to models
gcm_Target_v_Phonological_EVT <- update(
  gcm_Target_v_Phonological,
  . ~ cEVT_GSV * (ot1 + ot2) + (ot1 + ot2 | Subj))

gcm_Target_v_Unrelated_EVT <- update(
  gcm_Target_v_Unrelated,
  . ~ cEVT_GSV * (ot1 + ot2) + (ot1 + ot2 | Subj))


# The next two models don't include interaction between EVT and time terms.
# Results are similar. Conclusion is to use models with interactions.
gcm_Target_v_Phonological_EVT_noInter <- update(
  gcm_Target_v_Phonological,
  . ~ cEVT_GSV + (ot1 + ot2) + (ot1 + ot2 | Subj))

gcm_Target_v_Unrelated_EVT_noInter <- update(
  gcm_Target_v_Unrelated,
  . ~ cEVT_GSV + (ot1 + ot2) + (ot1 + ot2 | Subj))

summary(gcm_Target_v_Phonological_EVT_noInter)
summary(gcm_Target_v_Unrelated_EVT_noInter)

# Control for EVT AND Age
gcm_Target_v_Phonological_EVT_age <- update(
  gcm_Target_v_Phonological,
  . ~ cEVT_GSV * cAge * (ot1 + ot2) + (ot1 + ot2 | Subj))

gcm_Target_v_Unrelated_EVT_age <- update(
  gcm_Target_v_Unrelated,
  . ~ cEVT_GSV * cAge * (ot1 + ot2) + (ot1 + ot2 | Subj))

summary(gcm_Target_v_Phonological_EVT_age)
summary(gcm_Target_v_Unrelated_EVT_age)

# Don't include age because it doesn't improve model fit.
anova(gcm_Target_v_Phonological_EVT, gcm_Target_v_Phonological_EVT_age)
anova(gcm_Target_v_Unrelated_EVT, gcm_Target_v_Unrelated_EVT_age)

# Bundle up models
e1_phon_unre <- list(
  gcm_Target_v_Phonological = gcm_Target_v_Phonological,
  gcm_Target_v_Unrelated = gcm_Target_v_Unrelated,
  gcm_Target_v_Phonological_EVT = gcm_Target_v_Phonological_EVT,
  gcm_Target_v_Unrelated_EVT = gcm_Target_v_Unrelated_EVT,
  d_phon = d_phon
)

save(e1_phon_unre, file = data_out_phon)




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
gcm_Target_v_Semantic_EVT <- update(
  gcm_Target_v_Semantic,
  . ~ cEVT_GSV * (ot1 + ot2 + ot3) + (ot1 + ot2 + ot3 | Subj))

gcm_Target_v_Unrelated2_EVT <- update(
  gcm_Target_v_Unrelated2,
  . ~ cEVT_GSV * (ot1 + ot2 + ot3) + (ot1 + ot2 + ot3 | Subj))

summary(gcm_Target_v_Semantic_EVT)
summary(gcm_Target_v_Unrelated2_EVT)


# Bundle up models
e1_semy_unre <- list(
  gcm_Target_v_Semantic = gcm_Target_v_Semantic,
  gcm_Target_v_Unrelated2 = gcm_Target_v_Unrelated2,
  gcm_Target_v_Semantic_EVT = gcm_Target_v_Semantic_EVT,
  gcm_Target_v_Unrelated2_EVT = gcm_Target_v_Unrelated2_EVT,
  d_semy = d_semy
)
save(e1_semy_unre, file = data_out_semy)



