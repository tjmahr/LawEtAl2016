# Study 1, analysis 2

options(stringsAsFactors = FALSE)
source("./R/FunctionsForRWLAnalysis.R")
source("./R/pretty_printing.R")
library("lme4")
library("broom")

# Load appropriate data sets
data_dir <- "./data/study1/modelling"

data_in_binary <- file.path(data_dir, "02_binary_bias.csv")
data_in_fourway <- file.path(data_dir, "02_fourway_bias.csv")
data_out <- file.path(data_dir, "02_bias_models.Rdata")

d_binary <- read_csv(data_in_binary)
d_fourway <- read_csv(data_in_fourway)

s1_phon_mod <- filter(d_fourway, Bias == "Phonological")
s1_semy_mod <- filter(d_fourway, Bias == "Semantic")
s1_unre_mod <- filter(d_fourway, Bias == "Unrelated")
s1_targ_mod <- filter(d_fourway, Bias == "Target")

glimpse(d_binary, width = 80)
table(d_binary$Bias)

# Target vs any distractor
gcm_target <- lmer(
  elogit ~ (ot1 + ot2 + ot3) * cEVT_GSV * Bias +
    (ot1 + ot2 + ot3 | Subj) +
    (ot1 + ot2 + ot3 | Subj:Bias),
  data = d_binary, REML = FALSE)

summary(gcm_target)
get_fixed_effects(gcm_target)

# Target vs any distractor including age in the model.
# Don't need to use this because age does not improve model fit.
gcm_target_age <- lmer(
  elogit ~ (ot1 + ot2 + ot3) * Bias +
    (ot1 + ot2 + ot3) * cEVT_GSV +
    (ot1 + ot2 + ot3) * cAge +
    cAge * Bias +
    cEVT_GSV * Bias +
    (ot1 + ot2 + ot3 | Subj) +
    (ot1 + ot2 + ot3 | Subj:Bias),
  data = d_binary, REML = FALSE)

summary(gcm_target_age)
get_fixed_effects(gcm_target_age)

anova(gcm_target, gcm_target_age)

# Visualize the bias effects
d_plot <- augment(gcm_target) %>% as.tbl %>% left_join(d_binary)
ggplot(d_plot, aes(x = Time, y = elogit, color = Bias)) +
  geom_hline(yintercept = qlogis(.25), color = "gray") +
  stat_summary(fun.data = mean_se, geom = "pointrange", alpha = .7, shape = 16) +
  stat_summary(aes(y = .fitted), fun.y = mean, geom = "line", size = 1)

last_plot() + facet_grid(~ EVTBreakThirds)


# EVT effect on recovery from PHONOLOGICAL
gcm_EVT_phon <-lmer(
  elogit ~ (ot1 + ot2) * cEVT_GSV + (ot1 + ot2 | Subj),
  data = s1_phon_mod,
  REML = FALSE)
summary(gcm_EVT_phon)

# EVT effect on recovery from SEMANTIC
gcm_EVT_semy <- update(gcm_EVT_phon, data = s1_semy_mod)
summary(gcm_EVT_semy)

# EVT effect on recovery from UNRELATED
gcm_EVT_unre <- update(gcm_EVT_phon, data = s1_unre_mod)
summary(gcm_EVT_unre)

# Plot the different recovery patterns
d_plot_phon <- augment(gcm_EVT_phon) %>% as.tbl %>% left_join(s1_phon_mod)
d_plot_semy <- augment(gcm_EVT_semy) %>% as.tbl %>% left_join(s1_semy_mod)
d_plot_unre <- augment(gcm_EVT_unre) %>% as.tbl %>% left_join(s1_unre_mod)

d_plot2 <- bind_rows(d_plot_phon, d_plot_semy, d_plot_unre)

# Visualize the bias effects
ggplot(d_plot2, aes(x = Time, y = elogit, color = Bias)) +
  geom_hline(yintercept = qlogis(.25), color = "gray") +
  stat_summary(fun.data = mean_se, geom = "pointrange", linetype = "solid", alpha = 0.7) +
  stat_summary(aes(y = .fitted), fun.y = mean, geom = "line", size = 1)

last_plot() + facet_grid(~ EVTBreakThirds)

# Bundle up the models
s1_a2 <- list(gcm_target = gcm_target,
              gcm_EVT_phon = gcm_EVT_phon,
              gcm_EVT_semy = gcm_EVT_semy,
              gcm_EVT_unre = gcm_EVT_unre,
              s1_unre_mod = s1_unre_mod,
              s1_semy_mod = s1_semy_mod,
              s1_phon_mod = s1_phon_mod,
              d_binary = d_binary)
save(s1_a2, file = data_out)
