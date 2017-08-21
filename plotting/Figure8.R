options(stringsAsFactors = FALSE)
source("./R/FunctionsForRWLAnalysis.R")
source("./R/plotting.R")
library("lme4")
library("broom")

analysis_opts <- yaml.load_file("./analysis_options.yaml")
converging_time <- analysis_opts$convergence$s2$phono

# Load the logistic regression models and the datasets used to fit the models

load("./data/study2/modelling/03_logistic_phon.Rdata")
names(s2_phon_unre)

load("./data/study2/modelling/03_logistic_semy.Rdata")
names(s2_semy_unre)

## Assemble the dataframes for the phonological models ------------------------
phon <- list(fits = list(), raw = list())

# Extract model's input data and fitted values and attach to full data-set, so
# we can map orthogonal times to Time values
phon$fits$phon <- augment(s2_phon_unre$gcm_Target_v_Phonological_EVT) %>%
  select(Subj, ot1, ot2, Fitted = .fitted) %>%
  inner_join(s2_phon_unre$d_phon) %>%
  mutate(Comparison = "Target vs. Phonological")

phon$fits$unre <- augment(s2_phon_unre$gcm_Target_v_Unrelated_EVT) %>%
  select(Subj, ot1, ot2, Fitted = .fitted) %>%
  inner_join(s2_phon_unre$d_phon) %>%
  mutate(Comparison = "Target vs. Unrelated")

# The solid lines (model fits) in 4a will come from this dataframe
phon$fits$both <- bind_rows(phon$fits$phon, phon$fits$unre)

# But the model fits only cover a subset of the data-set, so we need to prepare
# a data-frame for the data that did not enter the model

# Load the untrimmed dataframe
phon$raw$raw <- read_csv("data/study2/03_logistic_phon_whole_window.csv")

# Create a raw dataset for each comparison
phon$raw$unre <- phon$raw$raw %>%
  mutate(PropTarget = ToTarget / (ToTarget + ToUnrelated),
         Comparison = "Target vs. Unrelated")
phon$raw$phon <- phon$raw$raw %>%
  mutate(PropTarget = ToTarget / (ToTarget + ToPhonological),
         Comparison = "Target vs. Phonological")

# The data for the points will come from this data frame
phon$raw$both <- bind_rows(phon$raw$unre, phon$raw$phon)

str(phon, max.level = 2)



## Assemble the dataframes for the semantic models ----------------------------
semy <- list(fits = list(), raw = list())

# Extract model's input data and fitted values and attach to full data-set, so
# we can map orthogonal times to Time values
semy$fits$semy <- augment(s2_semy_unre$gcm_Target_v_Semantic_EVT_no_ranef_cov) %>%
  select(Subj, ot1, ot2, Fitted = .fitted) %>%
  inner_join(s2_semy_unre$d_semy) %>%
  mutate(Comparison = "Target vs. Semantic")

semy$fits$unre <- augment(s2_semy_unre$gcm_Target_v_Unrelated2_EVT_no_ranef_cov) %>%
  select(Subj, ot1, ot2, Fitted = .fitted) %>%
  inner_join(s2_semy_unre$d_semy) %>%
  mutate(Comparison = "Target vs. Unrelated")

# The solid lines (model fits) in 4a will come from this dataframe
semy$fits$both <- bind_rows(semy$fits$semy, semy$fits$unre)

# Create a raw dataset for each comparison
semy$raw$unre <- s2_semy_unre$d_semy %>%
  mutate(PropTarget = ToTarget / (ToTarget + ToUnrelated),
         Comparison = "Target vs. Unrelated")
semy$raw$semy <- s2_semy_unre$d_semy %>%
  mutate(PropTarget = ToTarget / (ToTarget + ToSemantic),
         Comparison = "Target vs. Semantic")

# The data for the points will come from this data frame
semy$raw$both <- bind_rows(semy$raw$unre, semy$raw$semy)

str(semy, max.level = 2)



## Create the template for the plots ------------------------------------------

plot_name <- "08_s2_by_foil.png"

# We're going to use "fake" facets to create labels above each figure
phon$raw$both$facet <- "A"
semy$raw$both$facet <- "B"

# Create a generic plot layout for these figures
fig_template <- ggplot() +
  # Reference line for chance performance at 50%
  geom_hline(yintercept =  0.5, colour = "grey20") +
  labs(x = "Time Relative to Target Onset (ms)",
       y = "Proportion of Looks to Target",
       linetype = "", shape = "") +
  # Set default styles for lines, points
  scale_linetype_manual(values = plotting$lines_sol_box) +
  scale_shape_manual(values = c(2, 1)) +
  scale_y_continuous(breaks = plotting$breaks_prop) +
  coord_cartesian(ylim = c(0.0, 0.95), xlim = c(200, 1750), expand = FALSE)
fig_template

# Define a bw and color templates
fig_template_bw <- fig_template +
  theme_light(plotting$base_font_size - 1) +
  theme(legend.key.height = unit(8, "points")) +
  # Arrange elements in the legend
  guides(linetype = guide_legend(nrow = 2, byrow = TRUE)) +
  minimal_lower_legend +
  bold_axis_titles +
  unscaled_axis_text +
  bw_facet_strip
fig_template_bw

fig_template_color <- fig_template +
  aes(color = Comparison) + labs(color = "") +
  theme_light() +
  bigger_legend_text +
  guides(linetype = guide_legend(nrow = 1, byrow = TRUE)) +
  minimal_lower_legend +
  bold_axis_titles +
  unscaled_axis_text +
  bw_facet_strip
fig_template_color



## BW plots -------------------------------------------------------------------

# Set data-source for phon figure (including convergence line)
p_phon <- fig_template_bw +
  # Include reference line at convergence point
  geom_vline(xintercept = converging_time, colour = "grey20",
             linetype = "longdash")
p_phon <- p_phon %+% phon$raw$both
p_phon

# Draw the data with bw colors in mind
p_phon_bw <- p_phon +
  # Draw mean+se of raw data
  stat_summary(aes(x = Time, y = PropTarget, shape = Comparison),
               fun.data = mean_se, geom = "pointrange",
               linetype = "solid", size = .5, stroke = .5,
               color = plotting$col_soft_points) +
  # Draw mean model fit
  stat_summary(aes(x = Time, y = plogis(Fitted), linetype = Comparison),
               data = phon$fits$both,
               fun.y = mean, geom = "line", size = .75) +
  facet_wrap("facet")
p_phon_bw

# Set data-source for semy figure
p_semy <- fig_template_bw %+% semy$raw$both

# Draw the data with bw colors in mind
p_semy_bw <- p_semy +
  # Draw mean+se of raw data
  stat_summary(aes(x = Time, y = PropTarget, shape = Comparison),
               fun.data = mean_se, geom = "pointrange",
               linetype = "solid", size = .5, stroke = .5,
               color = plotting$col_soft_points) +
  # Draw mean model fit
  stat_summary(aes(x = Time, y = plogis(Fitted), linetype = Comparison),
               data = semy$fits$both,
               fun.y = mean,  geom = "line", size = .75) +
  facet_wrap("facet")
p_semy_bw

figure8_bw <- arrangeGrob(p_phon_bw, p_semy_bw, nrow = 1)
figure8_bw

ggsave(plot_name, figure8_bw, width = 6, height = 5, path = "./figs/")



## Color plots ----------------------------------------------------------------

# Set data-source for phon figure (including convergence line)
p_phon2 <- fig_template_color +
  # Include reference line at convergence point
  geom_vline(xintercept = converging_time, colour = "grey20",
             linetype = "longdash")
p_phon2 <- p_phon2 %+% phon$raw$both
p_phon2

# Don't merge the plots in color version
p_phon_color <- p_phon2 +
  aes(color = Comparison) + labs(color = "") +
  # Draw mean+se of raw data
  stat_summary(aes(x = Time, y = PropTarget, shape = Comparison),
               fun.data = mean_se, geom = "pointrange",
               linetype = "solid") +
  # Draw mean model fit
  stat_summary(aes(x = Time, y = plogis(Fitted), linetype = Comparison),
               data = phon$fits$both, fun.y = mean, geom = "line",
               color = plotting$col_grey_points, size = .75)
p_phon_color

ggsave("s2_phon_vs_unre.png", p_phon_color, width = 6, height = 5, path = "./figs/color")


# Set data-source for semy figure
p_semy2 <- fig_template_color %+% semy$raw$both

p_semy_color <- p_semy2 +
  aes(color = Comparison) + labs(color = "") +
  # Draw mean+se of raw data
  stat_summary(aes(x = Time, y = PropTarget, shape = Comparison),
               fun.data = mean_se, geom = "pointrange",
               linetype = "solid") +
  # Draw mean model fit
  stat_summary(aes(x = Time, y = plogis(Fitted), linetype = Comparison),
               data = semy$fits$both, fun.y = mean, geom = "line",
               color = plotting$col_grey_points, size = .75)
p_semy_color

ggsave("s2_semy_vs_unre.png", p_semy_color, width = 6, height = 5, path = "./figs/color")

