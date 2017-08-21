## Figure 1, proportion of looking to each AOI

options(stringsAsFactors = FALSE)
source("./R/FunctionsForRWLAnalysis.R")
source("./R/plotting.R")

analysis_opts <- yaml.load_file("./analysis_options.yaml")
bin_width <- analysis_opts$bin_width

s1_good_phon <- analysis_opts$targets_s1$phono
s1_good_semy <- analysis_opts$targets_s1$semantic
s2_good_phon <- analysis_opts$targets_s2$phono
s2_good_semy <- analysis_opts$targets_s2$semantic

s1_looks <- read_csv("./data/study1/02_looking_data.csv")
s2_looks <- read_csv("./data/study2/01_looking_data.csv")

## Study 1 --------------------------------------------------------------------

# Exclude trials with more than 50% missing data
bad_trials <- find_mistracked_trials(s1_looks, analysis_opts$excessive_na)
s1_looks <- anti_join(s1_looks, bad_trials)

# Use all trials for target, unrelated curves
s1_binned <- s1_looks %>%
  standard_model_pipeline(bins = bin_width) %>%
  select(Subj:Trials)

# Use good phon foils for phon curve
s1_phonological <- s1_looks %>%
  filter(is.element(Target, s1_good_phon)) %>%
  standard_model_pipeline(bins = bin_width) %>%
  select(Subj:Trials)

# Use good semy foils for semy curve
s1_semantic <- s1_looks %>%
  filter(is.element(Target, s1_good_semy)) %>%
  standard_model_pipeline(bins = bin_width) %>%
  select(Subj:Trials)

# Compute proportion of looking curves for each AOI
s1_to_targ <- s1_binned %>%
  mutate(Proportion = ToTarget / ToAOIs, AOI = "Target")

s1_to_unre <- s1_binned %>%
  mutate(Proportion = ToUnrelated / ToAOIs, AOI = "Unrelated Foil")

s1_to_phon <- s1_phonological %>%
  mutate(Proportion = ToPhonological / ToAOIs, AOI = "Phonological Foil")

s1_to_semy <- s1_semantic %>%
  mutate(Proportion = ToSemantic / ToAOIs, AOI = "Semantic Foil")

# Bundle the four curves together
aoi_labs <- c("Target", "Phonological Foil", "Semantic Foil", "Unrelated Foil")
s1_plotting_data <- bind_rows(s1_to_targ, s1_to_unre, s1_to_phon, s1_to_semy) %>%
  mutate(AOI = factor(AOI, levels = aoi_labs))


## Study 2 --------------------------------------------------------------------

# Use all trials for target, unrelated curves
s2_binned <- s2_looks %>%
  standard_model_pipeline(bins = bin_width) %>%
  select(Subj:Trials)

# Use good phon foils for phon curve
s2_phonological <- s2_looks %>%
  filter(is.element(Target, s2_good_phon)) %>%
  standard_model_pipeline(bins = bin_width) %>%
  select(Subj:Trials)

# Use good semy foils for semy curve
s2_semantic <- s2_looks %>%
  filter(is.element(Target, s2_good_semy)) %>%
  standard_model_pipeline(bins = bin_width) %>%
  select(Subj:Trials)

# Compute proportion of looking curves for each AOI
s2_to_targ <- s2_binned %>%
  mutate(Proportion = ToTarget / ToAOIs, AOI = "Target")

s2_to_unre <- s2_binned %>%
  mutate(Proportion = ToUnrelated / ToAOIs, AOI = "Unrelated Foil")

s2_to_phon <- s2_phonological %>%
  mutate(Proportion = ToPhonological / ToAOIs, AOI = "Phonological Foil")

s2_to_semy <- s2_semantic %>%
  mutate(Proportion = ToSemantic / ToAOIs, AOI = "Semantic Foil")

# Bundle the four curves together
aoi_labs <- c("Target", "Phonological Foil", "Semantic Foil", "Unrelated Foil")
s2_plotting_data <- bind_rows(s2_to_targ, s2_to_unre, s2_to_phon, s2_to_semy) %>%
  mutate(AOI = factor(AOI, levels = aoi_labs))


## Plotting -------------------------------------------------------------------

# Constants for plotting, defined in R/plotting.R
str(plotting)

s1_plot_name <- "01_s1_looks_at_aois.png"
s2_plot_name <- "05_s2_looks_at_aois.png"

# Create the prototype plot from study1 data
p_s1_no_theme <- ggplot(s1_plotting_data) +
  aes(x = Time, y = Proportion, shape = AOI) +
  stat_summary(fun.data = mean_se, geom = "pointrange") +
  labs(x = plotting$lab_x_gaze,
       y = plotting$lab_y_gaze_aoi) +
  scale_shape_manual(name = plotting$lab_aoi,
                     values = plotting$shapes_circ_tri_box_x) +
  scale_y_continuous(breaks = plotting$breaks_prop) +
  coord_cartesian(ylim = c(0.0, 0.70), xlim = c(200, 1750), expand = FALSE)
p_s1_no_theme

# Theming stage
p_s1_bw <- p_s1_no_theme +
  theme_light(plotting$base_font_size) +
  minimal_lower_legend +
  bold_axis_titles +
  unscaled_axis_text +
  no_legend_title
p_s1_bw

# Alternative color/BUCLD theming
p_s1_color <- p_s1_no_theme +
  aes(color = AOI) +
  theme_light() +
  unscaled_axis_text +
  minimal_lower_legend +
  bigger_legend_text +
  no_legend_title
p_s1_color

ggsave(s1_plot_name, p_s1_bw, width = 6, height = 5, path = "./figs")
ggsave(s1_plot_name, p_s1_color, width = 6, height = 5, path = "./figs/color")

# Replace the data in the s1 plots with the s2 data-frame
p_s2_bw <- p_s1_bw %+% s2_plotting_data
p_s2_color <- p_s1_color %+% s2_plotting_data

ggsave(s2_plot_name, p_s2_bw, width = 6, height = 5, path = "./figs")
ggsave(s2_plot_name, p_s2_color, width = 6, height = 5, path = "./figs/color")
