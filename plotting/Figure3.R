## Figure 3, target vs distractor curves by EVT bin

options(stringsAsFactors = FALSE)
source("./R/FunctionsForRWLAnalysis.R")
source("./R/utils.R")
source("./R/plotting.R")
library("lme4")
library("broom")

load(file.path("data/study1/modelling", "02_bias_models.Rdata"))
names(s1_a2)

# Unpack model and data
gcm_target <- s1_a2$gcm_target
d_s1_a2 <- s1_a2$d_binary

# Combine model fits with raw data
d <- augment(gcm_target) %>%
  as.tbl %>%
  left_join(d_s1_a2)

plot_name <- "03_looks_by_evt_and_start.png"

p_no_theme <- ggplot(d) +
  aes(x = Time, y = elogit, linetype = Bias, shape = Bias) +
  scale_linetype_manual(values = plotting$lines_sol_box) +
  scale_shape_manual(values = plotting$shapes_box_circ) +
  facet_grid(~ EVTBreakThirds) +
  scale_y_continuous(breaks = plotting$breaks_fatlog,
                     labels = plotting$breaks_fatlog_labs) +
  labs(x = plotting$lab_x_gaze,
       y = plotting$lab_y_gaze_log,
       linetype = plotting$lab_bias,
       shape = plotting$lab_bias) +
  coord_cartesian(ylim = c(-4.1, 3))
p_no_theme

# bw theme
p_bw <- p_no_theme +
  stat_summary(fun.data = mean_se, geom = "pointrange", linetype = "solid",
               color = plotting$col_soft_points, size = .5, stroke = 0.5) +
  stat_summary(aes(y = .fitted), fun.y = mean, geom = "line", size = .75) +
  theme_light(plotting$base_font_size) +
  minimal_lower_legend +
  bold_axis_titles +
  unscaled_axis_text +
  bw_facet_strip
p_bw
ggsave(plot_name, p_bw, width = 6, height = 5, path = "./figs")


# Alternative color/BUCLD theming
p_color <- p_no_theme +
  aes(color = Bias) +
  labs(color = plotting$lab_bias) +
  stat_summary(fun.data = mean_se, geom = "pointrange", linetype = "solid",
               size = .5, alpha = .8) +
  stat_summary(aes(y = .fitted), fun.y = mean, geom = "line",
               color = plotting$col_grey_points, size = .75) +
  theme_light() +
  unscaled_axis_text +
  minimal_lower_legend +
  bigger_legend_text +
  bw_facet_strip
p_color

ggsave(plot_name, p_color, width = 6, height = 5, path = "./figs/color")
