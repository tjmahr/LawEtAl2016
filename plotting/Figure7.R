
options(stringsAsFactors = FALSE)
source("./R/FunctionsForRWLAnalysis.R")
source("./R/plotting.R")

library("lme4")
library("broom")

load("./data/study2/modelling/02_bias_analysis.Rdata")

plot_name <- "07_s2_looks_by_evt_by_bias.png"

gcm_target_medu <- s2_a2$gcm_target_medu
s2 <- s2_a2$data

# Combine model fits with raw data
d <- augment(gcm_target_medu) %>%
  as.tbl %>%
  left_join(s2) %>%
  mutate(medu = factor(str_to_title(medu), levels = c("Low", "Middle", "High")))
d

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
  coord_cartesian(ylim = c(-4, 3))
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
ggsave(plot_name, p_bw, width = 6, height = 5, path = "./figs/")

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
