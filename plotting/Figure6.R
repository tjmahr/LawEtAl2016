options(stringsAsFactors = FALSE)
source("./R/FunctionsForRWLAnalysis.R")
source("./R/plotting.R")

library("lme4")
library("broom")

load("./data/study2/modelling/01_plain_gca.Rdata")

gcm_EVT_medu_age <- s2_a1$gcm_EVT_medu_age
s2 <- s2_a1$data

# Combine model fits with raw data
d <- augment(gcm_EVT_medu_age) %>%
  as.tbl %>%
  left_join(s2) %>%
  mutate(medu = factor(str_to_title(medu), levels = c("Low", "Middle", "High")))
d

plot_name <- "06_s2_looks_by_evt_by_medu.png"

# Define base features
p_no_theme <- ggplot(d) +
  aes(x = Time, y = elogit) +
  aes(linetype = medu, shape = medu) +
  scale_linetype_manual(values = c("solid", "dashed", "11")) +
  scale_shape_manual(values = plotting$shapes_down_box_up) +
  scale_y_continuous(breaks = plotting$breaks_fatlog,
                     labels = plotting$breaks_fatlog_labs) +
  labs(x = plotting$lab_x_gaze,
       y = plotting$lab_y_gaze_log,
       linetype = plotting$lab_medu,
       shape = plotting$lab_medu) +
  facet_grid( ~ EVTBreakThirds) +
  coord_cartesian(ylim = c(-2.5, 2.5))
p_no_theme


# As in earlier scripts, we have to draw the geoms separately for the bw vs
# color plots since we manually set the color in the bw plot.
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
p_bw + mytheme



ggsave(plot_name, p_bw, width = 6, height = 5, path = "./figs/")

# Alternative color/BUCLD theming
p_color <- p_no_theme +
  aes(color = medu) +
  labs(color = plotting$lab_medu) +
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
