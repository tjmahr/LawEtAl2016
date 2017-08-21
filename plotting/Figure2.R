## Figure 2, looking to target by evt bin

options(stringsAsFactors = FALSE)
source("./R/FunctionsForRWLAnalysis.R")
source("./R/utils.R")
source("./R/plotting.R")
library("lme4")
library("broom")


# Unpack model and data
load(file.path("./data/study1/modelling", "01_gca_fit_models.Rdata"))
names(e1_a1)
gcm_EVTAge <- e1_a1$gcm_EVTAge
s1 <- e1_a1$data

# Combine model fits with raw data
d <- augment(gcm_EVTAge) %>%
  as.tbl %>%
  left_join(s1)

plot_name <- "02_s1_looks_by_evt.png"
str(plotting)

p <- ggplot(d) +
  aes(x = Time, y = elogit, linetype = EVTBreakThirds, shape = EVTBreakThirds) +
  scale_linetype_manual(values = c("solid", "11", "longdash")) +
  scale_shape_manual(values = plotting$shapes_down_box_up) +
  scale_y_continuous(breaks = plotting$breaks_log,
                     labels = plotting$breaks_log_labs) +
  labs(x = plotting$lab_x_gaze,
       y = plotting$lab_y_gaze_log,
       linetype = plotting$lab_evt,
       shape = plotting$lab_evt) +
  coord_cartesian(ylim = c(-1.5, 1))
p

# BW version
p_bw <- p +
  # We have to draw the data separately in each theme because the color version
  # using a column to get its colors but the bw version used a hard-coded
  # grey.
  stat_summary(fun.data = mean_se, geom = "pointrange",
               linetype = "solid", color = plotting$col_soft_points) +
  stat_summary(aes(y = .fitted), fun.y = mean, geom = "line", size = .75) +
  theme_light(plotting$base_font_size) +
  minimal_lower_legend +
  bold_axis_titles +
  unscaled_axis_text
p_bw

ggsave(plot_name, p_bw, width = 6, height = 5, path = "./figs")

# Alternative color/BUCLD theming
p_color <- p +
  aes(color = EVTBreakThirds) +
  labs(color = plotting$lab_evt) +
  stat_summary(fun.data = mean_se, geom = "pointrange", linetype = "solid") +
  stat_summary(aes(y = .fitted), fun.y = mean, geom = "line", size = .75) +
  theme_light(plotting$base_font_size) +
  unscaled_axis_text +
  minimal_lower_legend +
  bigger_legend_text
p_color

ggsave(plot_name, p_color, width = 6, height = 5, path = "./figs/color")


