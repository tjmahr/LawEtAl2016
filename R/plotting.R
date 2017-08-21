# Format used for manuscript plotting

library("ggplot2")
library("gridExtra")
library("grid")

plotting <- list(
  base_font_size = 11,
  lab_x_gaze = "Time Relative to Target Onset (ms)",
  lab_y_gaze_log = "Proportion of Looks to Target (log-scale)",
  lab_y_gaze_aoi = "Proportion of Looks to Each AOI",
  lab_aoi = "AOI",
  lab_bias = "Initial Look",
  lab_evt = "EVT-GSV",
  lab_medu = "Maternal Ed.",
  col_grey_points = "grey30",
  col_soft_points = "grey50",
  lines_sol_box = c("solid", 11),
  shapes_down_box_up = c(6, 0, 2),
  shapes_circ_tri_box_x = c(1, 2, 0, 4),
  shapes_box_circ = c(0, 1),
  breaks_log = seq(from = -3, to = 3, by = .5),
  breaks_fatlog = seq(from = -4, to = 4, by = 1),
  breaks_prop = seq(from = 0, to = 0.9, by = 0.1))

log_to_lab <- . %>% plogis %>% round(2)
plotting$breaks_log_labs <- log_to_lab(plotting$breaks_log)
plotting$breaks_fatlog_labs <- log_to_lab(plotting$breaks_fatlog)

## "Themelets": small tweaks that can be added to a plot's theme/

# Move the legend from the right to the bottom, remove the rectangle
# around/behind symbols in legend, remove legend title.
minimal_lower_legend <-
  theme(legend.position = 'bottom',
        legend.key = element_blank(),
        # Remove margin legend to minimize distance between axis title and legend
        legend.margin = unit(0, "pt"))

# Set of the legend title by using a bold font-face, not a larger font size
scale_legend_title <-
  theme(legend.title = element_text(face = "bold", size = rel(.8)))

no_legend_title <- theme(legend.title = element_blank())

# Use a bold, full-size font on the axis titles
bold_axis_titles <-
  theme(axis.title = element_text(face = "bold", size = rel(1)))

# Don't shrink the text in axis labels (set size to rel(1)). To compensate for
# larger text, add a 2.4 pt bottom-margin under x-axis label to give a little
# buffer between axis label and axis title.
unscaled_axis_text <-
  theme(axis.text = element_text(size = rel(1)),
        axis.text.x = element_text(margin = margin(2.4, 0, 2.4, 0)))



# Bump up the legend text size (for plots that will appear on slides)
bigger_legend_text <-
  theme(legend.text = element_text(size = rel(.9)),
        legend.title = element_text(face = "bold", size = rel(.9)))

bigger_legend_text2 <-
  theme(legend.text = element_text(size = rel(.9)),
        legend.title = element_text(size = rel(.9)))

bw_facet_strip <-
  theme(strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(color = "black", size = rel(1)))


# Old theme options that were used in first submission. As they reincorporated
# into the above "theme-lets", I delete the original lines.

theme_franzo <- theme_bw(plotting$base_font_size) +
  minimal_lower_legend +
  bw_facet_strip +
  theme(strip.background = element_rect(colour = NA),
        axis.title.x = element_text(vjust = -0.5, face = "bold"),
        axis.title.y = element_text(vjust = 0.4, face = "bold"),
        legend.text = element_text(size = 8),
        legend.key.height = unit(8, "points"),
        # panel.spacing = unit(20, "points"),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white")
  )
