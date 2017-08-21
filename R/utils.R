## Summary stats



# logits to proportions
inv_logit <- gtools::inv.logit

# normal approximation of p-values
normal_approx <- function(ts) 2 * (1 - pnorm(abs(ts)))

mean_center <- function(xs) xs - mean(xs, na.rm = TRUE)


# Split by +/-1 SD
cut_by_sd <- function(xs) {
  min_low_upp_max <- c(min(xs), mean(xs) - sd(xs), mean(xs) + sd(xs), max(xs))
  labs <- c(">1SD Below", "Within 1SD of Mean", ">1SD Above")
  cut(xs, breaks = min_low_upp_max, labels = labs, include.lowest = TRUE)
}

# Median split
cut_by_median <- function(xs) {
  min_mid_max <- fivenum(xs)[c(1, 3, 5)]
  labs <- c("Bottom Half", "Top Half")
  cut(xs, breaks = min_mid_max, labels = labs, include.lowest = TRUE)
}

# Tertile split
cut_by_thirds <- function(xs) {
  thirds <- quantile(xs, probs = seq(0, 1, by = 1 / 3))
  labs <- c("Bottom Third", "Middle Third", "Top Third")
  cut(xs, breaks = thirds, labels = labs, include.lowest = TRUE)
}




## Helper functions

# Used to filter out elements out of a list
is_not_element <- Negate(is.element)

# source all files in a directory
source_dir <- function(path, ...) {
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
    cat(nm, ":")
    source(file.path(path, nm), ...)
    cat("\n")
  }
}
