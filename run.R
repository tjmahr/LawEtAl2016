# Run an script in its own session
source_clean <- function(file, ...) {
  # source(file, local = new.env(parent = emptyenv()), ...)
  do <- paste0("Rscript --vanilla --default-packages=methods,datasets,utils,grDevices,graphics,stats ", file)
  system(do)
}

clean_figs <- function() {
  file.remove(list.files("./figs", full.names = TRUE, recursive = TRUE))
  invisible(NULL)
}

clean <- function() {
  file.remove(list.files("./data-raw/study1", "csv", full.names = TRUE))
  file.remove(list.files("./data", "csv", full.names = TRUE, recursive = TRUE))
  file.remove(list.files("./data", "Rdata", full.names = TRUE, recursive = TRUE))
  clean_figs()
  invisible(NULL)
}

# erase everything that the scripts produce
clean_figs()
clean()

# Prep the datasets
source_clean("./data-raw/prep_study1.R")
source_clean("./data-raw/reduce_study1.R")
source_clean("./data-raw/prep_study2.R")

# Study 1 analysis
source_clean("./analysis/s1/01_prep_model_data.R")
source_clean("./analysis/s1/02_plain_gca.R")
source_clean("./analysis/s1/03_bias_analysis.R")
source_clean("./analysis/s1/04_logistic_regression.R")

# Study 1 plots
source_clean("./plotting/Figure1.R")
source_clean("./plotting/Figure2.R")
source_clean("./plotting/Figure3.R")
source_clean("./plotting/Figure4.R")

# Study 2 analysis
source_clean("./analysis/s2/01_prep_model_data.R")
source_clean("./analysis/s2/02_plain_gca.R")
source_clean("./analysis/s2/03_logistic_regression.R")

# Study 2 plots
# source_clean("./plotting/Figure1.R")
source_clean("./plotting/Figure6.R")
source_clean("./plotting/Figure7.R")
source_clean("./plotting/Figure8.R")

file.remove("./Rplots.pdf")
