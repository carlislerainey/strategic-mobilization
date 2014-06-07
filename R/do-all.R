
# Make sure that working directory is set properly, e.g.,
# setwd("~/Dropbox/projects/strategic-mobilization/")

# Clear workspace
rm(list = ls())

# Create directory for output
dir.create(path = "output", showWarnings = FALSE)
dir.create(path = "doc/figs", showWarnings = FALSE)
dir.create(path = "doc/tabs", showWarnings = FALSE)

# Do each part of the analysis
source("R/build-data-sets.R", echo = TRUE)
source("R/mcmc.R", echo = TRUE)
source("R/figures.R", echo = TRUE)
source("R/robustness-checks.R", echo = TRUE)