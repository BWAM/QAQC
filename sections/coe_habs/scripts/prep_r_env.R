
# Load Packages -----------------------------------------------------------
# renv::update() # Will update all packages
library(DT)
library(here)
library(tidyverse)
# BWAM Packages---------
library(ALS)


# Establish the Root Directory --------------------------------------------
root_dir <- here::here()

# Set Global Knitr Options ------------------------------------------------
knitr::opts_chunk$set(
  # Hide all code chunks in the rendered report.
  echo = FALSE, 
  # Set a standard Figure width and height.
  fig.width = 6,
  fig.height = 6#,
  # message = FALSE,
  # warning = FALSE,
  # error = FALSE
)
