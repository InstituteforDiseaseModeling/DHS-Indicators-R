#!/bin/sh
# install_packages.sh - Install all R packages listed in renv.lock

Rscript -e "install.packages(c(
  'AzureStor',
  'DBI',
  'dplyr',
  'expss',
  'ggplot2',
  'glue',
  'haven',
  'here',
  'jsonlite',
  'knitr',
  'labelled',
  'lubridate',
  'magrittr',
  'matrixStats',
  'openxlsx',
  'optparse',
  'pillar',
  'pkgconfig',
  'plyr',
  'purrr',
  'readr',
  'readxl',
  'renv',
  'sjlabelled',
  'sjmisc',
  'sparklyr',
  'stringr',
  'yaml',
  'zip'
), repos='https://packagemanager.posit.co/cran/latest')"
