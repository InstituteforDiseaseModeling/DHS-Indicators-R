#!/bin/sh
# install_packages.sh - Install all R packages listed in renv.lock

#  ensures GDAL, PROJ, GEOS, UDUNITS2 are available for sf package
sudo apt-get update
sudo apt-get install -y \
  gdal-bin libgdal-dev \
  libproj-dev libgeos-dev libudunits2-dev \
  build-essential python3-dev \
  libssl-dev mysql-client libmysqlclient-dev \
  libsqlite3-dev
  
Rscript -e "install.packages(c(
  'AzureStor',
  'DBI',
  'dplyr',
  'expss',
  'ggplot2',
  'glue',
  'gridExtra',
  'grid',
  'haven',
  'here',
  'jsonlite',
  'kableExtra',
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
  'rmarkdown',
  'sjlabelled',
  'sjmisc',
  'sf',
  'sparklyr',
  'stringr',
  'yaml',
  'zip'
), repos='https://packagemanager.posit.co/cran/latest')"
