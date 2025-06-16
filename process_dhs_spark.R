# process_dhs_spark.R
# Purpose: Loop over 'Chap' folders, find and process run_indicators_spark.R using Spark, and export results to CSV

library(sparklyr)
library(dplyr)
library(stringr)
library(DBI)
library(purrr)
library(here)

# Connect to Databricks Spark (adjust config as needed)
sc <- spark_connect(method = "databricks")

# Get all views in the 'dhs_recode' catalog
all_views <- dbGetQuery(sc, "SHOW VIEWS IN dhs_recode")
view_names <- all_views$name

# Helper to extract country code and version from view name
decode_view <- function(view) {
  m <- str_match(view, "([A-Z]{2,3})([A-Z]*)_([im]r)([0-9]+)")
  if (is.na(m[1,1])) return(NULL)
  list(
    country_code = m[1,2],
    ir_or_mr = m[1,4],
    version = m[1,5],
    view = view
  )
}

# Get all Chap folders
chap_dirs <- list.dirs(path = here::here(), recursive = FALSE, full.names = TRUE)
chap_dirs <- chap_dirs[grepl("Chap[0-9]{2,}", basename(chap_dirs))]

# Loop over Chap folders
for (chap_dir in chap_dirs) {
  chap_num <- str_extract(basename(chap_dir), "[0-9]{2,}")
  run_file <- file.path(chap_dir, "run_indicators_spark.R")
  if (!file.exists(run_file)) next
  source(run_file)

  # Find all IR and MR views for this chapter
  ir_views <- view_names[grepl("_ir[0-9]+", view_names, ignore.case = TRUE)]
  mr_views <- view_names[grepl("_mr[0-9]+", view_names, ignore.case = TRUE)]

  # Process IR views
  for (view in ir_views) {
    meta <- decode_view(view)
    if (is.null(meta)) next
    ir_tbl <- tbl(sc, sql(paste0('SELECT * FROM dhs_recode.', view)))
    ir_df <- collect(ir_tbl)
    res <- run_indicators(IRdata = ir_df, MRdata = NULL)
    out <- res$IRdata
    if (!is.null(out)) {
      out$filename <- view
      out$country <- meta$country_code
      out_path <- file.path("/dbfs/volume", paste0(view, "_", chap_num, ".csv"))
      write.csv(out, out_path, row.names = FALSE)
    }
  }

  # Process MR views
  for (view in mr_views) {
    meta <- decode_view(view)
    if (is.null(meta)) next
    mr_tbl <- tbl(sc, sql(paste0('SELECT * FROM dhs_recode.', view)))
    mr_df <- collect(mr_tbl)
    res <- run_indicators(IRdata = NULL, MRdata = mr_df)
    out <- res$MRdata
    if (!is.null(out)) {
      out$filename <- view
      out$country <- meta$country_code
      out_path <- file.path("/dbfs/volume", paste0(view, "_", chap_num, ".csv"))
      write.csv(out, out_path, row.names = FALSE)
    }
  }
}

spark_disconnect(sc)
