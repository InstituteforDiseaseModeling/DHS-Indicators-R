# ******************************************************************************
# Program: 			FP_Report.R
# Purpose: 			Create PDF report with family planning tables using kable
# Author:			Mei-kang Wu
# Date created: 	July 1, 2025
# ******************************************************************************

# Load required libraries
library(dplyr)
library(expss)
library(knitr)
library(kableExtra)
library(rmarkdown)
library(gridExtra)
library(grid)


# Function to replace column names with variable labels
# replace_with_labels <- function(df, data) {
#   new_names <- colnames(df)
#   for (i in seq_along(new_names)) {
#     col_name <- new_names[i]
#     # Try to get variable label from the data
#     if (col_name %in% names(data)) {
#       label <- attr(data[[col_name]], "label")
#       if (!is.null(label) && label != "") {
#         new_names[i] <- label
#       }
#     }
#     # Handle special case for total column
#     if (col_name == "total()") new_names[i] <- "Total"
#   }
#   colnames(df) <- new_names
#   return(df)
# }

# Function to extract only yes labels
filter_rows <- function(df){
  # Filter rows where row_labels end with "|yes"
  filtered_df <- df[grepl("\\|yes$", df$row_labels), ]
  
  # Remove "|yes" from row_labels
  filtered_df$row_labels <- sub("\\|yes$", "", filtered_df$row_labels)
  filtered_df
}

# Function to extract only yes columns
filter_cols <- function(df){
  # Filter columns where column names end with "|no"
  # Remove columns whose names end with "|no"
  filtered_df <- df[, !grepl("\\|no$", names(df))]
  names(filtered_df) <- sub("\\|yes$", "", names(filtered_df))
  
  filtered_df
}


# Enhanced function to convert data frame to HTML table with better formatting
df_to_html <- function(df, title) {
  html <- paste0(
    "<h2>", title, "</h2>\n",
    "<div class='table-container'>\n<table>\n",
    "<thead><tr>",
    "<th class='row-header'>Indicator</th>",
    paste0("<th>", colnames(df)[-1], "</th>", collapse = ""),
    "</tr></thead>\n<tbody>\n"
  )
  
  for (i in seq_len(nrow(df))) {
    html <- paste0(html, "<tr>")
    # first column: row label
    html <- paste0(html, "<td class='row-header'>", df[i,1], "</td>")
    
    for (j in 2:ncol(df)) {
      raw <- df[i, j]
      # treat NA as empty cell
      if (is.na(raw)) {
        cell <- ""
      } else {
        # try to coerce to numeric
        num <- suppressWarnings(as.numeric(raw))
        if (!is.na(num)) {
          # round + format to one decimal
          cell <- format(round(num, 1), nsmall = 1)
        } else {
          cell <- raw
        }
      }
      html <- paste0(html, "<td>", cell, "</td>")
    }
    
    html <- paste0(html, "</tr>\n")
  }
  
  html <- paste0(html, "</tbody></table>\n</div>\n")
  html
}


CREATE_REPORT <- function(result_data, output_dir, result_filename, mr=FALSE, has_geospatial=FALSE) {
  
  # Set expss package options to show one decimal place
  expss_digits(digits=1)
  
  # Create dynamic filename based on IR data file
  ir_basename <- tools::file_path_sans_ext(basename(result_filename))
  report_filename <- paste0(ir_basename, "_family_planning_report.html")
  html_filename <- file.path(output_dir, report_filename)
  
  # Create enhanced HTML content with better styling
  html_content <- paste0(
    "<!DOCTYPE html>\n<html>\n<head>\n",
    "<meta charset='UTF-8'>\n",
    "<title>Family Planning Indicators Report ",
    basename(result_filename),
    "</title>\n",
    "<style>\n",
    "body { \n",
    "  font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; \n",
    "  margin: 20px; \n",
    "  background-color: #f8f9fa;\n",
    "}\n",
    "h1 { \n",
    "  color: #2c3e50; \n",
    "  text-align: center; \n",
    "  border-bottom: 3px solid #3498db;\n",
    "  padding-bottom: 10px;\n",
    "}\n",
    "h2 { \n",
    "  color: #34495e; \n",
    "  margin-top: 40px;\n",
    "  padding: 10px;\n",
    "  background-color: #ecf0f1;\n",
    "  border-left: 5px solid #3498db;\n",
    "}\n",
    ".table-container { \n",
    "  overflow-x: auto; \n",
    "  margin: 20px 0;\n",
    "  box-shadow: 0 2px 8px rgba(0,0,0,0.1);\n",
    "  border-radius: 8px;\n",
    "  background-color: white;\n",
    "}\n",
    "table { \n",
    "  border-collapse: collapse; \n",
    "  width: 100%; \n",
    "  min-width: 800px;\n",
    "}\n",
    "th { \n",
    "  background-color: #3498db; \n",
    "  color: white; \n",
    "  font-weight: bold;\n",
    "  padding: 12px 8px;\n",
    "  text-align: left;\n",
    "  position: sticky;\n",
    "  top: 0;\n",
    "  z-index: 10;\n",
    "}\n",
    "td { \n",
    "  border: 1px solid #bdc3c7; \n",
    "  padding: 8px; \n",
    "  text-align: left;\n",
    "}\n",
    "tr:nth-child(even) {\n",
    "  background-color: #f8f9fa;\n",
    "}\n",
    "tr:hover {\n",
    "  background-color: #e8f4fd;\n",
    "}\n",
    ".row-header {\n",
    "  background-color: #ecf0f1 !important;\n",
    "  font-weight: bold;\n",
    "  position: sticky;\n",
    "  left: 0;\n",
    "  z-index: 5;\n",
    "}\n",
    ".summary {\n",
    "  background-color: #d5e8d4;\n",
    "  padding: 15px;\n",
    "  margin: 20px 0;\n",
    "  border-radius: 5px;\n",
    "  border-left: 5px solid #27ae60;\n",
    "}\n",
    "</style>\n</head>\n<body>\n",
    "<h1>Family Planning Indicators Report for ",
    basename(result_filename),
    "</h1>\n",
    "<div class='summary'>\n",
    "<strong>Report Summary:</strong> This report contains family planning knowledge and usage patterns across different demographic groups.\n",
    "</div>\n"
  )
  
  if (has_geospatial) {
    html_content <- paste0(html_content,
      "<div class='summary' style='background-color:#e3f2fd; border-left:5px solid #1976d2;'>",
      "<strong>Geospatial data is available for this survey and can be linked to cluster locations.</strong>",
      "</div>\n"
    )
  } else {
    html_content <- paste0(html_content,
      "<div class='summary' style='background-color:#fff9c4; border-left:5px solid #fbc02d;'>",
      "<strong>Warning:</strong> No geospatial data is available for this survey in databricks.",
      "</div>\n"
    )
  }

  # ...existing code...

  if(mr){
    # Survey set the design with survey weights
    result_data <- result_data %>%
      mutate(wt = mv005/1000000)
    
    # Dummy var for all Men
    result_data <- result_data %>% 
      mutate(fp_all = case_when(mv007>0 ~ "all"),
             fp_all = set_label(fp_all, label = "all men"))
    
    table1 <- result_data %>% 
      cross_cpct(
        cell_vars = list(fp_know_any, fp_know_mod, fp_know_fster, fp_know_mster, 
                         fp_know_pill, fp_know_iud, fp_know_inj, fp_know_imp, 
                         fp_know_mcond, fp_know_fcond, fp_know_ec, fp_know_sdm, fp_know_lam, 
                         fp_know_omod, fp_know_trad, fp_know_rhy, fp_know_wthd, fp_know_other),
        col_vars = list(fp_all),
        weight = wt) %>%   
      set_caption("Knowledge of family planning methods, Men")
    
    df1 <- filter_rows(table1)
    
    html_content <- paste0(html_content,
                           df_to_html(df1, "Table 1: Knowledge of Family Planning Methods, Men"),
                           "</body>\n</html>"
    )
  }
  else {
    # Survey set the design with survey weights
    result_data <- result_data %>%
      mutate(wt = v005/1000000)
   
    # Create list of current use of family planning variables
    fp_cruse_list <- result_data %>%  
      select(fp_cruse_any, fp_cruse_mod, fp_cruse_fster, fp_cruse_mster, 
             fp_cruse_pill, fp_cruse_iud, fp_cruse_inj, fp_cruse_imp, 
             fp_cruse_mcond, fp_cruse_fcond, fp_cruse_diaph, fp_cruse_lam, 
             fp_cruse_ec, fp_cruse_omod, fp_cruse_trad, fp_cruse_rhy, 
             fp_cruse_wthd, fp_cruse_other) %>%
      as.list()
    
    # Create population groups that are shown in tables
    
    # Dummy var for all women
    result_data <- result_data %>% 
      mutate(fp_all = case_when(v007>0 ~ "all"),
             fp_all = set_label(fp_all, label = "all women"))
    
    # Only married women
    result_data <- result_data %>% 
      mutate(fp_married = case_when(v502==1 ~ "married"),
             fp_married = set_label(fp_married, label = "currently married women"))
    
    # Sexually active unmarried women (SAUW); sexually active if had sex in last 30 days
    result_data <- result_data %>% 
      mutate(fp_sauw = case_when(v502!=1 & v528<=30 ~ "SAUW"),
             fp_sauw = set_label(fp_sauw, label = "sexually active, unmarried women"))
    
    # Create the 5 tables
    
    # Table 1: FP knowledge of any method, any modern method, any traditional method
    table1 <- result_data %>% 
      cross_cpct(
        cell_vars = list(fp_know_any, fp_know_mod, fp_know_fster, fp_know_mster, 
                         fp_know_pill, fp_know_iud, fp_know_inj, fp_know_imp, 
                         fp_know_mcond, fp_know_fcond, fp_know_ec, fp_know_sdm, fp_know_lam, 
                         fp_know_omod, fp_know_trad, fp_know_rhy, fp_know_wthd, fp_know_other),
        col_vars = list(fp_all, fp_married, fp_sauw),
        weight = wt) %>%   
      set_caption("Knowledge of family planning methods, Women")
    
    
    # Table 2: FP knowledge by background variables
    table2 <- result_data %>% 
      cross_rpct(
        cell_vars = list(v013, v025, v024, v106, v190),
        col_vars = list(fp_know_any, fp_know_mod, total()),
        weight = wt,
        total_label = "Weighted N",
        total_statistic = "w_cases",
        ) %>%   
      set_caption("Knowledge of family planning methods by background variables")
    
    # Table 3: Current use by age, all women
    table3 <- result_data %>% 
      cross_rpct(
        cell_vars = v013,
        col_vars = list(total(), fp_cruse_any, fp_cruse_mod, fp_cruse_fster, fp_cruse_mster, 
                        fp_cruse_pill, fp_cruse_iud, fp_cruse_inj, fp_cruse_imp, 
                        fp_cruse_mcond, fp_cruse_fcond, fp_cruse_diaph, fp_cruse_lam, 
                        fp_cruse_ec, fp_cruse_omod, fp_cruse_trad, fp_cruse_rhy, 
                        fp_cruse_wthd, fp_cruse_other),
        weight = wt,
        total_label = "Weighted N",
        total_statistic = "w_cases",
        total_row_position = c("below")) %>%  
      set_caption("Current use by age, all women")
    
    # Table 4: Current use by age, currently married women
    table4 <- result_data %>% 
      filter(v502==1) %>%
      cross_rpct(
        cell_vars = v013,
        col_vars = list(total(), fp_cruse_any, fp_cruse_mod, fp_cruse_fster, fp_cruse_mster, 
                        fp_cruse_pill, fp_cruse_iud, fp_cruse_inj, fp_cruse_imp, 
                        fp_cruse_mcond, fp_cruse_fcond, fp_cruse_diaph, fp_cruse_lam, 
                        fp_cruse_ec, fp_cruse_omod, fp_cruse_trad, fp_cruse_rhy, 
                        fp_cruse_wthd, fp_cruse_other),
        weight = wt,
        ) %>% 
      set_caption("Current use by age, currently married women")
    
    # Table 5: Current use by number of living children, sexually active, unmarried women
    table5 <- result_data %>% 
      filter(v502!=1 & v528<=30) %>%
      cross_rpct(
        cell_vars = v201,
        col_vars = list(total(), fp_cruse_any, fp_cruse_mod, fp_cruse_fster,
                        fp_cruse_mster, fp_cruse_pill, fp_cruse_iud, fp_cruse_inj,
                        fp_cruse_imp, fp_cruse_mcond, fp_cruse_fcond, fp_cruse_diaph,
                        fp_cruse_lam, fp_cruse_ec, fp_cruse_omod, fp_cruse_trad,
                        fp_cruse_rhy, fp_cruse_wthd, fp_cruse_other),
        weight = wt,
        ) %>% 
      set_caption("Current use by number of living children, sexually active, unmarried women")
    browser()
    df1 <- filter_rows(table1)
    df2 <- filter_cols(table2)
    df3 <- filter_cols(table3)
    df4 <- filter_cols(table4)
    df5 <- filter_cols(table5)
    
    
    # Transpose wide tables to make them more readable
    df2 <- t(df2)
    df3 <- t(df3)
    df4 <- t(df4)
    df5 <- t(df5)
    
    # Add all transposed tables
    html_content <- paste0(html_content,
                           df_to_html(df1, "Table 1: Knowledge of Family Planning Methods, Women"),
                           df_to_html(df2, "Table 2: Knowledge by Age, Women"),
                           df_to_html(df3, "Table 3: Current Use by Age, All Women"),
                           df_to_html(df4, "Table 4: Current Use by Age, Currently Married Women"),
                           df_to_html(df5, "Table 5: Current Use, Sexually Active, Unmarried Women")
    )
  }

  # Add a horizontally scrollable preview of fp_ columns (first 5 rows) at the end
  fp_cols <- grep('^fp_', names(result_data), value = TRUE)
  if (length(fp_cols) > 0) {
    if (has_geospatial && all(c("ADM1NAME", "URBAN_RURA", "LATNUM", "LONGNUM") %in% names(result_data))) {
      preview_cols <- c("ADM1NAME", "URBAN_RURA", "LATNUM", "LONGNUM", fp_cols)
    } else {
      preview_cols <- fp_cols
    }
    preview_df <- head(result_data[, preview_cols, drop = FALSE], 5)
    # Build HTML table for preview
    html_preview <- "<div style='overflow-x:auto; margin: 20px 0;'><table style='min-width:1200px; border-collapse:collapse;'>"
    # Header with row-header class for first column
    html_preview <- paste0(
      html_preview, "<thead><tr>",
      paste0(
        sapply(seq_along(preview_cols), function(j) {
          if (j == 1) paste0("<th>", preview_cols[j], "</th>")
          else paste0("<th>", preview_cols[j], "</th>")
        }),
        collapse = ""
      ),
      "</tr></thead><tbody>"
    )
    # Rows, first column as row-header
    for (i in 1:nrow(preview_df)) {
      html_preview <- paste0(
        html_preview, "<tr>",
        paste0(
          sapply(seq_along(preview_cols), function(j) {
            cell <- ifelse(is.na(preview_df[i, j]), "", as.character(preview_df[i, j]))
            if (j == 1) paste0("<td class='row-header'>", cell, "</td>")
            else paste0("<td>", cell, "</td>")
          }),
          collapse = ""
        ),
        "</tr>"
      )
    }
    html_preview <- paste0(html_preview, "</tbody></table></div>")
    html_content <- paste0(html_content, "<h3>Preview of Family Planning (fp_) Variables (first 5 rows)</h3>", html_preview)
  }

  # Write HTML file
  html_content <- paste0(html_content, "</body>\n</html>")
  writeLines(html_content, html_filename)
  cat("HTML report successfully created:", html_filename, "\n")
  cat("Open the file in your web browser to view the interactive tables with horizontal scrolling.\n")
}
