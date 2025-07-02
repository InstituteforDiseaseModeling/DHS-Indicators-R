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

CREATE_REPORT <- function(IRdata, output_dir, ir_filename) {
  
  # Survey set the design with survey weights
  IRdata <- IRdata %>%
    mutate(wt = v005/1000000)
  
  # Set expss package options to show one decimal place
  expss_digits(digits=1)
  
  # Create list of current use of family planning variables
  fp_cruse_list <- IRdata %>%  
    select(fp_cruse_any, fp_cruse_mod, fp_cruse_fster, fp_cruse_mster, 
           fp_cruse_pill, fp_cruse_iud, fp_cruse_inj, fp_cruse_imp, 
           fp_cruse_mcond, fp_cruse_fcond, fp_cruse_diaph, fp_cruse_lam, 
           fp_cruse_ec, fp_cruse_omod, fp_cruse_trad, fp_cruse_rhy, 
           fp_cruse_wthd, fp_cruse_other) %>%
    as.list()
  
  # Create population groups that are shown in tables
  
  # Dummy var for all women
  IRdata <- IRdata %>% 
    mutate(fp_all = case_when(v007>0 ~ "all"),
           fp_all = set_label(fp_all, label = "all women"))
  
  # Only married women
  IRdata <- IRdata %>% 
    mutate(fp_married = case_when(v502==1 ~ "married"),
           fp_married = set_label(fp_married, label = "currently married women"))
  
  # Sexually active unmarried women (SAUW); sexually active if had sex in last 30 days
  IRdata <- IRdata %>% 
    mutate(fp_sauw = case_when(v502!=1 & v528<=30 ~ "SAUW"),
           fp_sauw = set_label(fp_sauw, label = "sexually active, unmarried women"))
  
  # Create the 5 tables
  
  # Table 1: FP knowledge of any method, any modern method, any traditional method
  table1 <- IRdata %>% 
    cross_cpct(
      cell_vars = list(fp_know_any, fp_know_mod, fp_know_fster, fp_know_mster, 
                       fp_know_pill, fp_know_iud, fp_know_inj, fp_know_imp, 
                       fp_know_mcond, fp_know_fcond, fp_know_ec, fp_know_sdm, fp_know_lam, 
                       fp_know_omod, fp_know_trad, fp_know_rhy, fp_know_wthd, fp_know_other),
      col_vars = list(fp_all, fp_married, fp_sauw),
      weight = wt,
      expss_digits(digits=1)) %>%   
    set_caption("Knowledge of family planning methods")
  
  # Table 2: FP knowledge by background variables
  table2 <- IRdata %>% 
    cross_rpct(
      cell_vars = list(v013, v025, v024, v106, v190),
      col_vars = list(fp_know_any, fp_know_mod, total()),
      weight = wt,
      total_label = "Weighted N",
      total_statistic = "w_cases",
      expss_digits(digits=1)) %>%   
    set_caption("Knowledge of family planning methods by background variables")
  
  # Table 3: Current use by age, all women
  table3 <- IRdata %>% 
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
  table4 <- IRdata %>% 
    filter(v502==1) %>%
    cross_rpct(
      cell_vars = v013,
      col_vars = list(total(), fp_cruse_any, fp_cruse_mod, fp_cruse_fster, fp_cruse_mster, 
                      fp_cruse_pill, fp_cruse_iud, fp_cruse_inj, fp_cruse_imp, 
                      fp_cruse_mcond, fp_cruse_fcond, fp_cruse_diaph, fp_cruse_lam, 
                      fp_cruse_ec, fp_cruse_omod, fp_cruse_trad, fp_cruse_rhy, 
                      fp_cruse_wthd, fp_cruse_other),
      weight = wt) %>% 
    set_caption("Current use by age, currently married women")
  
  # Table 5: Current use by number of living children, sexually active, unmarried women
  table5 <- IRdata %>% 
    filter(v502!=1 & v528<=30) %>%
    cross_rpct(
      cell_vars = v201,
      col_vars = list(total(), fp_cruse_any, fp_cruse_mod, fp_cruse_fster,
                      fp_cruse_mster, fp_cruse_pill, fp_cruse_iud, fp_cruse_inj,
                      fp_cruse_imp, fp_cruse_mcond, fp_cruse_fcond, fp_cruse_diaph,
                      fp_cruse_lam, fp_cruse_ec, fp_cruse_omod, fp_cruse_trad,
                      fp_cruse_rhy, fp_cruse_wthd, fp_cruse_other),
      weight = wt) %>% 
    set_caption("Current use by number of living children, sexually active, unmarried women")
  
  # Convert tables to data frames for kable processing
  df1 <- as.data.frame(table1)
  df2 <- as.data.frame(table2)
  df3 <- as.data.frame(table3)
  df4 <- as.data.frame(table4)
  df5 <- as.data.frame(table5)
  
  # Function to replace column names with variable labels
  replace_with_labels <- function(df, data) {
    new_names <- colnames(df)
    for (i in seq_along(new_names)) {
      col_name <- new_names[i]
      # Try to get variable label from the data
      if (col_name %in% names(data)) {
        label <- attr(data[[col_name]], "label")
        if (!is.null(label) && label != "") {
          new_names[i] <- label
        }
      }
      # Handle special case for total column
      if (col_name == "total()") new_names[i] <- "Total"
    }
    colnames(df) <- new_names
    return(df)
  }
  
  # Apply label replacement to all data frames
  df1 <- replace_with_labels(df1, IRdata)
  df2 <- replace_with_labels(df2, IRdata)
  df3 <- replace_with_labels(df3, IRdata)
  df4 <- replace_with_labels(df4, IRdata)
  df5 <- replace_with_labels(df5, IRdata)
  
  # Transpose all tables to make them more readable (rows become columns, columns become rows)
  df1_t <- as.data.frame(t(df1))
  df2_t <- as.data.frame(t(df2))
  df3_t <- as.data.frame(t(df3))
  df4_t <- as.data.frame(t(df4))
  df5_t <- as.data.frame(t(df5))
  
  # Set proper column names for transposed tables (original row names become column names)
  colnames(df1_t) <- rownames(df1)
  colnames(df2_t) <- rownames(df2)
  colnames(df3_t) <- rownames(df3)
  colnames(df4_t) <- rownames(df4)
  colnames(df5_t) <- rownames(df5)
  
  # Create dynamic filename based on IR data file
  ir_basename <- tools::file_path_sans_ext(basename(ir_filename))
  report_filename <- paste0(ir_basename, "_family_planning_report.html")
  html_filename <- file.path(output_dir, report_filename)
  
  # Create enhanced HTML content with better styling
  html_content <- paste0(
    "<!DOCTYPE html>\n<html>\n<head>\n",
    "<meta charset='UTF-8'>\n",
    "<title>Family Planning Indicators Report ",
    basename(ir_filename),
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
    basename(ir_filename),
    "</h1>\n",
    "<div class='summary'>\n",
    "<strong>Report Summary:</strong> This report contains 5 comprehensive tables showing family planning knowledge and usage patterns across different demographic groups. Tables are transposed for better readability and include horizontal scrolling for wide data.\n",
    "</div>\n"
  )
  
  # Enhanced function to convert data frame to HTML table with better formatting
  df_to_html <- function(df, title) {
    html <- paste0("<h2>", title, "</h2>\n<div class='table-container'>\n<table>\n")
    
    # Add header with sticky positioning
    html <- paste0(html, "<thead><tr>")
    html <- paste0(html, "<th class='row-header'>Indicator</th>")  # First column for row names
    for (col in colnames(df)) {
      html <- paste0(html, "<th>", col, "</th>")
    }
    html <- paste0(html, "</tr></thead>\n<tbody>\n")
    
    # Add all rows with row names as first column
    for (i in 1:nrow(df)) {
      html <- paste0(html, "<tr>")
      # Add row name as first cell with special styling
      html <- paste0(html, "<td class='row-header'>", rownames(df)[i], "</td>")
      # Add data cells
      for (j in 1:ncol(df)) {
        value <- df[i, j]
        if (is.na(value)) value <- ""
        # Format numbers nicely
        if (is.numeric(value) && !is.na(as.numeric(value))) {
          value <- format(round(as.numeric(value), 1), nsmall = 1)
        }
        html <- paste0(html, "<td>", value, "</td>")
      }
      html <- paste0(html, "</tr>\n")
    }
    
    html <- paste0(html, "</tbody></table>\n</div>\n")
    return(html)
  }
  
  # Add all transposed tables
  html_content <- paste0(html_content,
    df_to_html(df1_t, "Table 1: Knowledge of Family Planning Methods"),
    df_to_html(df2_t, "Table 2: Knowledge by Background Variables"),
    df_to_html(df3_t, "Table 3: Current Use by Age, All Women"),
    df_to_html(df4_t, "Table 4: Current Use by Age, Currently Married Women"),
    df_to_html(df5_t, "Table 5: Current Use, Sexually Active, Unmarried Women"),
    "</body>\n</html>"
  )
  
  # Write HTML file
  writeLines(html_content, html_filename)
  
  cat("HTML report successfully created:", html_filename, "\n")
  cat("Open the file in your web browser to view the interactive tables with horizontal scrolling.\n")
  
  # Return the tables as a list for further use if needed
  return(list(
    table1 = table1,
    table2 = table2, 
    table3 = table3,
    table4 = table4,
    table5 = table5
  ))
}
