# ******************************************************************************
# Program: 			  FP_COMM.R
# Purpose: 		    Code communication related indicators: exposure to FP messages,
#                 decision on use/nonuse, discussions.  
# Data inputs: 		IR dataset
# Data outputs:		coded variables
# Author:				  Courtney Allen
# Date last modified: March 29 2021 by Courtney Allen
# ******************************************************************************
#   

# -----------------------------------------------------------------------------#
# # Variables created in this file:
# fp_message_radio		"Exposure to family planning message by radio"
# fp_message_tv			  "Exposure to family planning message by TV"
# fp_message_paper		"Exposure to family planning message by newspaper/magazine"
# fp_message_mobile		"Exposure to family planning message by mobile phone"
# fp_message_noneof4	"Not exposed to any of the four media sources"
# fp_message_noneof3 	"Not exposed to TV, radio, or paper media sources"
# 
# fp_decyes_user			"Who makes the decision to use family planning among users"
# fp_decno_nonuser		"Who makes decision not to use family planning among non-users"
# 
# fp_fpvisit_discuss	"Women non-users that were visited by a FP worker who discussed FP"
# fp_hf_discuss			  "Women non-users who visited a health facility in last 12 months and discussed FP"
# fp_hf_notdiscuss		"Women non-users who visited a health facility in last 12 months and did not discuss FP"
# fp_any_notdiscuss		"Women non-users who did not discuss FP neither with FP worker or in a health facility"
# ------------------------------------------------------------------------------
  

# Load necessary libraries
library(dplyr)
library(labelled)

## FAMILY PLANNING MESSAGES

# Family planning messages by radio 
#' @export
CREATE_FP_COMM <- function(IRdata, MRdata) {
    
  # Process IR data
  if (!is.null(IRdata)) {
    IRdata <- IRdata %>%
      mutate(fp_message_radio = as.numeric(
              ifelse(!"v384a" %in% colnames(IRdata), NA, ifelse(v384a == 1, 1, 0)))) %>%
      set_value_labels(fp_message_radio = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_message_radio = "Exposure to family planning message by radio")
  }

  if (!is.null(IRdata)) {
    IRdata <- IRdata %>%
      mutate(fp_message_radio = as.numeric(
              ifelse(!"v384a" %in% colnames(IRdata), NA, ifelse(v384a == 1, 1, 0)))) %>%
      set_value_labels(fp_message_radio = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_message_radio = "Exposure to family planning message by radio")
    
    
    # Family planning messages by TV 
    IRdata <- IRdata %>%
      mutate(fp_message_tv =  as.numeric(
              ifelse(!"v384b" %in% colnames(IRdata), NA, ifelse(v384b == 1, 1, 0)))) %>%
      set_value_labels(fp_message_tv = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_message_tv = "Exposure to family planning message by TV")
    
    
    # Family planning messages by newspaper and/or magazine 
    IRdata <- IRdata %>%
      mutate(fp_message_paper =  as.numeric(
              ifelse(!"v384c" %in% colnames(IRdata), NA, ifelse(v384c == 1, 1, 0)))) %>%
      set_value_labels(fp_message_paper = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_message_paper = "Exposure to family planning message by newspaper/magazine")
    
    
    # Family planning messages by newspaper and/or magazine 
    IRdata <- IRdata %>%
      mutate(fp_message_mobile =  as.numeric(
              ifelse(!"v384d" %in% colnames(IRdata), NA, ifelse(v384d == 1, 1, 0)))) %>%
      set_value_labels(fp_message_mobile = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_message_mobile = "Exposure to family planning message by mobile phone")
    
    
    # Did not hear a family planning message from any of the 4 media sources
    IRdata <- IRdata %>%
      mutate(fp_message_noneof4 = as.numeric(
              ifelse(!all(c("v384a", "v384b", "v384c", "v384d") %in% colnames(IRdata)), NA, ifelse(v384a!=1 & v384b!=1 & v384c!=1 & v384d!=1, 1, 0)))) %>%
      set_value_labels(fp_message_noneof4 = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_message_noneof4 = "Exposure to family planning message any of four sources (TV, radio, paper, mobile)")
    
    
    # Did not hear a family planning message from radio, TV or paper
    IRdata <- IRdata %>%
      mutate(fp_message_noneof3 = as.numeric(
              ifelse(!all(c("v384a", "v384b", "v384c") %in% colnames(IRdata)), NA, ifelse(v384a!=1 & v384b!=1 & v384c!=1, 1, 0)))) %>%
      set_value_labels(fp_message_noneof3 = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_message_noneof3 = "Not exposed to TV, radio, or paper media sources")
    
    
    
    ## FAMILY PLANNING DECISION MAKING AND DISCUSSION
      
    # Decision to use among users
    IRdata <- IRdata %>%
      mutate(fp_decyes_user =  as.numeric( 
              ifelse(!all(c("v502", "v213", "v312", "v632") %in% colnames(IRdata)), NA, ifelse((v502!=1 | v213!=0 | v312==0),NA,v632)))) %>%
      set_value_labels(fp_decyes_user =  get_value_labels(IRdata$v632)) %>%
      set_variable_labels(fp_decyes_user = "Who makes the decision to use family planning among users")
    
    # Decision to not use among non users
    IRdata <- IRdata %>%
      mutate(fp_decno_nonuser =  as.numeric( 
              ifelse(!all(c("v502", "v213", "v312", "v632") %in% colnames(IRdata)), NA, ifelse((v502!=1 | v213!=0 | v312!=0),NA,v632)))) %>%
      set_value_labels(fp_decno_nonuser = get_value_labels(IRdata$v632)) %>%
      set_variable_labels(fp_decno_nonuser = "Who makes decision not to use family planning among non-users")
    
    # Discussed with FP worker
    IRdata <- IRdata %>%
      mutate(fp_fpvisit_discuss = NA_real_)
    if (all(c("v312","v393a") %in% colnames(IRdata))) {
      IRdata <- IRdata %>%
        mutate(fp_fpvisit_discuss = 
                case_when(
                  v393a==1 ~ 1,
                  v312!=0 ~ NA))
    }
    IRdata <- IRdata %>%
      set_value_labels(fp_fpvisit_discuss = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_fpvisit_discuss = "Women non-users that were visited by a FP worker who discussed FP")
    
          
    # Discussed FP in a health facility
    IRdata <- IRdata %>%
      mutate(fp_hf_discuss = NA_real_)
    if (all(c("v312","v394","v395") %in% colnames(IRdata))) {
      IRdata <- IRdata %>%
        mutate(fp_hf_discuss = 
                case_when(
                  v394==1 & v395==1 ~ 1,
                  v312!=0 ~ NA))
    }
    IRdata <- IRdata %>%
      set_value_labels(fp_hf_discuss = c(yes=1, no=0)) %>%
      set_variable_labels(fp_hf_discuss = "Women non-users who visited a health facility in last 12 months and discussed FP")
    
    
    # Did not discuss FP in health facility
    IRdata <- IRdata %>%
      mutate(fp_hf_notdiscuss = NA_real_)
    if( all(c("v312","v394","v395") %in% colnames(IRdata)) ) {
      IRdata <- IRdata %>%
        mutate(fp_hf_notdiscuss = 
                case_when(
                  v394==1 & v395!=1 ~ 1,
                  v312!=0 ~ NA))
    }
    IRdata <- IRdata %>%
      set_value_labels(fp_hf_notdiscuss = c(yes=1, no=0)) %>%
      set_variable_labels(fp_hf_notdiscuss = "Women non-users who visited a health facility in last 12 months and did not discuss FP")
    
    
    # Did not discuss FP in health facility or with FP worker
    IRdata <- IRdata %>%
      mutate(fp_any_notdiscuss = NA_real_)
    if(all(c("v312", "v393a","v394","v395") %in% colnames(IRdata))) {
      IRdata <- IRdata %>%
        mutate(fp_any_notdiscuss = 
                case_when(
                  v393a!=1 & v394==1 & v395!=1 ~ 1,
                  v312!=0 ~ NA))
    }
    IRdata <- IRdata %>%
      set_value_labels(fp_any_notdiscuss = c(yes=1, no=0)) %>%
      set_variable_labels(fp_any_notdiscuss = "Women non-users who did not discuss FP neither with FP worker or in a health facility")
    
  }

  #-------------------------------------------------------------------------------      


  ## indicators from MR file


  ## FAMILY PLANNING MESSAGES



  # Family planning messages by radio

  if (!is.null(MRdata)) {
      
    MRdata <- MRdata %>%
      mutate(fp_message_radio = as.numeric(
              ifelse(!"mv384a" %in% colnames(MRdata), NA, ifelse(mv384a == 1, 1, 0)))) %>%
      set_value_labels(fp_message_radio = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_message_radio = "Exposure to family planning message by radio")
    
    
    # Family planning messages by TV 
    MRdata <- MRdata %>%
      mutate(fp_message_tv = as.numeric(
              ifelse(!"mv384b" %in% colnames(MRdata), NA, ifelse(mv384b == 1, 1, 0)))) %>%
      set_value_labels(fp_message_tv = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_message_tv = "Exposure to family planning message by TV")
    
    
    # Family planning messages by newspaper and/or magazine 
    MRdata <- MRdata %>%
      mutate(fp_message_paper = as.numeric(
              ifelse(!"mv384c" %in% colnames(MRdata), NA, ifelse(mv384c == 1, 1, 0)))) %>%
      set_value_labels(fp_message_paper = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_message_paper = "Exposure to family planning message by newspaper/magazine")
    
    
    # Family planning messages by newspaper and/or magazine 
    MRdata <- MRdata %>%
      mutate(fp_message_mobile = as.numeric(
              ifelse(!"mv384d" %in% colnames(MRdata), NA, ifelse(mv384d == 1, 1, 0)))) %>%
      set_value_labels(fp_message_mobile = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_message_mobile = "Exposure to family planning message by mobile phone")
    
    
    # Did not hear a family planning message from any of the 4 media sources
    MRdata <- MRdata %>%
      mutate(fp_message_noneof4 = as.numeric(
              ifelse(!all(c("mv384a", "mv384b", "mv384c", "mv384d") %in% colnames(MRdata)), NA, ifelse(mv384a!=1 & mv384b!=1 & mv384c!=1 & mv384d!=1, 1, 0)))) %>%
      set_value_labels(fp_message_noneof4 = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_message_noneof4 = "Exposure to family planning message any of four sources (TV, radio, paper, mobile)")
    
    
    # Did not hear a family planning message from radio, TV or paper
    MRdata <- MRdata %>%
      mutate(fp_message_noneof3 = as.numeric(
              ifelse(!all(c("mv384a", "mv384b", "mv384c")) %in% colnames(MRdata), NA,ifelse(mv384a!=1 & mv384b!=1 & mv384c!=1, 1, 0)))) %>%
      set_value_labels(fp_message_noneof3 = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_message_noneof3 = "Not exposed to TV, radio, or paper media sources")
  }
  return (list(IRdata = IRdata, MRdata=MRdata))
}