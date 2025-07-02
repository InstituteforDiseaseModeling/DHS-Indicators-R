# *****************************************************************************
# Program: 			  FP_KNOW.R
# Purpose: 			  Code contraceptive knowledge indicators
# Data inputs: 		IR dataset
# Data outputs:		coded variables
# Author:				  Courtney Allen
# Date last modified: March 29 2019 by Courtney Allen 
# ****************************************************************************

   
# ----------------------------------------------------------------------------# 
# Variables created in this file:
# 
# fp_know_any			"Know any contraceptive method"
# fp_know_mod			"Know any modern method"
# fp_know_fster		"Know female sterilization"
# fp_know_mster		"Know male sterilization"
# fp_know_pill		"Know pill"
# fp_know_iud			"Know IUD"
# fp_know_inj			"Know injectables"
# fp_know_imp			"Know implants"
# fp_know_mcond		"Know male condoms"
# fp_know_fcond		"Know female condom"
# fp_know_ec			"Know emergency contraception"
# fp_know_sdm			"Know standard days method"
# fp_know_lam			"Know LAM"
# fp_know_omod		"Know other modern method"
# fp_know_trad		"Know any traditional method"
# fp_know_rhy			"Know rhythm method"
# fp_know_wthd		"Know withdrawal method"
# fp_know_other		"Know other method"
# fp_know_mean_all	"Mean number of methods known - all"
# fp_know_mean_mar	"Mean number of methods known - among currently married"
# fp_know_fert_all	"Knowledge of fertile period among all women"
# fp_know_fert_rhy	"Knowledge of fertile period among rhythm method users"
# fp_know_fert_sdm	"Knowledge of fertile period among standard days method users"
# fp_know_fert_cor	"Correct knowledge of fertile period"
# ----------------------------------------------------------------------------*/
  


## KNOWLEDGE OF FAMILY PLANNING METHODS
#' @export
CREATE_FP_KNOW <- function(IRdata, MRdata){
  ## indicators from IR file


  # Check if the object exists
  if (!is.null(IRdata)) {
    
    # to correct for the situation where variables that should be named as v304_0`i' but where named v304_`i', where i is from 1 to 9.
    for (i in 1:9) {
      orig <- paste0("v304_", i)
      new  <- paste0("v304_0", i)
      if (orig %in% colnames(IRdata) && !(new %in% colnames(IRdata))) {
        colnames(IRdata)[colnames(IRdata) == orig] <- new
      }
    }
    
    # Check dups
    if (anyDuplicated(colnames(IRdata)) != 0) {
      stop("IRdata has duplicated column names: ",
       colnames(IRdata)[anyDuplicated(colnames(IRdata))])
    }
    # Any method 
    IRdata <- IRdata %>%
      mutate(fp_know_any = as.numeric(
              ifelse(!"v301" %in% colnames(IRdata), NA, ifelse(!is.na(v301) & v301 > 0 & v301 < 8, 1, 0)))) %>%
      set_value_labels(fp_know_any = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_know_any = "Know any contraceptive method")
    
    
    # Modern method
    IRdata <- IRdata %>%
      mutate(fp_know_mod = as.numeric(
              ifelse(!"v301" %in% colnames(IRdata), NA, ifelse(v301 ==3, 1, 0)))) %>%
      set_value_labels(fp_know_mod = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_know_mod = "Know any modern method")
    
    
    # Female sterilization  
    IRdata <- IRdata %>%
      mutate(fp_know_fster = as.numeric(
              ifelse(!"v304_06" %in% colnames(IRdata), NA, ifelse(v304_06>0 & v304_06<8, 1, 0)))) %>%
      set_value_labels(fp_know_fster = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_know_fster = "Know female sterilization")
    
    
    # Male sterilization  
    IRdata <- IRdata %>%
      mutate(fp_know_mster = as.numeric(
              ifelse(!"v304_07" %in% colnames(IRdata), NA, ifelse(v304_07>0 & v304_07<8, 1, 0)))) %>%
      set_value_labels(fp_know_mster = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_know_mster = "Know male sterilization")
    
    
    # The contraceptive pill 
    IRdata <- IRdata %>%
      mutate(fp_know_pill = as.numeric(
              ifelse(!"v304_01" %in% colnames(IRdata), NA, ifelse(v304_01>0 & v304_01<8, 1, 0)))) %>%
      set_value_labels(fp_know_pill = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_know_pill = "Know pill")
    
    
    # Intrauterine contraceptive device 
    IRdata <- IRdata %>%
      mutate(fp_know_iud = as.numeric(
              ifelse(!"v304_02" %in% colnames(IRdata), NA, ifelse(v304_02>0 & v304_02<8, 1, 0)))) %>%
      set_value_labels(fp_know_iud = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_know_iud = "Know IUD")
    
    
    # Injectables (Depo-Provera) 
    IRdata <- IRdata %>%
      mutate(fp_know_inj = as.numeric(
              ifelse(!"v304_03" %in% colnames(IRdata), NA, ifelse(v304_03>0 & v304_03<8, 1, 0)))) %>%
      set_value_labels(fp_know_inj = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_know_inj = "Know injectables")
    
    
    # Implants (Norplant) 
    IRdata <- IRdata %>%
      mutate(fp_know_imp = as.numeric(
              ifelse(!"v304_11" %in% colnames(IRdata), NA, ifelse(v304_11>0 & v304_11<8, 1, 0)))) %>%
      set_value_labels(fp_know_imp = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_know_imp = "Know implants")
    
    
    # Male condom 
    IRdata <- IRdata %>%
      mutate(fp_know_mcond = as.numeric(
        ifelse(!"v304_05" %in% colnames(IRdata), NA, ifelse(v304_05>0 & v304_05<8, 1, 0)))) %>%
      set_value_labels(fp_know_mcond = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_know_mcond = "Know male condoms")
    
    
    # Female condom 
    IRdata <- IRdata %>%
      mutate(fp_know_fcond = as.numeric(
        ifelse(!"v304_14" %in% colnames(IRdata), NA, ifelse(v304_14>0 & v304_14<8, 1, 0)))) %>%
      set_value_labels(fp_know_fcond = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_know_fcond = "Know female condom")
    
    
    # Emergency contraception 
    IRdata <- IRdata %>%
      mutate(fp_know_ec = as.numeric(
        ifelse(!"v304_16" %in% colnames(IRdata), NA, ifelse(v304_16>0 & v304_16<8, 1, 0)))) %>%
      set_value_labels(fp_know_ec = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_know_ec = "Know emergency contraception")
    
    
    # Standard days method (SDM) 
    IRdata <- IRdata %>%
      mutate(fp_know_sdm = as.numeric(
        ifelse(!"v304_18" %in% colnames(IRdata), NA, ifelse(v304_18>0 & v304_18<8, 1, 0)))) %>%
      set_value_labels(fp_know_sdm = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_know_sdm = "Know standard days method")
    
    
    # Lactational amenorrhea method (LAM) 
    IRdata <- IRdata %>%
      mutate(fp_know_lam = as.numeric(
        ifelse(!"v304_13" %in% colnames(IRdata), NA, ifelse(v304_13>0 & v304_13<8, 1, 0)))) %>%
      set_value_labels(fp_know_lam = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_know_lam = "Know LAM")
    
    
    # Country-specific modern methods and other modern contraceptive methods 
    IRdata <- IRdata %>%
      mutate(fp_know_omod = as.numeric(
        ifelse(!"v304_17" %in% colnames(IRdata), NA, ifelse(v304_17>0 & v304_17<8, 1, 0)))) %>%
      set_value_labels(fp_know_omod = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_know_omod = "Know other modern method")
    
    
    # Periodic abstinence (rhythm, calendar method) 
    IRdata <- IRdata %>%
      mutate(fp_know_rhy = as.numeric(
        ifelse(!"v304_08" %in% colnames(IRdata), NA, ifelse(v304_08>0 & v304_08<8, 1, 0)))) %>%
      set_value_labels(fp_know_rhy = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_know_rhy = "Know rhythm method")
    
    
    # Withdrawal (coitus interruptus) 
    IRdata <- IRdata %>%
      mutate(fp_know_wthd = as.numeric(
        ifelse(!"v304_09" %in% colnames(IRdata), NA, ifelse(v304_09>0 & v304_09<8, 1, 0)))) %>%
      set_value_labels(fp_know_wthd = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_know_wthd = "Know withdrawal method")
    
    
    # Country-specific traditional methods, and folk methods 
    IRdata <- IRdata %>%
      mutate(fp_know_other = as.numeric(
        ifelse(!"v304_10" %in% colnames(IRdata), NA, ifelse(v304_10>0 & v304_10<8, 1, 0)))) %>%
      set_value_labels(fp_know_other = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_know_other = "Know other method")
    
    
    # Any traditional
    IRdata <- IRdata %>%
      mutate(fp_know_trad = as.numeric(
              ifelse(fp_know_rhy==1 | fp_know_wthd==1 | fp_know_other==1, 1, 0))) %>%
      set_value_labels(fp_know_trad = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_know_trad = "Know any traditional method")
    
    
    # Sum of methods known
    IRdata <- IRdata %>%
      mutate(fp_know_sum =
              rowSums((IRdata[,c("fp_know_fster", "fp_know_mster", "fp_know_pill", "fp_know_iud", 
                                  "fp_know_inj", "fp_know_imp", "fp_know_mcond", "fp_know_fcond", 
                                  "fp_know_ec", "fp_know_sdm", "fp_know_lam", "fp_know_rhy", 
                                  "fp_know_wthd", "fp_know_omod", "fp_know_other")]))) %>%
      set_variable_labels(fp_know_sum = "Sum of known methods")
    
    
    # Mean methods known
    IRdata$wt <- IRdata$v005/1000000  # create weight for weighted mean
    fp_know_mean_all <- weighted.mean(IRdata$fp_know_sum, IRdata$wt)
    var_label(fp_know_mean_all) <- "Mean number of methods known - all"
    
    
    # Mean methods known among married
    if ("v502" %in% colnames(IRdata)){
    fp_know_mean_mar <- IRdata %>%
      filter(., v502==1) %>%
      summarise(fp_know_mean_mar = weighted.mean(fp_know_sum, wt)) %>%
      pull(fp_know_mean_mar)
    
    var_label(fp_know_mean_mar) <- "Mean number of methods known - currently married"
    }
    
    # Mean methods known sexually active, unmarried (SAUW)
    if (all(c("v502", "v528") %in% colnames(IRdata))){
      fp_know_mean_sauw <- IRdata %>%
        filter(., v502!=1 & v528<=30) %>%
        summarise(fp_know_mean_sauw = weighted.mean(fp_know_sum, wt)) %>%
        pull(fp_know_mean_sauw)
      var_label(fp_know_mean_sauw) <- "Mean number of methods known - currently married"
    }
    
    
    ## Knowledge of fertile period
    IRdata <- IRdata %>%
      mutate(fp_know_fert_all = NA_real_)
    if(all(c("v217", "v312") %in% colnames(IRdata))){
      IRdata <- IRdata %>%
        # mutate(fp_know_fert_all = 
        #          case_match(IRdata$v217, 
        #                 4~1, 1~2, 2~3, 3~4, 6~5, 5~6,)) %>%
        mutate(fp_know_fert_all = case_when(
          v217 == 4 ~ 1,  # Before period begins -> 1
          v217 == 1 ~ 2,  # During her period -> 2
          v217 == 2 ~ 3,  # After period ended -> 3
          v217 == 3 ~ 4,  # Middle of the cycle -> 4
          v217 == 6 ~ 5,  # Other -> 5
          v217 == 5 ~ 6,  # at any time -> 6
          v217 == 8 ~ 7,   # Don't know -> 7
          .default = 8
        ))
    }
    IRdata <- IRdata %>%
      set_value_labels(fp_know_fert_all = c(
        "Just before her menstrual period begins" = 1,
        "During her menstrual period" = 2,
        "Right after her menstrual period has ended" = 3,
        "Halfway between two menstrual periods" = 4,
        "Other" = 5,
        "No specific time" = 6,
        "Don't know" = 7,
        "Missing" = 8)) %>%
      set_variable_labels(fp_know_fert_all = "Knowledge of fertile period among users")
    
    
    IRdata <- IRdata %>%
      mutate(fp_know_fert_rhy = NA_real_)
    if (all(c("v217", "v312") %in% colnames(IRdata))){
    IRdata <- IRdata %>%
      mutate(fp_know_fert_rhy = case_when(
        v312 == 8 & v217 == 4 ~ 1,  # Before period begins -> 1
        v312 == 8 & v217 == 1 ~ 2,  # During her period -> 2
        v312 == 8 & v217 == 2 ~ 3,  # After period ended -> 3
        v312 == 8 & v217 == 3 ~ 4,  # Middle of the cycle -> 4
        v312 == 8 & v312 == 8 &   v312 == 8 & v217 == 6 ~ 5,  # Other -> 5
        v312 == 8 & v217 == 5 ~ 6,  # at any time -> 6
        v312 == 8 & v217 == 8 ~ 7,   # Don't know -> 7
        .default = 8
      ))
    }
    IRdata <- IRdata %>%
      set_value_labels(fp_know_fert_rhy = get_value_labels(IRdata$fp_know_fert_all)) %>%
      set_variable_labels(fp_know_fert_rhy = "Know fertile period among rhythm method users")
    
    
    IRdata <- IRdata %>%
      mutate(fp_know_fert_sdm = NA_real_)
    if (all(c("v217", "v312") %in% colnames(IRdata))){
      IRdata <- IRdata %>%
        mutate(fp_know_fert_sdm= case_when(
          v312==18 & v217 == 4 ~ 1,  # Before period begins -> 1
          v312==18 & v217 == 1 ~ 2,  # During her period -> 2
          v312==18 & v217 == 2 ~ 3,  # After period ended -> 3
          v312==18 & v217 == 3 ~ 4,  # Middle of the cycle -> 4
          v312==18 & v217 == 6 ~ 5,  # Other -> 5
          v312==18 & v217 == 5 ~ 6,  # at any time -> 6
          v312==18 & v217 == 8 ~ 7,   # Don't know -> 7
          .default = 8
        ))
    }
    IRdata <- IRdata%>%
      set_value_labels(fp_know_fert_sdm = get_value_labels(IRdata$fp_know_fert_all)) %>%
      set_variable_labels(fp_know_fert_sdm = "Know fertile period among SDM users")
    
    
    
    # Correct knowledge of fertile period
    IRdata <- IRdata %>%
      mutate(fp_know_fert_cor = as.numeric(
              ifelse(!"v217" %in% colnames(IRdata), NA, ifelse(v217==3,1,0)))) %>%
      set_value_labels(fp_know_fert_cor = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_know_fert_cor = "Correct knowledge of fertile period")
    
    #-------------------------------------------------------------------------------      

  }

  if (!is.null(MRdata)){
    
    
    ## indicators from MR file
    
    ## KNOWLEDGE OF CONTRACEPTIVE METHODS
    
    # note: In the case some surveys have the variables mv304_01 as mv304_1 for instance
   
    for (i in 1:9) {
      orig <- paste0("mv304_0", i)
      new  <- paste0("mv304_", i)
      if (orig %in% colnames(MRdata) && !(new %in% colnames(MRdata))) {
        colnames(MRdata)[colnames(MRdata) == orig] <- new
      }
    }
    
    # Any method 
    MRdata <- MRdata %>%
      mutate(fp_know_any = as.numeric(
              ifelse(!"mv301" %in% colnames(MRdata), NA, ifelse(!is.na(mv301) & mv301 > 0 & mv301 < 8, 1, 0)))) %>%
      set_value_labels(fp_know_any = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_know_any = "Know any contraceptive method")
    
    
    # Modern method
    MRdata <- MRdata %>%
      mutate(fp_know_mod = as.numeric(
              ifelse(!"mv301" %in% colnames(MRdata), NA,ifelse(!is.na(mv301) & mv301 ==3, 1, 0)))) %>%
      set_value_labels(fp_know_mod = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_know_mod = "Know any modern method")
    
    
    # Female sterilization  
    MRdata <- MRdata %>%
      mutate(fp_know_fster = as.numeric(
              ifelse(!"mv304_6" %in% colnames(MRdata), NA, ifelse(!is.na(mv304_6) & mv304_6>0 & mv304_6<8, 1, 0)))) %>%
      set_value_labels(fp_know_fster = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_know_fster = "Know female sterilization")
    
    
    # Male sterilization  
    MRdata <- MRdata %>%
      mutate(fp_know_mster = as.numeric(
              ifelse(!"mv304_7" %in% colnames(MRdata), NA, ifelse(!is.na(mv304_7) & mv304_7>0 & mv304_7<8, 1, 0)))) %>%
      set_value_labels(fp_know_mster = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_know_mster = "Know male sterilization")
    
    
    # The contraceptive pill 
    MRdata <- MRdata %>%
      mutate(fp_know_pill = as.numeric(
              ifelse(!"mv304_1" %in% colnames(MRdata), NA, ifelse(!is.na(mv304_1) & mv304_1>0 & mv304_1<8, 1, 0))))%>%
      set_value_labels(fp_know_pill = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_know_pill = "Know pill")
    
    
    # Intrauterine contraceptive device 
    MRdata <- MRdata %>%
      mutate(fp_know_iud = as.numeric(
              ifelse(!"mv304_2" %in% colnames(MRdata), NA, ifelse(!is.na(mv304_2) & mv304_2>0 & mv304_2<8, 1, 0)))) %>%
      set_value_labels(fp_know_iud = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_know_iud = "Know IUD")
    
    
    # Injectables (Depo-Provera) 
    MRdata <- MRdata %>%
      mutate(fp_know_inj = as.numeric(
              ifelse(!"mv304_8" %in% colnames(MRdata), NA, ifelse(!is.na(mv304_3) & mv304_3>0 & mv304_3<8, 1, 0)))) %>%
      set_value_labels(fp_know_inj = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_know_inj = "Know injectables")
    
    
    # Implants (Norplant) 
    MRdata <- MRdata %>%
      mutate(fp_know_imp = as.numeric(
              ifelse(!"mv304_11" %in% colnames(MRdata), NA, ifelse(!is.na(mv304_11) & mv304_11>0 & mv304_11<8, 1, 0)))) %>%
      set_value_labels(fp_know_imp = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_know_imp = "Know implants")
    
    
    # Male condom 
    MRdata <- MRdata %>%
      mutate(fp_know_mcond = as.numeric(
              ifelse(!"mv304_5" %in% colnames(MRdata), NA, ifelse(!is.na(mv304_5) & mv304_5>0 & mv304_5<8, 1, 0)))) %>%
      set_value_labels(fp_know_mcond = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_know_mcond = "Know male condoms")
    
    
    # Female condom 
    MRdata <- MRdata %>%
      mutate(fp_know_fcond = as.numeric(
              ifelse(!"mv304_14" %in% colnames(MRdata), NA, ifelse(!is.na(mv304_14) & mv304_14>0 & mv304_14<8, 1, 0)))) %>%
      set_value_labels(fp_know_fcond = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_know_fcond = "Know female condom")
    
    
    # Emergency contraception 
    MRdata <- MRdata %>%
      mutate(fp_know_ec = as.numeric(
              ifelse(!"mv304_16" %in% colnames(MRdata), NA, ifelse(!is.na(mv304_16) & mv304_16>0 & mv304_16<8, 1, 0)))) %>%
      set_value_labels(fp_know_ec = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_know_ec = "Know emergency contraception")
    
    
    # Standard days method (SDM) 
    MRdata <- MRdata %>%
      mutate(fp_know_sdm = as.numeric(
              ifelse(!"mv304_18" %in% colnames(MRdata), NA, ifelse(!is.na(mv304_18) & mv304_18>0 & mv304_18<8, 1, 0)))) %>%
      set_value_labels(fp_know_sdm = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_know_sdm = "Know standard days method")
    
    
    # Lactational amenorrhea method (LAM) 
    MRdata <- MRdata %>%
      mutate(fp_know_lam = as.numeric( 
              ifelse(!"mv304_13" %in% colnames(MRdata), NA, ifelse(!is.na(mv304_13) & mv304_13>0 & mv304_13<8, 1, 0)))) %>%
      set_value_labels(fp_know_lam = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_know_lam = "Know LAM")
    
    
    # Country-specific modern methods and other modern contraceptive methods 
    MRdata <- MRdata %>%
      mutate(fp_know_omod = as.numeric(
              ifelse(!"mv304_17" %in% colnames(MRdata), NA, ifelse(!is.na(mv304_17) & mv304_17>0 & mv304_17<8, 1, 0)))) %>%
      set_value_labels(fp_know_omod = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_know_omod = "Know other modern method")
    
    
    # Periodic abstinence (rhythm, calendar method) 
    MRdata <- MRdata %>%
      mutate(fp_know_rhy = as.numeric(
              ifelse(!"mv304_8" %in% colnames(MRdata), NA, ifelse(!is.na(mv304_8) & mv304_8>0 & mv304_8<8, 1, 0)))) %>%
      set_value_labels(fp_know_rhy = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_know_rhy = "Know rhythm method")
    
    
    # Withdrawal (coitus interruptus) 
    MRdata <- MRdata %>%
      mutate(fp_know_wthd = as.numeric(
              ifelse(!"mv304_9" %in% colnames(MRdata), NA, ifelse(!is.na(mv304_9) & mv304_9>0 & mv304_9<8, 1, 0)))) %>%
      set_value_labels(fp_know_wthd = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_know_wthd = "Know withdrawal method")
    
    
    # Country-specific traditional methods, and folk methods 
    MRdata <- MRdata %>%
      mutate(fp_know_other = as.numeric(
              ifelse(!"mv304_10" %in% colnames(MRdata), NA, ifelse(!is.na(mv304_10) & mv304_10>0 & mv304_10<8, 1, 0)))) %>%
      set_value_labels(fp_know_other = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_know_iud = "Know other method")
    
    
    # Any traditional
    MRdata <- MRdata %>%
      mutate(fp_know_trad = as.numeric(
              ifelse(fp_know_rhy==1 | fp_know_wthd==1 | fp_know_other==1, 1, 0))) %>%
      set_value_labels(fp_know_trad = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_know_trad = "Know any traditional method")
    
    
    # Sum of methods known
    MRdata <- MRdata %>%
      mutate(fp_know_sum = 
              rowSums(MRdata[,c("fp_know_fster", "fp_know_mster", "fp_know_pill", "fp_know_iud", 
                                "fp_know_inj", "fp_know_imp", "fp_know_mcond", "fp_know_fcond", 
                                "fp_know_ec", "fp_know_sdm", "fp_know_lam", "fp_know_rhy", 
                                "fp_know_wthd", "fp_know_omod", "fp_know_other")])) %>%
      set_value_labels(fp_know_sum = c(yes = 1, no = 0)) %>%
      set_variable_labels(fp_know_sum = "Sum of known methods")
    
    
    # Mean methods known
    MRdata$wt <- MRdata$mv005/1000000  # create weight for weighted mean
    fp_know_mean_all <- weighted.mean( MRdata$fp_know_sum,  MRdata$wt)
    var_label(fp_know_mean_all) <- "Mean number of methods known - all"
    
    
    # Mean methods known among married
    if ("mv502" %in% colnames(MRdata)){
      fp_know_mean_mar <- MRdata %>%
        filter(., mv502==1) %>%
        summarize(fp_know_mean_mar = weighted.mean(fp_know_sum, wt)) %>%
        pull(fp_know_mean_mar)
      
      var_label(fp_know_mean_mar) <- "Mean number of methods known - currently married"
      
    }
  }
  return (list(IRdata = IRdata, MRdata=MRdata))
}
