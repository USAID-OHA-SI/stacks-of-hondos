# PURPOSE: Creation of base table
# AUTHOR: EA team | SIEI
# LICENSE: MIT
# DATE: 2021-09-30
# NOTES: use this function to create the main table that you can then pass through different gt helpers
#functions

# LOCALS & SETUP ============================================================================

  # Libraries
library(dplyr)
library(devtools)
library(tidyverse)
library(tidyr)
library(here)
library(data.table)
library(gt)
library(glue)
library(glitr)
library(glamr)
library(gisr)
library(gophr)
library(scales)
library(sf)
library(devtools)
  
  # Set paths  
    proj_paths
   
    si_paths 
    
  

# LOAD DATA ============================================================================  
    #Use "here" function to find folder
    #need to sub "here for si_path*********
    here()
    here("Raw Datasets")
    
    df_fsd<-read.delim(here("Raw Datasets/Finanical_Structured_Dataset_COP17-20_20210813.txt")) 
  
    df_fsd<-si_path()%>%
      glamr::return_latest("Fin")%>%
      gophr::read_msd()
  
  #source<-source_info(si_path(),"Fin")
  
  # Functions  
  #use this to call utilties functions
  source("Github/stacks-of-hondos/utilties.R")




#prep and cleaning fsd
#issues with source note at the end of the function
prep_fsd <-function(df){ 
  
  #removing
  df<-df %>% 
    dplyr::select(-c("prime_partner_duns","prime_partner_org_type",
                                  "is_indigenous_prime_partner", "subrecipient_duns",
                                  "award_number","procurement_type")) %>% 
    glamr::remove_mo()%>% # filter out M&O
    #dplyr::rename("fiscal_year"= implementation_year) %>% 
    #dplyr::filter(fiscal_year == c(2020 ,2021))
  
 
  ##concatenate mech id and mech name
  dplyr::mutate( mech_id_mech_name = paste(mech_code,"-", mech_name))%>%
  
  #mutate data type double into integer to have round numbers
  dplyr::mutate_if(is.double, as.integer)%>%
  #mutate data type for mechanism id and fiscal year
  #df<-df %>% dplyr::mutate_at(vars(mech_code, fiscal_year),list(as.character))
  
  #drop NA for numeric amounts
  
    mutate_at(vars(cop_budget_new_funding:expenditure_amt),~replace_na(.,0))%>%
  
  
  #recode values to match naming in Financial Integrated Dataset
  dplyr::mutate(`interaction_type`= recode (`interaction_type`, "Service Delivery"= "SD",
                                                      "Non Service Delivery"= "NSD"))%>%
  
  #Add in agency category column to group agencies
  
    glamr::clean_agency()%>%
    agency_category()%>%
   
  #mutating & calculating budget execution
  group_by(operatingunit, countryname, fundingagency, agency_category, fiscal_year,primepartner,mech_id_mech_name,program, interaction_type) %>% 
    summarise_at(vars(cop_budget_total, expenditure_amt), sum, na.rm = TRUE) %>% 
  ungroup()
  
 # df<-df%>%
  #  dplyr::mutate(budget_execution = percent_clean(expenditure_amt, cop_budget_total))
  
  #df<-df%>%
   # dplyr::mutate(budget_execution= as.numeric(`budget_execution`))
 
  return(df)  
  
}


