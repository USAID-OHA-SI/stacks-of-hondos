# PURPOSE: Munge and Analysis of FSD for base function
# AUTHOR: Ben Kasdan | SIEI
# LICENSE: MIT
# DATE: 2021-09-23
# NOTES: 

# LOCALS & SETUP ============================================================================

  # Libraries

library(glamr)
library(tidyverse)
library(extrafont)
library(tidytext)
library(glue)
  
  # Set paths  
    proj_paths
   
    si_paths 
    
  # Functions  
    
    
    df_fsd<-si_path()%>%
      return_latest("Fin")%>%
      read_msd()
    
    source<-source_info(si_path(),"Fin")
    fy <- source_info(si_path(),"Fin",return = "fiscal_year")%>%
      stringr::str_remove_all("L")
    
    source("percent_clean.R")
    
   

# LOAD DATA ============================================================================  
ea_base_table<-function(df){
  df<-df%>%
  remove_mo()
    df$mech_id_mech_name=paste(df$mech_name,"-",df$mech_code)
      #dplyr::mutate(mech_id_mech_name=paste0("mech_name-mech_code}"))%>%
    df<-df%>%
      dplyr::select (c(fiscal_year,operatingunit,fundingagency,primepartner,mech_id_mech_name,program ,cop_budget_total,expenditure_amt))%>%
     
      mutate_at(vars(cop_budget_total,expenditure_amt),~replace_na(.,0))%>%
      group_by(fiscal_year,operatingunit,fundingagency,primepartner,mech_id_mech_name,program)
    
    
    
      df<-df%>%
        summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
        mutate(budget_execution = percent_clean(expenditure_amt, cop_budget_total))%>%
      
      ungroup()
     
    return(df)
     
}    

# MUNGE ============================================================================
  
   df4<-df3 %>% 
      #filter(mech_id_mech_name=="CDU  Angola - 17397")%>%
    
      group_by(fiscal_year,operatingunit,fundingagency,primepartner,mech_id_mech_name)%>%
      summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
      mutate(budget_execution = percent_clean(expenditure_amt, cop_budget_total))%>%
      
      ungroup()
      
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

