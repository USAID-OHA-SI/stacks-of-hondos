

library(glamr)
library(tidyverse)
library(gophr)
library(extrafont)
library(gt)
library(glue)
library(webshot)
library(dplyr)
library(devtools)
library(tidyr)
library(gisr)
library(scales)
library(sf)
library(glitr)
library(readxl)




df_fsd<-si_path()%>%
  return_latest("Fin")%>%
  gophr::read_msd()


#This function can be used to print out budget execution by partner type (local, international)for USAID at a global level. 
#You will need to ensure that you have load_secrets from the glamr package set up beforehand
#Be sure to load the following source files below before running

source("~/GitHub/stacks-of-hondos/Scripts/prep_fsd.R")
source("~/GitHub/stacks-of-hondos/Scripts/utilities.R")






#Mechanism Section====================================================================
#Run the following function to have COP19 & COP20 Financial performance by OUxMechanismxProgramArea
get_ou_mechanism_pa<-function(df, ou="operatingunit"){
  df<-df%>%
    prep_fsd()%>%
    #filter for fiscal year
    dplyr::filter(fiscal_year=="2020" | fiscal_year=="2021")%>%
    
    #filter for OU
    #dplyr::filter(operatingunit== "Mozambique")%>%
    dplyr::filter(operatingunit %in% ou)%>%
    #dplyr::filter(fundingagency %in% funding_agency) %>% 
    #dplyr::filter(mech_id_mech_name %in% id) %>% 
    #dplyr::filter(program %in% prog) %>% 
    #select specific variables
    dplyr::select (c(fundingagency, mech_id_mech_name,program, fiscal_year,cop_budget_total,expenditure_amt))%>%
    mutate_at(vars(cop_budget_total,expenditure_amt),~replace_na(.,0))%>%
      
    mutate( fundingagency = fct_relevel(fundingagency, "USAID","CDC"))%>%
    
    
    
    group_by(fundingagency,mech_id_mech_name,program, fiscal_year)%>%
    summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
    dplyr::mutate(budget_execution=percent_clean(expenditure_amt,cop_budget_total))%>%
    ungroup(program, fiscal_year)%>%
    
    
    dplyr::arrange(fiscal_year)%>%
    
    pivot_wider(names_from = fiscal_year,values_from = cop_budget_total:budget_execution, values_fill = 0)%>%
    dplyr::relocate(expenditure_amt_2020, .before = cop_budget_total_2020) %>%
    dplyr::relocate(expenditure_amt_2021, .before = cop_budget_total_2021) %>%
    dplyr::relocate(budget_execution_2021, .after = cop_budget_total_2021)%>%
    dplyr::relocate(budget_execution_2020, .after = cop_budget_total_2020) %>%
    dplyr::filter(budget_execution_2021>0)
  return(df)
}
    



