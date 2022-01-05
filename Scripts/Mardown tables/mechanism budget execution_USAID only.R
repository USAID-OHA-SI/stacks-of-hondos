library(glamr)
library(tidyverse)
library(gophr)


df_fsd<-si_path()%>%
  return_latest("Fin")%>%
  read_msd()

get_ou_mechanism<-function(df, ou="operatingunit"){
  df<- df %>% 
    
    dplyr::filter(fiscal_year=="2020" | fiscal_year=="2021")%>%
    dplyr::filter(fundingagency=="USAID")%>%
    dplyr::filter(operatingunit %in% ou)%>%
    dplyr::select(-c("prime_partner_duns","prime_partner_org_type",
                     "is_indigenous_prime_partner", "subrecipient_duns",
                     "award_number","procurement_type")) %>% 
    dplyr::mutate( mech_id_mech_name = paste(mech_code,"-", mech_name))%>%
    
    #mutate data type double into integer to have round numbers
    dplyr::mutate_if(is.double, as.integer)%>%
    
    
    #drop NA for numeric amounts
    mutate_at(vars(cop_budget_new_funding:expenditure_amt),~replace_na(.,0))%>%
    
    
   
    # filter out M&O
    glamr::remove_mo()%>% 
  
    dplyr::select (c(fundingagency,mech_id_mech_name, fiscal_year,cop_budget_total,expenditure_amt))%>%
    mutate_at(vars(cop_budget_total,expenditure_amt),~replace_na(.,0))%>%
   
    group_by(fundingagency,mech_id_mech_name,fiscal_year)%>%
    summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
    dplyr::mutate(budget_execution=(expenditure_amt/cop_budget_total)*100)%>%
    ungroup()%>%
    
    
    dplyr::arrange(fiscal_year)%>%
    
    pivot_wider(names_from = fiscal_year,values_from = cop_budget_total:budget_execution, values_fill = 0)%>%
    dplyr::relocate(expenditure_amt_2020, .before = cop_budget_total_2020) %>%
    dplyr::relocate(expenditure_amt_2021, .before = cop_budget_total_2021) %>%
    dplyr::relocate(budget_execution_2021, .after = cop_budget_total_2021)%>%
    dplyr::relocate(budget_execution_2020, .after = cop_budget_total_2020) %>%
    dplyr::filter(`cop_budget_total_2021` !=0 | `expenditure_amt_2021` !=0) %>%
    select(-(fundingagency))
}