library(glamr)
library(tidyverse)

get_ou_mechanism<-function(df, ou="operatingunit"){
  df<- df %>% 
    prep_fsd()%>%
    
    dplyr::filter(fiscal_year=="2020" | fiscal_year=="2021")%>%
  
    dplyr::filter(operatingunit %in% ou)%>%
  
    dplyr::select (c(fundingagency,mech_id_mech_name, fiscal_year,cop_budget_total,expenditure_amt))%>%
    mutate_at(vars(cop_budget_total,expenditure_amt),~replace_na(.,0))%>%
    mutate( fundingagency = fct_relevel(fundingagency, "USAID","CDC"))%>%
   
    group_by(fundingagency,mech_id_mech_name,fiscal_year)%>%
    summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
    dplyr::mutate(budget_execution=percent_clean(expenditure_amt,cop_budget_total))%>%
    ungroup(mech_id_mech_name)%>%
    
    
    dplyr::arrange(fiscal_year)%>%
    
    pivot_wider(names_from = fiscal_year,values_from = cop_budget_total:budget_execution, values_fill = 0)%>%
    dplyr::relocate(expenditure_amt_2020, .before = cop_budget_total_2020) %>%
    dplyr::relocate(expenditure_amt_2021, .before = cop_budget_total_2021) %>%
    dplyr::relocate(budget_execution_2021, .after = cop_budget_total_2021)%>%
    dplyr::relocate(budget_execution_2020, .after = cop_budget_total_2020) %>%
    
   
    
    dplyr::filter(`cop_budget_total_2021` !=0 | `expenditure_amt_2021` !=0) 