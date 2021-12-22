git_src <- "~/GitHub"
source(glue("{git_src}/stacks-of-hondos/scripts/prep_fsd.R"))
source(glue("{git_src}/stacks-of-hondos/scripts/utilities.R"))


### Function ===================================================================
get_ou_pa<-function(df, ou="operatingunit"){
  df<-df%>%
    prep_fsd()%>%
    agency_category()%>%
    
    #filter for fiscal year
    dplyr::filter(fiscal_year=="2020" | fiscal_year=="2021")%>%
    
    #filter for OU
    dplyr::filter(operatingunit %in% ou)%>%
    
    #select specific variables
    dplyr::select (c(agency_category,program, fiscal_year,cop_budget_total,expenditure_amt))%>%
    mutate_at(vars(cop_budget_total,expenditure_amt),~replace_na(.,0))%>%
    mutate( agency_category = fct_relevel(agency_category, "USAID","CDC"))%>%
    
    group_by(agency_category,program,fiscal_year)%>%
    summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
    dplyr::mutate(budget_execution=percent_clean(expenditure_amt,cop_budget_total))%>%
    ungroup()%>%
    
    
    pivot_wider(names_from = fiscal_year,values_from = cop_budget_total:budget_execution, values_fill = 0)%>%
    dplyr::relocate(expenditure_amt_2020, .before = cop_budget_total_2020) %>%
    dplyr::relocate(expenditure_amt_2021, .before = cop_budget_total_2021) %>%
    dplyr::relocate(budget_execution_2021, .after = cop_budget_total_2021)%>%
    dplyr::relocate(budget_execution_2020, .after = cop_budget_total_2020) %>%
    
  return(df)
}

