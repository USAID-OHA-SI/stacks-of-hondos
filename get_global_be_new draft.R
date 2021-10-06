get_global_agency_be<-function(df){
  df<-df%>%
    prep_fsd()%>%
    dplyr::filter(fiscal_year=="2020" | fiscal_year=="2021")%>%
    dplyr::select (c(agency_category,fiscal_year,cop_budget_total,expenditure_amt))%>%
    group_by(agency_category,fiscal_year)%>%
    summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
    dplyr::mutate(budget_execution=percent_clean(expenditure_amt,cop_budget_total))%>%
    ungroup()%>%
    pivot_wider(names_from = fiscal_year,values_from = cop_budget_total:budget_execution, values_fill = 0)%>%
    dplyr::relocate(expenditure_amt_2020, .before = cop_budget_total_2020) %>%
    dplyr::relocate(expenditure_amt_2021, .before = cop_budget_total_2021) %>%
    dplyr::relocate(budget_execution_2021, .after = cop_budget_total_2021)%>%
    dplyr::relocate(budget_execution_2020, .after = cop_budget_total_2020) %>%
    #gt()%>%
    ea_style()%>%
    cols_label( #update GT to check on tidy select), also look at clean_names, also potentially case_when
      agency_category = "Agency")%>%

    tab_header(
      title = (" COP 2019 & 2020 Global Financial Performance Summary"),
      subtitle = legend_chunk)%>%
    tab_source_note(
      source_note = md("*Other* based on aggregated funding agencies"))
  
  
  return(df)
}

get_global_agency_be(df_fsd)
