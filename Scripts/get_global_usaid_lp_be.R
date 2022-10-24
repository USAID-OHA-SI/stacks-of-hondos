library(glamr)
library(tidyverse)
library(gophr)
library(extrafont)
library(tidytext)
library(gt)
library(glue)
library(webshot2)


df_fsd<-si_path()%>%
  return_latest("Fin")%>%
  gophr::read_msd()


#This function can be used to print out budget execution by partner type (local, international)for USAID at a global level. 
#You will need to ensure that you have load_secrets from the glamr package set up beforehand
#Be sure to load the following source files below before running
source("~/GitHub/stacks-of-hondos/Scripts/ea_style.R")
source("~/GitHub/stacks-of-hondos/Scripts/utilities.R")

#ensure glamr load_secrets is loaded to get partner type data
glamr::load_secrets()
#apply partner type data to fsd before starting
df_fsd<-df_fsd%>%
  glamr::apply_partner_type()



get_global_usaid_lp_be<-function(df){
  df<-df%>%
    agency_category()%>%
    remove_mo()%>%
    remove_sch("SGAC")%>%
    dplyr::filter(fiscal_year %in% fys)%>%
    dplyr::filter(fundingagency=="USAID")%>%
    dplyr::filter(partner_type_usaid_adjusted=="Local" | partner_type_usaid_adjusted=="International" )%>%
    dplyr::select (c(partner_type_usaid_adjusted,fiscal_year,cop_budget_total,expenditure_amt))%>%
    group_by(partner_type_usaid_adjusted,fiscal_year)%>%
    summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
    dplyr::mutate(budget_execution=percent_clean(expenditure_amt,cop_budget_total))%>%
    ungroup()%>%
    pivot_wider(names_from = fiscal_year,values_from = cop_budget_total:budget_execution, values_fill = 0)%>%
    dplyr::relocate(expenditure_amt_2022, .before = cop_budget_total_2022) %>%
    dplyr::relocate(expenditure_amt_2021, .before = cop_budget_total_2021) %>%
    dplyr::relocate(budget_execution_2021, .after = cop_budget_total_2021)%>%
    dplyr::relocate(budget_execution_2022, .after = cop_budget_total_2022) %>%
    gt()%>%
    fmt_percent(
      columns = c(`budget_execution_2022`, `budget_execution_2021`),
      decimals = 0)%>%
    fmt_currency( # add dolar signs
      columns = c(`cop_budget_total_2022`,`expenditure_amt_2022`,`cop_budget_total_2021`,`expenditure_amt_2021` ),
      decimals = 0,
      currency = "USD")%>%
    tab_options(
      table.font.names = "Source Sans Pro"
    ) %>% 
    cols_width(
      everything() ~ px(90))%>%
    cols_label(
      partner_type_usaid_adjusted = "Partner Type",
      expenditure_amt_2022 = "Expenditure",
      cop_budget_total_2022 = "Budget",
      budget_execution_2022="Budget Execution",
      #agency_category = "Agency",
      expenditure_amt_2021 = "Expenditure",
      cop_budget_total_2021 = "Budget",
      budget_execution_2021="Budget Execution"
    )%>%
    tab_spanner(
      label = "COP20 Performance",
      columns = c(
        expenditure_amt_2021,cop_budget_total_2021, budget_execution_2021,))%>%
    tab_spanner(
        label = "COP21 Performance",
      columns = c(
        expenditure_amt_2022,cop_budget_total_2022,budget_execution_2022))%>%
    gt::tab_style(
      style = list(
        gt::cell_text(weight = "bold")), 
      locations = gt::cells_column_spanners(spanners = tidyselect::everything())
    )%>%
    tab_style(
      style = cell_text(weight = 700),
      locations = cells_body(
        columns = tidyselect::contains("_execution_")
      ))%>%
        gt::tab_options(
          source_notes.font.size = 8,
          table.font.size = 13, 
          data_row.padding = gt::px(5),
          source_notes.padding = gt::px(1),) %>%
   
    tab_style(
      style = cell_borders(
        sides = "right",
        weight = px(1),
      ),
      locations = cells_body(
        columns = everything(),
        rows = everything()
      ))%>%
  
    cols_align(
      align = "center",
      columns = everything()
    )%>%
    cols_align(
      align = "left",
      columns = tidyselect::contains("partner")
    )%>%
    tab_style(style = cell_fill(color = "#5bb5d5",alpha = .75),      
              locations = cells_body(               
                columns = (budget_execution_2022),
                rows = (budget_execution_2022) >= 0.9 & (budget_execution_2022) < 1.1)) %>%
    tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),      
              locations = cells_body(               
                columns = (budget_execution_2022),
                rows =(budget_execution_2022) < 0.9 ))%>%
    tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),      
              locations = cells_body(               
                columns = (budget_execution_2022),
                rows = (budget_execution_2022)>= 1.1 & (budget_execution_2022) < 1.2))%>%
    tab_style(style = cell_fill(color = "#ff989f",alpha = .75),      
              locations = cells_body(               
                columns = (budget_execution_2022),
                rows = (budget_execution_2022) >= 1.2 ))%>%
    
    tab_style(style = cell_fill(color = "#5bb5d5",alpha = .75),      
              locations = cells_body(               
                columns = (budget_execution_2021),
                rows = (budget_execution_2021) >= 0.9 & (budget_execution_2021) < 1.1)) %>%
    tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),      
              locations = cells_body(               
                columns = (budget_execution_2021),
                rows =(budget_execution_2021) < 0.9 ))%>%
    tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),      
              locations = cells_body(               
                columns = (budget_execution_2021),
                rows = (budget_execution_2021)>= 1.1 & (budget_execution_2021) < 1.2))%>%
    tab_style(style = cell_fill(color = "#ff989f",alpha = .75),      
              locations = cells_body(               
                columns = (budget_execution_2021),
                rows = (budget_execution_2021) >= 1.2 ))%>%
    
    
    tab_footnote(
      footnote = "Excluding M&O and Commodities",
      locations = cells_column_labels(
        columns =c(expenditure_amt_2022, expenditure_amt_2021)))%>%
    tab_header(
      title = glue::glue(" COP20 & COP21 Local Partner Program Financial Summary: Global"),
      subtitle = legend_chunk)%>%
    gt::tab_source_note(
      source_note = ("USAID mechanisms only. Partner designations provided by the OHA Local Partners Team. Visual excludes TBDs"))%>%
    gt::tab_source_note(
      source_note = gt::md(glue::glue("**Source**: {source} | Please reach out to oha.ea@usaid.gov for questions."))
    ) %>%
    opt_table_outline()%>%
    
    opt_row_striping()
   
  return(df)
}
####Output========
table_out<-"GitHub/stacks-of-hondos/Images/Global Performance"
get_global_usaid_lp_be(df_fsd)%>%
gtsave(path=table_out,filename = "global_usaid_lp_performance.png")



#You can run a version of the function above for just LTS countries=============================
get_global_usaid_lp_be_lts<-function(df){
  df<-df%>%
    agency_category()%>%
    remove_mo()%>%
    remove_sch("SGAC")%>%
    dplyr::filter(fiscal_year %in% fys)%>%
    dplyr::filter(fundingagency=="USAID")%>%
    dplyr::filter(!operatingunit=="Asia Region")%>%
    dplyr::filter(!operatingunit=="Western Hemisphere Region")%>%
    dplyr::filter(!operatingunit=="Angola")%>%
    dplyr::filter(!operatingunit=="West Africa Region")%>%
    dplyr::filter(!operatingunit=="Dominican Republic")%>%
    dplyr::filter(!operatingunit=="Ukraine")%>%
    dplyr::filter(partner_type_usaid_adjusted=="Local" | partner_type_usaid_adjusted=="International" )%>%
    dplyr::select (c(partner_type_usaid_adjusted,fiscal_year,cop_budget_total,expenditure_amt))%>%
    group_by(partner_type_usaid_adjusted,fiscal_year)%>%
    summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
    dplyr::mutate(budget_execution=percent_clean(expenditure_amt,cop_budget_total))%>%
    ungroup()%>%
    pivot_wider(names_from = fiscal_year,values_from = cop_budget_total:budget_execution, values_fill = 0)%>%
    dplyr::relocate(expenditure_amt_2022, .before = cop_budget_total_2022) %>%
    dplyr::relocate(expenditure_amt_2021, .before = cop_budget_total_2021) %>%
    dplyr::relocate(budget_execution_2021, .after = cop_budget_total_2021)%>%
    dplyr::relocate(budget_execution_2022, .after = cop_budget_total_2022) %>%
    gt()%>%
    fmt_percent(
      columns = c(`budget_execution_2022`, `budget_execution_2021`),
      decimals = 0)%>%
    fmt_currency( # add dolar signs
      columns = c(`cop_budget_total_2022`,`expenditure_amt_2022`,`cop_budget_total_2021`,`expenditure_amt_2021` ),
      decimals = 0,
      currency = "USD")%>%
    tab_options(
      table.font.names = "Source Sans Pro"
    ) %>% 
    cols_width(
      everything() ~ px(90))%>%
    cols_label(
      partner_type_usaid_adjusted = "Partner Type",
      expenditure_amt_2022 = "Expenditure",
      cop_budget_total_2022 = "Budget",
      budget_execution_2022="Budget Execution",
      #agency_category = "Agency",
      expenditure_amt_2021 = "Expenditure",
      cop_budget_total_2021 = "Budget",
      budget_execution_2021="Budget Execution"
    )%>%
    tab_spanner(
      label = "COP20 Performance",
      columns = c(
        expenditure_amt_2021,cop_budget_total_2021, budget_execution_2021,))%>%
    tab_spanner(
      label = "COP21 Performance",
      columns = c(
        expenditure_amt_2022,cop_budget_total_2022,budget_execution_2022))%>%
    gt::tab_style(
      style = list(
        gt::cell_text(weight = "bold")), 
      locations = gt::cells_column_spanners(spanners = tidyselect::everything())
    )%>%
    tab_style(
      style = cell_text(weight = 700),
      locations = cells_body(
        columns = tidyselect::contains("_execution_")
      ))%>%
    gt::tab_options(
      source_notes.font.size = 8,
      table.font.size = 13, 
      data_row.padding = gt::px(5),
      source_notes.padding = gt::px(1),) %>%
    
    tab_style(
      style = cell_borders(
        sides = "right",
        weight = px(1),
      ),
      locations = cells_body(
        columns = everything(),
        rows = everything()
      ))%>%
    
    cols_align(
      align = "center",
      columns = everything()
    )%>%
    cols_align(
      align = "left",
      columns = tidyselect::contains("partner")
    )%>%
    tab_style(style = cell_fill(color = "#5bb5d5",alpha = .75),      
              locations = cells_body(               
                columns = (budget_execution_2022),
                rows = (budget_execution_2022) >= 0.9 & (budget_execution_2022) < 1.1)) %>%
    tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),      
              locations = cells_body(               
                columns = (budget_execution_2022),
                rows =(budget_execution_2022) < 0.9 ))%>%
    tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),      
              locations = cells_body(               
                columns = (budget_execution_2022),
                rows = (budget_execution_2022)>= 1.1 & (budget_execution_2022) < 1.2))%>%
    tab_style(style = cell_fill(color = "#ff989f",alpha = .75),      
              locations = cells_body(               
                columns = (budget_execution_2022),
                rows = (budget_execution_2022) >= 1.2 ))%>%
    
    tab_style(style = cell_fill(color = "#5bb5d5",alpha = .75),      
              locations = cells_body(               
                columns = (budget_execution_2021),
                rows = (budget_execution_2021) >= 0.9 & (budget_execution_2021) < 1.1)) %>%
    tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),      
              locations = cells_body(               
                columns = (budget_execution_2021),
                rows =(budget_execution_2021) < 0.9 ))%>%
    tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),      
              locations = cells_body(               
                columns = (budget_execution_2021),
                rows = (budget_execution_2021)>= 1.1 & (budget_execution_2021) < 1.2))%>%
    tab_style(style = cell_fill(color = "#ff989f",alpha = .75),      
              locations = cells_body(               
                columns = (budget_execution_2021),
                rows = (budget_execution_2021) >= 1.2 ))%>%
    
    
    tab_footnote(
      footnote = "Excluding M&O and Commodities",
      locations = cells_column_labels(
        columns =c(expenditure_amt_2022, expenditure_amt_2021)))%>%
    tab_header(
      title = glue::glue(" COP20 & COP21 Local Partner Program Financial Summary: LTS Countries (Excluding Ukraine)"),
      subtitle = legend_chunk)%>%
    gt::tab_source_note(
      source_note = ("USAID mechanisms only. Partner designations provided by the OHA Local Partners Team. Visual excludes TBDs"))%>%
    gt::tab_source_note(
      source_note = gt::md(glue::glue("**Source**: {source} | Please reach out to oha.ea@usaid.gov for questions."))
    ) %>%
    opt_table_outline()%>%
    
    opt_row_striping()
  
  return(df)
}
####Output========
table_out<-"GitHub/stacks-of-hondos/Images/Global Performance"
get_global_usaid_lp_be_lts(df_fsd)%>%
  gtsave(path=table_out,filename = "lts_usaid_lp_performance.png")

#Uploading to google drive===============================================
 source("~/GitHub/EA-Utilities/upload_dir_to_gdrive.R")
# 
 local_p <- table_out
 g_path <- '1HwKnJUrcil0oXGAejzVkLwMEV7e88aZw'
# 
 upload_dir_to_gdrive(local_p, g_path)


