library(glamr)
library(tidyverse)
library(gophr)
library(extrafont)
library(tidytext)
library(gt)
library(glue)
library(webshot)


df_fsd<-si_path()%>%
  return_latest("Fin")%>%
  gophr::read_msd()


#This function can be used to print out budget execution by partner type (local, international)for USAID at an OU level. You can use this to 
#print information for multiple OUs. You will need to ensure that you have load_secrets from the glamr package set up beforehand
#Be sure to load the following source files below before running
source("~/GitHub/stacks-of-hondos/utilities.R")

#ensure glamr load_secrets is loaded to get partner type data
glamr::load_secrets()
#apply partner type data to fsd before starting
df_fsd<-df_fsd%>%
  glamr::apply_partner_type()


get_ou_usaid_lp_be<-function(df, ou="operatingunit"){
  
  df<-df%>%
    remove_mo()%>%
    remove_sch("SGAC")%>%
    dplyr::filter(fiscal_year=="2020" | fiscal_year=="2021")%>%
    dplyr::filter(fundingagency=="USAID")%>%
    dplyr::filter(operatingunit %in% ou)%>%
    dplyr::filter(partner_type_usaid_adjusted=="Local" | partner_type_usaid_adjusted=="International" )%>%
    dplyr::select (c(partner_type_usaid_adjusted,fiscal_year,cop_budget_total,expenditure_amt))%>%
    group_by(partner_type_usaid_adjusted,fiscal_year)%>%
    summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
    dplyr::mutate(budget_execution=percent_clean(expenditure_amt,cop_budget_total))%>%
    ungroup()%>%
    pivot_wider(names_from = fiscal_year,values_from = cop_budget_total:budget_execution, values_fill = 0)%>%
    dplyr::relocate(expenditure_amt_2020, .before = cop_budget_total_2020) %>%
    dplyr::relocate(expenditure_amt_2021, .before = cop_budget_total_2021) %>%
    dplyr::relocate(budget_execution_2021, .after = cop_budget_total_2021)%>%
    dplyr::relocate(budget_execution_2020, .after = cop_budget_total_2020) %>%
    gt()%>%
    fmt_percent(
      columns = c(`budget_execution_2020`, `budget_execution_2021`),
      decimals = 0)%>%
    fmt_currency( # add dolar signs
      columns = c(`cop_budget_total_2020`,`expenditure_amt_2020`,`cop_budget_total_2021`,`expenditure_amt_2021` ),
      decimals = 0,
      currency = "USD")%>%
    tab_options(
      table.font.names = "Source Sans Pro"
    ) %>% 
    cols_width(
      everything() ~ px(90))%>%
    cols_label(
      partner_type_usaid_adjusted = "Partner Type",
      expenditure_amt_2020 = "Expenditure",
      cop_budget_total_2020 = "Budget",
      budget_execution_2020="Budget Execution",
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
        label = "COP19 Performance",
      columns = c(
        expenditure_amt_2020,cop_budget_total_2020,budget_execution_2020))%>%
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
        sides = "all",
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
                columns = (budget_execution_2020),
                rows = (budget_execution_2020) >= 0.9 & (budget_execution_2020) < 1.1)) %>%
    tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),      
              locations = cells_body(               
                columns = (budget_execution_2020),
                rows =(budget_execution_2020) < 0.9 ))%>%
    tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),      
              locations = cells_body(               
                columns = (budget_execution_2020),
                rows = (budget_execution_2020)>= 1.1 & (budget_execution_2020) < 1.2))%>%
    tab_style(style = cell_fill(color = "#ff989f",alpha = .75),      
              locations = cells_body(               
                columns = (budget_execution_2020),
                rows = (budget_execution_2020) >= 1.2 ))%>%
    
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
        columns =c(expenditure_amt_2020, expenditure_amt_2021)))%>%
    tab_header(
      title = glue::glue(" COP19 & COP20 Local Partner Program Financial Summary: {ou}"),
      subtitle = legend_chunk)%>%
    gt::tab_source_note(
      source_note = ("USAID mechanisms only. Partner designations provided by the OHA Local Partners Team. Visual excludes TBDs"))%>%
    gt::tab_source_note(
      source_note = gt::md(glue::glue("**Source**: {source} | Please reach out to oha.ea@usaid.gov for questions. Please note that FY21 ER data does not include the following 
    mechanisms due to data import issues: 70031-Cameroon, 80052-DR, 81108-DR, 18093-DRC, 81894-South Africa,70388-Uganda, 81978-Uganda, 85157-WAR, 85155-WAR, 85158-WAR, 85213-WHR. "))
    ) 
   
  return(df)
}
##Output the file=======
table_out<-"GitHub/stacks-of-hondos/Images/Local Partners"
#to run for one OU, change the OU name below and change test to OU name.
get_ou_usaid_lp_be(df_fsd, "Mozambique")%>%
gtsave(.,path=table_out,"Mozambique__lp_budget_execution.png") 

#to run for all OUs.You can also run for country use country_list in place of ou_list
purrr::map(ou_list, ~get_ou_usaid_lp_be(df_fsd, ou = .x)%>%
gtsave(.,path=table_out,filename = glue::glue("{.x}_lp_budget_execution.png")))

#Uploading to google drive===============================================
source("~/GitHub/EA-Utilities/upload_dir_to_gdrive.R")


local_p <- table_out
g_path <- '1V_58kCkggfpY89_-C1rmmrIn4wHzGJ_D'

upload_dir_to_gdrive(local_p, g_path)


