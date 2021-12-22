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



#use this function to print out budget execution for just USAID in all OUs. 
#You should load the source files below before running

source("~/GitHub/stacks-of-hondos/ea_style.R")
source("~/GitHub/stacks-of-hondos/prep_fsd.R")
source("~/GitHub/stacks-of-hondos/utilities.R")

get_ou_agency_ou<-function(df){
  df<-df%>%
    prep_fsd()%>%
    dplyr::filter(fiscal_year=="2020" | fiscal_year=="2021")%>%
    dplyr::select (c(operatingunit,agency_category,fiscal_year,cop_budget_total,expenditure_amt))%>%
    mutate_at(vars(cop_budget_total,expenditure_amt),~replace_na(.,0))%>%
    mutate( agency_category = fct_relevel(agency_category, "USAID","CDC"))%>%
    group_by(operatingunit,agency_category,fiscal_year)%>%
    summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
    dplyr::mutate(budget_execution=percent_clean(expenditure_amt,cop_budget_total))%>%
    ungroup()%>%
    pivot_wider(names_from = fiscal_year,values_from = cop_budget_total:budget_execution, values_fill = 0)%>%
    dplyr::relocate(expenditure_amt_2020, .before = cop_budget_total_2020) %>%
    dplyr::relocate(expenditure_amt_2021, .before = cop_budget_total_2021) %>%
    dplyr::relocate(budget_execution_2021, .after = cop_budget_total_2021)%>%
    dplyr::relocate(budget_execution_2020, .after = cop_budget_total_2020) %>%
    
      gt(
        groupname_col = "operatingunit"
        
      )%>%
      
      fmt_percent(
        columns = tidyselect::contains("_execution_"),
        decimals = 0)%>%
      fmt_currency( # add dolar signs
        columns = tidyselect::contains("_budget_total"),
        decimals = 0,
        currency = "USD")%>%
      fmt_currency( # add dolar signs
        columns = tidyselect::contains("expenditure_amt"),
        decimals = 0,
        currency = "USD")%>%
      tab_options(
        table.font.names = "Source Sans Pro"
      ) %>% 
      fmt_missing(columns = everything(),
                  missing_text = "-") %>%
      cols_width(
        everything() ~ px(90))%>%
      
      tab_style(
        style = cell_borders(
          sides = "all",
          weight = px(1.5),
        ),
        locations = cells_body(
          columns = everything(),
          rows = everything()
        ))%>%
      tab_style(
        style = cell_text(weight = 700),
        locations = cells_body(
          columns = tidyselect::contains("_execution_")
        )
      )%>%
      tab_spanner(
        label = "COP20 Performance",
        columns = tidyselect::contains("2021"))%>%
      tab_spanner(
        label = "COP19 Performance",
        columns = tidyselect::contains("2020"))%>%
      gt::tab_style(
        style = list(
          gt::cell_text(weight = "bold")), 
        locations = gt::cells_column_spanners(spanners = tidyselect::everything())
      )%>%
      cols_align(
        align = "center",
        columns = everything()
      )%>%
      cols_label( #update GT to check on tidy select), also look at clean_names, also potentially case_when
        expenditure_amt_2020 = "Expenditure",
        cop_budget_total_2020 = "Budget",
        budget_execution_2020="Budget Execution",
        expenditure_amt_2021 = "Expenditure",
        cop_budget_total_2021 = "Budget",
        budget_execution_2021="Budget Execution",
        agency_category="Agency"
        
      )%>%
      cols_align(
        align = "left",
        columns = 1
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
      
      gt::tab_options(
        source_notes.font.size = 8,
        table.font.size = 13, 
        data_row.padding = gt::px(5),
        source_notes.padding = gt::px(1),) %>%
      
      tab_footnote(
        footnote = "Excluding M&O",
        locations = cells_column_labels(
          columns =c(expenditure_amt_2020, expenditure_amt_2021)))%>%
      gt::tab_source_note(
        source_note = gt::md(glue::glue("**Source**: {source} | Please reach out to oha.ea@usaid.gov for questions"))
      )%>%
    tab_source_note(
      source_note = md("*Other* based on aggregated funding agencies"))%>%
      #cols_label(
       # program = "Program Area")%>%
      tab_header(
        title = glue::glue("COP19 & COP20 Program Financial Summary: All OUs"),
        subtitle = legend_chunk) %>%
      
      tab_options(footnotes.font.size = "small")
    
    return(df)
}
  
  



#output of table========================================================
table_out<-"GitHub/stacks-of-hondos/Images/global performance"
get_ou_agency_ou(df_fsd)%>%
  gtsave(., path=table_out, filename="global performance_all_ou.png")


#Uploading to google drive===============================================
source("~/GitHub/stacks-of-hondos/upload_dir_to_gdrive.R")

local_p <- table_out
g_path <- '1V_58kCkggfpY89_-C1rmmrIn4wHzGJ_D'

upload_dir_to_gdrive(local_p, g_path)