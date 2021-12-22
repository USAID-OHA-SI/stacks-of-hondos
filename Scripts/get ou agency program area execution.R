# Produces GT table for Budget Execution by Program Area
# Inputs:
#        * df: FSD dataframe
#        * funding_agency: name of the funding agency, e.g. USAID or CDC. Datatype: string
#        * ou: name of operating unit selected. Datatype: string
# Output: gt table for a specific operating unit

### Libaries and Utility functions =============================================
###         Please download below before using the code ========================
library(glamr)
library(gt)
library(tidyverse)
library(glue)

git_src <- "~/GitHub"
source(glue("{git_src}/stacks-of-hondos/ea_style.R"))
source(glue("{git_src}/stacks-of-hondos/prep_fsd.R"))
source(glue("{git_src}/stacks-of-hondos/utilities.R"))


### Function ===================================================================
get_ou_pa_agency<-function(df, ou="operatingunit"){
  df<-df%>%
    prep_fsd()%>%
    
    #filter for fiscal year
    dplyr::filter(fiscal_year=="2020" | fiscal_year=="2021")%>%
    mutate( fundingagency = fct_relevel(fundingagency, "USAID","CDC"))%>%
    
    #filter for OU
    #dplyr::filter(operatingunit %in% ou)%>%
    #dplyr::filter(fundingagency %in% funding_agency) %>% 
    
    #select specific variables
    dplyr::select (c(fundingagency,program, fiscal_year,cop_budget_total,expenditure_amt))%>%
    mutate_at(vars(cop_budget_total,expenditure_amt),~replace_na(.,0))%>%
    
    
    group_by(fundingagency,program,fiscal_year)%>%
    summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
    dplyr::mutate(budget_execution=percent_clean(expenditure_amt,cop_budget_total))%>%
    ungroup()%>%
    
    
    pivot_wider(names_from = fiscal_year,values_from = cop_budget_total:budget_execution, values_fill = 0)%>%
    dplyr::relocate(expenditure_amt_2020, .before = cop_budget_total_2020) %>%
    dplyr::relocate(expenditure_amt_2021, .before = cop_budget_total_2021) %>%
    dplyr::relocate(budget_execution_2021, .after = cop_budget_total_2021)%>%
    dplyr::relocate(budget_execution_2020, .after = cop_budget_total_2020) %>%
    ungroup()%>%

    gt(
      groupname_col = "fundingagency"
      
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
        sides = "right",
        weight = px(1),
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
      budget_execution_2021="Budget Execution"
      
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
    cols_label(
      program = "Program Area")%>%
    tab_header(
      title = glue::glue("Agency Program Area COP19 & COP20 {ou} Financial Performance Summary"),
      subtitle = legend_chunk) %>%
    
    tab_options(footnotes.font.size = "small")
  
  return(df)
}

### Run Code for Output ========================================================
# Read in the FSD data.frame from where you locally saved it
df_fsd<-si_path()%>%
  return_latest("Fin")%>%
  read_msd()

# To use function, change the "fundingagency" or "ou" options
get_ou_pa_agency(df_fsd, "Mozambique")

# Output ============================================================================
table_out<-"GitHub/stacks-of-hondos/Images/OU"
#to run for one OU, be sure to change the ou to the ou name
get_ou_pa_agency(df_fsd, "Mozambique")%>%
  gtsave(.,path=table_out,filename = glue::glue("Mozambique_agency_program_area_expenditure.png"))
#to run for all OUs. You can use country_list to do countries 
purrr::map(ou_list, ~get_ou_pa_agency(df, ou = .x)%>%
             gtsave(.,path=table_out,filename = glue::glue("{.x}_agency_program_area_expenditure.png")))


#Uploading to google drive===============================================
source("~/GitHub/EA-Utilities/upload_dir_to_gdrive.R")

local_p <- table_out
g_path <- '1V_58kCkggfpY89_-C1rmmrIn4wHzGJ_D'

upload_dir_to_gdrive(local_p, g_path)
