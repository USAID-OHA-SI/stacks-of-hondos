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
library(gophr)
library(webshot2)

git_src <- "~/GitHub"
source(glue("{git_src}/stacks-of-hondos/Scripts/ea_style.R"))
source(glue("{git_src}/stacks-of-hondos/Scripts/prep_fsd.R"))
source(glue("{git_src}/stacks-of-hondos/Scripts/utilities.R"))


### Function ===================================================================
get_ou_pa_reg<-function(df, ou="operatingunit"){
  df<-df%>%
    prep_fsd()%>%
    agency_category()%>%
    dplyr::filter(stringr::str_detect(operatingunit, "Region")) %>% 
    label_aggregation ("Regional")%>%
    #filter for fiscal year
    dplyr::filter(fiscal_year %in% fys)%>%
    
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
    dplyr::relocate(budget_execution_2021, .before= budget_execution_2022 )%>%
      dplyr::relocate(cop_budget_total_2022,.after=budget_execution_2021)%>%

    dplyr::relocate(expenditure_amt_2021,.before=cop_budget_total_2021)%>%

      dplyr::relocate(budget_execution_2021,.before=expenditure_amt_2022)%>%

    
    
    # names(df)
    
   group_by(agency_category)%>%
    #break into separate functions
    
    ea_style()%>%
    cols_label(
      program = "Program Area")%>%
    tab_header(
      title = glue::glue("COP20 & COP21 Program Financial Summary: {ou} By Program Area"),
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
#get_ou_pa_reg(df_fsd,"Asia Region-Thailand")%>%
 
# gtsave(.,path=table_out,filename = glue::glue("Mozambique_pa_budget_execution.png"))



#Output========
table_out<-"GitHub/stacks-of-hondos/Images/Regional"

#to run for one OU below. Be sure to name the ou 
#get_ou_pa_reg(df_fsd, "Western Hemisphere Region-Guatemala")
#to run all
purrr::map(country_list_regionals, ~get_ou_pa_reg(df_fsd, ou = .x)%>%
             gtsave(.,path=table_out,filename = glue::glue("{.x}_pa_budget_execution_2023.png")))

#need a special one for Benin for COP21 only
df<-df%>%
  prep_fsd()%>%
  agency_category()%>%
  dplyr::filter(stringr::str_detect(operatingunit, "Region")) %>% 
  label_aggregation ("Regional")%>%
  #filter for fiscal year
  dplyr::filter(fiscal_year=="2022")%>%
  
  #filter for OU
  
  dplyr::filter(operatingunit=="West Africa Region-Benin")%>%
  
  #select specific variables
  dplyr::select (c(agency_category,program, fiscal_year,cop_budget_total,expenditure_amt))%>%
  mutate_at(vars(cop_budget_total,expenditure_amt),~replace_na(.,0))%>%
  mutate( agency_category = fct_relevel(agency_category, "USAID","CDC"))%>%
  
  group_by(agency_category,program,fiscal_year)%>%
  summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
  dplyr::mutate(budget_execution=percent_clean(expenditure_amt,cop_budget_total))%>%
  ungroup()%>%
  pivot_wider(names_from = fiscal_year,names_glue="{.value}_{fiscal_year}",values_from = cop_budget_total:budget_execution, values_fill = 0)


df<-df %>%
  dplyr::relocate(expenditure_amt_2022, .before = cop_budget_total_2022) %>%
  # dplyr::relocate(expenditure_amt_2021, .before = cop_budget_total_2021) %>%
  # dplyr::relocate(budget_execution_2021, .after = cop_budget_total_2021)%>%
  dplyr::relocate(budget_execution_2022, .after = cop_budget_total_2022) %>%
  
  
  # names(df)
  
  group_by(agency_category)%>%
  #break into separate functions
  
  gt()%>%
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
  sub_missing(columns = everything(),
              missing_text = "-") %>%
  cols_width(
    everything() ~ px(90))%>%
  
  tab_style(
    style = cell_borders(
      sides = "all",
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
  # tab_spanner(
  #   label = glue("COP{stringr::str_sub(fy_beg, -2)} Performance"),
  #   columns = tidyselect::matches(as.character(fy_beg)))%>%
  tab_spanner(
    label = glue("COP{stringr::str_sub(fy_end, -2)} Performance"), #glue::glue("COP{stringr::str_sub(fy_beg, -2)} Performance")
    columns = tidyselect::matches(as.character(fy_end)))%>%
  gt::tab_style(
    style = list(
      gt::cell_text(weight = "bold")), 
    locations = gt::cells_column_spanners(spanners = tidyselect::everything())
  )%>%
  cols_align(
    align = "center",
    columns = everything())%>%
  cols_label(
    # budget_execution_2021="Budget Execution",
    # expenditure_amt_2021 = "Expenditure",
    # cop_budget_total_2021 = "Budget",
    budget_execution_2022="Budget Execution",
    expenditure_amt_2022 = "Expenditure",
    cop_budget_total_2022 = "Budget",)%>%
  cols_align(
    align = "left",
    columns = 1
  )%>%
  
  
  # tab_style(style = cell_fill(color = "#5bb5d5",alpha = .75),
  #           locations = cells_body(
  #             columns = (budget_execution_2021),
  #             rows = (budget_execution_2021) >= 0.9 & (budget_execution_2021) < 1.1)) %>%
  # tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),
  #           locations = cells_body(
  #             columns = (budget_execution_2021),
  #             rows =(budget_execution_2021) < 0.9 ))%>%
  # tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),
  #           locations = cells_body(
  #             columns = (budget_execution_2021),
  #             rows = (budget_execution_2021)>= 1.1 & (budget_execution_2021) < 1.2))%>%
  # tab_style(style = cell_fill(color = "#ff989f",alpha = .75),
  #           locations = cells_body(
  #             columns = (budget_execution_2021),
  #             rows = (budget_execution_2021) >= 1.2 ))%>%
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
  
  gt::tab_options(
    source_notes.font.size = 8,
    table.font.size = 13, 
    data_row.padding = gt::px(5),
    source_notes.padding = gt::px(1),) %>%
  
  tab_footnote(
    footnote = "Excluding M&O",
    locations = cells_column_labels(
      columns = tidyselect::contains("_execution_")))%>%
  opt_table_outline()%>%
  
  opt_row_striping(row_striping = TRUE)%>%
  gt::tab_source_note(
    source_note = gt::md(glue::glue("**Source**: {source} | Please reach out to oha.ea@usaid.gov for questions."))
  )%>%
  cols_label(
    program = "Program Area")%>%
  tab_header(
    title = glue::glue("COP21 Program Financial Summary: West Africa Region-Benin By Program Area"),
    subtitle = legend_chunk) %>%
  
  tab_options(footnotes.font.size = "small")
  df%>%gtsave(.,path=table_out,filename = glue::glue("Western Africa Region-Benin_pa_budget_execution_2023.png"))


#Uploading to google drive===============================================
source("~/GitHub/EA-Utilities/upload_dir_to_gdrive.R")
# 
local_p <- table_out
g_path <- '1HwKnJUrcil0oXGAejzVkLwMEV7e88aZw'
# 
upload_dir_to_gdrive(local_p, g_path)




