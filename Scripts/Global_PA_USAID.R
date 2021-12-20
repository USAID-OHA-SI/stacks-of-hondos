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
get_global_pa_usaid<-function(df,funding_agency="fundingagency", ou="operatingunit"){
  df<-df%>%
    prep_fsd()%>%
    
    #filter for fiscal year
    dplyr::filter(fiscal_year=="2020" | fiscal_year=="2021")%>%

    #filter for OU
    dplyr::filter(operatingunit %in% ou)%>%
    dplyr::filter(fundingagency %in% funding_agency) %>% 
    
    #select specific variables
    dplyr::select (c(fundingagency,program, fiscal_year,cop_budget_total,expenditure_amt))%>%
    mutate_at(vars(cop_budget_total,expenditure_amt),~replace_na(.,0))%>%
    
    
    group_by(program,fiscal_year)%>%
    summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
    dplyr::mutate(budget_execution=percent_clean(expenditure_amt,cop_budget_total))%>%
    ungroup()%>%
    
    
    pivot_wider(names_from = fiscal_year,values_from = cop_budget_total:budget_execution, values_fill = 0)%>%
    dplyr::relocate(expenditure_amt_2020, .before = cop_budget_total_2020) %>%
    dplyr::relocate(expenditure_amt_2021, .before = cop_budget_total_2021) %>%
    dplyr::relocate(budget_execution_2021, .after = cop_budget_total_2021)%>%
    dplyr::relocate(budget_execution_2020, .after = cop_budget_total_2020) %>%
    
    #break into separate functions
    
    ea_style()%>%
    cols_label(
      program = "Program Area")%>%
    tab_header(
      title = glue::glue("{funding_agency} COP19 & COP20 {ou} Financial Performance Summary"),
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
get_global_pa_usaid(df_fsd,"CDC", "Malawi")
