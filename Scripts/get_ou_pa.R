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

git_src <- "~/GitHub"
source(glue("{git_src}/stacks-of-hondos/Scripts/ea_style.R"))
source(glue("{git_src}/stacks-of-hondos/Scripts/prep_fsd.R"))
source(glue("{git_src}/stacks-of-hondos/Scripts/utilities.R"))


### Function ===================================================================
get_ou_pa<-function(df, ou="operatingunit"){
  df<-df%>%
    prep_fsd()%>%
    agency_category()%>%
    
    #filter for fiscal year
    dplyr::filter(fiscal_year%in% fys)%>%
    
    #filter for OU
    dplyr::filter(operatingunit %in% ou)%>%
    
    #remove not specified PA (moz issue)
    dplyr::filter(!program=="Not Specified")%>%
    
    #select specific variables
    dplyr::select (c(agency_category,program, fiscal_year,cop_budget_total,expenditure_amt))%>%
    mutate_at(vars(cop_budget_total,expenditure_amt),~replace_na(.,0))%>%
    mutate( agency_category = fct_relevel(agency_category, "USAID","CDC"))%>%
    
    group_by(agency_category,program,fiscal_year)%>%
    summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
    dplyr::mutate(budget_execution=percent_clean(expenditure_amt,cop_budget_total))%>%
    ungroup()%>%
    
    
    pivot_wider(names_from = fiscal_year,values_from = cop_budget_total:budget_execution, values_fill = 0)%>%
    dplyr::relocate(expenditure_amt_2023, .before = cop_budget_total_2023) %>%
    dplyr::relocate(expenditure_amt_2022, .before = cop_budget_total_2022) %>%
    dplyr::relocate(budget_execution_2022, .after = cop_budget_total_2022)%>%
    dplyr::relocate(budget_execution_2023, .after = cop_budget_total_2023) %>%
    
   group_by(agency_category)%>%
    #break into separate functions
    
    ea_style()%>%
    cols_label(
      program = "Program Area")%>%
    tab_header(
      title = glue::glue("COP21 & COP22 Program Financial Summary: {ou} By Program Area"),
      subtitle = legend_chunk) %>%
    
    tab_options(footnotes.font.size = "small")
  
  return(df)
}

### Run Code for Output ========================================================
# Read in the FSD data.frame from where you locally saved it
df_fsd<-si_path()%>%
  return_latest("Fin")%>%
  read_psd()

# To use function, change the "fundingagency" or "ou" options
#get_ou_pa(df_fsd,"Zambia")%>%
 
# gtsave(.,path=table_out,filename = glue::glue("Mozambique_pa_budget_execution.png"))



#Output========
table_out<-"GitHub/stacks-of-hondos/Images"
#to run for one OU below. Be sure to name the ou 
get_ou_pa(df_fsd, "Zimbabwe")

#to run for all ous
purrr::map(ou_list, ~get_ou_pa(df_fsd, ou = .x)%>%
             gtsave(.,path=table_out,filename = glue::glue("{.x}_pa_budget_execution.png")))

#Uploading to google drive===============================================
source("~/GitHub/EA-Utilities/upload_dir_to_gdrive.R")

local_p <- table_out
g_path <- '1V_58kCkggfpY89_-C1rmmrIn4wHzGJ_D'

upload_dir_to_gdrive(local_p, g_path)
