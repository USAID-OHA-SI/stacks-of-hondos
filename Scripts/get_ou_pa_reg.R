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
    pivot_wider(names_from = fiscal_year,names_glue="{.value}_{fiscal_year}",values_from = cop_budget_total:budget_execution, values_fill = 0)
   
    
  df<-df %>%
    dplyr::relocate(expenditure_amt_2022, .before = cop_budget_total_2022) %>%
    dplyr::relocate(expenditure_amt_2021, .before = cop_budget_total_2021) %>%
    dplyr::relocate(budget_execution_2021, .after = cop_budget_total_2021)%>%
    dplyr::relocate(budget_execution_2022, .after = cop_budget_total_2022) %>%
    
    
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
#get_ou_pa_reg(df_fsd,"Asia Region-Asia Region")%>%
 
# gtsave(.,path=table_out,filename = glue::glue("Mozambique_pa_budget_execution.png"))



#Output========
table_out<-"GitHub/stacks-of-hondos/Images/Regional"

#to run for one OU below. Be sure to name the ou 
#get_ou_agency_be_reg(df_fsd, "	
#Asia Region-Asia Region")
#to run all
purrr::map(country_list_regionals, ~get_ou_pa_reg(df_fsd, ou = .x)%>%
             gtsave(.,path=table_out,filename = glue::glue("{.x}_pa_budget_execution.png")))



#Uploading to google drive===============================================
source("~/GitHub/EA-Utilities/upload_dir_to_gdrive.R")
# 
local_p <- table_out
g_path <- '1HwKnJUrcil0oXGAejzVkLwMEV7e88aZw'
# 
upload_dir_to_gdrive(local_p, g_path)




