# Purpose: Use this function to print out budget execution by agency at the global level.

### Libraries ==================================================================
library(glamr)
library(tidyverse)
library(gophr)
library(extrafont)
library(tidytext)
library(gt)
library(glue)
library(webshot2)

# Be sure to load the following files below before running
source("~/GitHub/stacks-of-hondos/Scripts/ea_style.R")
source("~/GitHub/stacks-of-hondos/Scripts/prep_fsd.R")
source("~/GitHub/stacks-of-hondos/Scripts/utilities.R")


### Function ===================================================================
get_global_agency_be<-function(df){
  df<-df%>%
    prep_fsd()%>%
    dplyr::filter(fiscal_year %in% fys)%>%
    filter(!operatingunit=="Ukraine")%>%
    dplyr::select (c(agency_category,fiscal_year,cop_budget_total,expenditure_amt))%>%
    group_by(agency_category,fiscal_year)%>%
    summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
    dplyr::mutate(budget_execution=percent_clean(expenditure_amt,cop_budget_total))%>%
    ungroup()%>%
    pivot_wider(names_from = fiscal_year,values_from = cop_budget_total:budget_execution, values_fill = 0)%>%
    dplyr::relocate(expenditure_amt_2022, .before = cop_budget_total_2022) %>%
    dplyr::relocate(expenditure_amt_2021, .before = cop_budget_total_2021) %>%
    dplyr::relocate(budget_execution_2021, .after = cop_budget_total_2021)%>%
    dplyr::relocate(budget_execution_2022, .after = cop_budget_total_2022) %>%
    #gt()%>%
    ea_style()%>%
    cols_label( #update GT to check on tidy select), also look at clean_names, also potentially case_when
      agency_category = "Agency")%>%

    tab_header(
      title = (" COP20 & COP21 Program Financial Summary: Global"),
      subtitle = legend_chunk)%>%
    tab_source_note(
      source_note = md("*Other* based on aggregated funding agencies"))
  
  
  return(df)
}

### Run Code to Produce Output =================================================

# Read in the FSD dataframe from where it is locally saved
df_fsd<-si_path()%>%
  return_latest("Fin")%>%
  read_msd()

# Output: 
#       * Feel free to change the path you are saving the file to, by changing
#         the variable "table_out"
table_out<-"GitHub/stacks-of-hondos/Images/Global Performance"
get_global_agency_be(df_fsd)%>%
  gtsave(., path=table_out, filename="global performance_all_agencies.png")

#Uploading to google drive===============================================
# source("~/GitHub/EA-Utilities/upload_dir_to_gdrive.R")
# 
# local_p <- table_out
# g_path <- '1V_58kCkggfpY89_-C1rmmrIn4wHzGJ_D'
# 
# upload_dir_to_gdrive(local_p, g_path)