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
  


#use this function to print out budget executionfor just USAID in all OUs. 
#You should load the source files below before running

source("~/GitHub/stacks-of-hondos/ea_style.R")
source("~/GitHub/stacks-of-hondos/prep_fsd.R")
source("~/GitHub/stacks-of-hondos/utilities.R")

get_global_usaid_ou<-function(df){
  df<-df%>%
    prep_fsd()%>%
    filter(fundingagency=="USAID")%>%
    dplyr::filter(fiscal_year=="2020" | fiscal_year=="2021")%>%
    group_by(operatingunit,fiscal_year)%>%
    #mutate_at(vars(cop_budget_total,expenditure_amt),~replace_na(.,0))%>%
    summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
    dplyr::mutate(budget_execution=expenditure_amt/cop_budget_total)%>%
    ungroup()%>%
    pivot_wider(names_from = fiscal_year,values_from = cop_budget_total:budget_execution, values_fill = 0)%>%
    dplyr::relocate(expenditure_amt_2020, .before = cop_budget_total_2020) %>%
    dplyr::relocate(expenditure_amt_2021, .before = cop_budget_total_2021) %>%
    dplyr::relocate(budget_execution_2021, .after = cop_budget_total_2021)%>%
    dplyr::relocate(budget_execution_2020, .after = cop_budget_total_2020) %>%
    ea_style()%>%
    cols_label( #update GT to check on tidy select), also look at clean_names, also potentially case_when
      operatingunit = "Operating Unit")%>%
    
    tab_header(
      title = (" COP19 & COP20 Program Financial Summary: USAID "),
      subtitle = legend_chunk)
  return(df)
}
  
#output of table========================================================
table_out<-"GitHub/stacks-of-hondos/Images/global performance"
get_global_usaid_ou(df_fsd)%>%
  gtsave(., path=table_out, filename="global performance_usaid.png")


#Uploading to google drive===============================================
source("~/GitHub/EA-Utilities/upload_dir_to_gdrive.R")

local_p <- table_out
g_path <- '1V_58kCkggfpY89_-C1rmmrIn4wHzGJ_D'

upload_dir_to_gdrive(local_p, g_path)
