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
read_psd()


#use this function to print out budget execution by agency at different OUs. 
#Be sure to load the following files below before running


source("~/GitHub/stacks-of-hondos/scripts/ea_style.R")
source("~/GitHub/stacks-of-hondos/scripts/prep_fsd.R")
source("~/GitHub/stacks-of-hondos/scripts/utilities.R")

get_ou_agency_be<-function(df, ou="operatingunit"){
  df<-df%>%
    prep_fsd()%>%
    dplyr::filter(fiscal_year%in% fys)%>%
    dplyr::filter(operatingunit %in% ou)%>%
    dplyr::select (c(funding_agency,fiscal_year,cop_budget_total,expenditure_amt))%>%
    mutate_at(vars(cop_budget_total,expenditure_amt),~replace_na(.,0))%>%
   
    mutate( funding_agency = fct_relevel(funding_agency, "USAID","CDC"))%>%
    group_by(funding_agency,fiscal_year)%>%
    summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
    dplyr::mutate(budget_execution=percent_clean(expenditure_amt,cop_budget_total))%>%
    ungroup()%>%
    pivot_wider(names_from = fiscal_year,values_from = cop_budget_total:budget_execution, values_fill = 0)%>%
    dplyr::relocate(expenditure_amt_2023, .before = cop_budget_total_2023) %>%
    dplyr::relocate(expenditure_amt_2022, .before = cop_budget_total_2022) %>%
    dplyr::relocate(budget_execution_2022, .after = cop_budget_total_2022)%>%
    dplyr::relocate(budget_execution_2023, .after = cop_budget_total_2023) %>%
    ea_style()%>%
    cols_label(
     funding_agency = "Funding Agency")%>%
    tab_header(
      title = glue::glue(" COP21 & COP22 Program Financial Summary: {ou}"),
      subtitle = legend_chunk)
      
     
    
  return(df)
}
#Output========
table_out<-"GitHub/stacks-of-hondos/Images/OU"
#to run for one OU below. Be sure to name the ou 
# get_ou_agency_be(df_fsd, "South Africa")%>%
#   gtsave(.,path=table_out,"test_be.png")
#to run for all ous
purrr::map(ou_list, ~get_ou_agency_be(df_fsd, ou = .x)%>%
             gtsave(.,path=table_out,filename = glue::glue("{.x}_budget_execution.png")))

#Uploading to google drive===============================================
source("~/GitHub/EA-Utilities/upload_dir_to_gdrive.R")

local_p <- table_out
g_path <- '1HwKnJUrcil0oXGAejzVkLwMEV7e88aZw'

upload_dir_to_gdrive(local_p, g_path)



