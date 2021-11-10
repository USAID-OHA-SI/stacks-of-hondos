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
  read_msd()


#use this function to print out budget execution by agency at different regional-country OUs. 
#Be sure to load the following files below before running


source("~/GitHub/stacks-of-hondos/ea_style.R")
source("~/GitHub/stacks-of-hondos/prep_fsd.R")
source("~/GitHub/stacks-of-hondos/utilities.R")



get_ou_agency_be_reg<-function(df, ou="operatingunit"){
  df<-df%>%
    prep_fsd()%>%
    dplyr::filter(stringr::str_detect(operatingunit, "Region")) %>% 
    label_aggregation ("Regional")%>%
    dplyr::filter(fiscal_year=="2020" | fiscal_year=="2021")%>%
    dplyr::filter(operatingunit %in% ou)%>%
    dplyr::select (c(fundingagency,fiscal_year,cop_budget_total,expenditure_amt))%>%
    mutate_at(vars(cop_budget_total,expenditure_amt),~replace_na(.,0))%>%
    mutate( fundingagency = fct_relevel(fundingagency, "USAID","CDC"))%>%
    group_by(fundingagency,fiscal_year)%>%
    summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
    dplyr::mutate(budget_execution=percent_clean(expenditure_amt,cop_budget_total))%>%
    ungroup()%>%
    pivot_wider(names_from = fiscal_year,values_from = cop_budget_total:budget_execution, values_fill = 0)%>%
    dplyr::relocate(expenditure_amt_2020, .before = cop_budget_total_2020) %>%
    dplyr::relocate(expenditure_amt_2021, .before = cop_budget_total_2021) %>%
    dplyr::relocate(budget_execution_2021, .after = cop_budget_total_2021)%>%
    dplyr::relocate(budget_execution_2020, .after = cop_budget_total_2020) %>%
    
    ea_style()%>%
    cols_label(
      fundingagency = "Funding Agency")%>%
    tab_header(
      title = glue::glue(" COP19 & COP20 {ou} Agency Financial Performance Summary"),
      subtitle = legend_chunk)
  
  
  
  return(df)
}

#Output========
table_out<-"GitHub/stacks-of-hondos/Images/Regional"

#to run for one OU below. Be sure to name the ou 
get_ou_agency_be_reg(df_fsd, "Asia Region-Asia Regional Program")
#to run all
purrr::map(country_list_regionals, ~get_ou_agency_be_reg(df_fsd, ou = .x)%>%
             gtsave(.,path=table_out,filename = glue::glue("{.x}_budget_execution.png")))



#Uploading to google drive===============================================
source("~/GitHub/stacks-of-hondos/upload_dir_to_gdrive.R")

local_p <- table_out
g_path <- '1V_58kCkggfpY89_-C1rmmrIn4wHzGJ_D'

upload_dir_to_gdrive(local_p, g_path)


