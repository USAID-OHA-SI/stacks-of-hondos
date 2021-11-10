

library(glamr)
library(tidyverse)
library(gophr)
library(extrafont)
library(gt)
library(glue)
library(webshot)
library(dplyr)
library(devtools)
library(tidyr)
library(gisr)
library(scales)
library(sf)
library(glitr)



#Set path where data sources are is located in folder
set_paths(folderpath_msd="C:/Users/jmontespenaloza/Documents/Raw Datasets")


#select which data source to use
df_fsd<-si_path()%>%
  return_latest("Fin")%>%
  read_msd()


#This function can be used to print out budget execution by partner type (local, international)for USAID at a global level. 
#You will need to ensure that you have load_secrets from the glamr package set up beforehand
#Be sure to load the following source files below before running
source("~/GitHub/stacks-of-hondos/ea_style.R")
source("~/GitHub/stacks-of-hondos/prep_fsd.R")
source("~/GitHub/stacks-of-hondos/utilities.R")




#Partner Section====================================================================
#Run the following function to have COP19 & COP20 Financial performance by OUxPartnerxProgramArea
get_ou_partner_pa<-function(df, ou="operatingunit"){
  df<-df%>%
    prep_fsd()%>%
    
    #filter for fiscal year
    dplyr::filter(fiscal_year=="2020" | fiscal_year=="2021")%>%
    dplyr::filter(fundingagency == "USAID") %>% 
    
    #filter for OU
    #dplyr::filter(operatingunit== "Mozambique")%>%
    dplyr::filter(operatingunit %in% ou)%>%
    #dplyr::filter(fundingagency %in% funding_agency) %>% 
    
    
    #select specific variables
    dplyr::select (c(fundingagency,primepartner, program, fiscal_year,cop_budget_total,expenditure_amt))%>%
    mutate_at(vars(cop_budget_total,expenditure_amt),~replace_na(.,0))%>%
    #mutate( fundingagency = fct_relevel(fundingagency, "USAID","CDC"))%>%
   
    
    
    group_by(primepartner,program,fiscal_year)%>%
    summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
    dplyr::mutate(budget_execution=percent_clean(expenditure_amt,cop_budget_total))%>%
    ungroup(program,fiscal_year)%>%
    
    
    
    
    
    pivot_wider(names_from = fiscal_year,values_from = cop_budget_total:budget_execution, values_fill = 0)%>%
    dplyr::relocate(expenditure_amt_2020, .before = cop_budget_total_2020) %>%
    dplyr::relocate(expenditure_amt_2021, .before = cop_budget_total_2021) %>%
    dplyr::relocate(budget_execution_2021, .after = cop_budget_total_2021)%>%
    dplyr::relocate(budget_execution_2020, .after = cop_budget_total_2020) %>%
    
    
   
    #break into separate functions
    
    ea_style()%>%
    #gt()%>%
    cols_label(
      primepartner = "Partner",
      program = "Program Area")%>%
    tab_header(
      title = glue::glue("COP2019 & COP2020 {ou} Financial Performance Summary"),
      subtitle = legend_chunk) %>%
    
    tab_options(footnotes.font.size = "small")
    
   

  
  return(df)
}


#select path where images will be exported to
table_out<-"GitHub/stacks-of-hondos/Images/ou_partner_pa"
#to run for one OU testing below
get_ou_partner_pa(df_fsd, "Zimbabwe")%>%
  gtsave(.,path=table_out,"Zimbabwe_Partner_PA.png")
#to run for all OUs. Can also run for country use country_list in place of ou_list
purrr::map(ou_list, ~get_ou_partner_pa(df_fsd, ou = .x)%>%
             gtsave(.,path=table_out,filename = glue::glue("{.x}_mechanism.png")))


#Mechanism Section====================================================================
#Run the following function to have COP19 & COP20 Financial performance by OUxMechanismxProgramArea
get_ou_mechanism_pa<-function(df, ou="operatingunit"){
  df<-df%>%
    prep_fsd()%>%
    
    #filter for fiscal year
    dplyr::filter(fiscal_year=="2020" | fiscal_year=="2021")%>%
    #dplyr::filter(fundingagency == "USAID") %>% 
    
    #filter for OU
    #dplyr::filter(operatingunit== "Mozambique")%>%
    dplyr::filter(operatingunit %in% ou)%>%
    #dplyr::filter(fundingagency %in% funding_agency) %>% 
    
    #select specific variables
    dplyr::select (c(fundingagency,mech_id_mech_name,program, fiscal_year,cop_budget_total,expenditure_amt))%>%
    mutate_at(vars(cop_budget_total,expenditure_amt),~replace_na(.,0))%>%
    #mutate( fundingagency = fct_relevel(fundingagency, "USAID","CDC"))%>%
    
    
    
    group_by(mech_id_mech_name,program, fiscal_year)%>%
    summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
    dplyr::mutate(budget_execution=percent_clean(expenditure_amt,cop_budget_total))%>%
    ungroup(program, fiscal_year)%>%
    
    
    pivot_wider(names_from = fiscal_year,values_from = cop_budget_total:budget_execution, values_fill = 0)%>%
    dplyr::relocate(expenditure_amt_2020, .before = cop_budget_total_2020) %>%
    dplyr::relocate(expenditure_amt_2021, .before = cop_budget_total_2021) %>%
    dplyr::relocate(budget_execution_2021, .after = cop_budget_total_2021)%>%
    dplyr::relocate(budget_execution_2020, .after = cop_budget_total_2020) %>%
    
    #break into separate functions
    
    ea_style()%>%
    #gt()%>%
   
    cols_label(
      mech_id_mech_name = "Mechanism",
      program= "Program Area")%>%
    tab_header(
      title = glue::glue("COP2019 & COP2020 {ou} Financial Performance Summary"),
      subtitle = legend_chunk) %>%
    
    tab_options(footnotes.font.size = "small")
  
  return(df)
}

#select path where images will be exported to
table_out<-"GitHub/stacks-of-hondos/Images/ou_partner_pa"
#to run for one OU testing below
get_ou_mechanism_pa(df_fsd, "Zimbabwe") %>% 
gtsave(.,path=table_out,"Zimbabwe_Mechanism_PA.png")

#to run for all OUs. Can also run for country use country_list in place of ou_list
purrr::map(ou_list, ~get_ou_mechanism_pa(df_fsd, ou = .x)%>%
             gtsave(.,path=table_out,filename = glue::glue("{.x}_mechanism.png")))
