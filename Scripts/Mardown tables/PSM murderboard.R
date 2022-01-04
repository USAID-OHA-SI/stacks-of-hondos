library(glamr)
library(tidyverse)
library(gophr)
library(janitor)
library(glue)




#This function can be used to generate unit expenditure tables across the treatment continuum
#It can be used to generate one table for one ou, or a batch for all ous.
#Be sure to load the following source files below before running
source("~/GitHub/stacks-of-hondos/Scripts/utilities.R")






# MUNGE FSD ============================================================================
psm_murder<-function(df, ou="operatingunit"){
df<-df%>%
  glamr::remove_mo()%>%
  filter(fundingagency=="USAID",
         fiscal_year=="2021",
         )%>%
  dplyr::filter(operatingunit %in% ou)%>%
  filter(str_detect(mech_name, 'GHSC'))%>%
  mutate(mech=glue("{mech_code}-{mech_name}"))%>%
  
  group_by(operatingunit, mech, program,sub_program) %>%
  summarise_at(vars(cop_budget_total, expenditure_amt), sum, na.rm = TRUE) %>% 
  ungroup()%>%
  select(mech,program: expenditure_amt)%>%
  relocate(expenditure_amt, .before= cop_budget_total)%>%
  relocate(mech, .before= program)%>%
  adorn_totals("row",,,, -mech)%>%
  mutate(budget_execution=round(expenditure_amt/cop_budget_total*100))%>%
return(df)
}

df_fsd<-si_path()%>%
  return_latest("Fin")%>%
  read_msd()
df_mozambique<-psm_murder(df_fsd,"Mozambique")

