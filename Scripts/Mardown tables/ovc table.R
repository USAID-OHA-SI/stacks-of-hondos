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

df_msd<-si_path()%>%
  return_latest("OU_IM")%>%
  gophr::read_msd()

#This function can be used to generate unit expenditure tables across the treatment continuum
#It can be used to generate one table for one ou, or a batch for all ous.
#Be sure to load the following source files below before running
source("~/GitHub/stacks-of-hondos/Scripts/utilities.R")

indics<-c("OVC_SERV")
progs<-c("OVC")




# MUNGE FSD ============================================================================

df_fsd<-df_fsd%>%
  remove_mo()%>%
  clean_agency()%>%
  mutate( fundingagency = fct_relevel(fundingagency, "USAID","CDC"))%>%
  dplyr::mutate(program = dplyr::case_when(beneficiary    == "OVC"    ~"OVC", 
                                           
                                           
                                           TRUE ~program))%>%
  group_by(operatingunit,fundingagency,fiscal_year, mech_code, mech_name, primepartner, program,) %>%
  
  #group_by(country, mech_code, mech_name, primepartner, fiscal_year, `Program Area: Sub Program Area-Service Level`,`Beneficiary-Sub Beneficiary`)%>%
  summarise_at(vars(cop_budget_total, expenditure_amt), sum, na.rm = TRUE) %>% 
  ungroup()%>%
  filter(program %in% progs)%>%
  mutate(budget_execution=round(expenditure_amt/cop_budget_total*100))


# MUNGE MSD ============================================================================

df_msd<-df_msd%>%
  filter(standardizeddisaggregate=="Total Numerator")%>%
  filter(indicator %in% indics)%>%
  clean_agency()%>%
  mutate( fundingagency = fct_relevel(fundingagency, "USAID","CDC"))%>%
  #dplyr::select(operatingunit,fundingagency,fiscal_year, mech_code, mech_name, primepartner,indicator cumulative,targets)%>%
  group_by(operatingunit,fundingagency,fiscal_year, mech_code, mech_name, primepartner,indicator) %>% 
  #group_by(country, mech_code, mech_name, primepartner, fiscal_year, `Program Area: Sub Program Area-Service Level`,`Beneficiary-Sub Beneficiary`)%>%
  summarise_at(vars(cumulative,targets), sum, na.rm = TRUE) %>% 
  ungroup()%>%
  
  dplyr::mutate(program = dplyr::case_when(indicator    == "OVC_SERV"    ~"OVC", 
                                           
                                           
                                           TRUE ~indicator))
df_msd<-df_msd%>%
  filter(!fundingagency=="DEDUP")%>%
  dplyr::filter(targets>0)%>%
  mutate(achievement=round (cumulative/targets*100))

df_msd<-df_msd%>%
  select(operatingunit,fundingagency,fiscal_year,mech_code,mech_name,program,indicator,achievement)%>%
   pivot_wider(names_from = indicator,
               values_from=achievement)






#join datasets together 
df_ue<-full_join(df_fsd,df_msd)%>%
  mutate_at(vars(cop_budget_total: OVC_SERV),~replace_na(.,0))%>%
  mutate(mech=glue("{mech_code}-{mech_name}"))%>%
  relocate(expenditure_amt, .before= cop_budget_total)%>%
  relocate(mech, .before= expenditure_amt)%>%
  filter(fiscal_year=="2021")%>%
  filter(operatingunit %in% ou)%>%
  filter(OVC_SERV>0
         |budget_execution>0)%>%
  select(fundingagency,mech:OVC_SERV)

  
 

