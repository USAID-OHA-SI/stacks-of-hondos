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

indics<-c("HTS_TST","HTS_TST_POS", "TX_CURR", "TX_NEW","OVC_SERV")
progs<-c("HTS", "C&T","OVC")



# MUNGE FSD ============================================================================

df_fsd<-df_fsd%>%
  remove_mo()%>%
  clean_agency()%>%
  mutate( fundingagency = fct_relevel(fundingagency, "USAID","CDC"))%>%
  dplyr::mutate(program = dplyr::case_when(beneficiary    == "OVC"    ~"OVC", 
                                           
                                           
                                           TRUE ~program))%>%
  group_by(operatingunit,fundingagency,fiscal_year, mech_code, mech_name, primepartner, program) %>% 
  #group_by(country, mech_code, mech_name, primepartner, fiscal_year, `Program Area: Sub Program Area-Service Level`,`Beneficiary-Sub Beneficiary`)%>%
  summarise_at(vars(cop_budget_total, expenditure_amt), sum, na.rm = TRUE) %>% 
  filter(fiscal_year="2021")%>%
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
  
  dplyr::mutate(program = dplyr::case_when(indicator    == "TX_CURR"    ~"C&T", 
                                           indicator    == "TX_NEW"    ~"C&T",
                                           indicator =="HTS_TST" ~"HTS",
                                           indicator == "HTS_TST_POS" ~"HTS",
                                           indicator    == "OVC_SERV"    ~"OVC",
                                           
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
df_ue<-left_join(df_fsd,df_msd)%>%
  filter(fiscal_year=="2021")%>%
  mutate_at(vars(cop_budget_total: OVC_SERV),~replace_na(.,0))%>%
  mutate(mech=glue("{mech_code}-{mech_name}"))%>%
  relocate(expenditure_amt, .before= cop_budget_total)%>%
  relocate(mech, .before= expenditure_amt)%>%
 
  select(operatingunit,fundingagency,program,mech:OVC_SERV)
  
get_program_specfic<-function(df, ou="operatingunit",programs=c("HTS","C&T","OVC")){
  
  df<-df_ue%>%
    filter(program %in% programs,
           operatingunit %in% ou)%>%
    select_if(~!( all(. == 0)))%>%
    select(-(operatingunit))
  return(df)
}


df_hts<-df_ue%>%
  filter(program=="HTS",
         
         operatingunit %in% ou)%>%
  select_if(~!( all(. == 0)))%>%
   filter(HTS_TST>0 | HTS_TST_POS>0
          |budget_execution>0)%>%
  select(-(operatingunit))

df_ct<-df_ue%>%
  filter(program=="C&T",
         
         operatingunit %in% ou)%>%
  
select_if(~!( all(. == 0)))%>%
  select(operatingunit:budget_execution,TX_CURR,TX_NEW)%>%
  filter(TX_CURR>0 | TX_NEW>0
         |budget_execution>0)%>%
  select(fundingagency,mech:TX_NEW)

df_ovc<-df_ue%>%
  filter(program=="OVC",
        
         operatingunit %in% ou)%>%
  select_if(~!( all(. == 0)))%>%
  select(operatingunit:budget_execution,OVC_SERV)%>%
  filter(OVC_SERV>0 
         |budget_execution>0)%>%
  select(fundingagency,mech:OVC_SERV)
 
