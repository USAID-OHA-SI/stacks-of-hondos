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

indics<-c("PrEP_NEW")
progs<-c("PrEP")




# MUNGE FSD ============================================================================

df_fsd<-df_fsd%>%
  remove_mo()%>%
  clean_agency()%>%
  mutate( fundingagency = fct_relevel(fundingagency, "USAID","CDC"))%>%
  dplyr::mutate(program = dplyr::case_when(grepl("PrEP", sub_program) ~ "PrEP", 
                                           
                                           TRUE ~program))%>%
  group_by(operatingunit,fundingagency,fiscal_year, mech_code, mech_name, primepartner, program,) %>%
  
  #group_by(country, mech_code, mech_name, primepartner, fiscal_year, `Program Area: Sub Program Area-Service Level`,`Beneficiary-Sub Beneficiary`)%>%
  summarise_at(vars(cop_budget_total, expenditure_amt), sum, na.rm = TRUE) %>% 
  ungroup()%>%
  filter(program %in% progs)%>%
  mutate(budget_execution=round(expenditure_amt/cop_budget_total*100))


# MUNGE MSD ============================================================================

df_msd<-df_msd%>%
   dplyr::rename(fundingagency=funding_agency)%>%
  filter(standardizeddisaggregate=="Total Numerator")%>%
  
filter(indicator %in% indics)%>%
  clean_agency()%>%
  mutate( fundingagency = fct_relevel(fundingagency, "USAID","CDC"))%>%
  #dplyr::select(operatingunit,fundingagency,fiscal_year, mech_code, mech_name, primepartner,indicator cumulative,targets)%>%
  group_by(operatingunit,fundingagency,fiscal_year, mech_code, mech_name, primepartner,indicator) %>% 
  #group_by(country, mech_code, mech_name, primepartner, fiscal_year, `Program Area: Sub Program Area-Service Level`,`Beneficiary-Sub Beneficiary`)%>%
  summarise_at(vars(cumulative,targets), sum, na.rm = TRUE) %>% 
  ungroup()%>%
  dplyr::mutate(program = dplyr::case_when(grepl("PrEP", indicator) ~ "PrEP", 
                                           
                                           TRUE ~indicator))
  
df_msd<-df_msd%>%
  filter(!fundingagency=="DEDUP")%>%
  dplyr::filter(targets>0)%>%
  mutate(achievement=round (cumulative/targets*100))

df_msd<-df_msd%>%
  select(operatingunit,fundingagency,fiscal_year,mech_code,mech_name,program,indicator,achievement, cumulative)%>%
   pivot_wider(names_from = indicator,
               values_from=cumulative:achievement)






#join datasets together 
df_ue<-full_join(df_fsd,df_msd)%>%
  mutate_at(vars(cop_budget_total: achievement_PrEP_NEW),~replace_na(.,0))%>%
  mutate(mech=glue("{mech_code}-{mech_name}"))%>%
  relocate(expenditure_amt, .before= cop_budget_total)%>%
  relocate(mech, .before= expenditure_amt)%>%
  filter(fiscal_year=="2021")%>%
  filter(operatingunit %in% ou)%>%
  filter(achievement_PrEP_NEW>0
         |budget_execution>0)%>%
select(fundingagency,mech:achievement_PrEP_NEW)

#====for the briefer   
# # ```{r echo=FALSE}
#  df_ue_indiv <- gen_ue_indiv(fsd_tgt, msd_tgt)
#  prep_ue <- get_program_specific(df_ue_indiv, programs="PrEP")%>%
# 
# df_ue<-df_ue%>%
#   mutate(across(3:4, formattable::currency,digits=0)) %>%
#    mutate(across(c(7,5), function(x) x/100)) %>%
#    mutate(across(c(7,5), formattable::percent,digits=0))
# df_ue<-df_ue %>%
#    select(-c(fundingagency)) %>%
#    kable("latex", booktabs=T, longtable=T,
#         col.names = c("Mechanism", budg_ex_cols[1:3],
#                        "PrEP NEW (#)", "PrEP NEW Achieved")) %>%
#    kable_styling(latex_options=c("hold_position", "striped", "repeat_header")) %>%
#    pack_rows(index = ordered_table(df_ue, "fundingagency")) %>%
#    column_spec(1, width="15em") %>%
#   column_spec(5:6, width="5em")%>%
#   cat(., file = "df.html")
# ```

