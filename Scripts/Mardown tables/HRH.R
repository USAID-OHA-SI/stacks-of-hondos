library(glamr)
library(tidyverse)
library(gophr)
library(glue)
library(janitor)
library(xlsx)



df_hrh<-si_path()%>%
  return_latest("HRH")%>%
  gophr::read_msd()



library(readxl)
Financial_Structured_Datasets_COP17_22_mock_dataset <- read_excel("~/Data/FY22Q3/Financial_Structured_Datasets_COP17-22_mock-dataset.xlsx", 
                                                                  col_types = c("text", "text", "text", 
                                                                                "text", "text", "text", "text", "text", 
                                                                                "text", "text", "text", "text", "text", 
                                                                                "text", "text", "text", "text", "text", 
                                                                                "text", "text", "text", "text", "text", 
                                                                                "text", "text", "text", "numeric", 
                                                                                "numeric", "numeric", "numeric", 
                                                                                "numeric"))
View(Financial_Structured_Datasets_COP17_22_mock_dataset)
df_hrh<-Financial_Structured_Datasets_COP17_22_mock_dataset

df_fsd<-si_path()%>%
  return_latest("Financial")%>%
  gophr::read_msd()
df_msd<-si_path()%>%
  return_latest("OU_IM")%>%
  gophr::read_msd()

# Munge HRH========
df_hrh<-df_hrh %>%
  rename(funding_agency=funding_agency_fing)%>%
  rename(operatingunit=operating_unit)%>%
  clean_agency()%>%
  mutate( funding_agency = fct_relevel(funding_agency,"USAID","CDC"))%>%
  mutate(annual_fte=as.numeric(annual_fte),
         individual_count=as.numeric(individual_count),
         actual_salary_expenditure =as.numeric(actual_salary_expenditure ),
         actual_annual_spend=as.numeric(actual_annual_spend),
         actual_fringe_expenditure=as.numeric(actual_fringe_expenditure),)%>%
  group_by(operatingunit,funding_agency,fiscal_year, mech_code, mech_name,interaction_type, program ,er_category)%>%
  summarise_at(vars(annual_fte,individual_count,actual_salary_expenditure,actual_annual_spend, actual_fringe_expenditure), sum, na.rm = TRUE)%>%
  ungroup()


#Munge FSD===
df_fsd<-df_fsd%>%
  clean_agency()%>%
  group_by(operatingunit,funding_agency,fiscal_year, )%>%
  summarise_at(vars(expenditure_amt), sum, na.rm = TRUE)%>%
  ungroup()

# first HRH Table=====
df_hrh1<-df_hrh%>%
  group_by(operatingunit,funding_agency,fiscal_year,)%>%
  summarise_at(vars(annual_fte,individual_count,actual_annual_spend), sum, na.rm = TRUE)%>%
  ungroup()

df_hrh_fsd1<-left_join(df_hrh1,df_fsd)%>%
  filter(operatingunit %in% ou)%>%
  filter(fiscal_year=="2022")%>%
  select(-c(fiscal_year,operatingunit))%>%
  adorn_totals("row",,,, -funding_agency)%>%
  mutate(hrh_share=round(actual_annual_spend/expenditure_amt *100))
 


# staff breakdown table====
df_hrh2<-df_hrh%>%
   
    dplyr::mutate(interaction_type  = dplyr::case_when(interaction_type    == "Service Delivery"    ~"SD",
                                                       interaction_type    == "Non Service Delivery"    ~"NSD",
                                                       TRUE ~interaction_type))%>%
    mutate(er_staff=glue("{er_category}-{interaction_type}"))%>%
    group_by(operatingunit,fundingagency,fiscal_year,er_staff,)%>%
    summarise_at(vars(actual_annual_spend,annual_fte), sum, na.rm = TRUE)%>%
   ungroup%>%
  pivot_longer(actual_annual_spend:annual_fte, names_to="key", values_to="value")%>%
  pivot_wider(names_from = c(er_staff, key), 
              values_from = value,
              values_fill=0
  )%>%
  filter(fiscal_year=="2022",
         operatingunit %in% ou)%>%
  select(-c(fiscal_year,operatingunit))%>%
  adorn_totals("row",,,, -fundingagency,)%>%
  dplyr::rowwise() %>%
  mutate(total_spend=sum(across(matches("annual_spend"), na.rm = T)))%>%
  mutate(total_fte=sum(across(matches("annual_fte"), na.rm = T)))%>%
    mutate( #pivot to get totals and shares
           other_staff_spend_share=round(`Other Staff-NSD_actual_annual_spend`/total_spend*100) ,
           other_staff_fte_share=round(`Other Staff-NSD_annual_fte`/total_fte*100) ,
           pm_spend_share=round(`Program Management-NSD_actual_annual_spend`/total_spend*100),
           pm_fte_share=round(`Program Management-NSD_annual_fte` /total_fte*100) ,
           hcw_clinial_spend_share=round(`HCW: Clinical-SD_actual_annual_spend`/total_spend*100) ,
           hcw_clinical_fte_share=round(`HCW: Clinical-SD_annual_fte`/total_fte*100) ,
           hcw_ancillary_spend_share=round(`HCW: Ancillary-SD_actual_annual_spend`/total_spend*100) ,
           hcw_ancillary_fte_share=round(`HCW: Ancillary-SD_annual_fte`/total_fte*100))%>% 
        
    select(-c(total_spend,total_fte,))%>%
  
  dplyr::relocate(pm_spend_share, .after = `Program Management-NSD_actual_annual_spend`)%>%

dplyr::relocate(pm_fte_share, .after = `Program Management-NSD_annual_fte`)%>%
  dplyr::relocate(other_staff_spend_share, .after = `Other Staff-NSD_actual_annual_spend`)%>%
  dplyr::relocate(other_staff_fte_share, .after = `Other Staff-NSD_annual_fte`)%>%
  dplyr::relocate(hcw_clinial_spend_share, .after = `HCW: Clinical-SD_actual_annual_spend`)%>%
  dplyr::relocate(hcw_clinical_fte_share, .after = `HCW: Clinical-SD_annual_fte`)%>%
  dplyr::relocate(hcw_ancillary_spend_share, .after = `HCW: Ancillary-SD_actual_annual_spend`)%>%
  dplyr::relocate(hcw_ancillary_fte_share, .after = `HCW: Ancillary-SD_annual_fte`)
                

# MER HRH table ==========
df_hrh3<-df_hrh%>%
  filter(fiscal_year=="2022")%>%
  
  dplyr::mutate(interaction_type  = dplyr::case_when(interaction_type    == "Service Delivery"    ~"SD",
                                                     interaction_type    == "Non Service Delivery"    ~"NSD",
                                                     TRUE ~interaction_type))%>%
  mutate(pa_level=glue("{interaction_type}-{program}"))%>%
  group_by(funding_agency,operatingunit,fiscal_year,pa_level)%>%
  summarise_at(vars(actual_annual_spend,annual_fte,), sum, na.rm = TRUE)%>%
  ungroup()%>%

  pivot_longer(actual_annual_spend:annual_fte, names_to="key", values_to="value")%>%
  pivot_wider(names_from = c(pa_level, key), 
              values_from = value,
              values_fill=0
  )%>%
  
  dplyr::rowwise() %>%
  mutate(total_spend=sum(across(matches("annual_spend"), na.rm = T)))%>%
  mutate(total_fte=sum(across(matches("annual_fte"), na.rm = T)))%>%
  mutate( #pivot to get totals and shares
    sd_ct_spend_share=round(`SD-C&T_actual_annual_spend`/total_spend*100) ,
    sd_ct_fte_share=round(`SD-C&T_annual_fte`/total_fte*100),
    nsd_ct_spend_share=round(`NSD-C&T_actual_annual_spend`/total_spend*100) ,
    nsd_ct_fte_share=round(`NSD-C&T_annual_fte`/total_fte*100))%>% 
  
  select(fiscal_year, operatingunit,funding_agency,`SD-C&T_actual_annual_spend`,
         sd_ct_spend_share,`SD-C&T_annual_fte`,sd_ct_fte_share,`NSD-C&T_actual_annual_spend`,
         nsd_ct_spend_share,`NSD-C&T_annual_fte`, nsd_ct_fte_share
         )
  
df_msd<-df_msd%>%
  filter(standardizeddisaggregate=="Total Numerator",
         indicator=="TX_CURR",
         !funding_agency=="DEDUP")%>%
  
  clean_agency()%>%
  mutate( funding_agency = fct_relevel(funding_agency,"USAID","CDC"))%>%
  group_by(fiscal_year,operatingunit,funding_agency,indicator)%>%
  summarise_at(vars(targets,cumulative), sum, na.rm=TRUE)%>%
  ungroup()%>%
  mutate(achievement=round(cumulative/targets*100))%>%
  select(-c(targets,indicator))
 
# final MER and HRH join
df_merhrh<-full_join(df_hrh3,df_msd)%>%
    filter(fiscal_year=="2022",
           operatingunit %in% ou)%>%
    select(-c(fiscal_year,operatingunit))
  

##=== new HRH for 2022
  
library(readr)
df_hrh_new <- read_csv("C:/Users/Bkasdan/Downloads/HRH_FY22mockdataset_10102022.csv")

df<-df_hrh%>%
  
  filter(fiscal_year=="2022")%>%
  filter(!interaction_type=="Non Service Delivery")
# df<-df%>%
#   select(operating_unit,funding_agency_fing,is_community_primarily,annual_fte,actual_annual_spend)
# # 
# df_prime<-df%>%
#   select(operating_unit,funding_agency_fing,prime_or_sub,individual_count,annual_fte,annual_expenditure)%>%
#   group_by(operating_unit,funding_agency_fing,prime_or_sub)%>%
#   summarise_at(vars(individual_count,annual_fte,annual_expenditure), sum, na.rm=TRUE)%>%
#   ungroup()
# 
# df_prime<-df_prime%>%
#    pivot_longer(c(individual_count,annual_fte,annual_expenditure), names_to="key", values_to="value")%>%
#   pivot_wider(names_from = c(prime_or_sub, key), 
#               values_from = value,
#               values_fill=0
#   )%>%
#  
#   # filter(fiscal_year=="2021",
#          # operatingunit %in% ou)%>%
#    # select(-c(fiscal_year,operatingunit))%>%
#   adorn_totals("row",,,, -funding_agency_fing,)%>%
#   dplyr::rowwise() %>%
#   mutate(total_count=sum(across(matches("individual_count"), na.rm = T)))%>%
#   mutate(total_fte=sum(across(matches("annual_fte"), na.rm = T)))%>%
#   mutate(total_spend=sum(across(matches("annual_expenditure"), na.rm = T)))%>%
#   mutate( #pivot to get totals and shares
#     prime_staff_count_share=round(`Prime_individual_count`/total_count*100) ,
#     prime_staff_fte_share=round(`Prime_annual_fte`/total_fte*100) ,
#     sub_spend_share=round(`Sub_individual_count`/total_count*100),
#     sub_fte_share=round(`Sub_annual_fte` /total_fte*100) ,
#   sub_spend_share=round(`Sub_annual_expenditure`/ total_spend*100),
#   prime_spend_share=round(`Prime_annual_expenditure`/ total_spend*100))%>%
#   select(-c(total_fte,total_count,total_spend))
# 
# ## comm type FTE and count 
   
df_com<-df%>%
  select(operating_unit,funding_agency_fing,is_community_primarily,individual_count,annual_fte,actual_annual_spend)%>%
   mutate(actual_annual_spend=as.numeric(actual_annual_spend))%>%
  mutate(annual_fte=as.numeric(annual_fte))%>%
  group_by(operating_unit,funding_agency_fing,is_community_primarily)%>%
  summarise_at(vars(annual_fte,actual_annual_spend), sum, na.rm=TRUE)%>%
  ungroup()%>%
  pivot_longer(c(annual_fte,actual_annual_spend), names_to="key", values_to="value")%>%
  pivot_wider(names_from = c(is_community_primarily, key), 
              values_from = value,
              values_fill=0
  )%>%
  
  # filter(fiscal_year=="2021",
  # operatingunit %in% ou)%>%
  # select(-c(fiscal_year,operatingunit))%>%
  adorn_totals("row",,,, -funding_agency_fing,)%>%
  dplyr::rowwise() %>%
  # mutate(total_count=sum(across(matches("individual_count"), na.rm = T)))%>%
  mutate(total_fte=sum(across(matches("annual_fte"), na.rm = T)))%>%
  mutate(total_spend=sum(across(matches("actual_annual_spend"), na.rm = T)))%>%
  mutate( #pivot to get totals and shares
    # comm_staff_count_share=round(`Community Based_individual_count`/total_count*100) ,
    comm_staff_fte_share=round(Yes_annual_fte/total_fte*100) ,
    # noncomm_spend_share=round(`Non-Community Based_individual_count`/total_count*100),
    noncomm_fte_share=round(No_annual_fte /total_fte*100) ,
    noncomm_spend_share=round(No_actual_annual_spend/ total_spend*100),
    com_spend_share=round(Yes_actual_annual_spend/ total_spend*100))%>%
  dplyr::relocate(total_fte, .after=funding_agency_fing)%>%
  dplyr::relocate(total_spend, .after=noncomm_fte_share)%>%
  dplyr::relocate(comm_staff_fte_share, .after=Yes_annual_fte)%>%
  dplyr::relocate(noncomm_fte_share , .after=No_annual_fte)%>%
  dplyr::relocate(Yes_actual_annual_spend , .after=total_spend)%>%
  dplyr::relocate(com_spend_share, .after=Yes_actual_annual_spend)%>%
  dplyr::relocate(No_actual_annual_spend, .after=com_spend_share)%>%
  dplyr::relocate(noncomm_spend_share , .after=No_actual_annual_spend)
  # select(-c(total_fte,total_spend))
