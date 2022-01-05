library(glamr)
library(tidyverse)
library(gophr)
library(glue)
library(janitor)



df_hrh<-si_path()%>%
  return_latest("HRH")%>%
  gophr::read_msd()

df_fsd<-si_path()%>%
  return_latest("Financial")%>%
  gophr::read_msd()
df_msd<-si_path()%>%
  return_latest("OU_IM")%>%
  gophr::read_msd()

# Munge HRH========
df_hrh<-df_hrh %>%
  
  rename(operatingunit=operating_unit,
        fundingagency=funding_agency )%>%
  clean_agency()%>%
  mutate( fundingagency = fct_relevel(fundingagency,"USAID","CDC"))%>%
  mutate(annual_fte=as.numeric(annual_fte),
         individual_count=as.numeric(individual_count),
         annual_expenditure=as.numeric(annual_expenditure),
         actual_annual_spend=as.numeric(actual_annual_spend),
         annual_fringe=as.numeric(annual_fringe),)%>%
  group_by(operatingunit,fundingagency,fiscal_year, mech_code, mech_name,interaction_type, program ,er_category)%>%
  summarise_at(vars(annual_fte,individual_count,annual_expenditure,actual_annual_spend, annual_fringe), sum, na.rm = TRUE)%>%
  ungroup()


#Munge FSD===
df_fsd<-df_fsd%>%
  clean_agency()%>%
  group_by(operatingunit,fundingagency,fiscal_year, )%>%
  summarise_at(vars(expenditure_amt), sum, na.rm = TRUE)%>%
  ungroup()

# first HRH Table=====
df_hrh1<-df_hrh%>%
  group_by(operatingunit,fundingagency,fiscal_year,)%>%
  summarise_at(vars(annual_fte,individual_count,actual_annual_spend), sum, na.rm = TRUE)%>%
  ungroup()

df_hrh_fsd1<-left_join(df_hrh1,df_fsd)%>%
  filter(operatingunit %in% ou)%>%
  filter(fiscal_year=="2021")%>%
  select(-c(fiscal_year,operatingunit))%>%
  adorn_totals("row",,,, -fundingagency)%>%
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
  filter(fiscal_year=="2021",
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
  
  dplyr::mutate(interaction_type  = dplyr::case_when(interaction_type    == "Service Delivery"    ~"SD",
                                                     interaction_type    == "Non Service Delivery"    ~"NSD",
                                                     TRUE ~interaction_type))%>%
  mutate(pa_level=glue("{interaction_type}-{program}"))%>%
  group_by(fundingagency,operatingunit,fiscal_year,pa_level)%>%
  summarise_at(vars(actual_annual_spend,annual_fte,), sum, na.rm = TRUE)%>%
  ungroup%>%

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
  
  select(fiscal_year, operatingunit,fundingagency,`SD-C&T_actual_annual_spend`,
         sd_ct_spend_share,`SD-C&T_annual_fte`,sd_ct_fte_share,`NSD-C&T_actual_annual_spend`,
         nsd_ct_spend_share,`NSD-C&T_annual_fte`, nsd_ct_fte_share
         )
  
df_msd<-df_msd%>%
  filter(standardizeddisaggregate=="Total Numerator",
         indicator=="TX_CURR",
         !fundingagency=="DEDUP")%>%
  
  glamr::clean_agency()%>%
  mutate( fundingagency = fct_relevel(fundingagency,"USAID","CDC"))%>%
  group_by(fiscal_year,operatingunit,fundingagency,indicator)%>%
  summarise_at(vars(targets,cumulative), sum, na.rm=TRUE)%>%
  ungroup()%>%
  mutate(achievement=round(cumulative/targets*100))%>%
  select(-c(targets,indicator))
 
# final MER and HRH join
df_merhrh<-full_join(df_hrh3,df_msd)%>%
    filter(fiscal_year=="2021",
           operatingunit %in% ou)%>%
    select(-c(fiscal_year,operatingunit))
  
  
 
  
  