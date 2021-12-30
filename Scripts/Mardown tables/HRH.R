library(glamr)
library(tidyverse)
library(gophr)
library(glue)



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

df_hrh1<-df_hrh%>%
  group_by(operatingunit,fundingagency,fiscal_year,)%>%
  summarise_at(vars(annual_fte,individual_count,actual_annual_spend), sum, na.rm = TRUE)%>%
  ungroup()

#Munge FSD===
df_fsd1<-df_fsd%>%
  clean_agency()%>%
  group_by(operatingunit,fundingagency,fiscal_year, )%>%
  summarise_at(vars(expenditure_amt), sum, na.rm = TRUE)%>%
  ungroup()

# first HRH Table=====
df_hrh_fsd1<-left_join(df_hrh1,df_fsd1)%>%
  filter(operatingunit %in% ou)%>%
  mutate(hrh_share=round(actual_annual_spend/expenditure_amt *100))%>%
  filter(fiscal_year=="2021")%>%
  select(-c(fiscal_year,operatingunit))


# staff breakdown table====
df_hrh2<-df_hrh%>%
   
    dplyr::mutate(interaction_type  = dplyr::case_when(interaction_type    == "Service Delivery"    ~"SD",
                                                       interaction_type    == "Non Service Delivery"    ~"NSD",
                                                       TRUE ~interaction_type))%>%
    mutate(er_staff=glue("{er_category}-{interaction_type}"))%>%
    group_by(operatingunit,fundingagency,fiscal_year,er_staff,)%>%
    summarise_at(vars(actual_annual_spend), sum, na.rm = TRUE)%>%
   ungroup%>%
    pivot_wider(
                names_from="er_staff",
                values_from="actual_annual_spend",values_fill=0)%>%
    mutate(total = `Other Staff-NSD`+`Program Management-NSD`+`HCW: Clinical-SD`+`HCW: Ancillary-SD`, #pivot to get totals and shares
           other_staff_share=round(`Other Staff-NSD`/total*100) ,
           pm_share=round(`Program Management-NSD`/total*100),
           hcw_clinical_share=round(`HCW: Clinical-SD`/total*100),
           hcw_ancillary_share=round(`HCW: Ancillary-SD`/total*100))%>%

    filter(fiscal_year=="2021",
           operatingunit %in% ou)%>%
    select(-c(total,fiscal_year,operatingunit))%>%
    dplyr::relocate(pm_share, .after = `Program Management-NSD`) %>%
    dplyr::relocate(other_staff_share, .after = `Other Staff-NSD`) %>%
    dplyr::relocate( hcw_ancillary_share, .after = `HCW: Ancillary-SD`) %>%
    dplyr::relocate( hcw_clinical_share, .after = `HCW: Clinical-SD`) 

# MER HRH table (drafty)
df_hrh3<-df_hrh%>%
  
  dplyr::mutate(interaction_type  = dplyr::case_when(interaction_type    == "Service Delivery"    ~"SD",
                                                     interaction_type    == "Non Service Delivery"    ~"NSD",
                                                     TRUE ~interaction_type))%>%
  mutate(pa_level=glue("{interaction_type}-{program}"))%>%
  group_by(operatingunit,fundingagency,fiscal_year,pa_level)%>%
  summarise_at(vars(actual_annual_spend), sum, na.rm = TRUE)%>%
  ungroup%>%
  pivot_wider(
    names_from="pa_level",
    values_from="actual_annual_spend",values_fill=0)%>%
  mutate(total=(rowSums(across(`NSD-ASP`:`NSD-SE`))))%>%
  mutate(sd_ct_share=(`SD-C&T`/total*100))%>%
  
  
  filter(fiscal_year=="2021",
         operatingunit %in% ou)%>%
  select(fundingagency,`SD-C&T`,sd_ct_share)