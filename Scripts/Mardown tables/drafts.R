# PURPOSE: Tables for RMD
# AUTHOR: Ben Kasdan | SIEI
# LICENSE: MIT
# DATE: 2021-12-20
# NOTES: 

# LOCALS & SETUP ============================================================================

  # Libraries
    
    library(glamr)
    library(tidyverse)
    library(gophr)
library(scales)
library(glitr)
library(glue)

    
source(("~/GitHub/stacks-of-hondos/scripts/prep_fsd.R"))
source(("~/GitHub/stacks-of-hondos/scripts/utilities.R"))
 
  # Functions  
service<-function(df, ou="operatingunit"){
df<-df_fsd%>%
  remove_sch("SGAC")%>%
  prep_fsd()%>%
  agency_category()%>%
  filter(!fiscal_year=="2022")%>%
  
  
  #dplyr::filter(operatingunit %in% ou)%>%
  
 
  
  #select specific variables
  dplyr::select (c(agency_category,interaction_type, fiscal_year,expenditure_amt))%>%
  mutate_at(vars(expenditure_amt),~replace_na(.,0))%>%
  mutate( agency_category = fct_relevel(agency_category, "USAID","CDC"))%>%
  group_by(agency_category,interaction_type,fiscal_year)%>% 
  summarise(across(expenditure_amt, sum, na.rm = TRUE)) %>% 
  ungroup() 
 
  df<-df%>%
  pivot_wider(names_from = interaction_type, values_from = expenditure_amt) 
df<-df%>% 
  mutate(total = NSD+SD+PM, #pivot to get totals and shares
         nsd_share=NSD/total*100,
         sd_share = SD / total*100,
         pm_share=PM/total*100)

df2<-df%>%
  pivot_longer(cols = NSD:SD, names_to = "interaction_type")%>%
  pivot_longer(cols=nsd_share:pm_share, names_to="perct", values_to="values")
df<-df%>%
  select(agency_category,fiscal_year,total,nsd_share:pm_share)%>%
  pivot_longer(cols = nsd_share:pm_share, names_to = "interaction_type")
  #filter(interaction_type=="total")
  
  df1<-df%>%
 # mutate(fiscal_year=as.character(fiscal_year))%>%
  filter(!interaction_type=="total",
         !fiscal_year=="2022")
  
library(glitr)
  library(scales)
df2%>%ggplot(aes(fiscal_year, value)) +
  geom_col(aes( fill = interaction_type))+
  facet_wrap("agency_category", scales='free_x')+
  scale_y_continuous(labels=label_number_si(),
                     position = "left", expand = c(.005, .005)) +
  scale_fill_manual(values = c(denim ,golden_sand, genoa)) +
  geom_text(aes(label = paste0(round(value),"%")), position = position_stack(vjust = 0.5),family="Source Sans Pro")
  # geom_text(aes(label=paste0(round(sd_share),"%"),vjust = 2, 
  #               family = "Source Sans Pro"))+
  
  scale_alpha_identity() +
  labs(x = "Fiscal Year", y = "Expenditure", fill = "Interaction Type",
       title = glue("Expenditure by Interaction Type: ou"),
       caption =  glue("Source: {source}-Excluding M&O and Commodities",
                       )) +
  si_style_nolines()+
  theme(legend.position="bottom")



}

 

# LOAD DATA ============================================================================  

df_fsd<-si_path()%>%
  return_latest("Fin")%>%
  read_msd()


# MUNGE ============================================================================
  
  #   summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
  dplyr::mutate(budget_execution=percent_clean(expenditure_amt,cop_budget_total))%>%
    ungroup()%>%
    
    
    pivot_wider(names_from = fiscal_year,values_from = cop_budget_total:budget_execution, values_fill = 0)%>%
    dplyr::relocate(expenditure_amt_2020, .before = cop_budget_total_2020) %>%
    dplyr::relocate(expenditure_amt_2021, .before = cop_budget_total_2021) %>%
    dplyr::relocate(budget_execution_2021, .after = cop_budget_total_2021)%>%
    dplyr::relocate(budget_execution_2020, .after = cop_budget_total_2020)
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

