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

# LOAD DATA ============================================================================  

df_fsd<-si_path()%>%
  return_latest("Fin")%>%
  read_msd()

 
  # Functions  
service_table<-function(df, ou="operatingunit"){
df<-df_fsd%>%
  remove_sch("SGAC")%>%
  prep_fsd()%>%
  agency_category()%>%
  filter(agency_category=="USAID"|agency_category=="CDC")%>%
  filter(!fiscal_year=="2022")%>%
  
  
  dplyr::filter(operatingunit %in% ou)%>%
  
 
  
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
         nsd_share=NSD/total ,
         sd_share = SD / total ,
         pm_share=PM/total )%>%

  pivot_longer(cols = NSD:SD, names_to = "interaction_type")%>%
  group_by(fiscal_year,agency_category,nsd_share, pm_share,sd_share,interaction_type,total)%>%
  summarise(value)%>%
  mutate(cumulative=cumsum(value))%>%
  ungroup%>%
  mutate(share=case_when(interaction_type=="NSD"~ nsd_share,
                         interaction_type=="SD"~sd_share,
                         interaction_type=="PM"~pm_share,
                         TRUE ~as.numeric(interaction_type)))%>%
  select(agency_category,fiscal_year,interaction_type,cumulative,total,share) %>%
  ggplot(aes(fiscal_year,cumulative)) + #can add value to geom_col-use cumulative sum
  geom_col(aes(y = cumulative, fill = fct_rev(interaction_type))) +
  facet_wrap("agency_category", scales='free_x')+
  scale_y_continuous(labels=label_number_si(),
                     position = "left", expand = c(.005, .005)) +
  scale_fill_manual(values = c(genoa,golden_sand,denim )) +
  geom_text(aes(y=cumulative, label = percent(share, 1),family="Source Sans Pro"), position=position_stack(vjust=.8))+
 
  # geom_text(aes(label=paste0(round(sd_share),"%"),vjust = 2, 
  #               family = "Source Sans Pro"))+
  
  scale_alpha_identity() +
  labs(x = "Fiscal Year", y = "Expenditure", fill = "Interaction Type",
       title = glue("Expenditure by Interaction Type (Excluding Commodities): {ou}"),
       caption =  glue("Source: {source}",
                       )) +
  si_style_nolines()+
  theme(legend.position="bottom")



}

 



#