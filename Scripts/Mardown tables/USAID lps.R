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


#This function can be used to print out budget execution by partner type (local, international)for USAID at an OU level. You can use this to 
#print information for multiple OUs. You will need to ensure that you have load_secrets from the glamr package set up beforehand
#Be sure to load the following source files below before running
source("~/GitHub/stacks-of-hondos/utilities.R")

#ensure glamr load_secrets is loaded to get partner type data
glamr::load_secrets()
#apply partner type data to fsd before starting
df_fsd<-df_fsd%>%
  glamr::apply_partner_type()



  
  df<-df%>%
    glamr::remove_mo()%>%
    glamr::remove_sch("SGAC")%>%
    dplyr::filter( fiscal_year=="2021")%>%
    dplyr::filter(fundingagency=="USAID")%>%
    dplyr::filter(operatingunit %in% "Angola")%>%
    dplyr::filter(partner_type_usaid_adjusted=="Local" )%>%
    mutate(mech=glue("{mech_code}-{mech_name}"))%>%
    dplyr::select (c(operatingunit,partner_type_usaid_adjusted,mech,program,fiscal_year,cop_budget_total,expenditure_amt))%>%
    group_by(operatingunit,partner_type_usaid_adjusted,mech,program,fiscal_year)%>%
    summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
    
    dplyr::mutate(budget_execution=round(expenditure_amt/cop_budget_total*100))%>%
    ungroup()%>%
    pivot_wider(names_from = fiscal_year,values_from = cop_budget_total:budget_execution, values_fill = 0)%>%
   
    #dplyr::relocate(cop_budget_total_2020, .before = cop_budget_total_2021)%>%
    #dplyr::relocate(expenditure_amt_2020, .before = cop_budget_total_2020) %>%
    dplyr::relocate(expenditure_amt_2021, .before = cop_budget_total_2021) %>%
    dplyr::relocate(budget_execution_2021, .after = cop_budget_total_2021)%>%
    #dplyr::relocate(budget_execution_2020, .after = cop_budget_total_2020)%>%
    filter(cop_budget_total_2021>0 | expenditure_amt_2021>0)%>%
    dplyr::select (!c(operatingunit,partner_type_usaid_adjusted))
  
  
  # nice chart ====
  
  df<-df_fsd%>%
    glamr::remove_mo()%>%
    glamr::remove_sch("SGAC")%>%
    dplyr::filter(fundingagency=="USAID")%>%
    #dplyr::filter(operatingunit %in% ou )%>%
    dplyr::filter(partner_type_usaid_adjusted=="Local" | partner_type_usaid_adjusted=="International")%>%
  dplyr::select (c(partner_type_usaid_adjusted, fiscal_year,expenditure_amt))%>%
    filter(!fiscal_year=="2022")%>%
    group_by(partner_type_usaid_adjusted,fiscal_year)%>%
    summarise_at(vars(expenditure_amt), sum, na.rm = TRUE)%>%
    pivot_wider(names_from = partner_type_usaid_adjusted, values_from = expenditure_amt) 
  df<-df%>% 
    mutate(total = Local+International, #pivot to get totals and shares
           lp_share = Local / total*100,
           ip_share=International/total*100,
           )
  df<-df%>%
    pivot_longer(cols = International:Local, names_to = "partner_type")
  #filter(interaction_type=="total")
  
 
  
  library(glitr)
  library(scales)
  df%>%ggplot(aes(fiscal_year, value,)) +
    geom_col(aes( fill = partner_type))+
     scale_y_continuous(labels=label_number_si(),
                        position = "left", expand = c(.005, .005)) +
    # geom_label(aes(label = ifelse(partner_type == 'Local', NA, lp_share)))
    # geom_text(aes(label = percent(value, 1), vjust = -1, 
    #               size = 12/.pt, family = "Source Sans Pro"))+
    geom_text(aes(label=ifelse(partner_type=="Local",paste0(round(lp_share),"%")," "),vjust = 2, 
                                 family = "Source Sans Pro"))+
    
    scale_fill_manual(values = c(siei_dgrey ,old_rose)) +
    scale_alpha_identity() +
    labs(x = "Fiscal Year", y = "Expenditure", fill = "Partner Type",
         title = glue("Expenditure by Partner Type: ou"),
         caption =  glue("Source: {source}-Excluding M&O and Commodities",
         )) +
    si_style_nolines()+
    theme(legend.position="bottom")
   
  

  
    
   