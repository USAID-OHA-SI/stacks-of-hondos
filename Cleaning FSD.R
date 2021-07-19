# PURPOSE: 0.0 Set up 
# AUTHOR: Ben Kasdan | SIEI
# LICENSE: MIT
# DATE: 2021-07-16
# NOTES: 

# LOCALS & SETUP ============================================================================

  # Libraries
    library(glitr)
    library(glamr)
    library(gisr)
    library(Wavelength)
    library(ICPIutilities)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(here)
    library(gt)
    
  #sources
team <- "EA Branch"
agency_order_long <- c("USAID", "CDC", "OTHER")
# Distinct list of OUS to loop over


  
  # Set paths  
    proj_paths
   
    si_paths 
    
  # Functions  
    FSD_Clean<-function(df){
      
      #nested read_msd. Can be removed and run separately
      df<-read_msd(df)
      
      # Drop columns you don't need and rename
      df<-df %>%dplyr::select( - c('prime_partner_duns',	'award_number',	
                                   'subrecipient_duns')) %>% 
        dplyr::rename("Operating Unit"= operatingunit,
                      "Country"= countryname,
                      "Funding Agency"= fundingagency,
                      "Prime Partner Name"= primepartner,
                      
                      "Program Area"= program,
                      "Sub Program Area" = sub_program,
                      
                      "Fiscal Year" = fiscal_year,
                      "COP Budget New Funding"=cop_budget_new_funding,
                      "COP Budget Pipeline"=cop_budget_pipeline,
                      "Total Planned Funding" = cop_budget_total,
                      "Workplan Budget" = workplan_budget_amt,
                      "Expenditure"=expenditure_amt)
      
      #replace NAs with 0s
      df<-df%>%
        mutate_at(vars(`COP Budget New Funding`:`Expenditure`),~replace_na(.,0))
      
      #add in a quarter for the budget data, this is useful for doing quarterly analytics of budget data
      # df$Quarter<-c("Quarter 1")
      
      #convert budget columns to numeric
      df<-df%>%
        dplyr::mutate(`Fiscal Year`= as.character(`Fiscal Year`))%>%
        dplyr::mutate(`COP Budget New Funding`=as.numeric(`COP Budget New Funding`))%>%
        dplyr::mutate(`COP Budget Pipeline`=as.numeric(`COP Budget Pipeline`))%>%
        dplyr::mutate(`Total Planned Funding`=as.numeric(`Total Planned Funding`))%>%
        dplyr::mutate(`Expenditure`=as.numeric(`Expenditure`))
      
      
      
    df<-df%>%  dplyr::mutate(`Agency Category` = `Funding Agency`)%>%
        mutate(`Agency Category` = ifelse(`Agency Category` == "USAID", "USAID",
                                          ifelse(`Agency Category` == "HHS/CDC", "CDC",
                                                 ifelse(`Agency Category` =="Dedup", "Dedup","Other"))))
    return(df)
    }

# LOAD DATA ============================================================================  

  df<-FSD_Clean("Data/Financial_Structured_Datasets_COP17-20_20210618.txt")
  
    #distinct OU list to loop over
    ou_list <- df %>% 
      distinct(operatingunit) %>% 
      pull()

# MUNGE ============================================================================
  
  #  Filter for 2020 and 2021
    fy<-c("2020","2021")
    df<-df%>%
      dplyr::filter(`Fiscal Year` %in% fy)
    
  #filter out for non M&O
    df<-df%>%
      dplyr::filter(record_type =="Implementing Mechanism")
   
    
    
     #testing for FY20
  #  df<-df%>%
   #   dplyr::filter(`Fiscal Year` =="2020")
  #group by OU, Agency, Total Planned funding, Expenditure and pull them out
  df<-df%>%
   select(`Operating Unit`,`Agency Category`,`Fiscal Year`,`Total Planned Funding`, Expenditure)
  #summarize total funding and expenditure
  df<-df%>%
    group_by(`Operating Unit`,`Agency Category`,`Fiscal Year`)%>%
    summarise_at(vars(`Total Planned Funding`:Expenditure), sum, na.rm = TRUE)
  
  #calculate budget execution
 df<-df%>%
   dplyr::mutate("Budget Execution"=`Expenditure` / `Total Planned Funding` )
 
 #df<-df%>%
  # pivot_longer(c(`Total Planned Funding`:`Budget Execution`), names_to = "Stream")
# df<-df%>% 
 #  pivot_wider(names_from = Stream,
  #             values_from = value,values_fill = 0) 

 #table testing 1 option (headers are spending funding and be)
 df<-df%>% 
   pivot_wider(names_from = `Fiscal Year`, values_from = `Total Planned Funding`:`Budget Execution`, values_fill = 0)
 df <- df %>%
   dplyr::relocate(Expenditure_2020, .before = `Total Planned Funding_2020`) %>% 
   dplyr::relocate(`Budget Execution_2020`, .after = `Total Planned Funding_2020`) %>%
   dplyr::relocate(Expenditure_2021, .before = `Total Planned Funding_2021`)
 df<-df%>%
   dplyr::rename("FY20 Spend"=Expenditure_2020,
                 "FY20 Budget"=`Total Planned Funding_2020`,
                 "FY21 Spend"=Expenditure_2021,
                 "FY21 Budget"=`Total Planned Funding_2021`,
                 "FY20 Budget Execution"=`Budget Execution_2020`,
                 "FY21 Budget Execution"=`Budget Execution_2021`)%>%
   mutate(`Agency Category` = fct_relevel(`Agency Category`, "CDC",
                                  "Other",
                                  "USAID",
                            )) %>% 
   arrange(`Agency Category`)
 
 df<-df%>%
  dplyr::mutate( `Agency Category` = fct_relevel(`Agency Category`, agency_order_long))
 

 #notes below ignore
 
 dfUSAID<-df%>%
   dplyr::filter(`Agency Category`=="USAID")
 
 #re-order table
 dfUSAID <- dfUSAID %>%
   dplyr::relocate(Expenditure_2020, .before = `Total Planned Funding_2020`) %>% 
   dplyr::relocate(`Budget Execution_2020`, .after = `Total Planned Funding_2020`) %>%
   dplyr::relocate(Expenditure_2021, .before = `Total Planned Funding_2021`)
  
 
 

 
  
# VIZ ============================================================================

  # use gt to build table at OU level
 df%>%
   
   gt(groupname_col = "Agency Category",
 )%>%
   cols_hide(
     columns = c(
       "Operating Unit"
     ))%>%
 
 
   fmt_percent(
     columns = vars(`FY20 Budget Execution`, `FY21 Budget Execution`),
     decimals = 0)%>%
   fmt_currency( # add dolar signs
     columns = vars(`FY20 Spend`,`FY20 Budget`,`FY21 Spend`,`FY21 Budget`),
     decimals = 0,
     currency = "USD")%>%
   tab_options(
     table.font.names = "Source Sans Pro"
   ) %>% 
   cols_width(
     everything() ~ px(110))%>%
   tab_header(title = "PEPFAR Budget Execution FY2020-2021") %>% 
   tab_source_note(
     source_note = paste("Produced on ",Sys.Date(), "by the ", team, " using PEPFAR FY21Q2c FSD released on 2021-06-18.")
   )%>% 
   tab_source_note(
     source_note = md("OTHER AGENCIES based on aggregates excluding de-duplication.")
   )%>%
   tab_style(
     style = cell_borders(
       sides = "right",
       weight = px(1.5),
     ),
     locations = cells_body(
       columns = everything(),
       rows = everything()
     ))%>%
   tab_spanner(
     label = "COP20 Performance",
     columns = c( "FY20 Budget", "FY20 Spend","FY20 Budget Execution"))%>%
   tab_spanner(
     label = "COP21 Performance",
     columns = c( "FY21 Budget", "FY21 Spend","FY21 Budget Execution"))%>%
   tab_style(style = cell_fill(color = "#5bb5d5",alpha = .75),      
                    locations = cells_body(               
                      columns = vars(`FY20 Budget Execution`),
 rows = `FY20 Budget Execution` >= 0.9 & `FY20 Budget Execution` < 1.1)) %>%
 tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),      
           locations = cells_body(               
             columns = vars(`FY20 Budget Execution`),
             rows = `FY20 Budget Execution` < 0.9 ))%>%
   tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),      
             locations = cells_body(               
               columns = vars(`FY20 Budget Execution`),
               rows = `FY20 Budget Execution` >= 1.1 & `FY20 Budget Execution` < 1.2))%>%
   tab_style(style = cell_fill(color = "#ff989f",alpha = .75),      
             locations = cells_body(               
               columns = vars(`FY20 Budget Execution`),
               rows = `FY20 Budget Execution` >= 1.2 ))
   
  
 

 #just usaid
 tbl<-
   dfUSAID%>%gt(groupname_col = "Agency category",
              )%>%
   cols_hide(
     columns = c(
       "Agency Category"
     )
   )%>%
   fmt_percent(
     columns = vars(`FY20 Budget Execution`, `FY21 Budget Execution`),
     decimals = 0)%>%
   fmt_currency( # add dolar signs
     columns = vars(`FY20 Spend`,`FY20 Budget`,`FY21 Spend`,`FY21 Budget`),
     decimals = 0,
     currency = "USD")%>%
   tab_options(
     table.font.names = "Source Sans Pro"
   ) %>% 
   cols_width(
     everything() ~ px(110))%>%
   tab_header(title = "USAID Budget Execution FY2020") %>% 
   tab_source_note("Source: USAID Phoenix Financial System May 2021")%>%
   

   tab_style(
     style = cell_borders(
       sides = "right",
       weight = px(1.5),
     ),
     locations = cells_body(
       columns = everything(),
       rows = everything()
     ))
# SPINDOWN ============================================================================

gtsave(tbl,"USAID_global.png")
 