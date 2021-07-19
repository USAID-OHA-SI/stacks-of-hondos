snippet # PURPOSE: Program Area Table
# AUTHOR: Ben Kasdan | SIEI
# LICENSE: MIT
# DATE: 2021-07-19
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
    
    
  
  # Set paths  
    proj_paths
   
    si_paths 
    
  # Functions  
  
  # Saved pieces
    team <- "EA Branch"
    agency_order_long <- c("USAID", "CDC", "OTHER")

# LOAD DATA ============================================================================  

     source("CleaningFSD.R")

# MUNGE ============================================================================
    #Filter by Program Areas
    #group by OU, Agency, Total Planned funding, Expenditure and pull them out
    
  
    
     df1<-df%>%
    select(`Operating Unit`,`Agency Category`,`Program Area`,`Fiscal Year`,`Total Planned Funding`, Expenditure)
    #summarize total funding and expenditure
    df1<-df1%>%
      group_by(`Operating Unit`,`Agency Category`,`Program Area`,`Fiscal Year`)%>%
      summarise_at(vars(`Total Planned Funding`:`Expenditure`), sum, na.rm = TRUE)
    
    #calculate budget execution
    df1<-df1%>%
      dplyr::mutate("Budget Execution"=`Expenditure` / `Total Planned Funding` )
    
    #df<-df%>%
    # pivot_longer(c(`Total Planned Funding`:`Budget Execution`), names_to = "Stream")
    # df<-df%>% 
    #  pivot_wider(names_from = Stream,
    #             values_from = value,values_fill = 0) 
    
    #table testing 1 option (headers are spending funding and be)
    df1<-df1%>% 
      pivot_wider(names_from = `Fiscal Year`, values_from = `Total Planned Funding`:`Budget Execution`, values_fill = 0)
    df1 <- df1 %>%
      dplyr::relocate(Expenditure_2020, .before = `Total Planned Funding_2020`) %>% 
      dplyr::relocate(`Budget Execution_2020`, .after = `Total Planned Funding_2020`) %>%
      dplyr::relocate(Expenditure_2021, .before = `Total Planned Funding_2021`)
    df1<-df1%>%
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
    #test 1 OU
    df1<-df%>%
      dplyr::filter(`Operating Unit`=="Mozambique")
  
# VIZ ============================================================================

     df1%>%gt(groupname_col = "Agency Category",
    )%>%
  cols_hide(
    columns = c(
      "Operating Unit"
    ))%>%
  
  
  fmt_percent(
    columns = vars(`FY20 Budget Execution`, `FY21 Budget Execution`),
    decimals = 0)%>%
  fmt_currency( # add dolar signs
    columns = vars(`FY20 Spend`,`FY20 Budget`,`FY21 Spend`,`FY21 Budget`), #columns = contains("FY20") as alternative
    decimals = 0,
    currency = "USD")%>%
  tab_options(
    table.font.names = "Source Sans Pro"
  ) %>% 
  cols_width(
    everything() ~ px(110))%>%
  tab_header(title = "PEPFAR Budget Execution Mozambique FY2020-2021") %>% 
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

# SPINDOWN ============================================================================

