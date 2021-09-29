library(dplyr)
library(devtools)
library(tidyverse)
library(tidyr)
library(here)
library(ICPIutilities)
library(data.table)
library(gt)
library(glue)
library(glitr)
library(glamr)
library(gisr)
library(gophr)
library(scales)
library(sf)
library(devtools)
##error library(extrafont)
##library(tidytext)


#Use "here" function to find folder
#need to sub "here for si_path*********
here()
here("Raw Datasets")

df_fsd<-read.delim(here("Raw Datasets/Finanical_Structured_Dataset_COP17-20_20210813.txt")) 


#prep and cleaning fsd
#issues with source note at the end of the function
prep_fsd <-function(df){ 
  
  #removing
  df<-df_fsd %>% dplyr::select(-c("prime_partner_duns","prime_partner_org_type",
                                  "is_indigenous_prime_partner", "subrecipient_duns",
                                  "award_number","procurement_type")) %>% 
    dplyr::rename("fiscal_year"= implementation_year) %>% 
    dplyr::filter(fiscal_year == c(2020 ,2021))
  
  #filter out for non M&O
  df<-df%>% remove_mo()
  
  ##concatenate mech id and mech name
  df <-df %>% dplyr::mutate( mech_id_mech_name = paste(mech_code,"-", mech_name))
  
  #mutate data type double into integer to have round numbers
  df<-df %>% dplyr::mutate_if(is.double, as.integer)
  #mutate data type for mechanism id and fiscal year
  df<-df %>% dplyr::mutate_at(vars(mech_code, fiscal_year),list(as.character))
  
  #drop NA for numeric amounts
  df<-df%>%
    mutate_at(vars(cop_budget_new_funding:expenditure_amt),~replace_na(.,0))
  
  
  #recode values to match naming in Financial Integrated Dataset
  df<-df %>% dplyr::mutate(`interaction_type`= recode (`interaction_type`, "Service Delivery"= "SD",
                                                       "Non Service Delivery"= "NSD"))
  
  #Add in agency category column to group agencies
  df<-df%>%  dplyr::mutate(`agency_category` = `fundingagency`)%>%
    mutate(`agency_category` = ifelse(`agency_category` == "USAID", "USAID",
                                      ifelse(`agency_category` == "HHS/CDC", "CDC",
                                             ifelse(`agency_category` =="Dedup", "Dedup","Other"))))
 
  #mutating & calculating budget execution
  df<-df%>% group_by(operatingunit, country, fundingagency, fiscal_year,program) %>% 
    summarise_at(vars(cop_budget_total, expenditure_amt), sum, na.rm = TRUE) %>% 
  ungroup()
  
  df<-df%>%
    dplyr::mutate("budget_execution"= expenditure_amt / cop_budget_total )
  
  df<-df%>%
    dplyr::mutate(`budget_execution`= as.numeric(`budget_execution`))
  
  df <-df %>% pivot_wider(names_from = fiscal_year ,values_from = cop_budget_total:budget_execution, values_fill = 0)%>%
    #dplyr::relocate(expenditure_amt_2018, .before = cop_budget_total_2018) %>%
    #dplyr::relocate(expenditure_amt_2019, .before = cop_budget_total_2019) %>%
    #dplyr::relocate(budget_execution_2018, .after = cop_budget_total_2018) %>%
    #dplyr::relocate(budget_execution_2019, .after = cop_budget_total_2019) %>% 
    dplyr::relocate(expenditure_amt_2020, .before = cop_budget_total_2020) %>%
    dplyr::relocate(expenditure_amt_2021, .before = cop_budget_total_2021) %>%
    dplyr::relocate(budget_execution_2021, .after = cop_budget_total_2021) %>%
    dplyr::relocate(budget_execution_2020, .after = cop_budget_total_2020) %>% 


  #GT Section================================================================================
    gt (df) %>% 
    fmt_percent(columns = tidyselect::contains("_execution_"),
      decimals = 0)%>%
    
    fmt_currency(columns = tidyselect::contains("_budget_total"),
      decimals = 0,
      currency = "USD")%>%
    
    fmt_currency(columns = tidyselect::contains("expenditure_amt"),
      decimals = 0,
      currency = "USD")%>%
    
    tab_options(table.font.names = "Source Sans Pro") %>% 
    
    cols_width(everything() ~ px(90))%>%
    
    tab_style(style = cell_borders(
        sides = "right",
        weight = px(1.5),
      ),
      locations = cells_body(
        columns = everything(),
        rows = everything()
      ))%>%
    
    tab_style(style = cell_text(weight = 700),
      locations = cells_body(
        columns = tidyselect::contains("_execution_")))%>%
    
    tab_spanner(label = "COP21 Performance",
      columns = c(
        expenditure_amt_2021,cop_budget_total_2021, budget_execution_2021,))%>%
    
    tab_spanner(label = "COP20 Performance",
      columns = c(
        expenditure_amt_2020,cop_budget_total_2020,budget_execution_2020))%>%
    
    gt::tab_style(
      style = list(
        gt::cell_text(weight = "bold")), 
      locations = gt::cells_column_spanners(spanners = tidyselect::everything()))%>%
    
    cols_align(align = "center",
      columns = everything())%>%
    
    cols_label( #update GT to check on tidy select), also look at clean_names, also potentially case_when
      expenditure_amt_2020 = "Expenditure",
      cop_budget_total_2020 = "Budget",
      budget_execution_2020="Budget Execution",
      expenditure_amt_2021 = "Expenditure",
      cop_budget_total_2021 = "Budget",
      budget_execution_2021="Budget Execution")%>%
    
    cols_align(
      align = "left",
      columns = tidyselect::contains("agency"))%>%
    
    tab_style(style = cell_fill(color = "#5bb5d5",alpha = .75),      
              locations = cells_body(               
                columns = (budget_execution_2020),
                rows = (budget_execution_2020) >= 0.9 & (budget_execution_2020) < 1.1)) %>%
    
    tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),      
              locations = cells_body(               
                columns = (budget_execution_2020),
                rows =(budget_execution_2020) < 0.9 ))%>%
    
    tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),      
              locations = cells_body(               
                columns = (budget_execution_2020),
                rows = (budget_execution_2020)>= 1.1 & (budget_execution_2020) < 1.2))%>%
    
    tab_style(style = cell_fill(color = "#ff989f",alpha = .75),      
              locations = cells_body(               
                columns = (budget_execution_2020),
                rows = (budget_execution_2020) >= 1.2 ))%>%
    
    tab_style(style = cell_fill(color = "#5bb5d5",alpha = .75),      
              locations = cells_body(               
                columns = (budget_execution_2021),
                rows = (budget_execution_2021) >= 0.9 & (budget_execution_2021) < 1.1)) %>%
    
    tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),      
              locations = cells_body(               
                columns = (budget_execution_2021),
                rows =(budget_execution_2021) < 0.9 ))%>%
    
    tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),      
              locations = cells_body(               
                columns = (budget_execution_2021),
                rows = (budget_execution_2021)>= 1.1 & (budget_execution_2021) < 1.2))%>%
    
    tab_style(style = cell_fill(color = "#ff989f",alpha = .75),      
              locations = cells_body(               
                columns = (budget_execution_2021),
                rows = (budget_execution_2021) >= 1.2 ))%>%
    
    gt::tab_options(
      source_notes.font.size = 8,
      table.font.size = 13, 
      data_row.padding = gt::px(5),
      source_notes.padding = gt::px(1),) %>%
    
    tab_footnote(
      footnote = "Excluding M&O",
      locations = cells_column_labels(
        columns =c(expenditure_amt_2020, expenditure_amt_2021)))#%>%
    #gt::tab_source_note(
     # source_note = gt::md(glue::glue("**Source**: {source} | Please reach out to oha.ea@usaid.gov for questions")))
  
  return(df)
}

#apply function to df_fsd
prep_fsd(df_fsd)



#Global table-OU agnostic, agency global comparison FY20-FY21-Ben=====================================


#Global table-USAID only by OU for FY20-FY21-Ben======================================================


#OU version of the global agency comparison for FY20 and FY21-Ben=====================================


#Global Program Area spend by USAID for FY20-FY21-Jairo===============================================


#OU Partner/Mechanism: Table-FY21 topline spend by partner for all agencies  - Jairo==================


#OU Partner/mechanism table FY21 spend by PA for only USAID Mechanisms - Jairo========================


#testing for export
name_____(___name___)%>%
  gtsave(.,path=table_out,filename="___name____.png")


#Ben exporting additional codes from "get_ou_agency_be.R"
table_out<-"GitHub/stacks-of-hondos/Images"
#to run for one OU testing below
get_ou_agency_be(df_fsd, "South Africa")%>%
  gtsave("test.png")
#to run for all (giving me)
purrr::map(ou_list, ~get_ou_agency_be(df_fsd, ou = .x)%>%
             gtsave(.,path=table_out,filename = glue::glue("{.x}_ou_budget_execution.png")))

glamr::export_drivefile()

