# PURPOSE: OU level tables
# AUTHOR: Ben Kasdan | SIEI
# LICENSE: MIT
# DATE: 2021-09-01
# NOTES: 

# LOCALS & SETUP ============================================================================

  # Libraries
    library(glitr)
    library(glamr)
    library(gisr)
    library(tidyverse)
    library(gophr)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(here)
    library(gophr)
    library(gt)
authors_footnote <- function(msd_source){glue::glue("Created by Core Analytics Cluster on {Sys.Date()} ")}  
agency_order_shrt <- c("USAID", "CDC", "OTHER")
  # Set paths  
df_fsd<-si_path()%>%
  return_latest("COP17")%>%
  gophr::read_msd()

fiscal_years<-c("2020","2021")
    
  # Functions
  clean_ou<-function(df,ou="countryname",fiscal_years=c("2020,2021"), agency=c("USAID","Other","CDC")){
    
    df<-df_fsd%>%
      remove_mo()%>%
    dplyr::select (c(countryname,fundingagency,fiscal_year,cop_budget_total,expenditure_amt))%>%
  #  dplyr::filter(fiscal_year %in% fiscal_years)%>%
     
      #dplyr::filter(countryname %in% ou)%>%
    mutate_at(vars(cop_budget_total,expenditure_amt),~replace_na(.,0))%>%
      dplyr::mutate(agency_category=fundingagency)%>%
      mutate(agency_category  = ifelse(agency_category == "USAID", "USAID",
                                       ifelse(agency_category  == "HHS/CDC", "CDC",
                                              ifelse(agency_category  =="Dedup", "Dedup","Other"))))%>%
     mutate( agency_category = fct_relevel(agency_category, "USAID","CDC","Other"))%>%
    group_by(countryname,agency_category,fiscal_year)%>%
    summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
      dplyr::mutate(budget_execution=expenditure_amt/cop_budget_total)%>%
    ungroup()%>%
     
      dplyr::filter(countryname %in% ou)%>%
      dplyr::filter(fiscal_year %in% fiscal_years)%>%
     # df<-df%>%
      #pivot_wider(names_from = fiscal_year,values_from = cop_budget_total:budget_execution, values_fill = 0)%>%
       # dplyr::relocate(expenditure_amt_2020, .before = cop_budget_total_2020) %>%
        #dplyr::relocate(expenditure_amt_2021, .before = cop_budget_total_2021) %>%
      #  dplyr::relocate(budget_execution_2021, .after = cop_budget_total_2021)%>%
       # dplyr::relocate(budget_execution_2020, .after = cop_budget_total_2020) 
    
       
       df%>% 
        gt(groupname_col = "Agency category",
        )%>%
        cols_hide(
          columns = c(
             "countryname"
          ))%>%
        fmt_percent(
          columns = c(`budget_execution`),
          decimals = 0)%>%
        fmt_currency( # add dolar signs
          columns = c(`cop_budget_total`,expenditure_amt),
          decimals = 0,
          currency = "USD")%>%
        tab_options(
          table.font.names = "Source Sans Pro"
        ) %>% 
        cols_width(
          everything() ~ px(140))%>%
        cols_label(
          agency_category = "Agency",
          expenditure_amt = "Expenditure",
          cop_budget_total = "Budget",
          budget_execution="Budget Execution"
        )%>%
  tab_style(style = cell_fill(color = "#5bb5d5",alpha = .75),      
            locations = cells_body(               
              columns = vars(budget_execution),
              rows = budget_execution >= 0.9 & budget_execution < 1.1)) %>%
  tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),      
            locations = cells_body(               
              columns = vars(budget_execution),
              rows =budget_execution < 0.9 ))%>%
  tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),      
            locations = cells_body(               
              columns = vars(budget_execution),
              rows = budget_execution>= 1.1 & budget_execution < 1.2))%>%
  tab_style(style = cell_fill(color = "#ff989f",alpha = .75),      
            locations = cells_body(               
              columns = vars(budget_execution),
              rows = budget_execution >= 1.2 ))%>%
        tab_header(
          title = glue::glue(" COP{fiscal_years} {ou} Financial Performance Summary"))%>%
        gt::tab_source_note("Create by EA Branch Excluding M&O")
      return(df)
  }
      
   
        
    #dplyr::select(-(fiscal_year))
    
    #df%>%
    #  gt(groupname_col = "countryname",
     #    rowname_col = "agency_category")
                  
                # df%>%
                 #   gt(groupname_col = "countryname",
                 #      rowname_col = "agency_category")

    
    
   
clean_ou(df_fsd,"South Africa","2020")   
tanzania<-get_ou(df_fsd, "Tanzania","2020")



get_ou<-function(df,ou="countryname",fiscal_years){
df<-df%>%
  dplyr::filter(countryname %in% ou)%>%
  dplyr::filter(fiscal_year %in% fiscal_years)%>%

  gt(groupname_col = "Agency category",
  )%>%
  cols_hide(
    columns = c(
      "fiscal_year", "countryname"
    ))%>%
  fmt_percent(
    columns = c(`budget_execution`),
    decimals = 0)%>%
  fmt_currency( # add dolar signs
    columns = c(`cop_budget_total`,expenditure_amt),
    decimals = 0,
    currency = "USD")%>%
  tab_options(
    table.font.names = "Source Sans Pro"
  ) %>% 
  cols_width(
    everything() ~ px(140))%>%
  cols_label(
    agency_category = "Agency",
    expenditure_amt = "Expenditure",
    cop_budget_total = "Budget",
    budget_execution="Budget Execution"
  )%>%
  tab_header(
    title = glue::glue(" COP20 {ou} Financial Performance Summary"))%>%
  gt::tab_source_note("Create by EA Branch Excluding M&O")
  return(df)
}
  
    #pivot_longer(cop_budget_total:budget_execution,
     #            names_to="indicator",
      #           values_to="value")
  
#  df<-df%>%
    dplyr::mutate(fiscal_year=as.character(fiscal_year))
 # df$period<-paste(df$fiscal_year,"-",df$indicator)
  
  
  
  
 df<-df %>%pivot_wider(names_from = period,
                 values_from=value)%>%
  
  
    group_by(countryname,fundingagency,fiscal_year,indicator)%>%
    summarise_at(vars(value), sum, na.rm = TRUE)%>%
   
    
    
  
  dplyr::mutate(budget_execution=expenditure_amt/cop_budget_total)%>%
      
  
    
    

# LOAD DATA ============================================================================  

  msd

# MUNGE ============================================================================
  
 clean_partner<-function(df,ou="countryname",fiscal_years=c("2020,2021"), agency=c("USAID","Other","CDC")){
   
   df<-df_fsd%>%
     remove_mo()%>%
     dplyr::filter(!primepartner=="TBD")%>%
     dplyr::select (c(countryname,fundingagency, primepartner,fiscal_year,cop_budget_total,expenditure_amt))%>%
     #  dplyr::filter(fiscal_year %in% fiscal_years)%>%
     
     #dplyr::filter(countryname %in% ou)%>%
     mutate_at(vars(cop_budget_total,expenditure_amt),~replace_na(.,0))%>%
     dplyr::mutate(agency_category=fundingagency)%>%
     mutate(agency_category  = ifelse(agency_category == "USAID", "USAID",
                                      ifelse(agency_category  == "HHS/CDC", "CDC",
                                             ifelse(agency_category  =="Dedup", "Dedup","Other"))))%>%
     mutate( agency_category = fct_relevel(agency_category, "USAID","CDC","Other"))%>%
     group_by(countryname,agency_category,primepartner,fiscal_year)%>%
     summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
     ungroup()%>%
     dplyr::mutate(budget_execution=expenditure_amt/cop_budget_total)%>%
     
     dplyr::relocate(expenditure_amt, .before = cop_budget_total) %>%
     
     dplyr::filter(countryname %in% ou)%>%
     dplyr::filter(fiscal_year %in% fiscal_years)%>%
    # dplyr::filter(agency_category %in% agency)%>%
     
     gt(groupname_col = "agency_category",
     )%>%
    # row_group_order(
     #  groups = c("USAID", "CDC", "Other")
     #)%>%
     cols_hide(
       columns = c(
         "fiscal_year", "countryname"
       ))%>%
     fmt_percent(
       columns = c(`budget_execution`),
       decimals = 1)%>%
     fmt_currency( # add dolar signs
       columns = c(`cop_budget_total`,expenditure_amt),
       decimals = 0,
       currency = "USD")%>%
     tab_options(
       table.font.names = "Source Sans Pro"
     ) %>% 
     
     cols_width(
       everything() ~ px(140))%>%
     cols_label(
       agency_category = "Agency",
       expenditure_amt = "Expenditure",
       cop_budget_total = "Budget",
       budget_execution="Budget Execution",
       primepartner = "Partner"
     )%>%
     tab_style(style = cell_fill(color = "#5bb5d5",alpha = .75),      
               locations = cells_body(               
                 columns = vars(budget_execution),
                 rows = budget_execution >= 0.9 & budget_execution < 1.1)) %>%
     tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),      
               locations = cells_body(               
                 columns = vars(budget_execution),
                 rows =budget_execution < 0.9 ))%>%
     tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),      
               locations = cells_body(               
                 columns = vars(budget_execution),
                 rows = budget_execution>= 1.1 & budget_execution < 1.2))%>%
     tab_style(style = cell_fill(color = "#ff989f",alpha = .75),      
               locations = cells_body(               
                 columns = vars(budget_execution),
                 rows = budget_execution >= 1.2 ))%>%
     tab_header(
       title = glue::glue(" COP{fiscal_years} {ou} Financial Performance Summary"))%>%
     gt::tab_source_note("Create by EA Branch Excluding M&O")
   return(df)
 }
 
  
# program area ============================================================================

  program_area<-function(df,ou="countryname",fiscal_years=c("2020,2021"), agency=c("USAID","Other","CDC")){
 
 df<-df_fsd%>%
   dplyr::select (c(countryname,fundingagency, program,fiscal_year,cop_budget_total,expenditure_amt))%>%
   #  dplyr::filter(fiscal_year %in% fiscal_years)%>%
   
   #dplyr::filter(countryname %in% ou)%>%
   mutate_at(vars(cop_budget_total,expenditure_amt),~replace_na(.,0))%>%
   dplyr::mutate(agency_category=fundingagency)%>%
   mutate(agency_category  = ifelse(agency_category == "USAID", "USAID",
                                    ifelse(agency_category  == "HHS/CDC", "CDC",
                                           ifelse(agency_category  =="Dedup", "Dedup","Other"))))%>%
   mutate( agency_category = fct_relevel(agency_category, "USAID","CDC","Other"))%>%
   group_by(countryname,agency_category,program,fiscal_year)%>%
   summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
   ungroup()%>%
   dplyr::mutate(budget_execution=expenditure_amt/cop_budget_total)%>%
   
   dplyr::relocate(expenditure_amt, .before = cop_budget_total) %>%
   
   dplyr::filter(countryname %in% ou)%>%
   dplyr::filter(fiscal_year %in% fiscal_years)%>%
   dplyr::filter(agency_category %in% agency)%>%
   # dplyr::filter(agency_category %in% agency)%>%
   
   gt(groupname_col = "agency_category",
   )%>%
   cols_hide(
     columns = c(
       "fiscal_year", "countryname", 
     ))%>%
   fmt_percent(
     columns = c(`budget_execution`),
     decimals = 1)%>%
   fmt_currency( # add dolar signs
     columns = c(`cop_budget_total`,expenditure_amt),
     decimals = 0,
     currency = "USD")%>%
   tab_options(
     table.font.names = "Source Sans Pro"
   ) %>% 
   
   cols_width(
     everything() ~ px(140))%>%
   cols_label(
     agency_category = "Agency",
     expenditure_amt = "Expenditure",
     cop_budget_total = "Budget",
     budget_execution="Budget Execution",
     program = "Program Area"
   )%>%
   tab_style(style = cell_fill(color = "#5bb5d5",alpha = .75),      
             locations = cells_body(               
               columns = vars(budget_execution),
               rows = budget_execution >= 0.9 & budget_execution < 1.1)) %>%
   tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),      
             locations = cells_body(               
               columns = vars(budget_execution),
               rows =budget_execution < 0.9 ))%>%
   tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),      
             locations = cells_body(               
               columns = vars(budget_execution),
               rows = budget_execution>= 1.1 & budget_execution < 1.2))%>%
   tab_style(style = cell_fill(color = "#ff989f",alpha = .75),      
             locations = cells_body(               
               columns = vars(budget_execution),
               rows = budget_execution >= 1.2 ))%>%
   tab_footnote(
     footnote = "Excluding M&O",
     locations = cells_column_labels(
       columns = "expenditure_amt"))%>%
   tab_header(
     title = glue::glue(" Fiscal Year {fiscal_years}  {ou} Financial Performance Summary"))%>%
   gt::tab_source_note(paste0("Created by the  EA Branch on ", Sys.Date(), " using the FY21Q4i FSD. For support please reach out to gh.oha.costingadvisors@usaid.gov"))
 return(df)
 }
 program_area(df_fsd,"Mozambique",("2020"))

# SPINDOWN ============================================================================

