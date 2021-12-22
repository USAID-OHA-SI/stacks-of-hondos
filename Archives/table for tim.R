library(glamr)
library(gophr)
library(glitr)
library(glamr)
library(gisr)
library(Wavelength)
library(gophr)
library(tidyverse)
library(scales)
library(sf)
library(extrafont)
library(tidytext)
library(patchwork)
library(ggtext)
library(here)
library(gt)
library(googlesheets4)
library(glue)
library(googledrive)

df_fsd<-si_path()%>%
  glamr::return_latest("Fin")%>%
  gophr::read_msd()
# helper % clean
percent_clean <- function(x, y) {
  ifelse(y > 0.000, (x / y), NA_real_)
}
# helper agency category
agency_category<-function(df){
  df<-df%>%
    glamr::clean_agency()%>%
    #add to separate function file
    dplyr::mutate(`agency_category` = `fundingagency`)%>%
    dplyr::mutate(`agency_category` = ifelse(`agency_category` == "USAID", "USAID",
                                             ifelse(`agency_category` == "CDC", "CDC",
                                                    ifelse(`agency_category` =="Dedup", "Dedup","Other"))))%>%
    mutate( agency_category = fct_relevel(agency_category, "USAID","CDC","Other"))
  return(df)
  
  
}

source<-source_info(si_path(),"Fin")
#building legends
legend_be<-'https://user-images.githubusercontent.com/5873344/136249989-046c8107-706f-42cf-be5e-1dfb15e29093.png?raw=true'
legend_chunk <- gt::md(glue::glue("Legend: Budget Execution <img src= '{legend_be}' style='height:15px;'>"))



# prep table
prep_fsd <-function(df){ 
  
  #removing
  df<-df %>% 
    dplyr::select(-c("prime_partner_duns","prime_partner_org_type",
                     "is_indigenous_prime_partner", "subrecipient_duns",
                     "award_number","procurement_type")) %>% 
    glamr::remove_mo()%>% # filter out M&O
    #dplyr::rename("fiscal_year"= implementation_year) %>% 
    #dplyr::filter(fiscal_year == c(2020 ,2021))
    
    
    ##concatenate mech id and mech name
    dplyr::mutate( mech_id_mech_name = paste(mech_code,"-", mech_name))%>%
    
    #mutate data type double into integer to have round numbers
    dplyr::mutate_if(is.double, as.integer)%>%
    #mutate data type for mechanism id and fiscal year
    #df<-df %>% dplyr::mutate_at(vars(mech_code, fiscal_year),list(as.character))
    
    #drop NA for numeric amounts
    
    mutate_at(vars(cop_budget_new_funding:expenditure_amt),~replace_na(.,0))%>%
    
    
    #recode values to match naming in Financial Integrated Dataset
    dplyr::mutate(`interaction_type`= recode (`interaction_type`, "Service Delivery"= "SD",
                                              "Non Service Delivery"= "NSD"))%>%
    
    #Add in agency category column to group agencies
    
    glamr::clean_agency()%>%
    agency_category()%>%
    
    #mutating & calculating budget execution
    group_by(operatingunit, countryname, fundingagency, agency_category, fiscal_year,primepartner,mech_id_mech_name,program, interaction_type) %>% 
    summarise_at(vars(cop_budget_total, expenditure_amt), sum, na.rm = TRUE) %>% 
    ungroup()
  
  # df<-df%>%
  #  dplyr::mutate(budget_execution = percent_clean(expenditure_amt, cop_budget_total))
  
  #df<-df%>%
  # dplyr::mutate(budget_execution= as.numeric(`budget_execution`))
  
  return(df)  
  
}
ea_style<-function(df){
  df%>%
    gt()%>%
    fmt_percent(
      columns = tidyselect::contains("_execution_"),
      decimals = 0)%>%
    fmt_currency( # add dolar signs
      columns = tidyselect::contains("_budget_total"),
      decimals = 0,
      currency = "USD")%>%
    fmt_currency( # add dolar signs
      columns = tidyselect::contains("expenditure_amt"),
      decimals = 0,
      currency = "USD")%>%
    tab_options(
      table.font.names = "Source Sans Pro"
    ) %>% 
    cols_width(
      everything() ~ px(90))%>%
    
    tab_style(
      style = cell_borders(
        sides = "right",
        weight = px(1.5),
      ),
      locations = cells_body(
        columns = everything(),
        rows = everything()
      ))%>%
    tab_style(
      style = cell_text(weight = 700),
      locations = cells_body(
        columns = tidyselect::contains("_execution_")
      )
    )%>%
    tab_spanner(
      label = "COP20 Performance",
      columns = tidyselect::contains("2021"))%>%
    tab_spanner(
      label = "COP19 Performance",
      columns = tidyselect::contains("2020"))%>%
    gt::tab_style(
      style = list(
        gt::cell_text(weight = "bold")), 
      locations = gt::cells_column_spanners(spanners = tidyselect::everything())
    )%>%
    cols_align(
      align = "center",
      columns = everything()
    )%>%
    cols_label( #update GT to check on tidy select), also look at clean_names, also potentially case_when
      expenditure_amt_2020 = "Expenditure",
      cop_budget_total_2020 = "Budget",
      budget_execution_2020="Budget Execution",
      expenditure_amt_2021 = "Expenditure",
      cop_budget_total_2021 = "Budget",
      budget_execution_2021="Budget Execution"
      
    )%>%
    cols_align(
      align = "left",
      columns = 1
    )%>%
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
        columns =c(expenditure_amt_2020, expenditure_amt_2021)))%>%
    gt::tab_source_note(
      source_note = gt::md(glue::glue("**Source**: {source} | Please reach out to oha.ea@usaid.gov for questions"))
    )
  
} #gt style

get_global_agency_be<-function(df){
  df<-df%>%
    prep_fsd()%>%
    dplyr::filter(fiscal_year=="2020" | fiscal_year=="2021")%>%
    dplyr::select (c(agency_category,fiscal_year,cop_budget_total,expenditure_amt))%>%
    group_by(agency_category,fiscal_year)%>%
    summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
    dplyr::mutate(budget_execution=percent_clean(expenditure_amt,cop_budget_total))%>%
    ungroup()%>%
    pivot_wider(names_from = fiscal_year,values_from = cop_budget_total:budget_execution, values_fill = 0)%>%
    dplyr::relocate(expenditure_amt_2020, .before = cop_budget_total_2020) %>%
    dplyr::relocate(expenditure_amt_2021, .before = cop_budget_total_2021) %>%
    dplyr::relocate(budget_execution_2021, .after = cop_budget_total_2021)%>%
    dplyr::relocate(budget_execution_2020, .after = cop_budget_total_2020) %>%
    #gt()%>%
    ea_style()%>%
    cols_label( #update GT to check on tidy select), also look at clean_names, also potentially case_when
      agency_category = "Agency")%>%
    
    tab_header(
      title = (" COP 2019 & 2020 Global Financial Performance Summary"),
      subtitle = legend_chunk)%>%
    tab_source_note(
      source_note = md("*Other* based on aggregated funding agencies"))
  
  
  return(df)
} #buildng out full table
#test
get_global_agency_be(df_fsd)
