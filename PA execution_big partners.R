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



source("~/GitHub/stacks-of-hondos/utilities.R")

#use this function to produce budget execution filles by PA for select partners

get_ou_mechanism_pa<-function(df, partner="primepartner"){
  df<-df%>%
    #filter for fiscal year
    dplyr::filter(fiscal_year=="2020" | fiscal_year=="2021")%>%
    dplyr::filter(fundingagency == "USAID") %>% 
    dplyr::mutate(primepartner = dplyr::case_when(primepartner    == "FHI Development 360 LLC"    ~"FHI360",
                                                  primepartner    ==   "Family Health International"    ~"FHI360",
                                                  primepartner    ==  "Abt Associates, Inc." ~ "Abt Associates Inc",
                                                  
                                                  TRUE ~primepartner))%>%
    
   
    
      dplyr::select(-c("prime_partner_duns","prime_partner_org_type",
                       "is_indigenous_prime_partner", "subrecipient_duns",
                       "award_number","procurement_type")) %>% 
      glamr::remove_mo()%>% # filter out M&O
      #dplyr::rename("fiscal_year"= implementation_year) %>% 
      #dplyr::filter(fiscal_year == c(2020 ,2021))
      
      
      ##concatenate country and mech name
      dplyr::mutate( country_mech_name = paste(countryname,"-", mech_name,"-",mech_code))%>%
      
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
      
      
      
   
    filter(primepartner %in% partner)%>% 
    
    #select specific variables
    dplyr::select (c(country_mech_name,primepartner,program, fiscal_year,cop_budget_total,expenditure_amt))%>%
    mutate_at(vars(cop_budget_total,expenditure_amt),~replace_na(.,0))%>%
    #mutate( fundingagency = fct_relevel(fundingagency, "USAID","CDC"))%>%
    
    
    
    group_by(country_mech_name,primepartner,program, fiscal_year)%>%
    summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
    dplyr::mutate(budget_execution=percent_clean(expenditure_amt,cop_budget_total))%>%
    ungroup(program, fiscal_year)%>%
    
    
    pivot_wider(names_from = fiscal_year,values_from = cop_budget_total:budget_execution, values_fill = 0)%>%
    dplyr::relocate(expenditure_amt_2020, .before = cop_budget_total_2020) %>%
    dplyr::relocate(expenditure_amt_2021, .before = cop_budget_total_2021) %>%
    dplyr::relocate(budget_execution_2021, .after = cop_budget_total_2021)%>%
    dplyr::relocate(budget_execution_2020, .after = cop_budget_total_2020) %>%
    ungroup()%>%
    select(-c("primepartner"))%>%
    
    #break into separate functions
    
   
    gt(groupname_col = "country_mech_name")%>%
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
    fmt_missing(columns = everything(),
                missing_text = "-") %>%
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
    
    gt::tab_source_note(
      source_note = gt::md(glue::glue("**Source**: {source} | Please reach out to oha.ea@usaid.gov for questions"))
    )%>%
    
    cols_label(
      country_mech_name = "Mechanism",
      program= "Program Area")%>%
    tab_header(
      title = glue::glue("{partner} COP19 & COP20  Financial Performance Summary"),
      subtitle = legend_chunk) %>%
    
    
    tab_options(footnotes.font.size = "small")
  
  return(df)
}

#output 


table_out<-"~/GitHub/stacks-of-hondos/Images"
#to run for one OU testing below
get_ou_mechanism_pa(df_fsd,"FHI360")%>%
  gtsave(.,path=table_out,filename = glue::glue("FHI360_pa_budget execution.png"))
