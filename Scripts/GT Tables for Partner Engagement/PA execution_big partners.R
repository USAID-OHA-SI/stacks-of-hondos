library(glamr)
library(tidyverse)
library(gophr)
library(extrafont)
library(tidytext)
library(gt)
library(glue)
library(webshot)
legend_be<-'https://user-images.githubusercontent.com/5873344/136249989-046c8107-706f-42cf-be5e-1dfb15e29093.png?raw=true'
legend_chunk <- gt::md(glue::glue("Legend: Budget Execution <img src= '{legend_be}' style='height:15px;'>"))

source<-source_info(si_path(),"Fin")
percent_clean <- function(x, y) {
  ifelse(y > 0.000, (x / y), NA_real_)
}

df_fsd<-si_path()%>%
  return_latest("Fin")%>%
  gophr::read_msd()


mech_list<- df_fsd%>%
  #filter for fiscal year
  dplyr::filter( fiscal_year=="2022")%>%
  dplyr::filter(fundingagency == "USAID" | fundingagency == "USAID/WCF") %>% 
  dplyr::mutate(primepartner = dplyr::case_when(primepartner    == "FHI Development 360 LLC"    ~"FHI360",
                                                primepartner    ==   "Family Health International"    ~"FHI360",
                                                primepartner    ==  "Abt Associates, Inc." ~ "Abt Associates Inc",
                                                
                                                TRUE ~primepartner))%>%
 
  glamr::remove_mo()%>% # filter out M&O
  #dplyr::rename("fiscal_year"= implementation_year) %>% 
  #dplyr::filter(fiscal_year == c(2020 ,2021))
  
  
  ##concatenate country and mech name
  dplyr::mutate( partner_mech_name = paste(countryname,"-",mech_code))%>%
  
  #mutate data type double into integer to have round numbers
  dplyr::mutate_if(is.double, as.integer)%>%
  #mutate data type for mechanism id and fiscal year
  #df<-df %>% dplyr::mutate_at(vars(mech_code, fiscal_year),list(as.character))
  
  #drop NA for numeric amounts
  
  mutate_at(vars(cop_budget_new_funding:expenditure_amt),~replace_na(.,0))%>%
  
  
  
  
  #filter(partner_mech_name %in% mech_list)%>% 
  
  #select specific variables
  dplyr::select (c(countryname,partner_mech_name, mech_code,program, fiscal_year,cop_budget_total,expenditure_amt))%>%
  mutate_at(vars(cop_budget_total,expenditure_amt),~replace_na(.,0))%>%
  #mutate( fundingagency = fct_relevel(fundingagency, "USAID","CDC"))%>%
  
  
  
  group_by(countryname,partner_mech_name,program, fiscal_year)%>%
  summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
  dplyr::mutate(budget_execution=percent_clean(expenditure_amt,cop_budget_total))%>%
  ungroup(program, fiscal_year)%>%
  
  
  pivot_wider(names_from = fiscal_year,values_from = cop_budget_total:budget_execution, values_fill = 0)%>%
  filter(budget_execution_2021>0)%>%
  distinct(partner_mech_name) %>%    pull()




#use this function to produce budget execution filles by PA for select partners

get_ou_mechanism_pa<-function(df, mech_list="partner_mech_name"){
  df<-df%>%
    #filter for fiscal year
    dplyr::filter( fiscal_year=="2021")%>%
    dplyr::filter(fundingagency == "USAID" | fundingagency == "USAID/WCF") %>% 
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
      dplyr::mutate( partner_mech_name = paste(countryname,"-",mech_code))%>%
      
      #mutate data type double into integer to have round numbers
      dplyr::mutate_if(is.double, as.integer)%>%
      #
      mutate_at(vars(cop_budget_new_funding:expenditure_amt),~replace_na(.,0))%>%
      
      
     
   
     filter(partner_mech_name %in% mech_list)%>% 
    
    #select specific variables
    dplyr::select (c(partner_mech_name,mech_name,program,cop_budget_total,expenditure_amt))%>%
    mutate_at(vars(cop_budget_total,expenditure_amt),~replace_na(.,0))%>%
    #mutate( fundingagency = fct_relevel(fundingagency, "USAID","CDC"))%>%
    
    
    
    group_by(partner_mech_name,mech_name,program)%>%
    summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
    adorn_totals("row",,,, -partner_mech_name,-mech_name,-program)%>%
    dplyr::mutate(budget_execution=percent_clean(expenditure_amt,cop_budget_total))%>%
   
    dplyr::relocate(expenditure_amt, .before = cop_budget_total) %>%
    ungroup()%>%
     #dplyr::relocate(budget_execution_2020, .after = cop_budget_total_2020) %>%
    
    
   
    
    #break into separate functions
  
    gt()%>%
    cols_hide(
      columns = c(partner_mech_name)
    ) %>%
    fmt_percent(
      columns = tidyselect::contains("_execution"),
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
      starts_with("mech") ~ px(140),
      everything() ~ px(90)
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
    tab_style(
      style = cell_text(weight = 700),
      locations = cells_body(
        columns = tidyselect::contains("_execution")
      )
    )%>%
    # tab_spanner(
    #   label = "COP20 Performance",
    #   columns = tidyselect::contains("2021"))%>%
    # # tab_spanner(
    # #   label = "COP19 Performance",
    # #   columns = tidyselect::contains("2020"))%>%
    # gt::tab_style(
    #   style = list(
    #     gt::cell_text(weight = "bold")), 
    #   locations = gt::cells_column_spanners(spanners = tidyselect::everything())
    # )%>%
    cols_align(
      align = "center",
      columns = everything()
    )%>%
    cols_label( #update GT to check on tidy select), also look at clean_names, also potentially case_when
      # expenditure_amt_2020 = "Expenditure",
      # cop_budget_total_2020 = "Budget",
      # budget_execution_2020="Budget Execution",
      expenditure_amt = "Expenditure",
      cop_budget_total = "Budget",
      budget_execution="Budget Execution",
        program= "Program Area",
      mech_name="Mechanism")%>%
    cols_align(
      align = "left",
      columns = 1
    )%>%
    # tab_style(style = cell_fill(color = "#5bb5d5",alpha = .75),
    #           locations = cells_body(
    #             columns = (budget_execution_2020),
    #             rows = (budget_execution_2020) >= 0.9 & (budget_execution_2020) < 1.1)) %>%
    # tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),
    #           locations = cells_body(
    #             columns = (budget_execution_2020),
    #             rows =(budget_execution_2020) < 0.9 ))%>%
    # tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),
    #           locations = cells_body(
    #             columns = (budget_execution_2020),
    #             rows = (budget_execution_2020)>= 1.1 & (budget_execution_2020) < 1.2))%>%
    # tab_style(style = cell_fill(color = "#ff989f",alpha = .75),
    #           locations = cells_body(
    #             columns = (budget_execution_2020),
    #             rows = (budget_execution_2020) >= 1.2 ))%>%
    
    tab_style(style = cell_fill(color = "#5bb5d5",alpha = .75),      
              locations = cells_body(               
                columns = (budget_execution),
                rows = (budget_execution) >= 0.9 & (budget_execution) < 1.1)) %>%
    tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),      
              locations = cells_body(               
                columns = (budget_execution),
                rows =(budget_execution) < 0.9 ))%>%
    tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),      
              locations = cells_body(               
                columns = (budget_execution),
                rows = (budget_execution)>= 1.1 & (budget_execution) < 1.2))%>%
    tab_style(style = cell_fill(color = "#ff989f",alpha = .75),      
              locations = cells_body(               
                columns = (budget_execution),
                rows = (budget_execution) >= 1.2 ))%>%
    
    gt::tab_options(
      source_notes.font.size = 8,
      table.font.size = 13, 
      data_row.padding = gt::px(5),
      source_notes.padding = gt::px(1),) %>%
    
    gt::tab_source_note(
      source_note = gt::md(glue::glue("**Source**: {source} | Please reach out to oha.ea@usaid.gov for questions"))
    )%>%
    tab_footnote(
      footnote =md( "USAID typically expects budget execution to fall within the 90-110% range. There are programmatic reasons for budget execution to fall outside that range, and it is important to be able to explain why a mechanism would overspend/underspend their budget"),
      locations = cells_column_labels(
        columns =c(budget_execution)))%>%
    
    
    tab_header(
      title = glue::glue("{mech_list}  COP20  Financial Performance Summary"),
      subtitle = legend_chunk) %>%
    
    
    tab_options(footnotes.font.size = "small")
  
  return(df)
}

#output 

table_out<-"GitHub/stacks-of-hondos/Images/Partner tables/BE"

purrr::map(mech_list, ~get_ou_mechanism_pa(df_fsd, mech_list = .x)%>%
             gtsave(.,path=table_out,filename = glue::glue("{.x}_budget_execution.png")))

             
#to run for one testing below
 get_ou_mechanism_pa(df_fsd,"Kenya - 84483")%>%
   gtsave("New Kenya.png")
 gtsave(.,path=table_out,filename = glue::glue("Kenya - ST JOHN'S COMMUNITY CENTRE: PUMWANI - 84483_budget execution.png"))
