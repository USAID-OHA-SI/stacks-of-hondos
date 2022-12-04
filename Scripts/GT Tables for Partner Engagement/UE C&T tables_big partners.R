# PURPOSE: Munge and Analysis of
# AUTHOR: Ben Kasdan | SIEI
# LICENSE: MIT
# DATE: 2021-10-08
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
    library(gt)
library(glue)
    

    
    indics<-c("HTS_TST","HTS_TST_POS", "TX_CURR", "TX_NEW")
    progs<-c("HTS", "C&T")

# LOAD DATA ============================================================================  

    df_fsd<-si_path()%>%
      return_latest("Fin")%>%
      gophr::read_msd()
    
    df_msd<-si_path()%>%
      return_latest("OU_IM")%>%
      gophr::read_msd()
    
    source("~/Github/stacks-of-hondos/Scripts/utilities.R")

# MUNGE FSD ============================================================================
  
  df_fsd<-df_fsd%>%
      remove_mo()%>%
      clean_agency()%>%
      filter(fundingagency=="USAID")%>%
      select(countryname,fundingagency,fiscal_year, mech_code, mech_name, primepartner, fiscal_year, program,cop_budget_total, expenditure_amt)%>%
      group_by(countryname,fundingagency,fiscal_year, mech_code, mech_name, primepartner, program) %>% 
      #group_by(country, mech_code, mech_name, primepartner, fiscal_year, `Program Area: Sub Program Area-Service Level`,`Beneficiary-Sub Beneficiary`)%>%
      summarise_at(vars(cop_budget_total, expenditure_amt), sum, na.rm = TRUE) %>% 
      ungroup()%>%
      filter(program %in% progs)
    
    
# MUNGE MSD ============================================================================
    
    df_msd<-df_msd%>%
      filter(standardizeddisaggregate=="Total Numerator")%>%
      filter(indicator %in% indics)%>%
      clean_agency()%>%
      #dplyr::select(operatingunit,fundingagency,fiscal_year, mech_code, mech_name, primepartner,indicator cumulative,targets)%>%
      group_by(countryname,fundingagency,fiscal_year, mech_code, mech_name, primepartner,indicator) %>% 
      #group_by(country, mech_code, mech_name, primepartner, fiscal_year, `Program Area: Sub Program Area-Service Level`,`Beneficiary-Sub Beneficiary`)%>%
      summarise_at(vars(cumulative,targets), sum, na.rm = TRUE) %>% 
      ungroup()%>%

          dplyr::mutate(program = dplyr::case_when(indicator    == "TX_CURR"    ~"C&T", 
                                                   indicator    == "TX_NEW"    ~"C&T",
                                                   indicator =="HTS_TST" ~"HTS",
                                                   indicator == "HTS_TST_POS" ~"HTS",
                                                      
                                                      TRUE ~indicator))%>%
          filter(fundingagency=="USAID")%>%
         dplyr::filter(targets>0)
        
    
  
   
    
    
#join datasets together 
   
      df_ue<-left_join(df_fsd,df_msd)
    
    df_ue<-df_ue%>%
      #mutate( fundingagency = fct_relevel(fundingagency, "USAID","CDC"))%>%
      group_by(countryname,fundingagency,fiscal_year, mech_code, mech_name, primepartner,indicator)%>%
      pivot_longer(expenditure_amt:cop_budget_total,
                   names_to ="financial",
                   values_to="amount")%>%
      pivot_longer(cumulative:targets,
                   names_to ="programatic",
                   values_to="value")%>%
      ungroup()
    df_ue<-df_ue%>%
      filter(programatic=="cumulative")
    
    df_ue<-df_ue%>%
      pivot_wider(names_from = financial,
                  values_from=amount)%>%
      pivot_wider(names_from = programatic,
                  values_from=value)
    df_ue<-df_ue%>%
      dplyr::mutate(unit_expenditure=percent_clean(expenditure_amt,cumulative))%>%
      filter(fiscal_year=="2021")
    df_ue<-df_ue%>%select(countryname,fundingagency,mech_code, mech_name, primepartner,program, indicator, unit_expenditure, cumulative)%>%
      pivot_wider(names_from =indicator,
                  values_from=cumulative:unit_expenditure)
    
    
    
    df_ue<-df_ue%>%
      select(countryname,fundingagency,mech_code, mech_name, primepartner,cumulative_HTS_TST,cumulative_HTS_TST_POS,
             cumulative_TX_CURR,cumulative_TX_NEW,
             unit_expenditure_HTS_TST,unit_expenditure_HTS_TST_POS,
             unit_expenditure_TX_CURR,unit_expenditure_TX_NEW,)
    
    
    df_ue<-df_ue%>%     group_by(countryname,fundingagency,mech_code, mech_name, primepartner,)%>%
      summarise_at(vars(cumulative_HTS_TST: unit_expenditure_TX_NEW  ), sum, na.rm = TRUE)%>%
      dplyr::mutate(primepartner = dplyr::case_when(primepartner    == "FHI Development 360 LLC"    ~"FHI360",
                                                    primepartner    ==   "Family Health International"    ~"FHI360",
                                                    primepartner    ==  "Abt Associates, Inc." ~ "Abt Associates Inc",
                                                    
                                                    TRUE ~primepartner))%>%
      mutate("country_mech"=glue("{countryname}-{mech_name}- {mech_code}"))%>%
      dplyr::relocate(country_mech, .before = cumulative_HTS_TST)%>%

      ungroup()
    
    df_ue<-df_ue%>%
      filter(unit_expenditure_HTS_TST>0 | cumulative_HTS_TST> 0 |unit_expenditure_HTS_TST_POS>0 |cumulative_HTS_TST_POS>0
             |unit_expenditure_TX_CURR>0 |cumulative_TX_CURR >0 |unit_expenditure_TX_NEW>0| cumulative_TX_NEW  >0)
    
   
      df_ue<-df_ue%>%
      dplyr::relocate(unit_expenditure_HTS_TST , .before = cumulative_HTS_TST)%>%
      dplyr::relocate( unit_expenditure_HTS_TST_POS, .before = cumulative_HTS_TST_POS)
    df_ue<-df_ue%>%
      dplyr::relocate(unit_expenditure_TX_CURR, .before = cumulative_TX_CURR)%>%
      dplyr::relocate(unit_expenditure_TX_NEW, .before = cumulative_TX_NEW)%>%
      ungroup()
    
    
   
    partner<-df_ue%>%
      distinct(primepartner)%>%
      pull()    
     
  
# gt function ============================================================================

  #   
    get_ue_partner<-function(df, partner="primepartner"){
    df<-df_ue%>%
      select(-c("mech_code","mech_name","fundingagency"))%>%
      filter(primepartner %in% partner)%>%
      gt()%>%
      cols_hide(
        columns=c("primepartner","countryname",
                 ))%>%
      fmt_currency( # add dolar signs
        columns = contains("unit_expenditure"),
        decimals = 0,
        currency = "USD")%>%
      fmt_number(
        columns = contains("cumulative"),
        decimals = 0,
        use_seps = TRUE
      )%>%
      fmt_missing(columns = everything(),
                  missing_text = "-")%>%
      tab_options(
        table.font.names = "Source Sans Pro"
      ) %>% 
      
      cols_width(
        starts_with("country") ~ px(140),
        everything() ~ px(80)
      )%>%
      cols_label( #update GT to check on tidy select), also look at clean_names, also potentially case_when
        country_mech = "Mechanism",
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
      cols_align(
        align = "center",
        columns = everything()
      )%>%
      cols_label( #update GT to check on tidy select), also look at clean_names, also potentially case_when
        country_mech = "Mechanism",
        unit_expenditure_HTS_TST="TST UE",
        unit_expenditure_HTS_TST_POS="TST POS UE",
        unit_expenditure_TX_CURR="TX CURR UE",
        unit_expenditure_TX_NEW="TX NEW UE",
        cumulative_HTS_TST ="TST Results",
        cumulative_HTS_TST_POS ="TST POS Results",
        cumulative_TX_CURR ="TX CURR Results",
        cumulative_TX_NEW ="TX NEW Results",
        
      )%>%
        cols_align(
          align = "left",
          columns = 1)%>%
      tab_header(
        title = ("  COP20 Unit Expenditure: Treatment Cascade"),
        subtitle = glue::glue("Prime Partner: {partner}"))%>% 
      gt::tab_source_note(
        source_note = gt::md(glue::glue("**Source**: {source} | Please reach out to oha.ea@usaid.gov for questions"))
      )%>%
      
      
      tab_footnote(
        footnote =md( "A unit expenditure (UE) is a calculation of partner-level expenditures for a given program area (source: ER) divided by the number of associated beneficiaries (source: MER). Total IM-level expenditure within a program area, divided by IM-specific result value.  Can only be calculated for mechanisms that have both expenditures and results within a given program area. It can be interpreted as the spend per beneficiary reached with those resources. **UEs across partners should be interpreted within the programmatic context, as there are differences in factors such as scope, funding profile, and geography.**"),
        locations = cells_column_labels(
          columns =c(unit_expenditure_HTS_TST)))%>%
      
      gt::tab_options(
        source_notes.font.size = 8,
        table.font.size = 13, 
        data_row.padding = gt::px(5),
        source_notes.padding = gt::px(1),)
    return(df)
    }

# testing ============================================================================
    table_out<-"GitHub/stacks-of-hondos/Images/Partner tables/UE"
    #to run for one OU testing below
    get_ue_partner(df_ue, "PATH")%>%
      gtsave(.,path=table_out,filename = glue::glue("PATH_unit_expenditure.png"))
    #to run for all
    purrr::map(partner, ~get_ue_partner(df_ue, partner = .x)%>%
                 gtsave(.,path=table_out,filename = glue::glue("{.x}_unit_expenditure.png")))
    
    
    ### for FHI 360
    partner<-"FHI360"
    df_fhi<-df_ue%>%
      filter(primepartner %in% partner)
    df_fhi1 <- df_fhi[c(1:12),c(1:14)]
    df_fhi1<-df_fhi1%>%
      select(-c("mech_code","mech_name","fundingagency"))%>%
      filter(primepartner %in% partner)%>%
      gt()%>%
      cols_hide(
        columns=c("primepartner","countryname",
        ))%>%
      fmt_currency( # add dolar signs
        columns = contains("unit_expenditure"),
        decimals = 0,
        currency = "USD")%>%
      fmt_number(
        columns = contains("cumulative"),
        decimals = 0,
        use_seps = TRUE
      )%>%
      fmt_missing(columns = everything(),
                  missing_text = "-")%>%
      tab_options(
        table.font.names = "Source Sans Pro"
      ) %>% 
      
      cols_width(
        starts_with("country") ~ px(140),
        everything() ~ px(80)
      )%>%
      cols_label( #update GT to check on tidy select), also look at clean_names, also potentially case_when
        country_mech = "Mechanism",
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
      cols_align(
        align = "center",
        columns = everything()
      )%>%
      cols_label( #update GT to check on tidy select), also look at clean_names, also potentially case_when
        country_mech = "Mechanism",
        unit_expenditure_HTS_TST="TST UE",
        unit_expenditure_HTS_TST_POS="TST POS UE",
        unit_expenditure_TX_CURR="TX CURR UE",
        unit_expenditure_TX_NEW="TX NEW UE",
        cumulative_HTS_TST ="TST Results",
        cumulative_HTS_TST_POS ="TST POS Results",
        cumulative_TX_CURR ="TX CURR Results",
        cumulative_TX_NEW ="TX NEW Results",
        
      )%>%
      cols_align(
        align = "left",
        columns = 1)%>%
      tab_header(
        title = ("  COP20 Unit Expenditure: Treatment Cascade"),
        subtitle = glue::glue("Prime Partner: {partner}"))%>% 
      gt::tab_source_note(
        source_note = gt::md(glue::glue("**Source**: {source} | Please reach out to oha.ea@usaid.gov for questions"))
      )%>%
      
      
      tab_footnote(
        footnote =md( "A unit expenditure (UE) is a calculation of partner-level expenditures for a given program area (source: ER) divided by the number of associated beneficiaries (source: MER). Total IM-level expenditure within a program area, divided by IM-specific result value.  Can only be calculated for mechanisms that have both expenditures and results within a given program area. It can be interpreted as the spend per beneficiary reached with those resources. **UEs across partners should be interpreted within the programmatic context, as there are differences in factors such as scope, funding profile, and geography.**"),
        locations = cells_column_labels(
          columns =c(unit_expenditure_HTS_TST)))%>%
      
      gt::tab_options(
        source_notes.font.size = 8,
        table.font.size = 13, 
        data_row.padding = gt::px(5),
        source_notes.padding = gt::px(1),)%>%
      opt_row_striping()%>%
      gtsave(.,path=table_out,filename = glue::glue("FHI_group1_unit_expenditure.png"))
      
    df_fhi2 <- df_fhi[c(13:25),c(1:14)]
    df_fhi2<-df_fhi2%>%
      select(-c("mech_code","mech_name","fundingagency"))%>%
      filter(primepartner %in% partner)%>%
      gt()%>%
      cols_hide(
        columns=c("primepartner","countryname",
        ))%>%
      fmt_currency( # add dolar signs
        columns = contains("unit_expenditure"),
        decimals = 0,
        currency = "USD")%>%
      fmt_number(
        columns = contains("cumulative"),
        decimals = 0,
        use_seps = TRUE
      )%>%
      fmt_missing(columns = everything(),
                  missing_text = "-")%>%
      tab_options(
        table.font.names = "Source Sans Pro"
      ) %>% 
      
      cols_width(
        starts_with("country") ~ px(140),
        everything() ~ px(80)
      )%>%
      cols_label( #update GT to check on tidy select), also look at clean_names, also potentially case_when
        country_mech = "Mechanism",
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
      cols_align(
        align = "center",
        columns = everything()
      )%>%
      cols_label( #update GT to check on tidy select), also look at clean_names, also potentially case_when
        country_mech = "Mechanism",
        unit_expenditure_HTS_TST="TST UE",
        unit_expenditure_HTS_TST_POS="TST POS UE",
        unit_expenditure_TX_CURR="TX CURR UE",
        unit_expenditure_TX_NEW="TX NEW UE",
        cumulative_HTS_TST ="TST Results",
        cumulative_HTS_TST_POS ="TST POS Results",
        cumulative_TX_CURR ="TX CURR Results",
        cumulative_TX_NEW ="TX NEW Results",
        
      )%>%
      cols_align(
        align = "left",
        columns = 1)%>%
      tab_header(
        title = ("  COP20 Unit Expenditure: Treatment Cascade"),
        subtitle = glue::glue("Prime Partner: {partner}"))%>% 
      gt::tab_source_note(
        source_note = gt::md(glue::glue("**Source**: {source} | Please reach out to oha.ea@usaid.gov for questions"))
      )%>%
      
      
      tab_footnote(
        footnote =md( "A unit expenditure (UE) is a calculation of partner-level expenditures for a given program area (source: ER) divided by the number of associated beneficiaries (source: MER). Total IM-level expenditure within a program area, divided by IM-specific result value.  Can only be calculated for mechanisms that have both expenditures and results within a given program area. It can be interpreted as the spend per beneficiary reached with those resources. **UEs across partners should be interpreted within the programmatic context, as there are differences in factors such as scope, funding profile, and geography.**"),
        locations = cells_column_labels(
          columns =c(unit_expenditure_HTS_TST)))%>%
      
      gt::tab_options(
        source_notes.font.size = 8,
        table.font.size = 13, 
        data_row.padding = gt::px(5),
        source_notes.padding = gt::px(1),)%>%
      opt_row_striping()%>%
      gtsave(.,path=table_out,filename = glue::glue("FHI_group2_unit_expenditure.png"))
    df_fhi3 <- df_fhi[c(26:35),c(1:14)]
    
    df_fhi3<-df_fhi3%>%
      
      select(-c("mech_code","mech_name","fundingagency"))%>%
      filter(primepartner %in% partner)%>%
      gt()%>%
      cols_hide(
        columns=c("primepartner","countryname",
        ))%>%
      fmt_currency( # add dolar signs
        columns = contains("unit_expenditure"),
        decimals = 0,
        currency = "USD")%>%
      fmt_number(
        columns = contains("cumulative"),
        decimals = 0,
        use_seps = TRUE
      )%>%
      fmt_missing(columns = everything(),
                  missing_text = "-")%>%
      tab_options(
        table.font.names = "Source Sans Pro"
      ) %>% 
      
      cols_width(
        starts_with("country") ~ px(140),
        everything() ~ px(80)
      )%>%
      cols_label( #update GT to check on tidy select), also look at clean_names, also potentially case_when
        country_mech = "Mechanism",
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
      cols_align(
        align = "center",
        columns = everything()
      )%>%
      cols_label( #update GT to check on tidy select), also look at clean_names, also potentially case_when
        country_mech = "Mechanism",
        unit_expenditure_HTS_TST="TST UE",
        unit_expenditure_HTS_TST_POS="TST POS UE",
        unit_expenditure_TX_CURR="TX CURR UE",
        unit_expenditure_TX_NEW="TX NEW UE",
        cumulative_HTS_TST ="TST Results",
        cumulative_HTS_TST_POS ="TST POS Results",
        cumulative_TX_CURR ="TX CURR Results",
        cumulative_TX_NEW ="TX NEW Results",
        
      )%>%
      cols_align(
        align = "left",
        columns = 1)%>%
      tab_header(
        title = ("  COP20 Unit Expenditure: Treatment Cascade"),
        subtitle = glue::glue("Prime Partner: {partner}"))%>% 
      gt::tab_source_note(
        source_note = gt::md(glue::glue("**Source**: {source} | Please reach out to oha.ea@usaid.gov for questions"))
      )%>%
      
      
      tab_footnote(
        footnote =md( "A unit expenditure (UE) is a calculation of partner-level expenditures for a given program area (source: ER) divided by the number of associated beneficiaries (source: MER). Total IM-level expenditure within a program area, divided by IM-specific result value.  Can only be calculated for mechanisms that have both expenditures and results within a given program area. It can be interpreted as the spend per beneficiary reached with those resources. **UEs across partners should be interpreted within the programmatic context, as there are differences in factors such as scope, funding profile, and geography.**"),
        locations = cells_column_labels(
          columns =c(unit_expenditure_HTS_TST)))%>%
      
      gt::tab_options(
        source_notes.font.size = 8,
        table.font.size = 13, 
        data_row.padding = gt::px(5),
        source_notes.padding = gt::px(1),)%>%
      opt_row_striping()%>%
      gtsave(.,path=table_out,filename = glue::glue("FHI_group3_unit_expenditure.png"))
    
    
    
      
