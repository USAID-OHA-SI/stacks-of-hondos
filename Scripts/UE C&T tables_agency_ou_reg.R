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
  gophr::read_psd()

df_msd<-si_path()%>%
  return_latest("OU_IM")%>%
  gophr::read_psd()

#This function can be used to generate unit expenditure tables across the treatment continuum
#It can be used to generate one table for one ou, or a batch for all ous.
#Be sure to load the following source files below before running
source("~/GitHub/stacks-of-hondos/Scripts/utilities.R")
    
    indics<-c("HTS_TST","HTS_TST_POS", "TX_CURR", "TX_NEW")
    progs<-c("HTS", "C&T")
glamr::load_secrets()


    # MUNGE FSD ============================================================================
    
    df_fsd<-df_fsd%>%
      remove_mo()%>%
      remove_sch("SGAC")%>%
      clean_agency()%>%
    dplyr::filter(stringr::str_detect(operatingunit, "Region")) %>% 
    label_aggregation ("Regional")%>%
      group_by(operatingunit,funding_agency,fiscal_year, program) %>% 
      #group_by(country, mech_code, mech_name, primepartner, fiscal_year, `Program Area: Sub Program Area-Service Level`,`Beneficiary-Sub Beneficiary`)%>%
      summarise_at(vars(cop_budget_total, expenditure_amt), sum, na.rm = TRUE) %>% 
      ungroup()%>%
      filter(program %in% progs)
    
    
    # MUNGE MSD ============================================================================
    
    df_msd<-df_msd%>%
      filter(standardizeddisaggregate=="Total Numerator")%>%
      filter(indicator %in% indics)%>%
      clean_agency()%>%
  dplyr::filter(stringr::str_detect(operatingunit, "Region")) %>% 
  label_aggregation ("Regional")%>%
      #dplyr::select(operatingunit,fundingagency,fiscal_year, mech_code, mech_name, primepartner,indicator cumulative,targets)%>%
      group_by(operatingunit,funding_agency,fiscal_year,indicator) %>% 
      #group_by(country, mech_code, mech_name, primepartner, fiscal_year, `Program Area: Sub Program Area-Service Level`,`Beneficiary-Sub Beneficiary`)%>%
      summarise_at(vars(cumulative,targets), sum, na.rm = TRUE) %>% 
      ungroup()%>%
      
      dplyr::mutate(program = dplyr::case_when(indicator    == "TX_CURR"    ~"C&T", 
                                               indicator    == "TX_NEW"    ~"C&T",
                                               indicator =="HTS_TST" ~"HTS",
                                               indicator == "HTS_TST_POS" ~"HTS",
                                               
                                               TRUE ~indicator))
      df_msd<-df_msd%>%
      filter(!funding_agency=="DEDUP")%>%
      dplyr::filter(targets>0)
    #  %>% pivot_wider(names_from = program,
    #               values_from=cumulative)
    
    
    
    
    
    
      #join datasets together 
      df_ue<-left_join(df_fsd,df_msd)
      
      df_ue<-df_ue%>%
        mutate( funding_agency = fct_relevel(funding_agency, "USAID","CDC"))%>%
        group_by(operatingunit,funding_agency,fiscal_year,indicator)%>%
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
        filter(fiscal_year=="2023")
      df_ue<-df_ue%>%select(operatingunit,funding_agency,program, indicator, unit_expenditure, cumulative)%>%
        pivot_wider(names_from =indicator,
                    values_from=cumulative:unit_expenditure)
      
      
      
      df_ue<-df_ue%>%
        select(operatingunit,funding_agency,cumulative_HTS_TST,cumulative_HTS_TST_POS,
               cumulative_TX_CURR,cumulative_TX_NEW,
               unit_expenditure_HTS_TST,unit_expenditure_HTS_TST_POS,
               unit_expenditure_TX_CURR,unit_expenditure_TX_NEW,)
      
      
      df_ue<-df_ue%>%     group_by(operatingunit,funding_agency,)%>%
        summarise_at(vars(cumulative_HTS_TST: unit_expenditure_TX_NEW  ), sum, na.rm = TRUE)
      #df_ue<-df_ue%>%    pivot_longer(unit_expenditure_HTS_TST:unit_expenditure_TX_NEW,
      #          names_to ="UE",
      #         values_to="value")%>%
      #    pivot_longer(cumulative_HTS_TST:cumulative_TX_NEW,
      #                names_to ="Results",
      #               values_to="total")%>%
      #df_ue<-df_ue%>% mutate_at(vars(total,value),~replace_na(.,0))%>%
      #  filter(total>0 & value>0)%>%
      # df_ue<-df_ue%>%     group_by(mech_code, mech_name, primepartner,countryname, UE, Results)%>%
      #  summarise_at(vars(value,total), sum, na.rm = TRUE)%>%
      # pivot_wider(names_from =UE,
      #            values_from=value)%>%
      
      
      # df_ue<-df_ue%>%
      
      #dplyr::relocate(prime_mech, .before = cumulative_HTS_TST)
      df_ue<-df_ue%>%
        dplyr::relocate(unit_expenditure_HTS_TST , .before = cumulative_HTS_TST)%>%
        dplyr::relocate( unit_expenditure_HTS_TST_POS, .before = cumulative_HTS_TST_POS)
      df_ue<-df_ue%>%
        dplyr::relocate(unit_expenditure_TX_CURR, .before = cumulative_TX_CURR)%>%
        dplyr::relocate(unit_expenditure_TX_NEW, .before = cumulative_TX_NEW)
      df_ue<-df_ue%>%  
        dplyr::rename("cumulative_TST_POS"="cumulative_HTS_TST_POS")%>%
        dplyr::rename("unit_expenditure_TST_POS"="unit_expenditure_HTS_TST_POS")%>%
        ungroup
      
      df_ue<-df_ue%>%
        filter(unit_expenditure_HTS_TST>0 | cumulative_HTS_TST> 0 |unit_expenditure_TST_POS>0 |cumulative_TST_POS>0
               |unit_expenditure_TX_CURR>0 |cumulative_TX_CURR >0 |unit_expenditure_TX_NEW>0| cumulative_TX_NEW  >0)
      
      # gt function ============================================================================
      country_list_ue<-df_ue%>%
        dplyr::distinct(operatingunit)%>%
        pull()
      
      #   
      get_ue_ou<-function(df, ou="operatingunit"){
        df<-df_ue%>%
          filter(operatingunit %in% ou)%>%
          # filter(fiscal_year=="2020")%>%
          #filter(TX_CURR!="NA")%>%
          gt(
            groupname_col = "funding_agency"
            
          )%>%
          cols_hide(
            columns = c(
              operatingunit
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
            starts_with("funding") ~ px(140),
            everything() ~ px(80)
          )%>%
          cols_label( #update GT to check on tidy select), also look at clean_names, also potentially case_when
            #prime_mech = "Mechanism",
            unit_expenditure_HTS_TST="TST UE",
            unit_expenditure_TST_POS="TST POS UE",
            unit_expenditure_TX_CURR="TX CURR UE",
            unit_expenditure_TX_NEW="TX NEW UE",
            cumulative_HTS_TST ="TST Results",
            cumulative_TST_POS ="TST POS Results",
            cumulative_TX_CURR ="TX CURR Results",
            cumulative_TX_NEW ="TX NEW Results",
            funding_agency="Funding Agency"
            
          )%>%
          
          tab_style(
            style = cell_borders(
              sides = "all",
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
          cols_align(
            align = "left",
            columns = 1)%>%
          tab_header(
            title = ("  COP22 Unit Expenditure: Treatment Cascade"),
            subtitle = glue::glue("Operating Unit: {ou}"))%>%
          gt::tab_source_note(
            source_note = gt::md(glue::glue("**Source**: {source} | Please reach out to oha.ea@usaid.gov for questions."))
          )%>%
          
          tab_footnote(
            footnote = md( "A unit expenditure (UE) is a calculation of partner-level expenditures for a given program area (source: ER) divided by the number of associated beneficiaries (source: MER). Total IM-level expenditure within a program area, divided by IM-specific result value.  Can only be calculated for mechanisms that have both expenditures and results within a given program area. It can be interpreted as the spend per beneficiary reached with those resources. **UEs across partners should be interpreted within the programmatic context, as there are differences in factors such as scope, funding profile, and geography.**"),
            locations = cells_column_labels(
              columns =c(unit_expenditure_HTS_TST)))%>%
          tab_footnote(
            footnote = md( "Excluding Commodities"),
            locations = cells_column_labels(
              columns =c(unit_expenditure_HTS_TST)))%>%
          
          gt::tab_options(
            source_notes.font.size = 8,
            table.font.size = 13, 
            data_row.padding = gt::px(5),
            source_notes.padding = gt::px(1),)%>%
          
          opt_row_striping()
        return(df)
      }

# Output ============================================================================
    table_out<-"GitHub/stacks-of-hondos/Images"
    #to run for one OU, be sure to change the ou to the ou name
    get_ue_ou(df_ue, "Democratic Republic of the Congo")%>%
      gtsave(.,path=table_out,filename = glue::glue("global_unit_expenditure.png"))
    
    #to run for all OUs.You can also run for country use country_list in place of ou_list
    purrr::map(country_list_ue, ~get_ue_ou(df_ue, ou = .x)%>%
                 gtsave(.,path=table_out,filename = glue::glue("{.x}_agency_unit_expenditure.png")))
   
