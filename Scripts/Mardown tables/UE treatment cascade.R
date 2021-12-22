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

df_msd<-si_path()%>%
  return_latest("OU_IM")%>%
  gophr::read_msd()

#This function can be used to generate unit expenditure tables across the treatment continuum
#It can be used to generate one table for one ou, or a batch for all ous.
#Be sure to load the following source files below before running
source("~/GitHub/stacks-of-hondos/scripts/utilities.R")
    
    indics<-c("HTS_TST","HTS_TST_POS", "TX_CURR", "TX_NEW")
    progs<-c("HTS", "C&T")



    # MUNGE FSD ============================================================================
    
    df_fsd<-df_fsd%>%
      remove_mo()%>%
      clean_agency()%>%
      group_by(operatingunit,fundingagency,fiscal_year, mech_code, mech_name, primepartner, program) %>% 
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
      group_by(operatingunit,fundingagency,fiscal_year, mech_code, mech_name, primepartner,indicator) %>% 
      #group_by(country, mech_code, mech_name, primepartner, fiscal_year, `Program Area: Sub Program Area-Service Level`,`Beneficiary-Sub Beneficiary`)%>%
      summarise_at(vars(cumulative,targets), sum, na.rm = TRUE) %>% 
      ungroup()%>%
      
      dplyr::mutate(program = dplyr::case_when(indicator    == "TX_CURR"    ~"C&T", 
                                               indicator    == "TX_NEW"    ~"C&T",
                                               indicator =="HTS_TST" ~"HTS",
                                               indicator == "HTS_TST_POS" ~"HTS",
                                               
                                               TRUE ~indicator))
      df_msd<-df_msd%>%
      filter(!fundingagency=="DEDUP")%>%
      dplyr::filter(targets>0)
    #  %>% pivot_wider(names_from = program,
    #               values_from=cumulative)
    
    
    
    
    
    
    #join datasets together 
    df_ue<-left_join(df_fsd,df_msd)
    
    df_ue<-df_ue%>%
      mutate( fundingagency = fct_relevel(fundingagency, "USAID","CDC"))%>%
      group_by(operatingunit,fundingagency,fiscal_year, mech_code, mech_name, primepartner,indicator)%>%
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
    df_ue<-df_ue%>%select(operatingunit,fundingagency,mech_code, mech_name, primepartner,program, indicator, unit_expenditure, cumulative)%>%
      pivot_wider(names_from =indicator,
                  values_from=cumulative:unit_expenditure)
    
    
    
    df_ue<-df_ue%>%
      select(operatingunit,fundingagency,mech_code, mech_name, primepartner,cumulative_HTS_TST,cumulative_HTS_TST_POS,
             cumulative_TX_CURR,cumulative_TX_NEW,
             unit_expenditure_HTS_TST,unit_expenditure_HTS_TST_POS,
             unit_expenditure_TX_CURR,unit_expenditure_TX_NEW,)
    
    
    df_ue<-df_ue%>%     group_by(operatingunit,fundingagency,mech_code, mech_name, primepartner,)%>%
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
    df_ue<-df_ue%>%
      dplyr::mutate(primepartner = dplyr::case_when(primepartner    == "FHI Development 360 LLC"    ~"FHI360",
                                                    primepartner    ==   "Family Health International"    ~"FHI360",
                                                    primepartner    ==  "Abt Associates, Inc." ~ "Abt Associates Inc",
                                                    
                                                    TRUE ~primepartner))%>%
      mutate("prime_mech"=glue("{primepartner}- {mech_code}"))
      
      df_ue<-df_ue%>%
      dplyr::relocate(prime_mech, .before = cumulative_HTS_TST)
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
  
#  function ============================================================================

  #   
    get_ue<-function(df_ue, ou="operatingunit"){
    df_ue<-df_ue%>%
      filter(operatingunit %in% ou)
     # filter(fiscal_year=="2020")%>%
      #filter(TX_CURR!="NA")%>%
 return(df)
    }
