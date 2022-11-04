library(glamr)
library(tidyverse)
library(gophr)
library(extrafont)
library(tidytext)
library(gt)
library(glue)
library(webshot)
library(janitor)


df_fsd<-si_path()%>%
  return_latest("Fin")%>%
  gophr::read_msd()

df_msd<-si_path()%>%
  return_latest("OU_IM")%>%
  gophr::read_msd()

#This function can be used to generate unit expenditure tables across the treatment continuum
#It can be used to generate one table for one ou, or a batch for all ous.
#Be sure to load the following source files below before running
source("~/GitHub/stacks-of-hondos/Scripts/utilities.R")

indics<-c("OVC_SERV")
progs<-c("OVC")




# MUNGE FSD ============================================================================

df_kp<-df_fsd%>%
  remove_mo()%>%
  clean_agency()%>%
  mutate( fundingagency = fct_relevel(fundingagency, "USAID","CDC"))%>%
  dplyr::mutate(program = dplyr::case_when(sub_program    == "PREV: PrEP"    ~"PrEP", 
                                           
                                           
                                           TRUE ~program))%>%
  dplyr::mutate( mech_id_mech_name = paste(mech_code,"-", mech_name))%>%
  group_by(operatingunit,fundingagency,fiscal_year, mech_id_mech_name, beneficiary,program) %>%
  
  #group_by(country, mech_code, mech_name, primepartner, fiscal_year, `Program Area: Sub Program Area-Service Level`,`Beneficiary-Sub Beneficiary`)%>%
  summarise_at(vars(cop_budget_total, expenditure_amt), sum, na.rm = TRUE) %>% 
  ungroup()%>%
  filter(beneficiary=="Key Pops")%>%
  
  mutate(budget_execution=(expenditure_amt/cop_budget_total))%>%
  filter(fiscal_year=="2021"| fiscal_year=="2020")
df_kp<-df_kp%>%
  pivot_wider(names_from = fiscal_year,values_from = cop_budget_total:budget_execution, values_fill = 0)%>%
  dplyr::relocate(expenditure_amt_2021, .before = cop_budget_total_2021) %>%
  dplyr::relocate(expenditure_amt_2020, .before = cop_budget_total_2020) %>%
  dplyr::relocate(budget_execution_2021, .after = cop_budget_total_2021)%>%
  dplyr::relocate(budget_execution_2020, .after = cop_budget_total_2020) %>%
  dplyr::filter(budget_execution_2021>0)

  
  
  kp_example_1<-df%>% 
    filter(operatingunit=="Malawi")%>%
    select(fundingagency,mech_id_mech_name,program:budget_execution_2021)%>%
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
    fmt_missing(columns = everything(),
                missing_text = "-") %>%
    cols_width(
      everything() ~ px(90))%>%
    
    tab_style(
      style = cell_borders(
        sides = "all",
        weight = px(1),
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
      label = glue("COP20 Performance"),
      columns = tidyselect::matches(as.character("2021")))%>%
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
      budget_execution_2021="Budget Execution",
      program ="Program Area",
      mech_id_mech_name="Mechanism",
      fundingagency="Agency"
      

    )%>%
    cols_align(
      align = "left",
      columns = 1
    )


# MUNGE MSD ============================================================================
progs<-c("HTS","PrEP","PREV","C&T")
df<-df_msd%>%
  filter(standardizeddisaggregate=="Total Numerator")%>%
  # filter(indicator %in% indics)%>%
    # rename(funding_agency="fundingagency")
  # clean_agency()%>%
  # mutate( funding_agency = fct_relevel(funding_agency, "USAID","CDC"))%>%
  #dplyr::select(operatingunit,fundingagency,fiscal_year, mech_code, mech_name, primepartner,indicator cumulative,targets)%>%
    dplyr::mutate( mech_id_mech_name = paste(mech_code,"-", mech_name))%>%
     group_by(operatingunit,funding_agency,fiscal_year, mech_id_mech_name,indicator) %>% 
  #group_by(country, mech_code, mech_name, primepartner, fiscal_year, `Program Area: Sub Program Area-Service Level`,`Beneficiary-Sub Beneficiary`)%>%
  summarise_at(vars(cumulative,targets), sum, na.rm = TRUE) %>% 
  ungroup()%>%
  
   dplyr::mutate(program = dplyr::case_when(indicator    == "TX_CURR"    ~"C&T",
                                            indicator    == "TX_NEW"    ~"C&T",
                                            indicator    == "HTS_TST"    ~"HTS",
                                            indicator    == "PrEP_NEW"    ~"PrEP",
                                            indicator    == "KP_PREV"    ~"PREV",
                                           
                                            
                                            TRUE ~indicator))
df<-df%>%
  filter(!funding_agency=="DEDUP")%>%
  dplyr::filter(targets>0)%>%
  mutate(achievement=round (cumulative/targets*100))%>%
    filter(fiscal_year=="2021")%>%
  filter(program %in% progs)%>%


  select(operatingunit,funding_agency,fiscal_year,mech_id_mech_name,indicator,achievement,program)%>%
   pivot_wider(names_from = indicator,
               values_from=achievement)






#join datasets together 
df_ue<-left_join(df_kp,df)%>%
  mutate_at(vars(cop_budget_total: OVC_SERV),~replace_na(.,0))%>%
  mutate(mech=glue("{mech_code}-{mech_name}"))%>%
  relocate(expenditure_amt, .before= cop_budget_total)%>%
  relocate(mech, .before= expenditure_amt)%>%
  filter(fiscal_year=="2021")%>%
  filter(operatingunit %in% ou)%>%
  filter(OVC_SERV>0
         |budget_execution>0)%>%
  select(fundingagency,mech:OVC_SERV)

##kp mechs with types=====

df_kp<-df_fsd%>%
  remove_mo()%>%
  clean_agency()%>%
  mutate( fundingagency = fct_relevel(fundingagency, "USAID","CDC"))%>%
  dplyr::mutate(program = dplyr::case_when(sub_program    == "PREV: PrEP"    ~"PrEP", 
                                           
                                           
                                           TRUE ~program))%>%
  dplyr::mutate( mech_id_mech_name = paste(mech_code,"-", mech_name))%>%
  filter(beneficiary=="Key Pops")%>%
  group_by(operatingunit,fundingagency,fiscal_year, mech_id_mech_name,mech_code, beneficiary,program) %>%
  
  #group_by(country, mech_code, mech_name, primepartner, fiscal_year, `Program Area: Sub Program Area-Service Level`,`Beneficiary-Sub Beneficiary`)%>%
  summarise_at(vars(cop_budget_total, expenditure_amt), sum, na.rm = TRUE) %>% 
  ungroup()%>%
  
  
  mutate(budget_execution=(expenditure_amt/cop_budget_total))%>%
  filter(fiscal_year=="2021")
df_kp<-df_kp%>%
  pivot_wider(names_from = fiscal_year,values_from = cop_budget_total:budget_execution, values_fill = 0)

df_kp<-df_kp%>%
  dplyr::relocate(budget_execution_2021, .after=cop_budget_total_2020)%>%
  dplyr::relocate(expenditure_amt_2021, .before = cop_budget_total_2021) %>%
  dplyr::relocate(expenditure_amt_2020, .before = cop_budget_total_2020) %>%
  dplyr::relocate(budget_execution_2021, .after = cop_budget_total_2021)%>%
  dplyr::relocate(budget_execution_2020, .after = cop_budget_total_2020) %>%
  dplyr::filter(budget_execution_2021>0)
  
sheet_id <-'1G3K5rXeU54FN54FehPa6LCsJLnAx0rD5aVtrJ5ys-hw'

df_Partners<- googlesheets4::read_sheet(googlesheets4::as_sheets_id(sheet_id))%>%
  dplyr::mutate(`mech_code`=as.character(`mech_code`)) %>%
  clean_names()%>%
  select(focus,mech_code)

df_kp <- dplyr::left_join(df_kp,df_Partners,by="mech_code")%>%
  rename(Focus=focus)


kp_example_1<-df_kp%>% 
  filter(operatingunit=="Malawi")%>%
  select(Focus,fundingagency,mech_id_mech_name,program:budget_execution_2021)%>%
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
  fmt_missing(columns = everything(),
              missing_text = "-") %>%
  cols_width(
    everything() ~ px(90))%>%
  
  tab_style(
    style = cell_borders(
      sides = "all",
      weight = px(1),
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
    label = glue("COP20 Performance"),
    columns = tidyselect::matches(as.character("2021")))%>%
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
    # Focus= "focus",
    expenditure_amt_2020 = "Expenditure",
    cop_budget_total_2020 = "Budget",
    budget_execution_2020="Budget Execution",
    expenditure_amt_2021 = "Expenditure",
    cop_budget_total_2021 = "Budget",
    budget_execution_2021="Budget Execution",
    program ="Program Area",
    mech_id_mech_name="Mechanism",
    fundingagency="Agency",
  )%>%
  cols_align(
    align = "left",
    columns = 1
  )

##=== KP with KP PREV MER %

df_kp<-df_fsd%>%
  remove_mo()%>%
  clean_agency()%>%
  mutate( fundingagency = fct_relevel(fundingagency, "USAID","CDC"))%>%
  dplyr::mutate(program = dplyr::case_when(sub_program    == "PREV: PrEP"    ~"PrEP", 
                                           
                                           
                                           TRUE ~program))%>%
  dplyr::mutate( mech_id_mech_name = paste(mech_code,"-", mech_name))%>%
  filter(beneficiary=="Key Pops")%>%
  group_by(operatingunit,fundingagency,fiscal_year, mech_id_mech_name,mech_code, beneficiary,) %>%
  
  #group_by(country, mech_code, mech_name, primepartner, fiscal_year, `Program Area: Sub Program Area-Service Level`,`Beneficiary-Sub Beneficiary`)%>%
  summarise_at(vars(cop_budget_total, expenditure_amt), sum, na.rm = TRUE) %>% 
  ungroup()%>%
  
  
  mutate(budget_execution=(expenditure_amt/cop_budget_total))%>%
  filter(fiscal_year=="2021")
df_kp<-df_kp%>%
  pivot_wider(names_from = fiscal_year,values_from = cop_budget_total:budget_execution, values_fill = 0)
df_kp<-df_kp%>%
  dplyr::relocate(expenditure_amt_2021, .before = cop_budget_total_2021)


progs<-c("PREV")
df<-df_msd%>%
  filter(standardizeddisaggregate=="Total Numerator")%>%
  # filter(indicator %in% indics)%>
   rename("fundingagency"=funding_agency)%>%
   #clean_agency()%>%
  # mutate( funding_agency = fct_relevel(funding_agency, "USAID","CDC"))%>%
  #dplyr::select(operatingunit,fundingagency,fiscal_year, mech_code, mech_name, primepartner,indicator cumulative,targets)%>%
  dplyr::mutate( mech_id_mech_name = paste(mech_code,"-", mech_name))%>%
  group_by(operatingunit,fundingagency,fiscal_year, mech_id_mech_name,indicator) %>% 
  #group_by(country, mech_code, mech_name, primepartner, fiscal_year, `Program Area: Sub Program Area-Service Level`,`Beneficiary-Sub Beneficiary`)%>%
  summarise_at(vars(cumulative,targets), sum, na.rm = TRUE) %>% 
  ungroup()%>%
  
  dplyr::mutate(program = dplyr::case_when(indicator    == "TX_CURR"    ~"C&T",
                                           indicator    == "TX_NEW"    ~"C&T",
                                           indicator    == "HTS_TST"    ~"HTS",
                                           indicator    == "PrEP_NEW"    ~"PrEP",
                                           indicator    == "KP_PREV"    ~"PREV",
                                           
                                           
                                           TRUE ~indicator))
df<-df%>%
  filter(!fundingagency=="DEDUP")%>%
  dplyr::filter(targets>0)%>%
  mutate(achievement=round (cumulative/targets*100))%>%
  filter(fiscal_year=="2021")%>%
  filter(program %in% progs)%>%
  
  
  select(operatingunit,fundingagency,,mech_id_mech_name,indicator,achievement,program)%>%
  pivot_wider(names_from = indicator,
              values_from=achievement)






#join datasets together 
df_ue<-left_join(df_kp,df)%>%
  select(operatingunit,fundingagency,mech_id_mech_name, expenditure_amt_2021:budget_execution_2021, KP_PREV)%>%
  mutate_at(vars(expenditure_amt_2021: KP_PREV),~replace_na(.,0))%>%
  # filter(fiscal_year=="2021")%>%
  filter(operatingunit %in% ou)%>%
  filter(KP_PREV>0
         |budget_execution_2021>0)%>%
  rename("KP PREV Achievement"=KP_PREV)
  select(fundingagency,mech:OVC_SERV)

  
 

