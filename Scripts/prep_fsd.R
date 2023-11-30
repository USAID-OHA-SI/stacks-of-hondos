

#use this function to read to clean and read in financial dataset
prep_fsd <-function(df){ 
  
  #removing
  df<-df %>% 
    dplyr::select(-c("prime_partner_duns","prime_partner_uei","prime_partner_org_type",
                                  "is_indigenous_prime_partner", "subrecipient_duns",
                                  "award_number","procurement_type")) %>% 
    # filter out M&O
    remove_mo()%>% #gophr::remove_mo
    
  
 
  ##concatenate mech id and mech name
  dplyr::mutate( mech_id_mech_name = paste(mech_code,"-", mech_name))%>%
  
  #mutate data type double into integer to have round numbers
  #dplyr::mutate_if(is.double, as.integer)%>%
  dplyr::mutate(across(where(is.double), round, 0)) %>% 
  
  #drop NA for numeric amounts
  mutate_at(vars(cop_budget_new_funding:expenditure_amt),~replace_na(.,0))%>%
  
  
  #recode values to match naming in Financial Integrated Dataset
    dplyr::mutate(`interaction_type`= recode (`interaction_type`, "Service Delivery"= "SD",
                                              "Non Service Delivery"= "NSD"))%>%
    dplyr::mutate(`interaction_type`  = dplyr::case_when(program     == "PM"    ~"PM",
                                                         TRUE ~`interaction_type`))%>%
  
  #Add in agency category column to group agencies
  
    clean_agency()%>% #gophr::clean_agency()
    agency_category()%>%
   
  #mutating & calculating budget execution
  group_by(operatingunit, country, funding_agency, agency_category, fiscal_year,prime_partner_name,mech_id_mech_name,program, interaction_type) %>% 
    summarise_at(vars(cop_budget_total, expenditure_amt), sum, na.rm = TRUE) %>% 
  ungroup()
  

 
  return(df)  
  
}


