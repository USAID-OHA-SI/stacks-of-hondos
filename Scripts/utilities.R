#clean_sch to help decide if you want commodities or don't
#select either SGAC list, SCH list, or none (keep commodities)

clean_sch <- function(df, poc= c("SCH","SGAC","none")){
  
  
  if ( !googlesheets4::gs4_has_token())
    stop("Function requires authentication,
         use googlesheets4::gs4_auth() or glamr::load_secrets()")
  
  
  sheet_id <- googlesheets4::as_sheets_id('1mCJWDo4FPW2cQ6LpbsSjtnRjT7sUpPEOqxfT2zQNo64')
  
  suppressMessages(
    df_check <- googlesheets4::read_sheet(sheet_id, "Cross check SGAC-SCGH")
  )
  
  if(poc == "SCH"){
    lst_mech <- df_check%>%
      dplyr::filter(POC %in% poc)%>%
      dplyr::mutate(mech_id = as.character(`Mech ID`))%>%
      dplyr::distinct(mech_id)%>%
      dplyr::pull(mech_id)
    
    df <- dplyr::filter(df, !mech_code %in% lst_mech)
    
    return (df)
    
    
  }
  
  if(poc == "SGAC"){
    lst_mech <- df_check%>%
      dplyr::filter(POC %in% poc)%>%
      dplyr::mutate(mech_id = as.character(`Mech ID`))%>%
      dplyr::distinct(mech_id)%>%
      dplyr::pull(mech_id)
    
    df <- dplyr::filter(df, !mech_code %in% lst_mech)
    return (df)
  } 
  
  if(poc == "none"){
    return(df)
  }  
  
  
  return(df)  
  
}


#add note for whether it includes commodities or not
note<-function(poc= c("SCH","SGAC","none")){
  
  
  
  if(poc == "SCH"){
    note<-"Excludes Commodites"
    return(note)
  }
  if(poc == "SGAC"){
    note<-"Excludes Commodites"
    return(note)
  }
  
  if(poc == "None"){
    note<-"Including Commodites"
    return(note)
  }
}

#add in agency category column
agency_category<-function(df){
  df<-df%>%
  glamr::clean_agency()%>%
     dplyr::mutate(fundingagency  = dplyr::case_when(fundingagency    == "WCF"    ~"USAID", #fix Kosovo
                                                 
                                                 TRUE ~fundingagency))%>%
   
    dplyr::mutate(`agency_category` = `fundingagency`)%>%
    dplyr::mutate(`agency_category` = ifelse(`agency_category` == "USAID", "USAID",
                                      ifelse(`agency_category` == "CDC", "CDC",
                                             ifelse(`agency_category` == "WCF", "USAID",
                                             ifelse(`agency_category` =="Dedup", "Dedup","Other")))))%>%
    mutate( agency_category = fct_relevel(agency_category, "USAID","CDC","Other"))
  return(df)
  
  
}


#helper for INF for removing Inf from Budget execution
percent_clean <- function(x, y) {
  ifelse(y > 0.000, (x / y), NA_real_)
}

pd <- si_path()%>%
  glamr::return_latest("Fin")%>%
  glamr::source_info(return="fiscal_year")
fy_end <- pd %>% substr(3, 4) %>% as.numeric() + 2000
fy_beg <- fy_end - 1 
max_pd <- pd
min_pd <- pd -1

#source<-"FY21Q4i FSD"
source<-source_info(si_path(),"Fin")
legend_be<-'https://user-images.githubusercontent.com/5873344/136249989-046c8107-706f-42cf-be5e-1dfb15e29093.png?raw=true'
legend_chunk <- gt::md(glue::glue("Legend: Budget Execution <img src= '{legend_be}' style='height:15px;'>"))

# dataframe used to generate ou_list, country_list, country_list_regionals, etc.
df_for_lsts <- si_path()%>%
  return_latest("COP17")%>%
  gophr::read_msd()

ou_list<- df_for_lsts%>%
  distinct(operatingunit)%>%
  pull()

country_list<-df_for_lsts%>%
  distinct(countryname)%>%
  pull()

#filter for country list as regionals but remove WAR-WAR and Benin which has no funding for now
country_list_regionals<-df_for_lsts%>%
  dplyr::filter(operatingunit=="Asia Region" | operatingunit=="Western Hemisphere Region" |operatingunit=="West Africa Region")%>%
  dplyr::mutate(agg_type = "Region-Country",
                operatingunit = paste(operatingunit, countryname, sep = "-")) %>% 
  distinct(operatingunit)%>%
  filter(!operatingunit=="West Africa Region-West Africa Region")%>%
  filter(!operatingunit=="West Africa Region-Benin")%>%
  filter(!operatingunit=="West Africa Region-Sierra Leone")%>%
  filter(!operatingunit=="Asia Region-Central Asia Region")%>%
  filter(!operatingunit=="Asia Region-Asia Regional Program")%>%
  filter(!operatingunit=="Western Hemisphere Region-Caribbean Region")%>%
  filter(!operatingunit=="Western Hemisphere Region-Central America Region")%>%
  #%>%
  pull()

lts_countries<-df_for_lsts%>%
  distinct(operatingunit)%>%
  dplyr::filter(!operatingunit=="Asia Region")%>%
  dplyr::filter(!operatingunit=="Western Hemisphere Region")%>%
  dplyr::filter(!operatingunit=="Angola")%>%
  dplyr::filter(!operatingunit=="West Africa Region")%>%
  dplyr::filter(!operatingunit=="Dominican Republic")%>%
  pull()

# Delete df_for_lsts from memory to improve performance
rm(df_for_lsts)
gc()

#Use this function to concatenate the regional countries into one column (i.e. Asia Region-Laos)
#In order to  create regional views
label_aggregation <- function(df, type = "OU") {
  
  if(!type %in% c("Regional", "OU")) {
    stop("Please select the type of aggregation label to apply: OU or Regional ")
  }  
  
  
 if (type == "Regional") {
    df %>% 
      dplyr::mutate(agg_type = "Region-Country",
                    operatingunit = paste(operatingunit, countryname, sep = "-")) %>% 
      dplyr::select(-countryname)
  } else {
    df %>% dplyr::mutate(agg_type = "OU")
  }
}
