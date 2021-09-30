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
    #add to separate function file
    dplyr::mutate(`agency_category` = `fundingagency`)%>%
    dplyr::mutate(`agency_category` = ifelse(`agency_category` == "USAID", "USAID",
                                      ifelse(`agency_category` == "CDC", "CDC",
                                             ifelse(`agency_category` =="Dedup", "Dedup","Other"))))
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


