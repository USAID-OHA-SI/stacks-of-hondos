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
    }
    
  if(poc == "SGAC"){
    lst_mech <- df_check%>%
      dplyr::filter(POC %in% poc)%>%
      dplyr::mutate(mech_id = as.character(`Mech ID`))%>%
      dplyr::distinct(mech_id)%>%
      dplyr::pull(mech_id)
    
    df <- dplyr::filter(df, !mech_code %in% lst_mech)
  } 
  
  if(poc == "none"){
    return(df)
  }  
        

return(df)  
  
}