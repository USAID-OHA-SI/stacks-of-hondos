library(gophr)
library(glamr)
library(stringr)

present_fy <- function(df){ 
  fy <- glamr::source_info(return="fiscal_year")%>%
    as.numeric()
  return(fy)
}
present_fy<-present_fy(df_fsd)
past_fy<-function(df){
  fy <- glamr::source_info(return="fiscal_year")%>%
  as.numeric()-1
return(fy)
}

present<-create_fy(df_fsd)

df_fsd<-si_path()%>%
  return_latest("Fin")%>%
  gophr::read_msd()%>%
  glamr::source_info( return="fiscal_year")

fy <- glamr::source_info(return="fiscal_year")