# PURPOSE: Supply Chain Exclusions List
# AUTHOR: Ben Kasdan | SIEI
# LICENSE: MIT
# DATE: 2021-07-19
# NOTES: 
#The below functions create 3 lists to reflect 3 ways of filtering SCH data:
#1. The SGAC list-based on PET system data
#2. USAID's SCH team list of what they consider commodities mechanisms
#3. The combination of the SGAC and sCH list
# LOCALS & SETUP ============================================================================

  # Libraries
    library(glitr)
    library(glamr)
    library(gisr)
    library(Wavelength)
    library(ICPIutilities)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(here)
    library(gt)
    library(googlesheets4)
    library(gargle)
    
   load_secrets() 
   
  
  # Set paths  
   sheet_id<-'1mCJWDo4FPW2cQ6LpbsSjtnRjT7sUpPEOqxfT2zQNo64'
   df <- read_sheet(as_sheets_id(sheet_id), "Cross check SGAC-SCGH")
    
  # Functions  
    #SGAC list. SGAC PET uses this list to filter on mechanism in Panorama
    SGAC_list<-df%>%
      dplyr::filter(POC=="SGAC")%>%
      pull(`Mech ID`)
    
    #SCH team list. The SCH team maintain their own list of 96 mechanisms that they consider
    #supply chain mechanisms for filtering 
    SCH_list<-df%>%
      dplyr::filter(POC=="SCH")%>%
      pull(`Mech ID`)
    
    #combined list of the SGAC and SCH list with duplicates removed
    Combined_SCH<-df%>%
      dplyr::select(-(POC))%>%
      dplyr::distinct(`Mech ID`)%>%
      pull(`Mech ID`)

