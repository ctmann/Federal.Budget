# SourceNotes -------------------------------------------------------------
#' Download and compile all versions of the OMB 'Public Database' from the GPO website:
#' https://www.gpo.gov/fdsys/browse/collectionGPO.action?collectionCode=BUDGET
#' 
#' #' Source Notes 
#' The Governnment Publishing Office (GPO) keeps an archive of Presidential
#' Budget Requests. The 'Public Database' (xls, .csv files) are available beginning
#' in FY1998.
#' 
#' Inconsistent Archive Medium
#' FY1998-FY2001 are stored as text or '.htm'
#' FY2002 is stred as text or xls
#' FY2003 and later are stored as .csv or xls
#' 
#' Inconsistent Data
#' -FY2003-FY2007 Have no column headers
#' -Span of years may be different for each type
#' 
#' GPO uses this format:
#' https://www.gpo.gov/fdsys/pkg/BUDGET-2003-DB/csv/BUDGET-2003-DB-1.csv
#' 
#' DB-1: Budget Authority
#' DB-2: Outlays
#' DB-3: Receipts
#' 

library(tidyverse)
library(readxl)
library(janitor)
library(friendlyeval)
library(curl)
library(feather)



# How to Update this File -------------------------------------------------

  to.year   <- 2019 # Update to current year. 
  
  # (Creates filenames, colnames, and adjusts for most current deflator.)
 

# Common Vars -------------------------------------------------------------
  #Select OMB datasets to download (consistent data begins in 2008)
  from.year <- 2008 # Don't change this! Consistent OMB data begins in 2008
  span.years <- (to.year - from.year)+1
  
# Deflator ----------------------------------------------------------------
#' Table 10.1 - Gross Domestic Product and Deflators Used in the Historical Tables: 1940–...
#'   OMB used FY2009 as base Year
#'   
#'   Inconsistent archive medium
#'   - FY2019 .xlsx, prior years .xls
#'   
#=#=#=#=#= Download File from GPO #=#=#=#=#=#=
  
#- Adjust filetype, by year
file.type.10.1 <- if_else(to.year>2018, ".xlsx", ".xls")
  
# Download
tbl.10.1_gdp.deflator <- sprintf("https://www.gpo.gov/fdsys/pkg/BUDGET-%s-TAB/xls/BUDGET-%s-TAB-11-1%s", to.year,to.year,file.type.10.1)
url <- tbl.10.1_gdp.deflator
destfile <- paste0("BUDGET", file.type.10.1)
curl_download(url, destfile)

gdp.deflator <- read_excel(destfile, 
    col_types = c("text", "skip", "numeric", 
        "skip", "skip", "skip", "skip", 
        "skip", "skip", "skip", "skip", 
        "skip", "skip", "skip", "skip", 
        "skip"), skip = 2) %>% 
  rename(FY = 1, deflator.index.gdp.2009 = 2) %>% 
  filter(!(is.na(FY)|                           #eliminate NAs in FY col
           is.na(deflator.index.gdp.2009))) %>% #eliminate NAs in deflator col
  mutate(FY = str_remove(FY, "[^0-9].+" ) %>% parse_integer )     #remove 'estimate' from FY col

# Create Current Year Index (based on 'to.year')
current.year.index <- gdp.deflator %>% 
  filter(FY %in% to.year) %>%  
  select(deflator.index.gdp.2009) %>% pull()

current.delator.name.index <- paste0("deflator.index.gdp.", to.year ) 

gdp.deflator <- gdp.deflator %>% 
  mutate( !!treat_input_as_col(current.delator.name.index) :=  deflator.index.gdp.2009/current.year.index) 

# Import Main Datasets------------------------------------------------------------------


##=#=#=#=#=#= Download all public databases from GPO during (from-to span) #=#=#=#=#=#=
   # - Each 'database' identified by its original FY.base
   #   Ex. FY2019 public database is base.year 2019
   #       FY2018 public database is base.year 2018

  # Create table of links for download
  omb <- tibble(
    base.year = rep( c(from.year:to.year), 2) ,
    budget.type = c(rep("budget.authority", span.years) , 
                    rep("outlays", span.years)) , 
    hyperlink = c(sprintf("https://www.gpo.gov/fdsys/pkg/BUDGET-%s-DB/csv/BUDGET-%s-DB-1.csv", c(from.year:to.year),c(from.year:to.year)),
             sprintf("https://www.gpo.gov/fdsys/pkg/BUDGET-%s-DB/csv/BUDGET-%s-DB-2.csv", c(from.year:to.year), c(from.year:to.year))) )
  
#=#=#=#=#=#= Download Budget Authority #=#=#=#=#=#=#=#=#=#=#=#=
#' budauth datasets are shaped differently than outlays

omb.budauth.1 <- omb %>% filter(budget.type %in% "budget.authority")

  # Download budauths
  omb.budauth.2 <-  omb.budauth.1 %>% 
    mutate(bob = map(.x = hyperlink, 
                     ~(.x %>% 
                         read_csv(col_types = (cols(.default="c"))) %>% 
                         clean_names() ) ) )
  
  omb.budauth.3 <- omb.budauth.2 %>% unnest() %>% 
    select(-x1976, -tq, -x1977) %>%    # unneeded years
    # Tidy
    gather(FY, amount, -base.year:-on_or_off_budget)  %>% 
    mutate(amount = parse_number(amount)*1e3 , #<Adjust for thousands of dollars
           FY = str_remove(FY, "x"), 
           subfunction_code = str_pad(subfunction_code, width=3,  side="left", pad="0") )  %>% 
    filter(!FY %in% c("tq", "1976", "1977"))  %>% #<Remove tq, 1976, 1977)
    mutate(FY = parse_integer(FY)) %>% 
    mutate(amount = replace_na(amount, 0) ) #<Remove damnable NAs not parsed
  
# Enrich dataset --------------------------------------------------

#=#=#=#=#=#=#=#=#= Add Explanatory data #=#=#=#=#=#=#=#=#=
  
  # Function Codes, Function Titles
  budget.functions <- read_csv("./Shared_Data/Budget_Functions.csv",
                                      col_types = (cols(.default="c") )) %>% clean_names() %>% 
    mutate_at(vars(contains("code")), str_pad, width=3, side="left", pad="0")   
  
  omb.budauth.4 <- left_join(omb.budauth.3, budget.functions)
  
  # Add meta-identifiers
  omb.budauth.4 <- omb.budauth.4 %>% 
      #> FYDP
    mutate(FYDP.yes.or.no = if_else(FY >= base.year, "yes", "no"),
      #>  Defense (based on Function 050)
           national.defense.yes.or.no = if_else(function_code %in% "050", "yes", "no") )
  

#=#= Deflators #=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
omb.budauth.5 <- left_join(omb.budauth.4, gdp.deflator)

# current deflator index is called 'current.deflator.name.index'
# current delated amount will be called 'current.deflator.name.amount'
# names vary according to year selected ('to.year')
current.deflator.name.amount <- paste0("amount.deflated.gdp.", to.year) 

omb.budauth.6 <- omb.budauth.5 %>% 
  mutate(amount.deflated.gdp.2009 = amount * deflator.index.gdp.2009,
         !!treat_input_as_col(current.deflator.name.amount) :=  amount * !!treat_input_as_col(current.delator.name.index)) 
                                                                          # 'deflator name based on current FY

# Export ------------------------------------------------------------------

# Final reordering
omb.budauth.7 <- omb.budauth.6 %>% 
  select(
      budget.type,               
      base.year,                 
      function_code,             
      function_title,        
      subfunction_code,          
      subfunction_title,         
      agency_code,               
      agency_name,               
      bureau_code,               
      bureau_name,               
      account_code,              
      account_name,              
      treasury_agency_code,      
      bea_category,              
      on_or_off_budget,      
      FYDP.yes.or.no,            
      national.defense.yes.or.no,          
      FY,                        
      amount,                    
      deflator.index.gdp.2009,   
      amount.deflated.gdp.2009,  
      deflator.index.gdp.2019,   
      amount.deflated.gdp.2019,  
      hyperlink,
      everything()) #<Just in case future fields are added                

# Tedious, but necessary
# my.data.folder.location <- paste0(getwd(), "/Compilation/Data/Processed")
#  my.data.folder.location <- paste0(getwd(), "/Compilation/Data/Processed/")
#  my.table.name <- paste0("omb.budauth.", from.year, ".to.", to.year)
#  my.timestamp <- paste('Updated', format(Sys.time(), format = ".%Y-%m-%d.%H%M") , sep = "")
#  my.file.name <-  sprintf("%s/%s_%s.csv", my.data.folder.location, my.table.name, my.timestamp)
# 
#  write_csv(omb.budauth.7, my.file.name)
# 
# my.file.name <-  sprintf("%s/%s_%s.feather", my.data.folder.location, my.table.name, my.timestamp)
# write_feather(omb.budauth.7, my.file.name)








