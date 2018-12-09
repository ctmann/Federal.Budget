# SourceNotes -------------------------------------------------------------
  # Detailed notes available at GitHub site
  # https://github.com/ctmann/Federal.Budget

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(janitor)
library(friendlyeval)
library(curl)
library(feather)

# How to Update this File -------------------------------------------------
  # 1. Choose 'to.year': All OMB public databases FY2008-to.year will be downloaded and stored in nested df
  # 2. Choose only one budget type:  budget.authority / outlays will be processed and exported as .csv
  # 3. Caution: Sync Github account after each selection, otherwise the .csv upload will exceed limits

  to.year   <- 2019                              # Update to current year. 
  process.this.budget.type <- "budget.authority" # Choose one
  #process.this.budget.type <- "outlays"         # Choose one

# Common Vars -------------------------------------------------------------
  #Select OMB datasets to download (consistent data begins in 2008)
  from.year <- 2008 # Don't change this! Consistent OMB data begins in 2008
  span.years <- (to.year - from.year)+1
  
# Deflator & GDP ----------------------------------------------------------------
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
destfile <- paste0("tbl.10.1_gdp.deflator", file.type.10.1)
curl_download(url, destfile)

#=#=#=#= Deflators #=#=#=#=#=#=
gdp.deflator <- read_excel(destfile, 
    col_types = c("text", "skip", "numeric", 
        "skip", "skip", "skip", "skip", 
        "skip", "skip", "skip", "skip", 
        "skip", "skip", "skip", "skip", 
        "skip"), skip = 2) %>% 
  rename(FY = 1, deflator.index.gdp.2009 = 2) %>% 
  filter(!(is.na(FY)|                           #eliminate NAs in FY col
           is.na(deflator.index.gdp.2009)|      #eliminate NAs in deflator col
           FY %in% "TQ")) %>%                   #eliminate TQ        
  mutate(FY = str_remove(FY, "[^0-9].+" ) %>% parse_integer )     #remove 'estimate' from FY col

# Create Current Year Index (based on 'to.year')
current.year.index <- gdp.deflator %>% 
  filter(FY %in% to.year) %>%  
  select(deflator.index.gdp.2009) %>% pull()

current.delator.name.index <- paste0("deflator.index.gdp.", to.year ) 

gdp.deflator <- gdp.deflator %>% 
  mutate( !!treat_input_as_col(current.delator.name.index) :=  deflator.index.gdp.2009/current.year.index) 

# Import All OMB Data  ------------------------------------------------------------------

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
  
  # Download outlays and budauths as nested tibbles
  omb1 <-  omb %>% 
    mutate(bob = map(.x = hyperlink, 
                     ~(.x %>% 
                         read_csv(col_types = (cols(.default="c"))) %>% 
                         clean_names() ) ) )

# Process by budget.type (budauth or outlays) -------------------------------

omb2 <- omb1 %>% filter(budget.type %in% process.this.budget.type)

  omb3 <- omb2 %>% unnest() %>% 
    select(-tq) %>%    # unneeded years
    # Tidy
    gather(FY, amount, -base.year:-on_or_off_budget)  %>% 
    mutate(amount = parse_number(amount)*1e3 , #<Adjust for thousands of dollars
           FY = str_remove(FY, "x"), 
           subfunction_code = str_pad(subfunction_code, width=3,  side="left", pad="0") )  %>% 
    mutate(FY = parse_integer(FY)) %>% 
    mutate(amount = replace_na(amount, 0) ) #<Remove damnable NAs not parsed
  
# Enrich dataset --------------------------------------------------

#=#=#=#=#=#=#=#=#= Add Explanatory data #=#=#=#=#=#=#=#=#=
  
  # Function Codes, Function Titles
  budget.functions <- read_csv("./Shared_Data/Budget_Functions.csv",
                                      col_types = (cols(.default="c") )) %>% clean_names() %>% 
    mutate_at(vars(contains("code")), str_pad, width=3, side="left", pad="0")   
  
  omb4 <- left_join(omb3, budget.functions)
  
  # Add meta-identifiers
  omb4 <- omb4 %>% 
      #> FYDP
    mutate(FYDP.yes.or.no = ifelse(FY >= base.year, "yes", "no"),
      #>  Defense (based on Function 050)
           national.defense.yes.or.no = ifelse(function_code %in% "050", "yes", "no") )

#=#= Deflators #=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
omb5 <- left_join(omb4, gdp.deflator)

# current deflator index is called 'current.deflator.name.index'
# current delated amount will be called 'current.deflator.name.amount'
# names vary according to year selected ('to.year')
current.deflator.name.amount <- paste0("amount.deflated.gdp.", to.year) 

omb6 <- omb5 %>% 
  mutate(amount.deflated.gdp.2009 = amount / deflator.index.gdp.2009,
         !!treat_input_as_col(current.deflator.name.amount) :=  amount / !!treat_input_as_col(current.delator.name.index)) 
                                                                          # 'deflator name based on current FY
# Export ------------------------------------------------------------------

# Final reordering
omb7 <- omb6 %>% 
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

# Export
my.export.function <- function(df, name.of.file){
 my.data.folder.location <- paste0(getwd(), "/Data/Processed/")
 my.timestamp <- paste('Updated', format(Sys.time(), format = ".%Y-%m-%d.%H%M") , sep = "")
 my.file.name <-  sprintf("%s/%s_%s.csv", my.data.folder.location, name.of.file, my.timestamp)
 write_csv(df, my.file.name)
 }
 
# Most Recent Year, otherwise, file too large for github)
name.of.file <- paste0("omb.", process.this.budget.type, ".FY", to.year)
omb7 %>% 
  filter(base.year %in% to.year) %>% 
  my.export.function(name.of.file)
 
# When writing entire file, use this name:
#my.table.name <- paste0("omb", from.year, ".to.", to.year)


# Secondary Data: GDP -----------------------------------------------------
# Purpose: Extract GDP from downloaded table 10.1

gdp <- read_excel(destfile, 
    col_types = c("text", "numeric", "skip", 
        "skip", "skip", "skip", "skip", 
        "skip", "skip", "skip", "skip", 
        "skip", "skip", "skip", "skip", 
        "skip"), skip = 2) %>% 
  rename(FY = 1, amount.gdp = 2) %>% 
  filter(!(is.na(FY)|                                       #eliminate NAs in FY col
           is.na(amount.gdp)|                                      #eliminate NAs in deflator col
           FY %in% "TQ")) %>%                               #eliminate TQ year       
  mutate(FY = str_remove(FY, "[^0-9].+" ) %>% parse_integer , #remove 'estimate' from FY col
         amount.gdp = amount.gdp *1e9)     

gdp1 <- left_join(gdp, gdp.deflator)

# current deflator index is called 'current.deflator.name.index'
# current delated amount will be called 'current.deflator.name.amount'
# names vary according to year selected ('to.year')
current.deflator.name.amount <- paste0("amount.gdp.deflated.gdp.", to.year) 

gdp2 <- gdp1 %>% 
  mutate(amount.gdp.deflated.gdp.2009 = amount.gdp / deflator.index.gdp.2009,
         !!treat_input_as_col(current.deflator.name.amount) :=  amount.gdp / !!treat_input_as_col(current.delator.name.index)) 
 
name.of.file <- paste0("GDP.as.of.FY", to.year)
my.export.function(df=gdp2, name.of.file = name.of.file)
                     
                     
                     
                     
                     
                     
                     
