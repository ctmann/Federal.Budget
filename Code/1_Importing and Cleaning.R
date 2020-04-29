# SourceNotes -------------------------------------------------------------
  # Detailed notes available at GitHub site
  # https://github.com/ctmann/Federal.Budget

  #' Output
  #'  1)  10+ raw .xlsx files: downloaded in "Deflator" section for processing
  #'  2)  budauth/outlays, filtered by most recent FY
  #'  3)  GDP compilation 
  #'  4)  Historical deflators (10.1)
  #'  
  #'  Upload constraints prevent exporting all historical budauth/outlays
  #'  To export locally, see Export section

  # Caution:.csv upload will exceed limits for outlays


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(janitor)
library(friendlyeval)
library(curl)
library(feather)

# How to Update this File -------------------------------------------------

  # Dashboard of Options -------------------------------------------------

  #Preliminaries: Downloads, Exports 
  # a) Do you want to download all new raw data? Use Download switch below. (default "OFF")
       download.switch <- "download.switch.off" #<-Default: Do not download new raw data when running code
       #download.switch <- "download.switch.on" #<-Download fresh raw data when running code 
  
  # b) Do you want to export files? Use export switch below.
       export.switch <-   "export.switch.off" #<-Default: Do not download export when running code
       #export.switch <- "export.switch.on" #<-Download fresh raw data when running code 
  
  #Update Options:
    # 1. Choose 'to.year'
        to.year   <- 2021       # Update to current year. 
    
    # 2. Choose budget type (Outlays export too big for GitHub)
        process.this.budget.type <- "budget.authority" 
        #process.this.budget.type <- "outlays"  
        
    # 3.a Adjust deflator index base year for OMB table 10.1
    #      - Currently FY2012; most recently FY2009
    
      deflator.base.year <- 2012   # Update to current deflator base year 
      
      # This tibble describes tbl.10.1 deflator index year (current year fy2012)
      # Add new year to series, if necessary
      deflator.base.year.tbl <- tibble(
        table.base.year = c(2008:2010,    2011:2014,    2015:2019,    2020:to.year), 
        index.base.year = c(rep(2000,3), rep(2005, 4),  rep(2009, 5), rep(deflator.base.year, to.year-2020+1) ) )
    
    # 3.b  deflator hyperlink keeps erratically (12-1 to 10.1 to 11-1). Add link to 10.1 manually.
      deflator.links <- c("https://www.govinfo.gov/content/pkg/BUDGET-2008-TAB/xls/BUDGET-2008-TAB-12-1.xls",  #2008
                          "https://www.govinfo.gov/content/pkg/BUDGET-2009-TAB/xls/BUDGET-2009-TAB-12-1.xls",
                          "https://www.govinfo.gov/content/pkg/BUDGET-2010-TAB/xls/BUDGET-2010-TAB-10-1.xls",
                          "https://www.govinfo.gov/content/pkg/BUDGET-2011-TAB/xls/BUDGET-2011-TAB-10-1.xls",
                          "https://www.govinfo.gov/content/pkg/BUDGET-2012-TAB/xls/BUDGET-2012-TAB-10-1.xls",
                          "https://www.govinfo.gov/content/pkg/BUDGET-2013-TAB/xls/BUDGET-2013-TAB-10-1.xls",
                          "https://www.govinfo.gov/content/pkg/BUDGET-2014-TAB/xls/BUDGET-2014-TAB-10-1.xls",
                          "https://www.govinfo.gov/content/pkg/BUDGET-2015-TAB/xls/BUDGET-2015-TAB-10-1.xls",
                          "https://www.govinfo.gov/content/pkg/BUDGET-2016-TAB/xls/BUDGET-2016-TAB-11-1.xls",
                          "https://www.govinfo.gov/content/pkg/BUDGET-2017-TAB/xls/BUDGET-2017-TAB-11-1.xls",
                          "https://www.govinfo.gov/content/pkg/BUDGET-2018-TAB/xls/BUDGET-2018-TAB-11-1.xls",
                          "https://www.govinfo.gov/content/pkg/BUDGET-2019-TAB/xls/BUDGET-2019-TAB-11-1.xlsx", 
                          "https://www.govinfo.gov/content/pkg/BUDGET-2020-TAB/xls/BUDGET-2020-TAB-11-1.xlsx",
                          "https://www.govinfo.gov/content/pkg/BUDGET-2021-TAB/xls/BUDGET-2021-TAB-11-1.xlsx") #2021 
                                                                                                               #2022 < Add new link here
  # Common Vars -------------------------------------------------------------
    #Select OMB datasets to download (consistent data begins in 2008)
    from.year <- 2008 # Don't change this! Consistent OMB data begins in 2008
    span.years <- (to.year - from.year)+1
    
    # Prep names
     current.deflator.name.amount <- paste0("amount.deflated.gdp.", to.year) 
     base.year.deflator.name.amount<-   paste0("amount.deflated.gdp.", deflator.base.year) #< Currently Fy2012
  
    # Export Function
    my.export.function <- function(df, name.of.file){
      my.data.folder.location <- paste0(getwd(), "/Data/Processed/")
      my.timestamp <- paste('Updated', format(Sys.time(), format = ".%Y-%m-%d.%H%M") , sep = "")
      my.file.name <-  sprintf("%s/%s_%s.csv", my.data.folder.location, name.of.file, my.timestamp)
      write_csv(df, my.file.name)
    }
    
# Deflators ----------------------------------------------------------------
#' Table 10.1 - Gross Domestic Product and Deflators Used in the Historical Tables

  # Step 1. Download all Excel files and combine ----------------------------------------------------------------
  
tbl.10.1_gdp.deflator <- tibble(
  table.base.year = rep( c(from.year:to.year), 1) ) %>% 
  mutate(file.format = if_else(table.base.year >2018, ".xlsx", ".xls"),
         hyperlink = deflator.links,
         file.name = paste0("./Data/Raw/", table.base.year, "_", "tbl.10.1", file.format))  %>% 
  select(-file.format)

  
# Download all 10.1 files to raw folder
ifelse(download.switch == "download.switch.on",
  Map(function(u, d)download.file(u, d, mode="wb"), 
    tbl.10.1_gdp.deflator$hyperlink, 
    tbl.10.1_gdp.deflator$file.name),
  print("--->Download Switch Off<----") )

# Read in excel files, unnest
tbl.10.1_gdp.deflator.compiled <- tbl.10.1_gdp.deflator %>% 
  mutate(my.data = map(file.name, ~(.x %>% read_excel(range = cell_cols(c("A","C") ), # weird merged cols
                                                      col_types="text")  %>% 
                                      rename(FY = 1, amount.gdp = 2, index = 3) %>% 
                                      filter(!(is.na(FY)|                             #eliminate NAs in FY col
                                                 is.na(index)|                        #eliminate NAs in deflator col
                                                 FY %in% "TQ")) %>%                   #eliminate TQ        
                                      mutate(FY = str_remove(FY, "[^0-9].+" ) %>% parse_integer) )  ) ) %>% 
  select(-file.name, -hyperlink) %>% unnest() %>% 
  # Base Year
  left_join(deflator.base.year.tbl) %>% 
  drop_na() %>% 
  mutate(index = as.numeric(index),
         amount.gdp = as.numeric(amount.gdp) * 1e9)

        # --Combined Deflator/GDP Compilation complete--

  # Step 2. Filter Deflator Compilation ---------------------------------------------
  # Only one deflator is really necessary
  # Filter for current year deflator

# Name, based on deflator index base year
most.recent.index.base.year <- paste0("deflator.index.gdp.", deflator.base.year)

#Filter compilation for most recent table, rename index (deflator.base.year)
gdp.deflator <- tbl.10.1_gdp.deflator.compiled %>% 
  filter(table.base.year %in% to.year) %>% #< filter by most recent year
  select(-index.base.year, -table.base.year) %>% 
  #rename
  rename(!!treat_input_as_col(most.recent.index.base.year) :=  index )

# Get current year index value
current.year.index <- gdp.deflator %>% 
  filter(FY %in% to.year) %>%  
  select(3) %>% pull()
# Prepare new name
current.delator.name.index <- paste0("deflator.index.gdp.", to.year ) 
# Divide base deflator by new value, name index
gdp.deflator <- gdp.deflator %>% 
  mutate( !!treat_input_as_col(current.delator.name.index) := !!treat_input_as_col(most.recent.index.base.year)/current.year.index ) 

# GDP --------------------------------------------------------------
gdp1 <- gdp.deflator
  
  gdp2 <- gdp1 %>% 
    mutate(!!treat_input_as_col(base.year.deflator.name.amount) := amount.gdp / !!treat_input_as_col(most.recent.index.base.year),
           !!treat_input_as_col(current.deflator.name.amount)   := amount.gdp / !!treat_input_as_col(current.delator.name.index))                                                                       # 'deflator name based on current FY

  # Current year deflator Index dataset
  gdp.deflator <- gdp.deflator %>% select(-amount.gdp)
  

# Import All Public Databases  ------------------------------------------------------------------
##=#=#=#=#=#= Download all public databases from GPO during (from-to span) #=#=#=#=#=#=
   # - Each 'database' identified by its original FY.base
   #   Ex. FY2019 public database is base.year 2019
   #       FY2018 public database is base.year 2018
  
  # Step 1. Download all Excel files and combine ----------------------------------------------------------------
  omb <- tibble(
    base.year = rep( c(from.year:to.year), 2) ,
    budget.type = c(rep("budget.authority", span.years) , 
                    rep("outlays", span.years)),
    #Hyperlink name is hard
    hyperlink = c(sprintf("https://www.govinfo.gov/content/pkg/BUDGET-%s-DB/xls/BUDGET-%s-DB-1.", 
                          c(from.year:to.year),
                          c(from.year:to.year)) ,
                  sprintf("https://www.govinfo.gov/content/pkg/BUDGET-%s-DB/xls/BUDGET-%s-DB-2.", 
                          c(from.year:to.year), 
                          c(from.year:to.year)
                          ) )) %>% 
    mutate(hyperlink = if_else(base.year > 2018, paste0(hyperlink,"xlsx"), paste0(hyperlink, "xls")) ) %>% 
    #name downloaded file (trim hyperlink to name file)
    mutate(file.name = paste0("./Data/Raw/", 
                              str_sub(hyperlink, start=56, -1))) 

  # Download raw Excel data
  ifelse(download.switch == "download.switch.on",
    Map(function(u, d)download.file(u, d, mode="wb"), 
        omb$hyperlink, 
        omb$file.name),
  print("--->Download Switch Off<----") )
  
  # Import outlays and budauths as nested tibbles
  omb1 <- omb %>% 
    mutate(datasets = map(.x = file.name,
                          ~(.x %>% read_excel(col_types="text") %>% clean_names() ) )) 
  
  # Step 2. Process by budget.type (budauth or outlays) ----------------------------------------------------------------

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
  
  # Step 3. Enrich dataset, Add Deflators ----------------------------------------------------------------

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
  
  # BCA Security and Non-Security
    # Descr: Original BCA legislation contained "security and non-security" categories
    #        that were later simplified to defense/non-defense.
    # See cbo final sequestration report (2012) for defs of original defs
    # Revised defense/non-defense same as discretionary 050
  
    omb4 <- omb4 %>% 
      mutate(BCA.original.security.category = 
               ifelse( "Discretionary" %in% bea_category &
                         ( "007" %in% agency_code|
                           "024" %in% agency_code|
                           "029" %in% agency_code|
                           "National Nuclear Security Administration" %in% bureau_name|
                           "Intelligence Community Management Account" %in% account_name |
                           "150" %in% function_code),
        "BCA.security", "BCA.non.security") ) %>% 
      mutate(BCA.revised.defense.category = 
               ifelse( "Discretionary" %in% bea_category &
                         "050" %in% function_code,
        "BCA.defense", "BCA.non.defense"))

  

#=#= Deflators #=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
omb5 <- left_join(omb4, gdp.deflator )

# Deflate 
omb6 <- omb5 %>% 
  mutate(!!treat_input_as_col(base.year.deflator.name.amount) := amount / !!treat_input_as_col(most.recent.index.base.year),
         !!treat_input_as_col(current.deflator.name.amount) :=  amount / !!treat_input_as_col(current.delator.name.index))   # 'deflator name based on current FY



#=#= Final Reorder and Filter #=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=

# Reorder
  omb.complete.public.db.collection <- omb6 %>% 
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
        BCA.revised.defense.category,
        BCA.original.security.category,
        FY,                        
        amount,                    
        21,                          # deflator.index.gdp.2012,   # This may change with base year
        23,                          # amount.deflated.gdp.2012,  # This may change with base year
        22,                          # deflator.index.gdp.2020,   # Change this to FriendlyEval
        24,                          # amount.deflated.gdp.2020,  # Change this to friendlyeval
        everything(), #<Just in case future fields are added  
        -hyperlink, -file.name)               
  
  # Note: [omb.complete.public.db.collection] contains all public databases, too large for github
    # When writing entire file, use this name:
      #my.table.name <- paste0("omb", from.year, ".to.", to.year)
  
  # Filter by Most Recent Year, otherwise, file too large for github
    omb.final.dataset <- omb.complete.public.db.collection %>% 
        filter(base.year %in% to.year) 

  #<<<<<End: [omb.final.dataset] contains current year data only >>>>


# Exports -----------------------------------------------------------------
# 3 Exports: Main, GDP, and Historical Deflators. Each saved as separate .csv file.

  # 1. Main Data ------------------------------------------------------------------
    # Export (only if export swich is turned on)
        # export.switch <- "export.switch.on"

      name.of.file <- paste0("omb.", process.this.budget.type, ".FY", to.year)
      ifelse(export.switch == "export.switch.on",
        omb.final.dataset %>% my.export.function(name.of.file),
      ("--->Export Switch Off<----") )

  # 2. FYDP Defense Data -------------------------------------------------------
    #smaller FYDP dataset
    # export.switch <- "export.switch.on"

 fydp.defense.dataset <- omb.final.dataset %>% 
    filter(national.defense.yes.or.no %in% "yes",
           FYDP.yes.or.no %in% "yes")
      
    ifelse(test=export.switch == "export.switch.on",
           yes= my.export.function(fydp.defense.dataset, paste0("fydp.compilation.as.of.FY", to.year)),
           no= "--->Export Switch Off<----"  )

  # 3. Historical Deflators -------------------------------------
  
    # (Only most Recent Year, otherwise, file too large for github)
    name.of.compilation.file <- paste0("omb.tbl.10.1.historical.deflators_FY2008.to.", to.year)
    name.of.current.year.file <- paste0("omb.tbl.10.1.deflators.", to.year)
    
    #Export
    
    #All Historical Deflators (Uncomment for export)
      # my.export.function(tbl.10.1_gdp.deflator.compiled %>% 
      #                  select(-amount.gdp), 
      #                  name.of.current.year.file) 
    
    #Current year only
  ifelse(test=export.switch == "export.switch.on",
         yes = my.export.function(tbl.10.1_gdp.deflator.compiled %>% 
                         filter(table.base.year %in% to.year)  %>% 
                         select(-amount.gdp), 
                         name.of.current.year.file),
         no= "--->Export Switch Off<----"  )
    
  # 4. Historical GDP -------------------------------------
    name.of.file <- paste0("GDP.as.of.FY", to.year)
    
    ifelse(test=export.switch == "export.switch.on",
           yes = my.export.function(df=gdp2, name.of.file = name.of.file),
           no= "--->Export Switch Off<----"  )

  
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  