# Federal Budget

## Purpose
Download and compile all versions of the OMB 'Public Database' from the GPO website:
https://www.gpo.gov/fdsys/browse/collectionGPO.action?collectionCode=BUDGET

## What does the code do?
The main cleaning script dowloads all GPO data FY2008-current year. Only a single type may be selected at one time (either budget authority or outlays). Only the most recent version will be exported as .csv (due to size limits), however all are available in the final R dataframe prior to export.

## How To Use the Code
1. Choose 'to.year': All OMB public databases FY2008-to.year will be downloaded and stored in nested df
2. Choose only one budget type:  budget.authority / outlays. The result will be processed and exported as .csv (filtered by most recent year).

*Caution: If using Github, sync after each selection to avoid exceeding upload limits and breaking the repository.*

 
## Source Notes 
The Governnment Publishing Office (GPO) keeps an archive of Presidential Budget Requests. The 'Public Database' (xls, .csv files) are available beginning in FY1998.

GPO uses this format for creating source hyperlinks:
 https://www.gpo.gov/fdsys/pkg/BUDGET-2003-DB/csv/BUDGET-2003-DB-1.csv
 
* DB-1: Budget Authority
* DB-2: Outlays
* DB-3: Receipts

OMB public databases prior to FY2008 contain the following inconsistencies (and are not included in this repository):

*Inconsistent Archive Medium*
* FY1998-FY2001 are stored as text or '.htm'
* FY2002 is stred as text or xls
* FY2003 and later are stored as .csv or xls
 
*Inconsistent Data*
* FY2003-FY2007 Have no column headers
* Span of years may be different for each type
* Outlays begin in FY1962; Budget authority begins in 1976

## Repository File Structure 
* **Code** folder contains R scripts (currently, only a single cleaning file). 
* **Data** folder contains raw (*Data/Raw*)and processed (*Data/Processed*) data. 
* **Shared** folder is for common files.

## What Does this Repository Contain?
Cleaning script creates a table of nested dataframes which are tidied and enriched. Currently, only FY2008-FY2019 budget authority is available. Due to GitHub space constraints, only the most recent year (FY2019) data is exported as .csv file to the Data/Processed subfolder. 

## How was the Original Data Altered?
The R script in this repository alters the original data:
* Recalculated amounts from thousands
* Added GDP.chained deflator index for each record (for example, *deflator.index.gdp.2009*, *deflator.index.gdp.2019*). Index based on OMB "Table 10.1—Gross Domestic Product and Deflators Used in the Historical Tables: 1940–2023." 
* Added column of inflation adjusted amounts (for example, *amount.deflated.gdp.2019*)
* Added budget functions (and titles)
* Added convenience columns (FYDP.yes.or.no, national.defense.yes.or.no)
* Added *base.year* variable denoting public database version. 
* Removed quarter year *TQ* from all datasets (1976- TQ- 1977) and deflators. This is not the best solution, but I'm not sure how else to handle it. Use caution for years during this span.

## Q&A
*Question:* Do I need to download deflators separately?
    *- No. Hyperlinks to table 10.1 (OMB Deflators) are created automatically based on to.year.*

*Question:* Can I choose a different deflator?
    *Not at this time. Base year (default = to.year) can easily be altered in code.*


## To Do
1. Add receipts
2. Add GDP
 
## Narrative Description of Original Docs
Each year, OMB publishes raw historical data that supplements the release of the Administration's budget request. These "White House Historical Tables" are considered authoritative sources for analyzing long term budget data and are released as .xlx files for the convenience of analysts. They frequently contain important footnotes and various changes changes from year to year; consequently, they are not machine readable.

OMB also publishes a machine-readable (.csv) version of the main datasets each year along with a helpful [user guide](https://www.whitehouse.gov/wp-content/uploads/2018/02/db_guide-fy2019.pdf). There are three types of files:

* budget authorizations 
* outlays
* receipts

Archived versions of each release can be found at [GPO - White House Historical Tables page.](https://www.gpo.gov/fdsys/browse/collection.action?collectionCode=BUDGET&browsePath=Fiscal+Year+2019&isCollapsed=true&leafLevelBrowse=false&isDocumentResults=true&ycord=86). Because each administration may change the layout of it's website, GPO should be considered the preferred source.

Note that Budget estimates for the current year (2019) and the budget year (2019) are prepared by agencies, based on the definitions and guidance contained in the OMB Circular A-11.



