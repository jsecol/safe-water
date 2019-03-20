CFB manage violations II
================
Jim Sheehan
March 18, 2019

<br>

0. Load packages
----------------

``` r
library(data.table)
library(dplyr) # some masking of data.table: between, first, last
library(ggplot2)
library(zipcode) # for getting lat/lon coordinates where zipcode is available
library(lubridate)
```

<br>

I. Load data
------------

<br>

``` r

# library(readr)
# viol_all <- read_csv("VIOLATION.csv")
# read_csv too slow with big files like VIOLATION
# data.table fread() uses multiple cores
# can check first:  
# getDTthreads()

# the latest download
viol_all <- fread("C:/Users/Jim/Files/CFB/JohnMeroth/SDWIS/VIOLATION.csv", 
                  sep = ",")
```

    ## Warning in fread("C:/Users/Jim/Files/CFB/JohnMeroth/SDWIS/VIOLATION.csv", :
    ## Detected 35 column names but the data has 34 columns. Filling rows
    ## automatically. Set fill=TRUE explicitly to avoid this warning.

``` r

orig_nrows <- nrow(viol_all)
orig_nrows
```

    ## [1] 2212450

<br>

#### Small fix of column names (dropping string before "."), and dropping extra empty column:

``` r
viol_all[, V35 := NULL]

varnames <- colnames(viol_all)
head(varnames)
```

    ## [1] "VIOLATION.PWSID"                   "VIOLATION.VIOLATION_ID"           
    ## [3] "VIOLATION.FACILITY_ID"             "VIOLATION.POPULATION_SERVED_COUNT"
    ## [5] "VIOLATION.NPM_CANDIDATE"           "VIOLATION.PWS_ACTIVITY_CODE"

``` r

varnames <- gsub("^.*\\.","",varnames)
colnames(viol_all) <- varnames
# fixed variable names
cat("\n")
```

``` r
varnames
```

    ##  [1] "PWSID"                    "VIOLATION_ID"            
    ##  [3] "FACILITY_ID"              "POPULATION_SERVED_COUNT" 
    ##  [5] "NPM_CANDIDATE"            "PWS_ACTIVITY_CODE"       
    ##  [7] "PWS_DEACTIVATION_DATE"    "PRIMARY_SOURCE_CODE"     
    ##  [9] "POP_CAT_5_CODE"           "PRIMACY_AGENCY_CODE"     
    ## [11] "EPA_REGION"               "PWS_TYPE_CODE"           
    ## [13] "VIOLATION_CODE"           "VIOLATION_CATEGORY_CODE" 
    ## [15] "IS_HEALTH_BASED_IND"      "CONTAMINANT_CODE"        
    ## [17] "COMPLIANCE_STATUS_CODE"   "VIOL_MEASURE"            
    ## [19] "UNIT_OF_MEASURE"          "STATE_MCL"               
    ## [21] "IS_MAJOR_VIOL_IND"        "SEVERITY_IND_CNT"        
    ## [23] "COMPL_PER_BEGIN_DATE"     "COMPL_PER_END_DATE"      
    ## [25] "LATEST_ENFORCEMENT_ID"    "RTC_ENFORCEMENT_ID"      
    ## [27] "RTC_DATE"                 "PUBLIC_NOTIFICATION_TIER"
    ## [29] "ORIGINATOR_CODE"          "SAMPLE_RESULT_ID"        
    ## [31] "CORRECTIVE_ACTION_ID"     "RULE_CODE"               
    ## [33] "RULE_GROUP_CODE"          "RULE_FAMILY_CODE"

<br>

#### (Fairly) quick test for duplicates: no unique row identifier column so whole thing:

``` r
dups_viol_all <- duplicated(viol_all)

dups_sum <- sum(dups_viol_all)
dups_sum

if(dups_sum == 0) {
  rm(dups_viol_all)
}
```

    ## [1] 0

<br>

#### Looking for duplicates in primary (composite) key:

``` r
viol_all %>% 
  count(PWSID, VIOLATION_ID) %>% 
  filter(n > 1)
```

    ## # A tibble: 0 x 3
    ## # ... with 3 variables: PWSID <chr>, VIOLATION_ID <chr>, n <int>

<br>

#### OK, create a unique key:

``` r
viol_all[, RowID := paste0(PWSID, VIOLATION_ID)]
setkey(viol_all, RowID)
key(viol_all)
```

    ## [1] "RowID"

<br>

#### Convert some columns to date format:

``` r
#### Testing to get format right
# tmpdate <- viol_all[1:10,]$COMPL_PER_BEGIN_DATE
# as.IDate(tmpdate, format = "%d-%b-%y")

viol_all[, cpbd := as.IDate(COMPL_PER_BEGIN_DATE, format = "%d-%b-%y")]
viol_all[, cped := as.IDate(COMPL_PER_END_DATE, format = "%d-%b-%y")]
viol_all[, rtcd := as.IDate(RTC_DATE, format = "%d-%b-%y")]

class(viol_all$cpbd)
summary(viol_all$cpbd)
```

    ## [1] "IDate" "Date" 
    ##         Min.      1st Qu.       Median         Mean      3rd Qu. 
    ## "1976-06-01" "2006-12-01" "2010-10-01" "2008-09-03" "2014-01-01" 
    ##         Max. 
    ## "2064-07-31"

*Note: max compliance period begin date way beyond present! Just a couple: PR0004565, LA1017050*

<br>

#### Add some new columns to help create yearly timeline (later):

``` r
viol_all[, cpbd_year := year(cpbd)]
viol_all[, cped_year := year(cped)]

viol_all[, ydiff := cped_year - cpbd_year]

cat("Difference in years: compliance end date - begin date (some oddities)", "\n\n")
summary(viol_all$ydiff)
```

    ## Difference in years: compliance end date - begin date (some oddities) 
    ## 
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##   -70.0     0.0     0.0     0.3     0.0    23.0  373275

<br>

#### \#*COMPLIANCE\_STATUS\_CODE is important for understanding violation timeline*

``` r
unique(viol_all$COMPLIANCE_STATUS_CODE)
```

    ## [1] "R" "K" "O" "I"

<br>

by PWS\_ACTIVITY\_CODE

-   Active, Inactive, M (missing?), N (what is this?)

``` r
table(viol_all$COMPLIANCE_STATUS_CODE, viol_all$PWS_ACTIVITY_CODE)
```

    ##    
    ##           A       I       M       N
    ##   I       0  174972       0  157333
    ##   K  187806       0       0       0
    ##   O   47765       0       1       0
    ##   R 1429345  116283       0   98945

<br>

#### \#*COMPLIANCE\_STATUS\_CODE I's have no active water systems, just need R, O, and K*

<br>

#### Filtering out COMPLIANCE\_STATUS\_CODE = "R" (returned to compliance), PWS\_ACTIVITY\_CODE = "Active", rtcd (returned to compliance date) within period of interest, and tallying by IS\_HEALTH\_BASED\_IND

-   *compliance period begin date could also be filtered for same period, as for O and K below, but just using RTC date = a lot more PWSIDs kept. Looking at tmp, it seems compliance period begin/end dates not necessarily tied to RTC date*

<br>

-   RTC\_y08y16\_nHB = "returned to compliance within 2008-2016 period, non-health based violation"
-   RTC\_y08y16\_HB = "returned to compliance within 2008-2016 period, health based violation"

``` r
# , between(cpbd, as.Date("2008-01-01"), as.Date("2016-12-31")) # taken out of filter

viol_RTC <- viol_all %>% filter(COMPLIANCE_STATUS_CODE == "R", PWS_ACTIVITY_CODE == "A",
                    between(rtcd, as.Date("2008-01-01"), as.Date("2016-12-31"))) %>% 
  group_by(PWSID, PWS_TYPE_CODE, PWS_ACTIVITY_CODE, IS_HEALTH_BASED_IND) %>% 
  count(name = "RTC_0816_n") %>% 
  tidyr::spread(key = IS_HEALTH_BASED_IND, value = RTC_0816_n, fill = 0) %>% 
  rename(RTC_y08y16_nHB = N, RTC_y08y16_HB = Y)

nrow(viol_RTC)
```

    ## [1] 87378

``` r
head(viol_RTC)
```

    ## # A tibble: 6 x 5
    ## # Groups:   PWSID, PWS_TYPE_CODE, PWS_ACTIVITY_CODE [6]
    ##   PWSID     PWS_TYPE_CODE PWS_ACTIVITY_CODE RTC_y08y16_nHB RTC_y08y16_HB
    ##   <chr>     <chr>         <chr>                      <dbl>         <dbl>
    ## 1 010109005 CWS           A                              1             0
    ## 2 010307001 CWS           A                             19             2
    ## 3 010502002 NTNCWS        A                             13             4
    ## 4 010502003 NTNCWS        A                              4             0
    ## 5 020000001 CWS           A                              8             0
    ## 6 020000004 CWS           A                             12             0

<br>

``` r
# explore of cpbd before/after period for these
tmp <- viol_all %>% filter(COMPLIANCE_STATUS_CODE == "R", PWS_ACTIVITY_CODE == "A",
                    between(rtcd, as.Date("2008-01-01"), as.Date("2016-12-31")),
                    cpbd < as.Date("2008-01-01") | cpbd > as.Date("2016-12-31"))

length(unique(tmp$PWSID))
```

    ## [1] 26616

``` r

tmp %>% select(cpbd:rtcd, PWSID:VIOLATION_ID, 
               PWS_TYPE_CODE:CONTAMINANT_CODE) %>% 
  arrange(cpbd) %>% head(n = 10)
```

    ##          cpbd       cped       rtcd     PWSID VIOLATION_ID PWS_TYPE_CODE
    ## 1  1978-01-01 1980-12-31 2011-10-19 TX0700020    790009357           CWS
    ## 2  1978-10-06 1981-10-05 2016-08-04 OK2003705          181           CWS
    ## 3  1978-12-04 1981-10-03 2016-07-26 OK2000206          181           CWS
    ## 4  1978-12-04 1981-10-03 2016-07-26 OK2000211          281           CWS
    ## 5  1978-12-04 1980-10-03 2016-07-26 OK2000608         4580           CWS
    ## 6  1978-12-04 1981-10-03 2016-07-26 OK2000610          281           CWS
    ## 7  1978-12-04 1981-10-03 2016-08-04 OK2001608          181           CWS
    ## 8  1978-12-04 1980-10-03 2016-08-04 OK2001612         4180           CWS
    ## 9  1978-12-04 1980-10-03 2016-08-04 OK2007103         3980           CWS
    ## 10 1978-12-08 1981-10-07 2016-08-04 OK2002207          181           CWS
    ##    VIOLATION_CODE VIOLATION_CATEGORY_CODE IS_HEALTH_BASED_IND
    ## 1              02                     MCL                   Y
    ## 2              02                     MCL                   Y
    ## 3              02                     MCL                   Y
    ## 4              02                     MCL                   Y
    ## 5              02                     MCL                   Y
    ## 6              02                     MCL                   Y
    ## 7              02                     MCL                   Y
    ## 8              02                     MCL                   Y
    ## 9              02                     MCL                   Y
    ## 10             02                     MCL                   Y
    ##    CONTAMINANT_CODE
    ## 1              1025
    ## 2              1040
    ## 3              1040
    ## 4              1040
    ## 5              1040
    ## 6              1040
    ## 7              1040
    ## 8              1040
    ## 9              1040
    ## 10             1040

``` r

tmp %>% select(cpbd:rtcd, PWSID:VIOLATION_ID, 
               PWS_TYPE_CODE:CONTAMINANT_CODE) %>% 
  arrange(cpbd) %>% tail(n = 10)
```

    ##              cpbd       cped       rtcd     PWSID VIOLATION_ID
    ## 247229 2017-09-28       <NA> 2016-11-29 KS2012302         1422
    ## 247230 2017-10-01 2017-12-31 2016-07-10 CA3400433      1600036
    ## 247231 2017-10-01       <NA> 2010-02-08 UTAH23012      5113105
    ## 247232 2017-10-08       <NA> 2016-10-17 NC1011045           11
    ## 247233 2017-10-24       <NA> 2014-01-04 NM3524530       159869
    ## 247234 2017-11-02       <NA> 2016-07-01 NC0286627      4366710
    ## 247235 2017-11-02       <NA> 2016-11-15 NC0326548            7
    ## 247236 2017-12-10       <NA> 2016-06-17 MO4031631          401
    ## 247237 2018-01-01 2018-03-31 2016-04-04 NY4942015            3
    ## 247238 2018-09-01       <NA> 2016-09-19 MA2226003            7
    ##        PWS_TYPE_CODE VIOLATION_CODE VIOLATION_CATEGORY_CODE
    ## 247229           CWS             35                      MR
    ## 247230           CWS             02                     MCL
    ## 247231         TNCWS             5A                   Other
    ## 247232           CWS             75                   Other
    ## 247233           CWS             45                      TT
    ## 247234         TNCWS             75                   Other
    ## 247235         TNCWS             75                   Other
    ## 247236           CWS             75                   Other
    ## 247237        NTNCWS             3A                     MON
    ## 247238         TNCWS             4F                     RPT
    ##        IS_HEALTH_BASED_IND CONTAMINANT_CODE
    ## 247229                   N             2950
    ## 247230                   Y             1005
    ## 247231                   N             8000
    ## 247232                   N             7500
    ## 247233                   Y             0700
    ## 247234                   N             7500
    ## 247235                   N             7500
    ## 247236                   N             7500
    ## 247237                   N             8000
    ## 247238                   N             8000

<br>

#### Now trying to get yearly presence (including 2017)

-   This works, but should be able to use data.table::dcast for speed, maybe after using dplyr to filter and create cast column (may need setDT)
-   Should adapt to create history for particular contaminant(s), even potentially a shiny app to pick and map/export dataset (create temp local directory to hold filter first, perhaps)
-   What to do about COMPLIANCE\_STATUS\_CODE = K and O?

``` r
viol_RTC_byYear <- viol_all %>% filter(COMPLIANCE_STATUS_CODE == "R", 
                                       PWS_ACTIVITY_CODE == "A",
                    between(rtcd, as.Date("2008-01-01"), as.Date("2017-12-31"))) %>% 
  mutate(rtcd_year = year(rtcd))

  
# # old:   
# viol_RTC_byYear_x <- dcast(viol_RTC_byYear, PWSID + 
#                              PWS_TYPE_CODE + IS_HEALTH_BASED_IND ~ rtcd_year, 
#                     fun.agg = function(x) length(x), 
#                     value.var = "rtcd_year") %>% 
#   tidyr::spread(key = IS_HEALTH_BASED_IND, value = "rtcd_year")
# head(viol_RTC_byYear_x)  
  
  

viol_RTC_byYear_x <- viol_RTC_byYear %>% 
  group_by(PWSID, PWS_TYPE_CODE, IS_HEALTH_BASED_IND, rtcd_year) %>% 
  count() %>% 
  mutate(hb_year = paste("RTC_HB", IS_HEALTH_BASED_IND, rtcd_year, sep = "_")) %>% 
  ungroup() %>% 
  select(PWSID, PWS_TYPE_CODE, hb_year, n) %>% 
  tidyr::spread(key = hb_year, value = n, fill = 0)


head(viol_RTC_byYear_x)
```

    ## # A tibble: 6 x 22
    ##   PWSID PWS_TYPE_CODE RTC_HB_N_2008 RTC_HB_N_2009 RTC_HB_N_2010
    ##   <chr> <chr>                 <dbl>         <dbl>         <dbl>
    ## 1 0101~ CWS                       0             0             0
    ## 2 0101~ CWS                       0             0             0
    ## 3 0103~ CWS                       0             2             5
    ## 4 0105~ NTNCWS                    0            11             0
    ## 5 0105~ NTNCWS                    1             1             0
    ## 6 0200~ CWS                       0             0             0
    ## # ... with 17 more variables: RTC_HB_N_2011 <dbl>, RTC_HB_N_2012 <dbl>,
    ## #   RTC_HB_N_2013 <dbl>, RTC_HB_N_2014 <dbl>, RTC_HB_N_2015 <dbl>,
    ## #   RTC_HB_N_2016 <dbl>, RTC_HB_N_2017 <dbl>, RTC_HB_Y_2008 <dbl>,
    ## #   RTC_HB_Y_2009 <dbl>, RTC_HB_Y_2010 <dbl>, RTC_HB_Y_2011 <dbl>,
    ## #   RTC_HB_Y_2012 <dbl>, RTC_HB_Y_2013 <dbl>, RTC_HB_Y_2014 <dbl>,
    ## #   RTC_HB_Y_2015 <dbl>, RTC_HB_Y_2016 <dbl>, RTC_HB_Y_2017 <dbl>

#### For export if needed:

``` r
# write.csv(viol_RTC_byYear_x, "data_export/viol_RTC_byYear0817_x.csv", row.names = FALSE)
```

<br>

@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

------------------------------------------------------------------------

<br>

#### Filtering out COMPLIANCE\_STATUS\_CODE = "O" (open violation), PWS\_ACTIVITY\_CODE = "Active", cpbd (compliance period begin date; these have no end date or rtc date) within period of interest, and tallying by IS\_HEALTH\_BASED\_IND

-   O\_y08y16\_nHB = "Open violation within 2008-2016 period, non-health based violation"
-   O\_y08y16\_nHB = "Open violation within 2008-2016 period, health based violation"

``` r
viol_O <- viol_all %>% filter(COMPLIANCE_STATUS_CODE == "O", PWS_ACTIVITY_CODE == "A",
                    between(cpbd, as.Date("2008-01-01"), as.Date("2016-12-31"))) %>% 
  group_by(PWSID, PWS_TYPE_CODE, PWS_ACTIVITY_CODE, IS_HEALTH_BASED_IND) %>% 
  count(name = "O_0816_n") %>% 
  tidyr::spread(key = IS_HEALTH_BASED_IND, value = O_0816_n, fill = 0) %>% 
  rename(O_y08y16_nHB = N, O_y08y16_HB = Y)

nrow(viol_O)
```

    ## [1] 9157

``` r
head(viol_O)
```

    ## # A tibble: 6 x 5
    ## # Groups:   PWSID, PWS_TYPE_CODE, PWS_ACTIVITY_CODE [6]
    ##   PWSID     PWS_TYPE_CODE PWS_ACTIVITY_CODE O_y08y16_nHB O_y08y16_HB
    ##   <chr>     <chr>         <chr>                    <dbl>       <dbl>
    ## 1 010307001 CWS           A                            2           0
    ## 2 010502002 NTNCWS        A                            1           0
    ## 3 055295104 CWS           A                            1           0
    ## 4 063500006 CWS           A                            0           1
    ## 5 063500007 CWS           A                            0           1
    ## 6 063500108 CWS           A                            0           2

<br>

#### Filtering out COMPLIANCE\_STATUS\_CODE = "K" (known: not really sure what this is yet), PWS\_ACTIVITY\_CODE = "Active", cpbd (compliance period begin date; these have end dates - some later than period, but not sure how/if to use, and no rtc date) within period of interest, and tallying by IS\_HEALTH\_BASED\_IND

-   K\_y08y16\_nHB = "Known within 2008-2016 period, non-health based violation"
-   K\_y08y16\_nHB = "Known within 2008-2016 period, health based violation"

``` r
viol_K <- viol_all %>% filter(COMPLIANCE_STATUS_CODE == "K", PWS_ACTIVITY_CODE == "A",
                    between(cpbd, as.Date("2008-01-01"), as.Date("2016-12-31"))) %>% 
  group_by(PWSID, PWS_TYPE_CODE, PWS_ACTIVITY_CODE, IS_HEALTH_BASED_IND) %>% 
  count(name = "K_0816_n") %>% 
  tidyr::spread(key = IS_HEALTH_BASED_IND, value = K_0816_n, fill = 0) %>% 
  rename(K_y08y16_nHB = N, K_y08y16_HB = Y)

head(viol_K)

# weird, just 1: 
sum(viol_K$V1)
# it's VI3000052, blank for IS_HEALTH_BASED_IND, dropping

viol_K <- viol_K %>% select(-V1)

nrow(viol_K)
head(viol_K)
```

    ## # A tibble: 6 x 6
    ## # Groups:   PWSID, PWS_TYPE_CODE, PWS_ACTIVITY_CODE [6]
    ##   PWSID     PWS_TYPE_CODE PWS_ACTIVITY_CODE    V1 K_y08y16_nHB K_y08y16_HB
    ##   <chr>     <chr>         <chr>             <dbl>        <dbl>       <dbl>
    ## 1 010307001 CWS           A                     0            1           0
    ## 2 010502002 NTNCWS        A                     0            5           2
    ## 3 010502003 NTNCWS        A                     0            2           0
    ## 4 020000001 CWS           A                     0            1           0
    ## 5 020000004 CWS           A                     0            2           0
    ## 6 020000007 TNCWS         A                     0            1           0
    ## [1] 1
    ## [1] 20454
    ## # A tibble: 6 x 5
    ## # Groups:   PWSID, PWS_TYPE_CODE, PWS_ACTIVITY_CODE [6]
    ##   PWSID     PWS_TYPE_CODE PWS_ACTIVITY_CODE K_y08y16_nHB K_y08y16_HB
    ##   <chr>     <chr>         <chr>                    <dbl>       <dbl>
    ## 1 010307001 CWS           A                            1           0
    ## 2 010502002 NTNCWS        A                            5           2
    ## 3 010502003 NTNCWS        A                            2           0
    ## 4 020000001 CWS           A                            1           0
    ## 5 020000004 CWS           A                            2           0
    ## 6 020000007 TNCWS         A                            1           0

<br>

#### Load new water system .csv:

``` r
water_systems <- fread("C:/Users/Jim/Files/CFB/JohnMeroth/SDWIS/WATER_SYSTEM.csv", 
                  sep = ",")
```

    ## Warning in fread("C:/Users/Jim/Files/CFB/JohnMeroth/SDWIS/
    ## WATER_SYSTEM.csv", : Detected 48 column names but the data has 47 columns.
    ## Filling rows automatically. Set fill=TRUE explicitly to avoid this warning.

<br>

#### Small fix of column names (dropping string before "."), and dropping extra empty column:

``` r
water_systems[, V48 := NULL]

varnames_ws <- colnames(water_systems)
head(varnames_ws)
```

    ## [1] "WATER_SYSTEM.PWSID"               "WATER_SYSTEM.PWS_NAME"           
    ## [3] "WATER_SYSTEM.NPM_CANDIDATE"       "WATER_SYSTEM.PRIMACY_AGENCY_CODE"
    ## [5] "WATER_SYSTEM.EPA_REGION"          "WATER_SYSTEM.SEASON_BEGIN_DATE"

``` r

varnames_ws <- gsub("^.*\\.","",varnames_ws)
colnames(water_systems) <- varnames_ws
# fixed variable names
cat("\n")
```

``` r
varnames_ws
```

    ##  [1] "PWSID"                          "PWS_NAME"                      
    ##  [3] "NPM_CANDIDATE"                  "PRIMACY_AGENCY_CODE"           
    ##  [5] "EPA_REGION"                     "SEASON_BEGIN_DATE"             
    ##  [7] "SEASON_END_DATE"                "PWS_ACTIVITY_CODE"             
    ##  [9] "PWS_DEACTIVATION_DATE"          "PWS_TYPE_CODE"                 
    ## [11] "DBPR_SCHEDULE_CAT_CODE"         "CDS_ID"                        
    ## [13] "GW_SW_CODE"                     "LT2_SCHEDULE_CAT_CODE"         
    ## [15] "OWNER_TYPE_CODE"                "POPULATION_SERVED_COUNT"       
    ## [17] "POP_CAT_2_CODE"                 "POP_CAT_3_CODE"                
    ## [19] "POP_CAT_4_CODE"                 "POP_CAT_5_CODE"                
    ## [21] "POP_CAT_11_CODE"                "PRIMACY_TYPE"                  
    ## [23] "PRIMARY_SOURCE_CODE"            "IS_GRANT_ELIGIBLE_IND"         
    ## [25] "IS_WHOLESALER_IND"              "IS_SCHOOL_OR_DAYCARE_IND"      
    ## [27] "SERVICE_CONNECTIONS_COUNT"      "SUBMISSION_STATUS_CODE"        
    ## [29] "ORG_NAME"                       "ADMIN_NAME"                    
    ## [31] "EMAIL_ADDR"                     "PHONE_NUMBER"                  
    ## [33] "PHONE_EXT_NUMBER"               "FAX_NUMBER"                    
    ## [35] "ALT_PHONE_NUMBER"               "ADDRESS_LINE1"                 
    ## [37] "ADDRESS_LINE2"                  "CITY_NAME"                     
    ## [39] "ZIP_CODE"                       "COUNTRY_CODE"                  
    ## [41] "STATE_CODE"                     "SOURCE_WATER_PROTECTION_CODE"  
    ## [43] "SOURCE_PROTECTION_BEGIN_DATE"   "OUTSTANDING_PERFORMER"         
    ## [45] "OUTSTANDING_PERFORM_BEGIN_DATE" "CITIES_SERVED"                 
    ## [47] "COUNTIES_SERVED"

<br>

#### Looking for duplicates in primary key:

``` r
water_systems %>% 
  count(PWSID) %>% 
  filter(n > 1)
```

    ## # A tibble: 0 x 2
    ## # ... with 2 variables: PWSID <chr>, n <int>

<br>

#### Set primary key:

``` r
setkey(water_systems, PWSID)
key(water_systems)
```

    ## [1] "PWSID"

<br>

#### Join preliminaries:

``` r
setDT(viol_RTC)
length(unique(viol_RTC$PWSID)) == nrow(viol_RTC)
setkey(viol_RTC, PWSID)
key(viol_RTC)

setDT(viol_O)
length(unique(viol_O$PWSID)) == nrow(viol_O)
setkey(viol_O, PWSID)
key(viol_O)

setDT(viol_K)
length(unique(viol_K$PWSID)) == nrow(viol_K)
setkey(viol_K, PWSID)
key(viol_K)
```

    ## [1] TRUE
    ## [1] "PWSID"
    ## [1] TRUE
    ## [1] "PWSID"
    ## [1] TRUE
    ## [1] "PWSID"

<br>

#### Joining and filling in NA with 0:

``` r
water_systems_A <- water_systems[PWS_ACTIVITY_CODE == "A"]
  
merge1 <- merge(water_systems_A, 
                viol_RTC[, c("PWSID", "RTC_y08y16_HB", "RTC_y08y16_nHB")], all.x = TRUE)

merge1 <- merge(merge1, 
                viol_O[, c("PWSID", "O_y08y16_HB", "O_y08y16_nHB")], all.x = TRUE)

merge1 <- merge(merge1, 
                viol_K[, c("PWSID", "K_y08y16_HB", "K_y08y16_nHB")], all.x = TRUE)

merge1[, c("RTC_y08y16_HB", "RTC_y08y16_nHB", 
           "O_y08y16_HB", "O_y08y16_nHB", 
           "K_y08y16_HB", "K_y08y16_nHB")] <- 
  lapply(merge1[, c("RTC_y08y16_HB", "RTC_y08y16_nHB", 
           "O_y08y16_HB", "O_y08y16_nHB", 
           "K_y08y16_HB", "K_y08y16_nHB")], function(x) ifelse(is.na(x), 0, x))
```

<br>

#### For export if needed:

``` r
# merge1 %>% select(PWSID, RTC_y08y16_HB:K_y08y16_nHB) %>% 
#   write.csv("data_export/ComplianceActivity_y08y16.csv", row.names = FALSE)
```

<br>

#### Create binary version:

``` r
merge1_binary <- merge1 %>% 
  mutate_at(vars(RTC_y08y16_HB:K_y08y16_nHB), function(x) ifelse(x > 0, 1, 0))
```

<br>

#### Summary of the number of water systems out of total for a state (plus DC & PR) that had a health based violation and returned to compliance over the 2008-2016 period:

``` r
merge1_binary %>% filter(PWS_TYPE_CODE == "CWS", STATE_CODE %in% c(state.abb, "DC", "PR")) %>% 
  count(STATE_CODE, RTC_y08y16_HB) %>% 
  group_by(STATE_CODE) %>% 
  tidyr::spread(key = RTC_y08y16_HB, value = n, sep = "_") %>% 
  mutate(Total = RTC_y08y16_HB_0 + RTC_y08y16_HB_1, 
         Prop_RTC_y08y16_HB_1 = round(RTC_y08y16_HB_1/Total, 2)) %>% 
  arrange(desc(Prop_RTC_y08y16_HB_1))
```

    ## # A tibble: 52 x 5
    ## # Groups:   STATE_CODE [52]
    ##    STATE_CODE RTC_y08y16_HB_0 RTC_y08y16_HB_1 Total Prop_RTC_y08y16_HB_1
    ##    <chr>                <int>           <int> <int>                <dbl>
    ##  1 OK                     167             767   934                 0.82
    ##  2 AR                     219             472   691                 0.68
    ##  3 DC                       1               2     3                 0.67
    ##  4 MO                     563             863  1426                 0.61
    ##  5 NM                     275             348   623                 0.56
    ##  6 VT                     177             222   399                 0.56
    ##  7 MT                     359             426   785                 0.54
    ##  8 KY                     192             199   391                 0.51
    ##  9 AK                     199             202   401                 0.5 
    ## 10 SD                     253             257   510                 0.5 
    ## # ... with 42 more rows

<br><br><br><br>
