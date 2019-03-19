CFB zipcode work
================
Jim Sheehan
March 19, 2019

0. Load packages
----------------

``` r
library(data.table)
library(dplyr) # some masking of data.table: between, first, last
# library(ggplot2)
library(zipcode) # for getting lat/lon coordinates where zipcode is available
# library(lubridate)
```

<br>

I. Load data
------------

<br>

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

#### Zipcode data from zipcode::zipcode

``` r
data(zipcode)
sum(nchar(zipcode$zip) != 5) # all 5 character length
```

    ## [1] 0

<br>

#### Zipcode clean:

``` r
# whoa:
unique(nchar(water_systems$ZIP_CODE))
```

    ## [1] 10  0  5  9  7  6 14

``` r
nrow(water_systems[nchar(water_systems$ZIP_CODE) != 5 & nchar(water_systems$ZIP_CODE) > 0, ])
```

    ## [1] 33507

``` r
water_systems$ZIP_CODE5 <- substr(water_systems$ZIP_CODE, start = 1, stop = 5) 

# now using zipcode clean package function (which doesn't fix >5 character zips)
water_systems$ZIP_CODE5 <- clean.zipcodes(water_systems$ZIP_CODE5)
```

<br>

#### Rename some of the columns for zipcode::zipcode prior to join

-   helps distinguish from similar SWDIS variable names, and maybe other lat/lon sources (e.g., zipcode tabulation area centroids for comparison)

``` r
colnames(zipcode)
```

    ## [1] "zip"       "city"      "state"     "latitude"  "longitude"

``` r
colnames(zipcode) <- c("zip", "Rzcpkg_city", "Rzcpkg_state", "Rzcpkg_lat", "Rzcpkg_lon")
colnames(zipcode)
```

    ## [1] "zip"          "Rzcpkg_city"  "Rzcpkg_state" "Rzcpkg_lat"  
    ## [5] "Rzcpkg_lon"

<br>

#### Join

``` r
merge1 <- left_join(water_systems, zipcode, by = c("ZIP_CODE5" = "zip"))

sum(is.na(merge1$Rzcpkg_lat))
```

    ## [1] 24030

<br>

#### Filter and count \# w/o coordinates, by PWS\_TYPE\_CODE (row TRUE)

-   also \# unique zipcodes

``` r
merge1_A <- merge1 %>% filter(PWS_ACTIVITY_CODE == "A")

sum(is.na(merge1_A$Rzcpkg_lat))
```

    ## [1] 621

``` r
table(is.na(merge1_A$Rzcpkg_lat), merge1_A$PWS_TYPE_CODE)
```

    ##        
    ##           CWS NTNCWS TNCWS
    ##   FALSE 49506  17488 78736
    ##   TRUE    205     71   345

``` r
length(unique(merge1_A$ZIP_CODE5))
```

    ## [1] 27444

<br>

#### Export dataset

``` r
merge1_A %>% select(PWSID, ZIP_CODE5, Rzcpkg_lat, Rzcpkg_lon) %>% 
  rename(LAT = Rzcpkg_lat, LON = Rzcpkg_lon) %>% 
  write.csv("data_export/PWSID_coordinates.csv", row.names = FALSE)
```

<br>

#### Just making sure

``` r
checkit <- readr::read_csv("data_export/PWSID_coordinates.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   PWSID = col_character(),
    ##   ZIP_CODE5 = col_character(),
    ##   LAT = col_double(),
    ##   LON = col_double()
    ## )

``` r
nrow(checkit)
```

    ## [1] 146351

``` r
head(checkit)
```

    ## # A tibble: 6 x 4
    ##   PWSID     ZIP_CODE5   LAT   LON
    ##   <chr>     <chr>     <dbl> <dbl>
    ## 1 010106001 06339      41.4 -72.0
    ## 2 010109005 06382      41.5 -72.1
    ## 3 010307001 02535      41.3 -70.8
    ## 4 010502002 02813      41.4 -71.7
    ## 5 010502003 02813      41.4 -71.7
    ## 6 020000001 14779      42.2 -78.7

``` r
rm(checkit)
```
