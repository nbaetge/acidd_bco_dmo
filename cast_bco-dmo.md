cast\_bco-dmo
================
Nicholas Baetge
8/18/2020

Here we compile our lab’s data from the ACIDD bottle file and prepare it
for submission to
    BCO-DMO.

    ## Warning: package 'tidyverse' was built under R version 4.0.2

    ## ── Attaching packages ──────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.0     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.1     ✓ dplyr   1.0.4
    ## ✓ tidyr   1.0.3     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## Warning: package 'dplyr' was built under R version 4.0.2

    ## ── Conflicts ─────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

    ## 
    ## Attaching package: 'googlesheets4'

    ## The following objects are masked from 'package:googledrive':
    ## 
    ##     request_generate, request_make

    ## 
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

    ## The following object is masked from 'package:purrr':
    ## 
    ##     transpose

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     hour, isoweek, mday, minute, month, quarter, second, wday, week,
    ##     yday, year

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

    ## Warning: package 'readxl' was built under R version 4.0.2

    ## Warning: package 'viridis' was built under R version 4.0.2

    ## Loading required package: viridisLite

# Import Data

``` r
data <- read_sheet("https://docs.google.com/spreadsheets/d/17l6UIFdWFT3ib3kXXG61SPfuYnKMAT5Tjv-nafnL0lI/edit#gid=103152813", sheet = "Bottle File", skip = 0) %>% 
  mutate(Leg = as.character(Leg),
         Date = ymd_hm(Time_Stamp)) %>% 
  select(Cruise:Time_Stamp, Date, everything())

saveRDS(data, "Input/google_bf.rds")


headers <-  read_sheet("https://docs.google.com/spreadsheets/d/17l6UIFdWFT3ib3kXXG61SPfuYnKMAT5Tjv-nafnL0lI/edit#gid=103152813", sheet = "Bottle File Headers", skip = 0) 

saveRDS(headers, "Input/google_bf_headers.rds")
```

# Check Profiles

![](cast_bco-dmo_files/figure-gfm/doc%20plot-1.png)<!-- -->

![](cast_bco-dmo_files/figure-gfm/ba%20plot-1.png)<!-- -->
![](cast_bco-dmo_files/figure-gfm/poc%20plot-1.png)<!-- -->
![](cast_bco-dmo_files/figure-gfm/n%20plot-1.png)<!-- -->

![](cast_bco-dmo_files/figure-gfm/po4%20plot-1.png)<!-- -->

![](cast_bco-dmo_files/figure-gfm/si%20plot-1.png)<!-- -->

![](cast_bco-dmo_files/figure-gfm/tdn%20plot-1.png)<!-- -->

![](cast_bco-dmo_files/figure-gfm/don%20plot-1.png)<!-- -->

# Subset Data

``` r
subset <-  data %>%
  select(Cruise, Station, Time_Stamp, Latitude, Longitude, CruiseCN, SCN, Leg, Niskin, Target_Z, Conductivity:Pressure, PO4:TDN_sd, BactAbund:BactAbund_QF) %>% 
  drop_na(Z) %>% 
  mutate(PO4_QF = ifelse(!is.na(PO4), PO4_QF, 9),
         PO4_QF = ifelse(is.na(PO4_QF), 2, PO4_QF),
         SiO4_QF = ifelse(!is.na(SiO4), SiO4_QF, 9),
         SiO4_QF = ifelse(is.na(SiO4_QF), 2, SiO4_QF),
         NO2_QF = ifelse(!is.na(NO2), NO2_QF, 9),
         NO2_QF = ifelse(is.na(NO2_QF), 2, NO2_QF),
         NO2_NO3_QF = ifelse(!is.na(NO2_NO3), NO2_NO3_QF, 9),
         NO2_NO3_QF = ifelse(is.na(NO2_NO3_QF), 2, NO2_NO3_QF),
         NH4_QF = ifelse(!is.na(NH4), NH4_QF, 9),
         NH4_QF = ifelse(is.na(NH4_QF), 2, NH4_QF),
         POC_QF = ifelse(!is.na(POC), POC_QF, 9),
         POC_QF = ifelse(is.na(POC_QF), 2, POC_QF),
         DOC_QF = ifelse(!is.na(DOC), DOC_QF, 9),
         DOC_QF = ifelse(is.na(DOC_QF), 2, DOC_QF),
         TDN_QF = ifelse(!is.na(TDN), TDN_QF, 9),
         TDN_QF = ifelse(is.na(TDN_QF), 2, TDN_QF),
         BactAbund_QF = ifelse(!is.na(BactAbund), BactAbund_QF, 9),
         BactAbund_QF = ifelse(is.na(BactAbund_QF), 2, BactAbund_QF)) %>% 
  mutate_all(funs(replace(., is.na(.), -999))) %>% 
  select(-Target_Z)
```

    ## Warning: `funs()` is deprecated as of dplyr 0.8.0.
    ## Please use a list of either functions or lambdas: 
    ## 
    ##   # Simple named list: 
    ##   list(mean = mean, median = median)
    ## 
    ##   # Auto named with `tibble::lst()`: 
    ##   tibble::lst(mean, median)
    ## 
    ##   # Using lambdas
    ##   list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

``` r
subset_headers <- headers %>% 
  filter(Type %in% c("Metadata", "CTD Data", "Nutrients", "POM", "DOM", "Bacteria")) %>% 
  filter(!Header %in% c("Type", "Bottom_Z", "MLD", "DCM", "Target_Z", "Bottle Trip", "Neutral_Density", "Sigma_Theta", "Potential_Temp", "16S_ID")) 
```

# Export Data

``` r
# subset %>%
#   write_csv(., "Output/ACIDD_BGC_Data.csv")
# 
# subset_headers %>%
#   write_csv(., "Output/ACIDD_BGC_Headers.csv")
# 
# path <- "Output/"
# merge_file_name <- "Output/ACIDD_biogeochem_data_BCO-DMO.xlsx"
# sheet_names <- c("ACIDD Data", "ACIDD Metadata")
# 
# filenames_list <- list.files(path = path, full.names = TRUE)
# 
# merge <- lapply(filenames_list,function(filename){
#     print(paste("Merging",filename,sep = " "))
#     read.csv(filename)
# })
# 
# names(merge) <- sheet_names
# 
# write.xlsx(merge, merge_file_name)
```
