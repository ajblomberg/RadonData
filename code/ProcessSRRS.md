---
title: "Process SRRS data"
author: "Annelise Blomberg"
date: '2018-03-21'
output:  
  html_document:
    toc: true
    keep_md: true
---

We are processing SRRS data into a clean form, and linking to FIPS codes and counties. 



# Load SRRS data from multi-tabbed excel format 

The data is in an excel table formatted into different tabs, each of which list the state name. We need to load in each table and connect to the correct state name. 

We start by loading the sheet names and connecting to state abbreviations. 


```r
radon.file <- here("data", "raw", "EPA-SRRS 1987-1992 PJalbert 30-JAN-2014.xlsx")

state.xwalk <- tibble(state = datasets::state.abb, state.name = datasets::state.name) %>% 
        add_row(state = "DC", state.name = "District of Columbia") %>% 
        add_row(state = "PR", state.name = "Puerto Rico") %>% 
        mutate(state.name = str_trim(state.name))

tab.name <- excel_sheets(radon.file)

dat1 <- tibble(tab.name = tab.name) %>% 
        mutate(state.name = gsub("Table 1 ", "", tab.name),
               state.name = gsub("Table 2 ", "", state.name),
               state.name = gsub("Table ", "", state.name),
               state.name = gsub("and 2 ", "", state.name),
               state.name = str_trim(state.name),
               state.name = ifelse(state.name == "West Virgnia", "West Virginia", state.name)) %>% 
        left_join(state.xwalk, by = "state.name")
```

Next, we load in the header for each table and see what columns are most common. This will be used to decide how to process all the data. 


```r
excel_colnames <- function(tab){
        data <- read_excel(radon.file, sheet = tab, skip = 3)
        cols <- colnames(data)
        header <- paste0(cols, collapse = " ")
} 

dat2 <- dat1 %>% 
        mutate(colnames = map_chr(tab.name, excel_colnames))

colnames <- dat2 %>% group_by(colnames) %>% count() %>% arrange(desc(n))
colnames
```

```
## # A tibble: 12 x 2
## # Groups:   colnames [12]
##    colnames                                                              n
##    <chr>                                                             <int>
##  1 "COUNTY NO. OF MEAS. MEAN GEOM.\r\nMEAN MEDIAN STD.\r\nDEV. MAXI~    39
##  2 BOROUGH NO. OF MEAS MEAN GEOM. MEAN MEDIAN STD.DEV. MAX %>4pCi/L~     1
##  3 CITY NO. OF MEAS. HIGH pCi/L LOW pCi/L AVERAGE pCi/L %>4 pCi/L        1
##  4 COUNTY NO. OF MEAS. ARITHMETIC MEAN %>4 pCi/L                         1
##  5 "COUNTY NO. OF MEAS. AVERAGE MEDIAN GEOM.\r\nMEAN MAXIMUM %>4 pC~     1
##  6 COUNTY NO. OF MEAS. AVERAGE MINIMUM MAXIMUM                           1
##  7 "COUNTY NO. OF MEAS. GEOM.\r\nMEAN MEDIAN STD.\r\nDEV. MAXIMUM %~     1
##  8 County Number of Homes Average pCi/L Standard Deviation pCi/L Ma~     1
##  9 Geologic Terrane # of Homes Geometric Mean Arithmetic Mean X__1 ~     1
## 10 "PARISH NO. OF MEAS. MEAN GEOM.\r\nMEAN MEDIAN STD.\r\nDEV. MAXI~     1
## 11 Town No. of Meas. Mean                                                1
## 12 ZIP CODE CITY COUNTY NO. OF MEAS AVERAGE MEDIAN GM STD MAX %>4 p~     1
```

The most common set of headers is "COUNTY NO. OF MEAS. MEAN GEOM.\r\nMEAN MEDIAN STD.\r\nDEV. MAXIMUM %>4 pCi/L %>20 pCi/L" 

### Load data with standard headers

Next, we load in data for this subset that matches the standard format. 


```r
excel_standard_dat <- function(tab){
        data <- read_excel(radon.file, sheet = tab, skip = 3,
                           col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
        data2 <- data %>% 
                select(county = `COUNTY`, n = `NO. OF MEAS.`, mean = `MEAN`, geom.mean = `GEOM.\r\nMEAN`,
                               median = MEDIAN, std.dev = `STD.\r\nDEV.`, max = MAXIMUM) %>% 
                filter(!is.na(county))
} 

dat.stand <- dat2 %>% 
        filter(colnames == "COUNTY NO. OF MEAS. MEAN GEOM.\r\nMEAN MEDIAN STD.\r\nDEV. MAXIMUM %>4 pCi/L %>20 pCi/L") %>% 
        mutate(data = map(tab.name, excel_standard_dat))

dat.stand2 <- dat.stand %>% 
        unnest() %>% 
        select(-tab.name, -state.name, -colnames)
```

### Load states with non-standard headers individually 

Now we have to decide what to do with tabs that do not match the standard format. In the end, all columns need to match: 
county (chr), n, mean, geom.mean, median, std.dev, max

We load tables individually and then bind together. 


```r
dat.odd <- dat2 %>% 
        filter(colnames != "COUNTY NO. OF MEAS. MEAN GEOM.\r\nMEAN MEDIAN STD.\r\nDEV. MAXIMUM %>4 pCi/L %>20 pCi/L")

AK <- dat.odd %>% filter(state == "AK") %>% 
        mutate(data = map(tab.name, ~ read_excel(radon.file, sheet = .x, skip = 3, 
                      col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")))) %>% 
        unnest() %>% 
        select(state, county = `BOROUGH`, n = `NO. OF MEAS`, mean = `MEAN`, geom.mean = `GEOM. MEAN`, 
               median = MEDIAN, std.dev = `STD.DEV.`, max = MAX) %>% 
        filter(!is.na(county))

CT <- dat.odd %>% filter(state == "CT") %>% 
        mutate(data = map(tab.name, ~ read_excel(radon.file, sheet = .x, skip = 19, 
                      col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric")))) %>% 
        unnest() %>% 
        select(state, county = `County`, n = "Number of\r\nHomes", mean = "Arithmetic\r\nMean", geom.mean = "Geometric\r\nMean", 
               max = "Maximum") %>% 
        filter(!is.na(county))

FL <- dat.odd %>% filter(state == "FL") %>% 
        mutate(data = map(tab.name, ~ read_excel(radon.file, sheet = .x, cell_rows(4:71), 
                      col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")))) %>% 
        unnest() %>% 
        select(state, county = `County`, n = `Number of Homes`, mean = `Average pCi/L`, 
               std.dev = `Standard Deviation pCi/L`, max = "Maximum pCi/L") %>% 
        filter(!is.na(county))

LA <- dat.odd %>% filter(state == "LA") %>% 
        mutate(data = map(tab.name, ~ read_excel(radon.file, sheet = .x, skip = 3, 
                      col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")))) %>% 
        unnest() %>% 
        select(state, county = "PARISH", n = "NO. OF MEAS.", mean = `MEAN`, geom.mean = "GEOM.\r\nMEAN", 
               median = MEDIAN, std.dev = "STD.\r\nDEV.", max = MAXIMUM) %>% 
        filter(!is.na(county))

NJ <-  dat.odd %>% filter(state == "NJ") %>% 
        mutate(data = map(tab.name, ~ read_excel(radon.file, sheet = .x, skip = 3, 
                      col_types = c("text", "numeric", "numeric", "numeric")))) %>% 
        unnest() %>% 
        select(state, county = "COUNTY", n = "NO. OF MEAS.", mean = `ARITHMETIC MEAN`) %>% 
        filter(!is.na(county))

NC <-  dat.odd %>% filter(state == "NC") %>% 
        mutate(data = map(tab.name, ~ read_excel(radon.file, sheet = .x, skip = 3, 
                      col_types = c("text", "numeric", "numeric", "numeric", "numeric")))) %>% 
        unnest() %>% 
        select(state, county = "COUNTY", n = "NO. OF MEAS.", mean = `AVERAGE`, max = "MAXIMUM") %>% 
        filter(!is.na(county))

RI <- dat.odd %>%  filter(state == "RI") %>% 
        mutate(data = map(tab.name, ~ read_excel(radon.file, sheet = .x, skip = 3, 
                      col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")))) %>% 
        unnest() %>% 
        select(state, county = `COUNTY`, n = "NO. OF MEAS.", geom.mean = "GEOM.\r\nMEAN", 
               median = MEDIAN, std.dev = "STD.\r\nDEV.", max = "MAXIMUM") %>% 
        filter(!is.na(county))

TX <- dat.odd %>%  filter(state == "TX") %>% 
        mutate(state, data = map(tab.name, ~ read_excel(radon.file, sheet = .x, skip = 3, 
                      col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")))) %>% 
        unnest() %>% 
        select(state, county = `COUNTY`, n = "NO. OF MEAS.", mean = "AVERAGE", geom.mean = "GEOM.\r\nMEAN", 
               median = MEDIAN, max = "MAXIMUM") %>% 
        filter(!is.na(county))
```

Add in NH and NV seperately, because it is by town (not county). We first match each town to corresponding county. We then aggregate to county, calculating mean as a weighted mean by the number of measures. 


```r
NH <- dat.odd %>% filter(state == "NH") %>% 
        mutate(data = map(tab.name, ~ read_excel(radon.file, sheet = .x, skip = 3, 
                      col_types = c("text", "numeric", "numeric")))) %>% 
        unnest() %>% 
        select(city = "Town", n = "No. of Meas.", mean = `Mean`) %>% 
        filter(!is.na(city)) %>% 
        mutate(state = "NH") %>% 
        filter(!(city == "Stratford" & n == 4)) # drop the second set of measurements for Stratford 

NV <-  dat.odd %>% filter(state == "NV") %>% 
        mutate(data = map(tab.name, ~ read_excel(radon.file, sheet = .x, skip = 3, 
                      col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric")))) %>% 
        unnest() %>% 
        select(state, city = "CITY", n = "NO. OF MEAS.", mean = `AVERAGE pCi/L`, 
               max = "HIGH pCi/L") %>% 
        filter(!is.na(city))

CityCountyXwalk <- read_csv(here("data", "raw", "CityCountyXwalk.csv"), na = "NA")
```

```
## Parsed with column specification:
## cols(
##   city = col_character(),
##   state = col_character(),
##   county = col_character()
## )
```

```r
city.dat <- bind_rows(NH, NV) %>% 
        left_join(CityCountyXwalk, by = c("city", "state"))


DE <- dat.odd %>% filter(state == "DE") %>% 
        mutate(data = map(tab.name, ~ read_excel(radon.file, sheet = .x, skip = 3, 
                      col_types = c("guess", "text", "text", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric")))) %>% 
        unnest() %>% 
        select(state, city = CITY, county = `COUNTY`, n = "NO. OF MEAS", mean = "AVERAGE", max = "MAX") %>% 
        filter(!is.na(city)) %>% 
        mutate(county = ifelse(county == "NEWCASTLE", "NEW CASTLE", county))

city.dat2 <- bind_rows(city.dat, DE)

# Calculate county averages as the mean of city values, weighted by n. Calculate county max by selecting the max. 

city.sum <- city.dat2 %>% 
        group_by(state, county) %>% 
        filter(!is.na(mean) & !(is.na(n)) & !is.na(county)) %>%
        filter(city != "Hart's Location") %>% 
        summarise(mean = (sum(mean*n, na.rm = T)/sum(n, na.rm = T)),
                  max = max(max, na.rm = T)) %>% 
        mutate(max = ifelse(max == -Inf, NA, max))
```

Now, we bind together all the standard data, non-standard states and county values calculated for NV and NH. 


```r
dat3 <- bind_rows(dat.stand2, AK, CT, FL, LA, NJ, NC, RI, TX, city.sum) %>% 
        mutate(county = str_to_upper(county)) %>% 
        filter(!(county %in% c("BARNSTABLE +DUKES", "FRANKLIN +HAMPSHIRE", "STATEWIDE", 
                               "CLIFTON FORGE", "SOUTH BOSTON"))) 
#DROPPING COUNTIES THAT DON'T EXIST ANYMORE
```

# Match to FIPS 

Load FIPS codes for cross-walking. We need to match by county name. Our end goal is to have the five-digit state-county FIPS code. 

Create two FIPS crosswalks. 
State crosswalk: merge in state abbreviations. (I don't think I need this). 
County crosswalk: Just state and county fips. 


```r
fips <- read_excel(here("data", "raw", "all-geocodes-v2016.xlsx"), skip = 4) 

# State fips
state.fips <- fips %>% filter(`Summary Level` == "040") %>%
        select(state.fips = `State Code (FIPS)`,
               state.name = "Area Name (including legal/statistical area description)")

state.xwalk <- tibble(state = datasets::state.abb, state.name = datasets::state.name) %>%
        add_row(state = "DC", state.name = "District of Columbia") %>%
        add_row(state = "PR", state.name = "Puerto Rico")

state.fips <- state.fips %>%
        left_join(state.xwalk, by = "state.name") %>%
       select(-state.name)

# County fips
county.fips <- fips %>% filter(`Summary Level` == "050") %>% 
        select(state.fips = `State Code (FIPS)`, 
               county.fips = `County Code (FIPS)`, 
               area.name = 'Area Name (including legal/statistical area description)') %>% 
        mutate(fips = paste0(state.fips, county.fips),
               area.name = str_to_upper(area.name)) %>% 
        select(-county.fips) %>% 
        left_join(state.fips , by = "state.fips")
```

We first have to drop the last word ("county", I think) from the county name in the FIPS crosswalk. 


```r
 last.word <- word(county.fips$area.name, 2, -1) 
 counts <- as.tibble(table(last.word)) %>% arrange(desc(n))

# List of endings: 
# county.names <- c("Census Area", "City and Borough", "County", "Municipio", "Parish", "Municipality")
```


```r
county.fips <- county.fips %>% 
        mutate(area.name.short = str_remove(area.name, " CENSUS AREA"),
               area.name.short = str_remove(area.name.short, " CITY AND BOROUGH"),
               area.name.short = str_remove(area.name.short, " COUNTY"), 
               area.name.short = str_remove(area.name.short, " CITY"),
               area.name.short = str_remove(area.name.short, " MUNICIPIO"),
               area.name.short = str_remove(area.name.short, " PARISH"),
               area.name.short = str_remove(area.name.short, " MUNICIPALITY")) %>% 
        mutate(area.name.short = ifelse(fips == 24510, "BALTIMORE CITY", area.name.short),
               area.name.short = ifelse(fips == 29510, "ST. LOUIS CITY", area.name.short),
               area.name.short = ifelse(fips == 51600, "FAIRFAX CITY", area.name.short),
               area.name.short = ifelse(fips == 51760, "RICHMOND CITY", area.name.short),
               area.name.short = ifelse(fips == 51770, "ROANOKE CITY", area.name.short)) %>% 
        select(state.fips, state, fips, area.name, area.name.short)
```


Now we do a rough match to see how many match. We need to match within state, as well. We remove all white-space to increase the number of matches. 

80 of the 3092 SRRS counties (3%) do not match the fips crosswalk.


```r
dat3 <- dat3 %>% 
        mutate(county.match = str_remove(county, " "),
               county.match = str_remove(county.match, "\\*"))

county.fips <- county.fips %>% 
        mutate(county.match = str_remove(area.name.short, " "))

nomatch <- anti_join(dat3, county.fips, by = c("state", "county.match"))
```

We manually adjust county names for non-matches. 


```r
dat4 <- dat3 %>% 
        mutate(county.match = ifelse(state == "AR" & county.match == "OUACFFLTA", "OUACHITA", county.match),
               county.match = ifelse(state == "FL" & county.match == "DADE", "MIAMI-DADE", county.match),
               county.match = ifelse(state == "FL" & county.match == "SUWANEE", "SUWANNEE", county.match),
               county.match = ifelse(state == "GA" & county.match == "BADE", "DADE", county.match),
               county.match = ifelse(state == "GA" & county.match == "MCDUFFFFI", "MCDUFFIE", county.match),
               county.match = ifelse(state == "IA" & county.match == "DUBUOUE", "DUBUQUE", county.match),
               county.match = ifelse(state == "IA" & county.match == "POWESHFFIK", "POWESHIEK", county.match),
               county.match = ifelse(state == "IA" & county.match == "WINNESFFLEK", "WINNESHIEK", county.match),
               county.match = ifelse(state == "ID" & county.match == "LEMFFL", "LEMHI", county.match),
               county.match = ifelse(state == "IL" & county.match == "TAZWELL", "TAZEWELL", county.match),
               county.match = ifelse(state == "IL" & county.match == "VERMILLION", "VERMILION", county.match),
               county.match = ifelse(state == "KS" & county.match == "ATCFFLSON", "ATCHISON", county.match),
               county.match = ifelse(state == "KS" & county.match == "HAUTAUQUA", "CHAUTAUQUA", county.match),
               county.match = ifelse(state == "KS" & county.match == "SUMMER", "SUMNER", county.match),
               county.match = ifelse(state == "KY" & county.match == "GALLOWAY", "CALLOWAY", county.match),
               county.match = ifelse(state == "LA" & county.match == "CALCASJEU", "CALCASIEU", county.match),
               county.match = ifelse(state == "LA" & county.match == "IBERVELLE", "IBERVILLE", county.match),
               county.match = ifelse(state == "LA" & county.match == "JEFFERSONDA VIS", "JEFFERSONDAVIS", county.match),
               county.match = ifelse(state == "LA" & county.match == "OUACFFLTA", "OUACHITA", county.match),
               county.match = ifelse(state == "LA" & county.match == "HIGHLAND", "RICHLAND", county.match),
               county.match = ifelse(state == "LA" & county.match == "ST.JOHN", "ST.JOHN THE BAPTIST", county.match),
               county.match = ifelse(state == "MI" & county.match == "BENZFFI", "BENZIE", county.match),
               county.match = ifelse(state == "MI" & county.match == "CFFLPPEWA", "CHIPPEWA", county.match),
               county.match = ifelse(state == "MI" & county.match == "MENOMNEE", "MENOMINEE", county.match),
               county.match = ifelse(state == "MN" & county.match == "CFFLPPEWA", "CHIPPEWA", county.match),
               county.match = ifelse(state == "MN" & county.match == "CFFLSAGO", "CHISAGO", county.match),
               county.match = ifelse(state == "MN" & county.match == "KANDIYOFFL", "KANDIYOHI", county.match),
               county.match = ifelse(state == "MN" & county.match == "KOOCFFLCHING", "KOOCHICHING", county.match),
               county.match = ifelse(state == "MN" & county.match == "MLLLELACS", "MILLELACS", county.match),
               county.match = ifelse(state == "MO" & county.match == "ATCFFLSON", "ATCHISON", county.match),
               county.match = ifelse(state == "MO" & county.match == "BQONE", "BOONE", county.match),
               county.match = ifelse(state == "MO" & county.match == "DAVFFISS", "DAVIESS", county.match),
               county.match = ifelse(state == "MS" & county.match == "CFFLCKASAW", "CHICKASAW", county.match), 
               county.match = ifelse(state == "MS" & county.match == "TALLAHATCFFLE", "TALLAHATCHIE", county.match),               
               county.match = ifelse(state == "MT" & county.match == "GARFEELD", "GARFIELD", county.match),
               county.match = ifelse(state == "NC" & county.match == "FENDER", "PENDER", county.match),
               county.match = ifelse(state == "ND" & county.match == "MCKENZFFI", "MCKENZIE", county.match),
               county.match = ifelse(state == "NE" & county.match == "GARFFFILD", "GARFIELD", county.match),
               county.match = ifelse(state == "NE" & county.match == "KIMEALL", "KIMBALL", county.match),
               county.match = ifelse(state == "NM" & county.match == "DONAANA", "DO?AANA", county.match),
               county.match = ifelse(state == "OK" & county.match == "N0WATA", "NOWATA", county.match),
               county.match = ifelse(state == "OK" & county.match == "POTTAWATOMFFI", "POTTAWATOMIE", county.match),
               county.match = ifelse(state == "OK" & county.match == "TELLMAN", "TILLMAN", county.match),
               county.match = ifelse(state == "OR" & county.match == "YAMFFLLL", "YAMHILL", county.match),
               county.match = ifelse(state == "PA" & county.match == "BTTTLER", "BUTLER", county.match),
               county.match = ifelse(state == "PA" & county.match == "CLEARFEELD", "CLEARFIELD", county.match),
               county.match = ifelse(state == "SC" & county.match == "EDGEFFFILD", "EDGEFIELD", county.match),
               county.match = ifelse(state == "SD" & county.match == "SHANNON", "OGLALALAKOTA", county.match),
               county.match = ifelse(state == "TN" & county.match == "SEQUATCHFFI", "SEQUATCHIE", county.match),
               county.match = ifelse(state == "TN" & county.match == "SUMMER", "SUMNER", county.match),
               county.match = ifelse(state == "TX" & county.match == "GILLESPFFI", "GILLESPIE", county.match),
               county.match = ifelse(state == "TX" & county.match == "HEMPFFLLL", "HEMPHILL", county.match),
               county.match = ifelse(state == "TX" & county.match == "SANPATRICK)", "SANPATRICIO", county.match),
               county.match = ifelse(state == "UT" & county.match == "GARFFFILD", "GARFIELD", county.match),
               county.match = ifelse(state == "VA" & county.match == "CHARLESCITY", "CHARLES", county.match),
               county.match = ifelse(state == "VA" & county.match == "JAMESCITY", "JAMES", county.match),
               county.match = ifelse(state == "VA" & county.match == "YORKCITY", "YORK", county.match),
               county.match = ifelse(state == "VA" & county.match == "ALEXANDRIACITY", "ALEXANDRIA", county.match),
               county.match = ifelse(state == "VA" & county.match == "BEDFORDCITY", "BEDFORD", county.match),
               county.match = ifelse(state == "VA" & county.match == "FAIRFAX-CITY", "FAIRFAXCITY", county.match),
               county.match = ifelse(state == "VA" & county.match == "RICHMOND-CITY", "RICHMONDCITY", county.match),
               county.match = ifelse(state == "VA" & county.match == "ROANOKE-CITY", "ROANOKECITY", county.match),
               county.match = ifelse(state == "VT" & county.match == "CHTTTENDEN", "CHITTENDEN", county.match),
               county.match = ifelse(state == "WA" & county.match == "KMTTAS", "KITTITAS", county.match),
               county.match = ifelse(state == "WI" & county.match == "BAYHELD", "BAYFIELD", county.match),
               county.match = ifelse(state == "WI" & county.match == "CFFLPPEWA", "CHIPPEWA", county.match),
               county.match = ifelse(state == "WI" & county.match == "OUTAGAMFFI", "OUTAGAMIE", county.match),
               county.match = ifelse(state == "WV" & county.match == "RITCFFLE", "RITCHIE", county.match),
               county.match = ifelse(state == "WY" & county.match == "LARAMFFI", "LARAMIE", county.match),
               county.match = ifelse(state == "WY" & county.match == "WASHAKFFI", "WASHAKIE", county.match))
```

After updating all matches, we see that the only counties that still don't match are in HI and AK.  


```r
nomatch2 <- anti_join(dat4, county.fips, by = c("state", "county.match"))
```

# Write final file

Our goal is to have a final file that includes state, county name, FIPS and final radon values. 


```r
dat.final <- dat4 %>% 
        left_join(county.fips, by = c("state", "county.match")) %>% 
        select(state, area.name, fips, n:max)

write_csv(dat.final, here("data", "SRRSClean.csv"))
```

