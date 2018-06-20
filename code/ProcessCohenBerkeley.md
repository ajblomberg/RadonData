---
title: "Process Cohen and Berkeley data"
author: "Annelise Blomberg"
date: '2018-03-21'
output:
  html_document:
    df_print: paged
    toc: true
    keep_md: true
---

This R notebook loads and processes the Cohen and Berkeley data. Our goal is to save clean versions of the file with FIPS codes and county names. 



# Load Data
Load Cohen and Berkeley data, as well as FIPS codes for cross-walking. 


```r
cohen <-  read_excel(here("data", "raw", "CohenRadon.xls"), col_types = "numeric") %>% 
        rename(radon = RADON, fips = COUNTY)

berkeley <-  read_excel(here("data", "raw", "BerkeleyRadon.xlsx"), col_types = "numeric") 
        
# Notice that there is a duplicate in the berkeley data
berkeley %>% group_by(fips) %>% filter(n() > 1)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["fips"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["GMest"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["GMer"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["AMest"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["AMer"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["f4est"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["f10est"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["f4_10"],"name":[8],"type":["dbl"],"align":["right"]},{"label":["f10_5"],"name":[9],"type":["dbl"],"align":["right"]}],"data":[{"1":"9001","2":"0.58","3":"1.2","4":"0.83","5":"0.17","6":"0.01","7":"0","8":"0","9":"0"},{"1":"9001","2":"0.68","3":"1.3","4":"0.94","5":"0.26","6":"0.01","7":"0","8":"0","9":"0"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
berkeley <- berkeley %>% 
        filter(!(fips == 9001 & GMest == 0.58))
```

Load FIPS codes for cross-walking. 

Create two FIPS crosswalks. 
State crosswalk: merge in state abbreviations.
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
               area.name = "Area Name (including legal/statistical area description)") %>% 
        mutate(fips = paste0(state.fips, county.fips)) %>% 
        select(-state.fips, -county.fips)
```

# Format FIPS and States
Checking fips lengths in Cohen and Berkeley data to confirm that they are all 4-5 units long. 

Add padding so that FIPS codes always have a length of five (first two numbers are state FIPS, last three are county FIPS) 


```r
cohen2 <- cohen %>% 
        mutate(fips = str_pad(fips, 5, pad = 0, side = "left"))
berkeley2 <- berkeley %>% 
        mutate(fips = str_pad(fips, 5, pad = 0, side = "left"))
```

Add in state names.

```r
cohen3 <- cohen2 %>% 
        mutate(state.fips = str_sub(fips, 1, 2)) %>% 
        left_join(state.fips, by = "state.fips") %>% 
        select(-state.fips)

berkeley3 <- berkeley2 %>% 
        mutate(state.fips = str_sub(fips, 1, 2)) %>% 
        left_join(state.fips, by = "state.fips") %>% 
        select(-state.fips)
```

Check to see if there are any entries that do not match to the FIPS table. 

```r
cohen_nomatch <- anti_join(cohen3, county.fips, by = "fips")
berkeley_nomatch <- anti_join(berkeley3, county.fips, by = "fips")
```

Six of the Berkeley entries do not match to the 2016 FIPS code list. Presumably this is because the FIPS codes have changed. 

```r
berkeley3 <- berkeley3 %>% 
        mutate(fips = ifelse(fips == "12025", "12086", fips)) %>% 
        mutate(fips = ifelse(fips == "30113", "56039", fips)) %>%
        mutate(fips = ifelse(fips == "46113", "46102", fips)) %>%
        mutate(fips = ifelse(fips == "51515", "51019", fips)) %>%
        mutate(fips = ifelse(fips == "51560", "51005", fips)) %>%
        mutate(fips = ifelse(fips == "51780", "51083", fips)) 

berkeley_nomatch <- anti_join(berkeley3, county.fips, by = "fips")
```

# Merge FIPS to Cohen and Berkeley 
Merge in county fips to Cohen and Berkeley

```r
cohen4 <- left_join(cohen3, county.fips, by = "fips") %>% 
        select(state, area.name, fips, radon) %>% 
        mutate(area.name = str_to_upper(area.name))

berkeley4 <- left_join(berkeley3, county.fips, by = "fips") %>% 
        select(state, area.name, fips, GMest:f10_5) %>% 
        mutate(area.name = str_to_upper(area.name))
```

# Save Finals
Write .csv files 

```r
write_csv(cohen4, here("data", "CohenClean.csv"))
write_csv(berkeley4, here("data", "BerkeleyClean.csv"))
```


