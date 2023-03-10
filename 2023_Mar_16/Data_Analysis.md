New Residents Patterns in Philadelphia: Part 3
================

## Data Sources

For this Leading Indicator, we collected data from the IPUMS USA micro
data series. We created a data extract from the [IPUMS
website](https://usa.ipums.org/usa/) for the following respondent-level
characteristics:

| Variable   | Description                                                                                                                                                                                                                                                                                                                                           |
|:-----------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| STATEFIP   | STATEFIP reports the state in which the household was located, using the Federal Information Processing Standards (FIPS) coding scheme, which orders the states alphabetically.                                                                                                                                                                       |
| COUNTYFIP  | COUNTYFIP identifies the county where the household was enumerated, using the Federal Information Processing Standard (FIPS) coding scheme. COUNTYFIP codes are state-dependent; they must be combined with state codes (see STATEFIP or STATEICP) to distinguish counties located in different states.                                               |
| SEX        | SEX reports whether the person was male or female.                                                                                                                                                                                                                                                                                                    |
| AGE        | AGE reports the person’s age in years as of the last birthday.                                                                                                                                                                                                                                                                                        |
| RACE       | The concept of RACE has changed over the more than 150 years represented in IPUMS. Currently, the Census Bureau and others consider race to be a sociopolitical construct, not a scientific or anthropological one. Many detailed RACE categories consist of national origin groups.                                                                  |
| HISPAN     | HISPAN identifies persons of Hispanic/Spanish/Latino origin and classifies them according to their country of origin when possible. Origin is defined by the Census Bureau as ancestry, lineage, heritage, nationality group, or country of birth. People of Hispanic origin may be of any race; see RACE for a discussion of coding issues involved. |
| EDUC       | EDUC indicates respondents’ educational attainment, as measured by the highest year of school or degree completed.                                                                                                                                                                                                                                    |
| INCTOT     | INCTOT reports each respondent’s total pre-tax personal income or losses from all sources for the previous year. The censuses collected information on income received from these sources during the previous calendar year; for the ACS and the PRCS, the reference period was the past 12 months.                                                   |
| MIGPLACE1  | For respondents who lived in a different residence 1 year before the survey date, MIGPLAC1 identifies the U.S. state, outlying territory, or the foreign country where the respondent lived at that time.                                                                                                                                             |
| MIGCOUNTY1 | For respondents who lived in a different residence 1 year before the survey date, MIGCOUNTY1 identifies the county (or county equivalent) where the respondent lived at that time, if the prior residence was in an identifiable county.                                                                                                              |
| MIGTYPE1   | MIGTYPE1 indicates whether the respondent lived in a metropolitan area one year ago and, if so, whether they also resided within a central/principal city.                                                                                                                                                                                            |
| PWSTATE2   | PWSTATE2 reports the state in which the respondent’s primary workplace was located.                                                                                                                                                                                                                                                                   |
| PWCOUNTY   | PWCOUNTY identifies the county (or county equivalent) where the respondent worked, if the respondent’s workplace was in an identifiable county.                                                                                                                                                                                                       |
| OCC        | OCC reports the person’s primary occupation, coded into a contemporary census classification scheme (some non-occupational activities are also recorded in the pre-1940 samples). Generally, the primary occupation is the one from which the person earns the most money.                                                                            |
| TRANWORK   | TRANWORK reports the respondent’s primary means of transportation to work on the most recent day worked (1970), or over the course of the previous week (the 1960 and 1980-2000 censuses, the ACS, and the PRCS).                                                                                                                                     |

## Importing Data

``` r
dat <- read.csv("ipums_nresidents.csv")
```

## Cleaning Data

### Attaching survey weights

``` r
#_________________________________________#
# separating data into pre and post pandemic 
pre_COVID <- dat %>% 
  filter(YEAR == 2018 | YEAR == 2019)
post_COVID <- dat %>% 
  filter(YEAR == 2020 | YEAR == 2021)
#_________________________________________#
# attaching survey weights 
dtaDesign_pre <- svydesign(id      = ~CLUSTER,
                           strata  = ~STRATA,
                           weights = ~PERWT,
                           nest    = TRUE,
                           data    = pre_COVID)
#_________________________________________#
dtaDesign_post <- svydesign(id     = ~CLUSTER,
                           strata  = ~STRATA,
                           weights = ~PERWT,
                           nest    = TRUE,
                           data    = post_COVID)
```

## Cross Tabulations

### 1. Top 5 Occupations

``` r
# 1. Occupation => describe the top 10 jobs held by migrant workers 
table <- svytable(~OCC+YEAR, design = dtaDesign_pre)
df_pre <- as.data.frame.matrix(table) 
df_pre <- rownames_to_column(df_pre, var = "Variable") %>% as_tibble()
#_________________________________________#
table <- svytable(~OCC+YEAR, design = dtaDesign_post)
df_post <- data.frame(rbind(table))
df_post <- rownames_to_column(df_post, var = "Variable") %>% as_tibble()
#_________________________________________#
df_occ <- df_pre %>% 
  left_join(df_post, by = c("Variable")) %>% 
  filter(Variable != "")
# calculating top 10 OCCUPATIONS that new residents are working in 
df_occ <- df_occ %>% 
  gather(YEAR, count, `2018`:X2021) %>% 
  group_by(YEAR) %>% 
  arrange(desc(count)) %>%
  group_by(YEAR) %>% 
  mutate(prop_count = count/sum(count)) %>% 
  slice(1:10) %>% 
  mutate(Variable = ifelse(Variable == "Secretaries and administrative assistants, except legal, medical, and executive", "Secretaries and administerative assistants", 
                           ifelse(Variable == "Lawyers, and judges, magistrates, and other judicial workers", "Lawyers, judges and judicial workers", Variable)))
```

### 2. Are New Residents Mostly Working in Philadelphia?

``` r
###########################################
# 6. Place of work => can point out whether numbers of migrants working outside PA 
# has increased since the pandemic or not
# separating data into pre and post pandemic 
#:::::::::::::::::::::::::::::::::::::::::
table <- svytable(~PWCOUNTY+YEAR, design = dtaDesign_pre)
df_pre <- as.data.frame.matrix(table) 
df_pre <- rownames_to_column(df_pre, var = "Variable") %>% as_tibble()
df_pre <- df_pre %>% 
  group_by(Variable) %>% 
  mutate(`Pre-COVID` = mean(`2018`:`2019`))
#_________________________________________#
table <- svytable(~PWCOUNTY+YEAR, design = dtaDesign_post)
df_post <- data.frame(rbind(table))
df_post <- rownames_to_column(df_post, var = "Variable") %>% as_tibble() 
df_post <- df_post %>% 
  group_by(Variable) %>% 
  mutate(`Post-COVID` = mean(X2020:X2021))
#_________________________________________#
df_pwork <- df_pre %>% 
  left_join(df_post, by = c("Variable"))
#_________________________________________#
# creating dummy of philly 
df_pwork <- df_pwork %>% 
  select(Variable, `2018`, `2019`, X2020, X2021) %>% 
  gather(Year, count, `2018`:X2021) %>% 
  mutate(Philly = ifelse(Variable == 101, "Philadelphia", "Not Philadelphia")) %>% 
  group_by(Year, Philly) %>% 
  mutate(sum_pwork = sum(count)) %>%
  group_by(Year) %>% 
  mutate(prop_pwork = sum_pwork/sum(count))
# creating lineplots of changing proportions in migrant workers working in philadelphia vs outside
df_pwork <- df_pwork %>% 
  mutate(Year = recode(Year,
                       "2018" = 2018,
                       "2019" = 2019,
                       "X2020" = 2020,
                       "X2021" = 2021)) %>% 
  distinct(prop_pwork, .keep_all = TRUE) %>% 
  group_by(Philly) %>% 
  mutate(index_prop = prop_pwork/prop_pwork[which(Year == 2018)])
# adding labels to maximum year in dataset 
df_pwork <- df_pwork %>% 
  mutate(label = ifelse(Philly == "Not Philadelphia" & Year == 2018, "Working Outside Philadelphia",
                        ifelse(Philly == "Philadelphia" & Year == 2018, "Working Inside Philadelphia", NA)))
#:::::::::::::::::::#
#       2018
#:::::::::::::::::::#
# RECORDING PWCOUNTIES THAT CORRESPOND TO PWSTATES 
# COUNTY FIPS CODE: 91 => Montgomery County, PA
# COUNTY FIPS CODE: 7 => Camden County, NJ
# COUNTY FIPS CODE: 29 => Chester County, PA
# COUNTY FIPS CODE: 45 => Delaware County, PA
# COUNTY FIPS CODE: 17 => Bucks County, PA
#:::::::::::::::::::#
#       2020
#:::::::::::::::::::#
# RECORDING PWCOUNTIES THAT CORRESPOND TO PWSTATES 
# COUNTY FIPS CODE: 91 => Montgomery County, PA
# COUNTY FIPS CODE: 3 => New Castle, DE
# COUNTY FIPS CODE: 7 => Camden County, NJ
# COUNTY FIPS CODE: 15 => Gloucester County, NJ
# COUNTY FIPS CODE: 45 => Delaware County, PA
#:::::::::::::::::::#
#       2021
#:::::::::::::::::::#
# RECORDING PWCOUNTIES THAT CORRESPOND TO PWSTATES 
# COUNTY FIPS CODE: 61 => New York County, PA
# COUNTY FIPS CODE: 7 => Camden County, NJ
# COUNTY FIPS CODE: 91 => Montgomery County, PA
# COUNTY FIPS CODE: 17 => Bucks County, PA
#:::::::::::::::::::::::::::::::::::::::::
###########################################
```

### 3. Is Remote Work Higher among New Residents After COVID-19

``` r
# 7a. Remote work => is remote work higher in migrants who came to Philly after the pandemic
table <- svytable(~TRANWORK+YEAR, design = dtaDesign_pre)
df_pre <- as.data.frame.matrix(table) 
df_pre <- rownames_to_column(df_pre, var = "Variable") %>% as_tibble() 
#_________________________________________#
table <- svytable(~TRANWORK+YEAR, design = dtaDesign_post)
df_post <- data.frame(rbind(table))
df_post <- rownames_to_column(df_post, var = "Variable") %>% as_tibble()
#_________________________________________#
df_remote <- df_pre %>% 
  left_join(df_post, by = c("Variable"))
#_________________________________________#
# calculating proportion of remote work among new residents
df_remote <- df_remote %>% 
  gather(Year, count, `2018`:X2021) %>% 
  group_by(Year) %>% 
  mutate(remote_prop = count[which(Variable == "80")]/sum(count) # remote work code = 80
  )
```

### 4. What are the top jobs among new residents who work remotely?

``` r
# 7b. Remote work jobs => what are the top 10 jobs that remote working migrants have?
#_________________________________________#
table <- svytable(~TRANWORK+OCC, design = dtaDesign_post)
df_post <- as.data.frame(table)
df_post <- df_post %>% rename(FREQ_postCOVID = Freq)
#_________________________________________#
df_remoteOCC <- df_post %>% 
  filter(TRANWORK == "80") # filtering for remote work only
#_________________________________________#
df_remoteOCC <- df_remoteOCC %>%
  mutate(OCC = as.character(OCC)) %>% 
  mutate(OCC = ifelse(OCC == "Other community and social service specialists", "Other social sercvice specialists",
                      ifelse(OCC == "Real estate brokers and sales agents", "Real estate brokers",
                             ifelse(OCC == "Transportation, storage, and distribution managers", "Transportation managers", OCC))))
#_________________________________________#
# calculating proportions of remote work occupations 
df_remoteOCC <- df_remoteOCC %>% 
  mutate(prop_postCOVID = FREQ_postCOVID/sum(FREQ_postCOVID)
         ) %>% 
  arrange(desc(prop_postCOVID)) %>% # choosing top 10 remote work occupations by proportion
  slice(1:10)
```
