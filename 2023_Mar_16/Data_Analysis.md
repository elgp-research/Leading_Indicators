New Residents Patterns in Philadelphia: Part 3
================

## Data Sources

For this Leading Indicator, we collected data from the IPUMS USA micro
data series. We created a data extract from the [IPUMS
website](https://usa.ipums.org/usa/) for the following respondent-level
characteristics:

- SEX
- AGE
- RACE
- HISPAN (Hispanic origin)
- EDUC (Educational attainment)
- INCT (Total personal income)
- MIGPLAC1 (State or county of residence 1 year ago)
- MIGCOUNTY1 (County of residence 1 year ago)
- MIGTYPE1 (Metropolitan status 1 year ago)
- PWSTATE2 (Place of work: state)
- PWCOUNTY (Place of work: county)
- OCC (Occupation)
- TRANWORK (Means of transportation to work)

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
  slice(1:5) %>% 
  mutate(Variable = ifelse(Variable == "Secretaries and administrative assistants, except legal, medical, and executive", "Secretaries and administerative assistants", Variable))
```

## Ranking of Top 5 Occupations Among New Residents

![](Data_Analysis_files/figure-gfm/visual_top5-1.png)<!-- -->
