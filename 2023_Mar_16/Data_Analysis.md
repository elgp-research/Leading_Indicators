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
dat_newresidents <- read.csv("ipums_nresidents.csv")
dat_allresidents <- read.csv("ipums_allresidents.csv")
```

## Cleaning Data

``` r
filenames <- list.files(pattern = ".csv",
                        full.names = TRUE)
#___________________________________________________#
# creating function for cleaning data 
clean <- function(filename) {
  dat <- read.csv(file = filename)
  dat <- dat %>% 
  mutate(MIGPLAC1 = as_factor(MIGPLAC1),
         MIGCOUNTY1 = as_factor(MIGCOUNTY1),
         MIGTYPE1 = as_factor(MIGTYPE1),
         
         PWSTATE2 = as_factor(PWSTATE2),
         PWCOUNTY = as_factor(PWCOUNTY),
         
         RACE = as_factor(RACE),
         RACED = as_factor(RACED),
         SEX = as_factor(SEX),
         EDUC = as_factor(EDUC),
         
         INDNAICS = as_factor(INDNAICS),
         inc_quartile = ntile(INCTOT, 4),
         
         TRANWORK = as_factor(TRANWORK),
         OCC = as_factor(OCC),
         INDNAICS = as_factor(INDNAICS),
         
         HISPAN = as_factor(HISPAN),
         HISPAND = as_factor(HISPAND), 
         
         OCC = recode(OCC,
                      `10` = "Chief executives and legislators",
                      `20` = "General and operations managers",
                      `51` = "Marketing managers",
                      `52` = "Sales managers",
                      `102` = "Facilities managers",
                      `110` = "Computer and information systems managers",
                      `120` = "Financial managers",
                      `136` = "Human resources managers",
                      `140` = "Industrial production managers",
                      `150` = "Purchasing managers",
                      `160` = "Transportation, storage, and distribution managers",
                      `220` = "Construction managers",
                      `230` = "Education and childcare administrators",
                      `310` = "Food service managers",
                      `335` = "Entertainment and recreation managers",
                      `350` = "Medical and health services managers",
                      `360` = "Natural sciences managers",
                      `410` = "Property, real estate, and community association managers",
                      `420` = "Social and community service managers",
                      `425` = "Emergency management directors",
                      `440` = "Other managers",
                      `530` = "Purchasing agents, except wholesale, retail, and farm products",
                      `540` = "Claims adjusters, appraisers, examiners, and investigators",
                      `565` = "Compliance officers",
                      `600` = "Cost estimators",
                      `630` = "Human resources workers",
                      `650` = "Training and development specialists",
                      `700` = "Logisticians",
                      `705` = "Project management specialists",
                      `710` = "Management analysts",
                      `726` = "Fundraisers",
                      `735` = "Market research analysts and marketing specialists",
                      `750` = "Business operations specialists, all other",
                      `800` = "Accountants and auditors",
                      `810` = "Property appraisers and assessors",
                      `845` = "Financial and investment analysts",
                      `850` = "Personal financial advisors",
                      `860` = "Insurance underwriters",
                      `930` = "Tax examiners and collectors, and revenue agents",
                      `940` = "Tax preparers",
                      
                      `1006` = "Computer systems analysts",
                      `1010` = "Computer programmers",
                      `1021` = "Software developers",
                      `1032` = "Web and digital interface designers",
                      `1050` = "Computer support specialists",
                      `1108` ="Computer occupations, all other",
                      `1220` ="Operations research analysts",
                      `1240` ="Other mathematical science occupations",
                      `1320` ="Aerospace engineers",
                      `1360` ="Civil engineers",
                      `1420` = "Environmental engineers",
                      `1430` = "Industrial engineers, including health and safety",
                      `1530` = "Other engineers",
                      `1610` = "Biological scientists",
                      `1650` = "Other life scientists",
                      `1720` = "Chemists and materials scientists",
                      `1745` = "Environmental scientists and specialists, including health",
                      `1760` = "Physical scientists, all other",
                      `1860` = "Other social scientists",
                      `1910` = "Biological technicians",
                      `1920` = "Chemical technicians",
                      `1970` = "Other life, physical, and social science technicians",
                      `2001` = "Substance abuse and behavioral disorder counselors",
                      `2002` = "Educational, guidance, and career counselors and advisors",
                      `2004` = "Mental health counselors",
                      `2014` = "Social workers, all other",
                      `2015` = "Probation officers and correctional treatment specialists",
                      `2016` = "Social and human service assistants",
                      `2025` =  "Other community and social service specialists",
                      `2040` = "Clergy",
                      `2050` = "Directors, religious activities and education",
                      `2060` = "Religious workers, all other",
                      `2100` = "Lawyers, and judges, magistrates, and other judicial workers",
                      `2145` = "Paralegals and legal assistants",
                      `2170` = "Title examiners, abstractors, and searchers",
                      `2180` = "Legal support workers, all other",
                      `2205` = "Postsecondary teachers",
                      `2300` = "Preschool and kindergarten teachers",
                      `2310` = "Elementary and middle school teachers",
                      `2320` = "Secondary school teachers",
                      `2330` = "Special education teachers",
                      `2350` = "Tutors",
                      `2360` = "Other teachers and instructors",
                      `2400` = "Archivists, curators, and museum technicians",
                      `2545` = "Teaching assistants", 
                      `2555` = "Other educational instruction and library workers", 
                      `2600` = "Artists and related workers",
                      `2634` = "Graphic designers",
                      `2635` = "Interior designers",
                      `2640` = "Other designers",
                      `2710` = "Producers and directors",
                      `2722` = "Coaches and scouts",
                      `2740` = "Dancers and choreographers",
                      `2752` = "Musicians and singers",
                      `2810` = "News analysts, reporters, and journalists", 
                      `2825` = "Public relations specialists",
                      `2830` = "Editors",
                      `2840` = "Technical writers",
                      `2850` = "Writers and authors",
                      `2861` = "Interpreters and translators",
                      `2910` = "Photographers",
                      `2920` = "Television, video, and film camera operators and editors",
                      `3010` = "Dentists",
                      `3050` = "Pharmacists",
                      `3090` = "Physicians",
                      `3100` = "Surgeons",
                      `3110` = "Physician assistants", 
                      `3140` = "Audiologists",
                      `3150` = "Occupational therapists",
                      `3160` = "Physical therapists",
                      `3230` = "Speech-language pathologists",
                      `3245` = "Other therapists",
                      `3250` = "Veterinarians",
                      `3255` = "Registered nurses",
                      `3256` = "Nurse anesthetists",
                      `3258` = "Nurse practitioners, and nurse midwives",
                      `3300` = "Clinical laboratory technologists and technicians",
                      `3322` = "Diagnostic medical sonographers",
                      `3323` = "Radiologic technologists and technicians",
                      `3421` = "Pharmacy technicians",
                      `3500` = "Licensed practical and licensed vocational nurses", 
                      `3515` = "Medical records specialists",
                      `3550` = "Other healthcare practitioners and technical occupations",
                      `3601` = "Home health aides",
                      `3602` = "Personal care aides",
                      `3603` = "Nursing assistants",
                      `3620` = "Physical therapist assistants and aides",
                      `3630` = "Massage therapists",
                      `3640` = "Dental assistants",
                      `3646` = "Medical transcriptionists",
                      `3649` = "Phlebotomists",
                      `3802` = "Correctional officers and jailers",
                      `3870` = "Police officers",
                      `3930` = "Security guards and gaming surveillance officers",
                      `3960` = "Other protective service workers",
                      `4000` = "Chefs and head cooks",
                      `4010` = "First-line supervisors of food preparation and serving workers",
                      `4020` = "Cooks",
                      `4030` = "Food preparation workers",
                      `4040` = "Bartenders",
                      `4055` = "Fast food and counter workers",
                      `4110` = "Waiters and waitresses",
                      `4130` = "Dining room and cafeteria attendants and bartender helpers",
                      `4140` = "Dishwashers",
                      `4210` = "First-line supervisors of landscaping, lawn service, and groundskeeping workers",
                      `4220` = "Janitors and building cleaners",
                      `4230` = "Maids and housekeeping cleaners",
                      `4255` = "Other grounds maintenance workers",
                      `4350` = "Animal caretakers",
                      `4435` = "Other entertainment attendants and related workers",
                      `4521` = "Manicurists and pedicurists",
                      `4540` = "Tour and travel guides",
                      `4600` = "Childcare workers",
                      `4621` = "Exercise trainers and group fitness instructors",
                      `4622` = "Recreation workers",
                      `4640` = "Residential advisors",
                      `4655` = "Personal care and service workers, all other", 
                      `4700` = "First-line supervisors of retail sales workers",
                      `4710` = "First-line supervisors of non-retail sales workers",
                      `4720` = "Cashiers",
                      `4760` = "Retail salespersons",
                      `4810` = "Insurance sales agents",
                      `4820` = "Securities, commodities, and financial services sales agents", 
                      `4850` = "Sales representatives, wholesale and manufacturing",
                      `4920` = "Real estate brokers and sales agents",
                      `4965` = "Sales and related workers, all other",
                      `5000` = "First-line supervisors of office and administrative support workers",
                      `5100` = "Bill and account collectors",
                      `5120` = "Bookkeeping, accounting, and auditing clerks",
                      `5220` = "Court, municipal, and license clerks",
                      `5230` = "Credit authorizers, checkers, and clerks",
                      `5240` = "Customer service representatives",
                      `5300` = "Hotel, motel, and resort desk clerks", 
                      `5320` = "Library assistants, clerical",
                      `5400` = "Receptionists and information clerks",
                      `5410` = "Reservation and transportation ticket agents and travel clerks",
                      `5510` = "Couriers and messengers",
                      `5730` = "Medical secretaries and administrative assistants", 
                      `5740` = "Secretaries and administrative assistants, except legal, medical, and executive",
                      `5810` = "Data entry keyers",
                      `5820` = "Word processors and typists",
                      `5860` = "Office clerks, general",
                      `5920` = "Statistical assistants",
                      `5940` = "Other office and administrative support workers",
                      `6050` = "Other agricultural workers",
                      `6230` = "Carpenters",
                      `6260` = "Construction laborers",
                      `6355` = "Electricians",
                      `6800` = "Derrick, rotary drill, and service unit operators, and roustabouts, oil, gas, and mining",
                      `7200` = "Automotive service technicians and mechanics",
                      `7315` = "Heating, air conditioning, and refrigeration mechanics and installers",
                      `7330` = "Industrial and refractory machinery mechanics",
                      `7340` = "Maintenance and repair workers, general",
                      `7540` = "Locksmiths and safe repairers",
                      `7610` = "Helpers--installation, maintenance, and repair workers",
                      `7750` = "Other assemblers and fabricators",
                      `7800` = "Bakers",
                      `7810` = "Butchers and other meat, poultry, and fish processing workers",
                      `7840` = "Food batchmakers",
                      `8030` = "Machinists",
                      `8140` = "Welding, soldering, and brazing workers",
                      `8255` = "Printing press operators",
                      `8300` = "Laundry and dry-cleaning workers",
                      `8350` = "Tailors, dressmakers, and sewers",
                      `8610` = "Stationary engineers and boiler operators",
                      `8740` = "Inspectors, testers, sorters, samplers, and weighers",
                      `8760` = "Dental and ophthalmic laboratory technicians and medical appliance technicians",
                      `8800` = "Packaging and filling machine operators and tenders",
                      `9050` = "Flight attendants",
                      `9130` = "Driver/sales workers and truck drivers",
                      `9142` = "Taxi drivers",
                      `9300` = "Sailors and marine oilers, and ship engineers",
                      `9310` = "Ship and boat captains and operators", 
                      `9415` = "Passenger attendants",
                      `9610` = "Cleaners of vehicles and equipment", 
                      `9620` = "Laborers and freight, stock, and material movers, hand",
                      `9640` = "Packers and packagers, hand",
                      `9645` = "Stockers and order fillers",
                      `9720` = "Refuse and recyclable material collectors",
                      `9830` = "Military, rank not specified"
                      ))
}
#______________________________________________________________
# applying clean function to IPUMS datasets
dat <- clean("./ipums_nresidents.csv")
dat_allresidents <- clean("./ipums_allresidents.csv")
```

### Attaching survey weights

#### Survey weights on dataset with only new residents in Philadelphia

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

#### Survey weights on dataset with all residents in Philadelphia

``` r
#_________________________________________#
# separating data into pre and post pandemic 
pre_COVID <- dat_allresidents %>% 
  filter(YEAR == 2018 | YEAR == 2019)
post_COVID <- dat_allresidents %>% 
  filter(YEAR == 2020 | YEAR == 2021)
#_________________________________________#
# attaching survey weights 
dtaDesign_pre_all <- svydesign(id      = ~CLUSTER,
                               strata  = ~STRATA,
                               weights = ~PERWT,
                               nest    = TRUE,
                               data    = pre_COVID)
#_________________________________________#
dtaDesign_post_all <- svydesign(id     = ~CLUSTER,
                                strata  = ~STRATA,
                                weights = ~PERWT,
                                nest    = TRUE,
                                data    = post_COVID)
```

## Cross Tabulations

### 1. Top 5 Occupations

``` r
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
  mutate(Variable = ifelse(Variable == "Secretaries and administrative assistants, except legal, medical, and executive", "Administerative assistants", 
                           ifelse(Variable == "Lawyers, and judges, magistrates, and other judicial workers", "Lawyers, judges and judicial workers", Variable)))
```

### 2. Are New Residents Mostly Working in Philadelphia?

#### 2.1 Cross-tabulation of new residents only

``` r
table <- svytable(~PWCOUNTY+YEAR, design = dtaDesign_pre) # creating table with IPUMS survey weights 
df_pre <- as.data.frame.matrix(table) 
df_pre <- rownames_to_column(df_pre, var = "Variable") %>% as_tibble()
df_pre <- df_pre %>% 
  group_by(Variable) %>% 
  mutate(`Pre-COVID` = mean(`2018`:`2019`))
#_________________________________________#
table <- svytable(~PWCOUNTY+YEAR, design = dtaDesign_post) # creating table with IPUMS survey weights 
df_post <- data.frame(rbind(table))
df_post <- rownames_to_column(df_post, var = "Variable") %>% as_tibble() 
df_post <- df_post %>% 
  group_by(Variable) %>% 
  mutate(`Post-COVID` = mean(X2020:X2021))
#_________________________________________#
df_pwork <- df_pre %>% # merging datasets togther
  left_join(df_post, by = c("Variable"))
#_________________________________________#
df_pwork <- df_pwork %>% 
  mutate(resident_type = "New Residents")
```

#### 2.2 Cross-tabulation of all residents

``` r
table <- svytable(~PWCOUNTY+YEAR, design = dtaDesign_pre_all) # creating table with IPUMS survey weights 
df_pre <- as.data.frame.matrix(table) 
df_pre <- rownames_to_column(df_pre, var = "Variable") %>% as_tibble()
df_pre <- df_pre %>% 
  group_by(Variable) %>% 
  mutate(`Pre-COVID` = mean(`2018`:`2019`))
#_________________________________________#
table <- svytable(~PWCOUNTY+YEAR, design = dtaDesign_post_all) # creating table with IPUMS survey weights 
df_post <- data.frame(rbind(table))
df_post <- rownames_to_column(df_post, var = "Variable") %>% as_tibble() 
df_post <- df_post %>% 
  group_by(Variable) %>% 
  mutate(`Post-COVID` = mean(X2020:X2021))
#_________________________________________#
df_pwork_all <- df_pre %>% # merging datasets together
  left_join(df_post, by = c("Variable"))
#_________________________________________#
df_pwork_all <- df_pwork_all %>% 
  mutate(resident_type = "All Residents")
```

#### 2.3 Combining datasets for all residents and only new residents in Philadelphia

``` r
# combining datasets of new residents and all residents
df <- rbind(df_pwork_all, df_pwork)
# creating dummy of philly 
df <- df %>% 
  select(Variable, `2018`, `2019`, X2020, X2021, resident_type) %>% 
  gather(Year, count, `2018`:X2021) %>% 
  mutate(Philly = ifelse(Variable == 101, "Philadelphia", "Not Philadelphia")) %>% 
  group_by(Year, resident_type, Philly) %>% 
  mutate(sum_pwork = sum(count)) %>%
  group_by(Year, resident_type) %>% 
  mutate(prop_pwork = sum_pwork/sum(count))
#_____________________________________________#
# keeping distinct values of prop_pwork in dataset
df <- df %>% 
  mutate(Year = recode(Year,
                       "2018" = 2018,
                       "2019" = 2019,
                       "X2020" = 2020,
                       "X2021" = 2021)) %>% 
  distinct(prop_pwork, .keep_all = TRUE) 
#_____________________________________________#
```

### Robustness Checks

We used different datasets compared to the main analysis. This dataset
includes estimates for previous years starting from 2005.

``` r
# cleaning historical data files
dat <- clean("./ipums_nresidents_historical.csv")
dat_all <- clean("./ipums_allresidents_historical.csv")

# attaching survey weights 
dtaDesign_nresidents <- svydesign(id      = ~CLUSTER,
                                  strata  = ~STRATA,
                                  weights = ~PERWT,
                                  nest    = TRUE,
                                  data    = dat)
#_________________________________________#
dtaDesign_allresidents <- svydesign(id     = ~CLUSTER,
                                    strata  = ~STRATA,
                                    weights = ~PERWT,
                                    nest    = TRUE,
                                    data    = dat_all)


## Cross Tabulations of all Residents Using Historical Data
#:::::::::::::::::::::::::::::::::::::::::
table <- svytable(~PWCOUNTY+YEAR, design = dtaDesign_nresidents) # creating table with IPUMS survey weights 
df_nresidents <- as.data.frame.matrix(table) 
df_nresidents <- rownames_to_column(df_nresidents, var = "Variable") %>% as_tibble()
df_nresidents <- df_nresidents %>% 
  mutate(resident_type = "New Residents")
#_________________________________________#
table <- svytable(~PWCOUNTY+YEAR, design = dtaDesign_allresidents) # creating table with IPUMS survey weights 
df_allresidents <- data.frame(rbind(table))
df_allresidents <- rownames_to_column(df_allresidents, var = "Variable") %>% as_tibble() 
df_allresidents <- df_allresidents %>% 
  mutate(resident_type = "All Residents")
#_________________________________________#
# changing year names in all residents dataset to match nresident dataset
df_allresidents <- df_allresidents %>% 
  rename(`2005` = X2005, `2006` = X2006, `2007` = X2007, `2008` = X2008, `2009` = X2009,
         `2010` = X2010, `2011` = X2011, `2012` = X2012, `2013` = X2013,
         `2014` = X2014, `2015` = X2015, `2016` = X2016, `2017` = X2017,
         `2018` = X2018, `2019` = X2019, `2020` = X2020, `2021` = X2021
         )

#_________________________________________#

df <- rbind(df_allresidents, df_nresidents)
#_________________________________________#

# creating dummy of philly 
df <- df %>% 
  gather(Year, count, `2005`:`2021`) %>% 
  mutate(County = ifelse(Variable == 101, "Philadelphia", "Not Philadelphia")) %>% 
  group_by(Year, resident_type, County) %>% 
  mutate(sum_pwork = sum(count)) %>%
  group_by(Year, resident_type) %>% 
  mutate(prop_pwork = sum_pwork/sum(count)) %>% 
  distinct(prop_pwork, .keep_all = TRUE) %>% 
  filter(County == "Philadelphia")
```

### Historical Trend for Resident Proportions

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.

![](Data_Analysis_files/figure-gfm/dumbell_plot-1.png)<!-- -->
