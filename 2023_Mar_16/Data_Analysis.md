New Residents Patterns in Philadelphia: Part 3
================

## GitHub Documents

This is an R Markdown format used for publishing markdown documents to
GitHub. When you click the **Knit** button all R code chunks are run and
a markdown file (.md) suitable for publishing to GitHub is generated.

## Installing Libraries

You are going to need the following libraries to run the r code from
IPUMS micro data series.

- library(tidyverse)
- library(ipumsr)
- library(shiny)
- library(htmltools)
- library(DT)
- library(ggtext)
- library(ggrepel)
- library(ggpubr)
- library(survey)

``` r
dat <- read.csv("ipums_nresidents.csv")
```

## Cleaning Data for Descriptive Analysis

``` r
# attaching labels to factor variables in IPUMS data 
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
         HISPAND = as_factor(HISPAND))
#:::::::::::::::::::::::::::::::::::::::::
# adding labels to OCCUPATION codes 
dat <- dat %>% 
  mutate(OCC = recode(OCC,
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
#:::::::::::::::::::::::::::::::::::::::::
# separating data into pre and post pandemic 
pre_COVID <- dat %>% 
  filter(YEAR == 2018 | YEAR == 2019)
post_COVID <- dat %>% 
  filter(YEAR == 2020 | YEAR == 2021)
#:::::::::::::::::::::::::::::::::::::::::
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

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
