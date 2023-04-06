## ----libraries-----------------------------------------------------------------------------------------------------
library(tidyverse)
library(fmsb)
library(patchwork)
library(scales)

## ----importing data------------------------------------------------------------------------------------------------
db_indicators <- read_csv("Data/db_indicators.csv")
db_sectors <- read_csv("Data/db_sectors.csv")

## ----filtering indicator data--------------------------------------------------------------------------------------
db_indicators <- db_indicators %>% 
  group_by(indicator) %>% 
  mutate(value_normalized = rescale(value, to = c(0,100))) %>% 
  filter(County == "OrleansParish, LA" | County == "Philadelphia, PA")

## ----changing indicator labels-------------------------------------------------------------------------------------
db_indicators <- db_indicators %>% 
  mutate(indicator = ifelse(indicator == "Total Population", "Population",
                            ifelse(indicator == "Median Household Income", "Median Income",
                                   ifelse(indicator == "Crimes per 100,000 residents", "Crime Rate",
                                          ifelse(indicator == "Laborforce Participation", "Employment Rate", indicator)))))
                          
## ----New Orleans Subset Indicators---------------------------------------------------------------------------------
df_orleans <- subset(db_indicators, County == "OrleansParish, LA")[,c("indicator", "value_normalized")]
df_orleans <- df_orleans %>% 
  spread(indicator, value_normalized)

## ----Creating minimum and maximum values for radar chart-----------------------------------------------------------
df_radar <- data.frame(`Crimes Rate` = c(100,0),
                        `Diversity Index` = c(100,0),
                        `Enrollment Rate` = c(100,0),
                        `GDP per capita` = c(100,0),
                        `Gini Coefficient` = c(100,0),
                        `Employment Rate` = c(100,0),
                        `Life Expectancy` = c(100,0),
                        `Male Proportion` = c(100,0),
                        `Median Income` = c(100,0),
                        `Poverty Rate` = c(100,0),
                        `Population` = c(100,0)
                        )

df_radar <- df_radar %>% 
  rename(`Crime Rate` = Crimes.Rate,
         `Diversity Index` = Diversity.Index,
         `Enrollment Rate` = Enrollment.Rate,
         `GDP per capita` = GDP.per.capita,
         `Gini Coefficient` = Gini.Coefficient,
         `Employment Rate` = Employment.Rate,
         `Life Expectancy` = Life.Expectancy,
         `Male Proportion` = Male.Proportion,
         `Median Income` = Median.Income,
         `Poverty Rate` = Poverty.Rate,
         `Population` = Population
         )

## ----Philadelphia Subset Indicators---------------------------------------------------------------------------------
df_philly <- subset(db_indicators, County == "Philadelphia, PA")[,c("indicator", "value_normalized")]
df_philly <- df_philly %>% 
  spread(indicator, value_normalized)

## ----Creating Radar Chart Data--------------------------------------------------------------------------------------
df_radar <- rbind(df_radar, df_orleans, df_philly)
# Set the name of the third row to "New Orleans"
rownames(df_radar)[3] <- "New Orleans"
# Set the name of the fourth row to "Philadelphia"
rownames(df_radar)[4] <- "Philadelphia"

## ----Layout for plots----------------------------------------------------------------------------------------------
# Set up a 1x2 panel plot
par(mfrow = c(1, 1))

# Color vector
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9))
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4))


# plot with default options:
radarchart( df_radar  , axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,5), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)

# Add a legend
legend(x=1.2, y=1, legend = rownames(df_radar[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "black", cex=1.2, pt.cex=3)



## ----New Orleans Radar Chart---------------------------------------------------------------------------------------
radarchart(df_orleans,
           # title for graph
           title = "New Orleans",
           
           #custom polygon
           pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
           
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
           
           #custom labels
           vlcex=0.6)

## ----Philadelphia Radar Chart---------------------------------------------------------------------------------------
radarchart(df_philly,
           # title for graph
           title = "Philadelphia",
           
           #custom polygon
           pcol=rgb(1,0.5,0,0.9) , pfcol=rgb(1,0.5,0,0.5) , plwd=4 , 
           
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
           
           #custom labels
           vlcex=0.6)
                              
## ----importing data------------------------------------------------------------------------------------------------
db_sectors <- db_sectors %>% 
  filter(County == "OrleansParish, LA" | County == "Philadelphia, PA")





