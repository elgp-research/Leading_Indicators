## ----libraries-----------------------------------------------------------------------------------------------------
library(tidyverse)
library(fmsb)
library(patchwork)
library(scales)
library(RColorBrewer)

## ----INDICATOR data------------------------------------------------------------------------------------------------
db_indicators <- read_csv("Data/db_indicators.csv")

## ----filtering indicator data--------------------------------------------------------------------------------------
db_indicators <- db_indicators %>% 
  group_by(indicator) %>% 
  mutate(value_normalized = rescale(value, to = c(0,100))) %>% 
  filter(County == "OrleansParish, LA" | County == "Philadelphia, PA")

## ----changing indicator labels-------------------------------------------------------------------------------------
db_indicators <- db_indicators %>% 
  mutate(indicator = ifelse(indicator == "Total Population", "Population",
                                   ifelse(indicator == "Crimes per 100,000 residents", "Crime Rate",
                                          ifelse(indicator == "Diversity Index", "Ethnic Heterogeneity", indicator))))
                          
## ----New Orleans Subset Indicators---------------------------------------------------------------------------------
df_orleans <- subset(db_indicators, County == "OrleansParish, LA")[,c("indicator", "value_normalized")]
df_orleans <- df_orleans %>% 
  spread(indicator, value_normalized)

## ----Creating minimum and maximum values for radar chart-----------------------------------------------------------
df_radar <- data.frame(`Crimes Rate` = c(100,0),
                        `Ethnic Heterogeneity` = c(100,0),
                        `Enrollment Rate` = c(100,0),
                        `GDP per capita` = c(100,0),
                        `Gini Coefficient` = c(100,0),
                        `Laborforce Participation` = c(100,0),
                        `Life Expectancy` = c(100,0),
                        `Male Proportion` = c(100,0),
                        `Median Household \nIncome` = c(100,0),
                        `Poverty Rate` = c(100,0),
                        `Population` = c(100,0)
                        )

df_radar <- df_radar %>% 
  rename(`Crime Rate` = Crimes.Rate,
         `Ethnic Heterogeneity` = Ethnic.Heterogeneity,
         `Enrollment Rate` = Enrollment.Rate,
         `GDP per capita` = GDP.per.capita,
         `Gini Coefficient` = Gini.Coefficient,
         `Laborforce Participation` = Laborforce.Participation,
         `Life Expectancy` = Life.Expectancy,
         `Male Proportion` = Male.Proportion,
         `Median Household \nIncome` = Median.Household..Income,
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

## ----Radar Chart---------------------------------------------------------------------------------------------
# Set up a 1x1 panel plot
par(mfrow = c(1, 1))

# Set up larger margins on the bottom and right sides
par(mar = c(b=4, l=0, t=2, r=0), family = "Georgia", bg = "white")

# Color vector
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(1,0.5,0,0.9))
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(1,0.5,0,0.5))


# plot with default options:
radarchart( df_radar  , axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=1.5 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="darkslategrey", caxislabels=seq(0,100,25), cglwd=0.8,
            #custom labels
            vlcex=0.8
)

title(main = "Multi-Dimensional Comparison Between \nNew Orleans and Philadelphia", 
      line = -0.4, adj = 0.01, col.main = "darkslategrey")

# Add a legend
legend(x=-0.5, y=-1.3, legend = rownames(df_radar[-c(1,2),]), bty = "n", 
       pch=20 , col=colors_in , text.col = "darkslategrey", cex=1.0, pt.cex=3,
       horiz = TRUE, xpd = TRUE)

# Adding a note
mtext("Note: All indicator estimates have been \nrescaled between 0 and 100 for both cities",
      side = 1, line = 2.5, adj = 0.98, col = "grey40", cex = 0.7)

## ---------------------------------------------------------------------------------------------------------------
## ----SECTOR data------------------------------------------------------------------------------------------------

db_sectors <- read_csv("Data/db_sectors.csv")

db_sectors <- db_sectors %>% 
  filter(County == "OrleansParish, LA" | County == "Philadelphia, PA")

db_sectors <- db_sectors %>% 
  group_by(County) %>% 
  mutate(sector = factor(sector,
                         levels = sector[order(size)]),
         County = ifelse(County == "OrleansParish, LA", "New Orleans",
                         ifelse(County == "Philadelphia, PA", "Philadelphia", County)))


## ----stacked bar chart------------------------------------------------------------------------------------------------
# Define the number of colors you want
nb.cols <- 18

# Create a color palette with 18 colors
mycolors <- colorRampPalette(brewer.pal(8, "Accent"))(nb.cols)

# Create a stacked bar chart
fig <- plot_ly(db_sectors, 
               x = ~County, 
               y = ~size, 
               type = 'bar',
               color = ~sector, 
               colors = mycolors,
               text = ~sector,
               hovertemplate=paste("<i>%{text}:</i><br>%{y}"))


fig <- fig %>% 
  layout(title = list(text = "<br>   Economic Sectors by Employment in New Orleans and Philadelphia", x=0,y=1),
         yaxis = list(title = 'Sector Size by Employment', tickformat = ".0%"), 
         barmode = 'stack',
         xaxis = list(title = ""),
         font = list(family = "Georgia", color = "darkslategrey"),
         hoverlabel = list(font = list(family = "Georgia")),
         bargap = 0.5,
         plot_bgcolor = 'grey60', paper_bgcolor = 'grey60',
         margin = list(l = 10, r = 10, b = 20, t = 50)) %>% 
  style(hoverinfo = 'none')
fig



