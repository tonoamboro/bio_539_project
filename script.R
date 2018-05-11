#install and load the gridExtra and tidyverse library
install.packages("plotly")
install.packages("gridExtra")
install.packages("ggpubr")
install.packages("janitor")
install.packages("FSA")
library(plotly)
library(gridExtra)
library(tidyverse)
library(ggpubr)
library(knitr)
library(janitor)
library(FSA)

#load and tidy up the data
benoa <- read.csv("data/landing_data.csv")

#DATA VISUALIZATION
benoa <- benoa %>% mutate(weight_ton = weight/1000)

#convert month as character
#benoa$month <- as.numeric(as.character(benoa$month))

#plot monthly landing per species (plot 2)
monthly_landing <- benoa %>% group_by(month, species) %>%
  summarize(weight_by_month_species = sum(weight_ton))
plot_monthly_landing <- ggplot(monthly_landing, aes(x = month, y = weight_by_month_species, color = species)) + 
  geom_line() + geom_point() + scale_x_continuous(name = "Month", breaks = seq(1, 12, 1)) + 
  scale_y_continuous(name = "Weight [ton]")

#compute total weight by species
totalweigth_by_species <- benoa %>% group_by(species) %>% summarise(total = sum(weight_ton))
total_prod <- totalweigth_by_species %>% summarise(total_production = sum(total))

#piechart of total weight by species (plot 1)
plot_ly(totalweigth_by_species, labels = ~species, values = ~total, type = 'pie',
             textposition = 'inside',
             textinfo = 'label+percent',
             insidetextfont = list(color = '#FFFFFF'),
             hoverinfo = 'text',
             text = ~paste('', total, 'ton'),
             marker = list(colors = colors,
                           line = list(color = '#FFFFFF', width = 1)),
             #The 'pull' attribute can also be used to create space between the sectors
             showlegend = FALSE) %>%
  layout(title = 'Total production by species in 2016',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#create table 1
table1 <- totalweigth_by_species %>% rename(Specie = species, 'Total production [ton]' = total) 
table1_fix <- table1 %>% adorn_totals("row")

#barchart for monthly catches
totalweight_by_month <- benoa %>% group_by(month) %>% summarise(total = sum(weight_ton))
plot_totalweight_by_month <- ggplot(totalweight_by_month, aes(x = month, y = total)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(size = 8, angle = 0)) + 
  scale_x_continuous(name = "Month", breaks = seq(1, 12, 1)) + 
  scale_y_continuous(name = "Weight [ton]")

#combine total weight by month and monthly landing
grid.arrange(plot_totalweight_by_month, plot_monthly_landing, ncol=2, widths=c(5,7))


#LENGTH-WEIGHT ANALYSIS
#data cleaning
benoa_clean <- benoa %>% filter(!is.na(weight)) %>% filter(!is.na(length)) %>%
  filter(handling !="") %>% filter(species !="")
#create table 2
table2_fix <- head(benoa_clean, 7)
#bigeye tuna
bigeye <- benoa_clean %>% filter(species == "BET")
#yellowfin
yellowfin <- benoa_clean %>% filter(species == "YFT")
#bluefin
bluefin <- benoa_clean %>% filter(species == "SBT")

#lw_raw_plots
bigeye_raw <- lm(weight ~ length, data = bigeye)
lwbigeye_plot<- fitPlot(bigeye_raw, xlab="Length [cm]", ylab="Weight [kg]", 
                        main="Bigeye tuna")
yellowfin_raw <- lm(weight ~ length, data = yellowfin)
lwyellowfin_plot<- fitPlot(yellowfin_raw, xlab="Length [cm]", ylab="Weight [kg]", 
                           main="Yellowfin tuna")
bluefin_raw <- lm(weight ~ length, data = bluefin)
lwbluefin_plot<- fitPlot(bluefin_raw, xlab="Length [cm]", ylab="Weight [kg]", 
                         main="Bluefin tuna")
par(mfrow=c(3,1), lwbigeye_plot, lwyellowfin_plot, lwbluefin_plot)

#fitting data to the model
##Bigeye tuna
bigeye$logL <- log(bigeye$length) 
bigeye$logW <- log(bigeye$weight)
lm1 <- lm(logW~logL, data=bigeye)
fitPlot(lm1,xlab="log Fork Length [cm]",ylab="log Weight [kg]")
summary(lm1)

##Yellowfin tuna
yellowfin$logL <- log(yellowfin$length) 
yellowfin$logW <- log(yellowfin$weight)
lm2 <- lm(logW~logL, data=yellowfin)
fitPlot(lm2,xlab="log Fork Length [cm]",ylab="log Weight [kg]")
summary(lm2)

##Bluefin tuna
bluefin$logL <- log(bluefin$length) 
bluefin$logW <- log(bluefin$weight)
lm3 <- lm(logW~logL, data=bluefin)
fitPlot(lm3,xlab="log Fork Length [cm]",ylab="log Weight [kg]")
summary(lm3)


