#install and load the gridExtra and tidyverse library
install.packages("plotly")
install.packages("gridExtra")
install.packages("ggpubr")
install.packages("plotly")
install.packages("janitor")
library(gridExtra)
library(plotly)
library(tidyverse)
library(ggpubr)
library(knitr)
library(janitor)


#load and tidy up the data
benoa <- read.csv("data/landing_data.csv")
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

#benoa <- benoa %>% mutate(month_name = month.abb[month])
head(benoa)
tail(benoa)
head_benoa <- head(benoa, 10)

#data cleaning for length analysis
benoa_clean <- benoa %>% filter(!is.na(weight)) %>% filter(!is.na(length)) %>%
  filter(handling !="") %>% filter(species !="")
write_csv(benoa_clean, "data/length_data.csv")

#load the clean data
benoa_length <- read.csv("data/length_data.csv")
unique(benoa_length$species)


#to insert a picture in rmarkdown
# ![Caption for the picture.](/path/to/image.png)

#fig.1. the composition of species landed in benoa fishing port 2016
ggplot(data = benoa) + geom_bar(mapping = aes(x = species)) + 
  theme(axis.text.x = element_text(size = 8, angle = 0))

#fig.2. the total weight (ton) of each species landed in benoa fishing port 2016
weigth_total <- aggregate(benoa["weight_ton"], benoa["species"], FUN = sum)
ggplot(weigth_total, aes(x = species, y = weight_ton)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(size = 8, angle = 0))

#fig.3. the distribution of weigth within each species
ggplot(data = benoa, aes(x = species, y = weight)) + geom_jitter(alpha = 0.25, color = "yellow") + 
  geom_boxplot(alpha = 0.1, color = "blue")





















