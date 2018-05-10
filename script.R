#install and load the gridExtra and tidyverse library
install.packages("gridExtra")
install.packages("ggpubr")
library(gridExtra)
library(tidyverse)
library("ggpubr")

#load the data
benoa <- read.csv("data/landing_data.csv")

#fig.1. the composition of species landed in benoa fishing port 2016
ggplot(data = benoa) + geom_bar(mapping = aes(x = species)) + 
  theme(axis.text.x = element_text(size = 8, angle = 0))

#fig.2. the total weight of each species landed in benoa fishing port 2016
weigth_total <- aggregate(benoa["weight"], benoa["species"], FUN = sum)
ggplot(weigth_total, aes(x = species, y = weight)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(size = 8, angle = 0))

#fig.3. 