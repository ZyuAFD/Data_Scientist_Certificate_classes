setwd('E:\\Classes\\Data Scientist Certificate\\Exploratory Data Analysis\\Project\\Project 2\\')

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(data.table)
NEI=data.table(NEI)
PM25Ann=NEI[NEI$fips=="24510",sum(Emissions),by=list(year,type)]


library(ggplot2)
library(scales)
Plot_theme = theme_bw() + theme(plot.title = element_text(size = 12), axis.title.x = element_text(size = 10), 
                                axis.title.y = element_text(size = 10), axis.text.x = element_text(size = 8), 
                                axis.text.y = element_text(size = 8))

ggplot(data=PM25Ann)+
  geom_bar(aes(factor(year),V1),stat='identity')+
  facet_grid(.~type)+
  labs(x="year", y=expression("Total PM"[2.5]*" Emission (Tons)")) + 
  labs(title=expression("PM"[2.5]*" Emissions, Baltimore City 1999-2008 by Source Type"))+
  Plot_theme 
