setwd('E:\\Classes\\Data Scientist Certificate\\Exploratory Data Analysis\\Project\\Project 2\\')

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Filter on coal combustion source
CombSCC=grep('comb',SCC$SCC.Level.One,ignore.case = T)
CoalSCC=grep('coal',SCC$SCC.Level.Four,ignore.case = T)
CoalCombSSC=SCC$SCC[intersect(CombSCC,CoalSCC)]
CoalCombNEI=NEI[NEI$SCC %in% CoalCombSSC,]

library(data.table)
CoalCombNEI=data.table(CoalCombNEI)
PM25Ann=CoalCombNEI[,sum(Emissions),by=list(year,type)]


library(ggplot2)
library(scales)
Plot_theme = theme_bw() + theme(plot.title = element_text(size = 12), axis.title.x = element_text(size = 10), 
                                axis.title.y = element_text(size = 10), axis.text.x = element_text(size = 8), 
                                axis.text.y = element_text(size = 8))

ggplot(data=PM25Ann)+
  geom_bar(aes(factor(year),V1/10^5),stat='identity')+
  facet_grid(.~type)+
  labs(x="year", y=expression(paste("Total",PM[2.5],"Emission (",10^5,"Tons)"))) + 
  labs(title=expression("Coal Combustion Source PM"[2.5]*" Emissions Across US from 1999-2008")))+
  Plot_theme 
