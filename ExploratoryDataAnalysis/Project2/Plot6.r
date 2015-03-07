setwd('E:\\Classes\\Data Scientist Certificate\\Exploratory Data Analysis\\Project\\Project 2\\')

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Filter on coal combustion source
VehSCC=grep('vehicle',SCC$SCC.Level.Two,ignore.case = T)
VehSSC=SCC$SCC[VehSCC]
VehNEI=NEI[NEI$SCC %in% VehSSC & (NEI$fips=="24510" | NEI$fips == "06037"),]
city=data.frame(city=c('Baltimore','Los Angeles County'),fips=c("24510","06037"))
VehNEI=merge(VehNEI,city,by='fips',x.all=T)

library(ggplot2)
library(scales)
Plot_theme = theme_bw() + theme(plot.title = element_text(size = 12), axis.title.x = element_text(size = 10), 
                                axis.title.y = element_text(size = 10), axis.text.x = element_text(size = 8), 
                                axis.text.y = element_text(size = 8))

ggplot(data=VehNEI)+
  geom_bar(aes(factor(year),Emissions),stat='identity')+
  facet_grid(.~city)+
  labs(x="year", y=expression(paste("Total",PM[2.5]," Emission (Tons)"))) + 
  labs(title=expression("Motor Vehicle Source PM"[2.5]*" Emissions Comparison between Baltimore and Los Angeles County from 1999-2008"))+
  Plot_theme 
