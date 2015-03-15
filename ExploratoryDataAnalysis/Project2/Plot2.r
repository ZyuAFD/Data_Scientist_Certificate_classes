setwd('E:\\Classes\\Data Scientist Certificate\\Exploratory Data Analysis\\Project\\Project 2\\')

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(data.table)
NEI=data.table(NEI)
PM25Ann=NEI[NEI$fips=="24510",sum(Emissions),by=year]

barplot(PM25Ann$V1/10^6,
        names.arg=PM25Ann$year,
        xlab="Year",
        ylab=expression(paste("Total",PM[2.5],"Emission (",10^6,' Tons)',sep='')),
        main="Total PM2.5 Emissions From All Baltimore City Sources"
        )
