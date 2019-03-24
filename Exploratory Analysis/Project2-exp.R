
# Reading data:
SCC <- readRDS("./exploratory/Source_Classification_Code.rds")
NEI <- readRDS("./exploratory/summarySCC_PM25.rds")

# Question 1 - Base plot 
totalPM <-with(NEI, tapply(Emissions,year,sum))
png(filename="question1.png",width=480,height=480)
plot(unique(NEI$year),totalPM, type="l",lwd= 2, lty = "dashed",col = "blue",xlab= "Year" , ylab = "Total PM2.5 emissions"  )
points(unique(NEI$year),totalPM, pch = 10)
dev.off()

# Reading data:
SCC <- readRDS("./exploratory/Source_Classification_Code.rds")
NEI <- readRDS("./exploratory/summarySCC_PM25.rds")
# Question 2 - Base plot 
subBal <- subset(NEI,fips == "24510")
totalSubBal <-with(subBal, tapply(Emissions,year,sum))
png(filename="question2.png",width=480,height=480)
plot(unique(NEI$year),totalSubBal, type="l",lwd= 2, lty = "dashed",col = "blue",xlab= "Year" , ylab = "Total PM2.5 emissions", main= "Total PM2.5 emissions in Baltimore" )
points(unique(NEI$year),totalSubBal, pch = 10)
dev.off()

# Question 3 - ggplot2
# Reading data:
SCC <- readRDS("./exploratory/Source_Classification_Code.rds")
NEI <- readRDS("./exploratory/summarySCC_PM25.rds")
library(dplyr)
subBal <- subset(NEI,fips == "24510")
subBal$type_year <- paste(subBal$type,subBal$year,sep="_")
Bal_type_source <-with(subBal, tapply(Emissions,type_year,sum))
Bal_type_years <-dimnames(Bal_type_source)
Bal_type_years <- strsplit(unlist(Bal_type_years),split= " ") 
Bal_years <-gsub("[^0-9]","", Bal_type_years)
Bal_type <-gsub("[0-9]","", Bal_type_years)
Bal_type_source <- data.frame(Bal_type,Bal_years,Bal_type_source)
colnames(Bal_type_source) <- c("Type","Year","Emissions")
png(filename="question3.png",width=480,height=480)
library(ggplot2)
Bal_type_source$Year <- as.numeric(as.character(Bal_type_source$Year))
ggplot(Bal_type_source) + aes(x =Year,y = Emissions) + facet_grid(Type ~. ) + geom_point(aes(color=Type)) +geom_line(aes(color=Type)) + labs(x= "Year" , y = "PM2.5 Emissions" )
dev.off()

# Question 4 - baseplot
# Reading data:
SCC <- readRDS("./exploratory/Source_Classification_Code.rds")
NEI <- readRDS("./exploratory/summarySCC_PM25.rds")
dummy1 <-grep("Coal",SCC$EI.Sector,value=TRUE)
subcoal<- subset(SCC, EI.Sector %in% dummy1)
subSCC<- subcoal$SCC
subNEICoil <- subset(NEI,SCC %in% subSCC) 
totalPMcoal<-with(subNEICoil, tapply(Emissions,year,sum))
png(filename="question4.png",width=480,height=480)
plot(unique(subNEICoil$year),totalPMcoal, type="l",lwd= 2, lty = "dashed",col = "blue",xlab= "Year" , ylab = "Total Coal PM2.5 emissions"  )
points(unique(subNEICoil$year),totalPMcoal, pch = 10)
dev.off()
#Question 5 - Base plot
# Reading data:
SCC <- readRDS("./exploratory/Source_Classification_Code.rds")
NEI <- readRDS("./exploratory/summarySCC_PM25.rds")
dummy2 <-grep("Highway Veh",SCC$Short.Name,value=TRUE)
submotor<- subset(SCC, Short.Name %in% dummy2)
subSCCmotor<- submotor$SCC
subNEImotor <- subset(NEI,SCC %in% subSCCmotor) 
subNEImotorBal <- subset(subNEImotor,fips =="24510" )
totalPMmotorBal<-with(subNEImotorBal, tapply(Emissions,year,sum))
png(filename="question5.png",width=480,height=480)
plot(unique(subNEImotorBal$year),totalPMmotorBal, type="l",lwd= 2, lty = "dashed",col = "blue",xlab= "Year" , ylab = "Total Motor Veh PM2.5 emissions in Baltimore"  )
points(unique(subNEImotorBal$year),totalPMmotorBal, pch = 10)
dev.off()

#Question 6 - Base plot
# Reading data:
SCC <- readRDS("./exploratory/Source_Classification_Code.rds")
NEI <- readRDS("./exploratory/summarySCC_PM25.rds")
dummy2 <-grep("Highway Veh",SCC$Short.Name,value=TRUE)
submotor<- subset(SCC, Short.Name %in% dummy2)
subSCCmotor<- submotor$SCC
subNEImotor <- subset(NEI,SCC %in% subSCCmotor) 
subNEImotorBal <- subset(subNEImotor,fips =="24510" )
totalPMmotorBal<-with(subNEImotorBal, tapply(Emissions,year,sum))
subNEImotorLos <- subset(subNEImotor,fips =="06037" )
totalPMmotorLos<-with(subNEImotorLos, tapply(Emissions,year,sum))
png(filename="question6.png",width=480,height=480)
ylim <- range(totalPMmotorBal,totalPMmotorLos)
plot(unique(subNEImotorBal$year),totalPMmotorBal, type="l",lwd= 2, lty = "dashed",col = "blue",xlab= "Year" , ylab = "Total Motor Veh PM2.5 emissions " ,ylim=ylim )
points(unique(subNEImotorBal$year),totalPMmotorBal, pch = 10)
lines(unique(subNEImotorLos$year),totalPMmotorLos, type="l",lwd= 2, lty = "dashed",col = "red")
points(unique(subNEImotorLos$year),totalPMmotorLos, pch = 10)
legend("center", legend=c("Baltimore","Los Angles"),col=c("blue","red"),lty="dashed")
dev.off()



