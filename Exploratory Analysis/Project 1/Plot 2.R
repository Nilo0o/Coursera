# Reading data from the file 
data1<- read.table("./exploratory/household_power_consumption.txt",sep=";",skip=2)


# Adding labbels based on txt file :Date;Time;Global_active_power;Global_reactive_power;Voltage;
# Global_intensity;Sub_metering_1;Sub_metering_2;Sub_metering_3
names1 <- t(as.data.frame(strsplit("Date;Time;Global_active_power;Global_reactive_power;Voltage;
                                   Global_intensity;Sub_metering_1;Sub_metering_2;Sub_metering_3","[;]")))

names(data1) <- names1

# Adjusting class of Time and Date :

data1$Date <- as.Date(data1$Date,format="%d/%m/%Y")

data1$Time<-format(strptime(data1$Time,format="%H:%M:%S") ,format="%H:%M:%S")

# Subsetting the testing period of 2007-02-01 till 2007-02-02

data1 <- subset(data1, (data1$Date == "2007-02-01") | (data1$Date =="2007-02-02"))


# Plot 2
data1$Global_active_power <- as.numeric(as.character(data1$Global_active_power))

png(filename="Plot 2.png",width= 480, height = 480)
par(mar=c(4,4,2,2))
data1 <- tbl_df(data1)
data1 <- mutate(data1, DateTime= strptime(paste(data1$Date,data1$Time, sep =" "),format= "%Y-%m-%d %H:%M:%S"))
with(data1,plot(DateTime,Global_active_power,ylab="Global active power (kilowatts)",xlab="",type="l",))
dev.off()



