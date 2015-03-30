setwd("C:/Users/Hannah/Desktop/omar/R/london_buses")
rm(list=ls())
#source('code.r')
source('functions.r')

bus.seq = read.csv("bus-sequences.csv")
bus.stops = read.csv("bus-stops.csv")
post.codes = read.csv("London postcodes.csv")

#subset only central london postcodes
center.postcodes.index = grep("WC|EC", post.codes$Postcode)
post.codes.central = post.codes[center.postcodes.index,]

#clean data
names(bus.seq)[1]="route"
names(bus.stops)[1]="Stop_Code_LBSL"
bus.stops$Stop_Area = as.character(bus.stops$Stop_Area)
bus.stops$Stop_Code_LBSL = as.character(bus.stops$Stop_Code_LBSL)
bus.seq$Stop_Code_LBSL = as.character(bus.seq$Stop_Code_LBSL)
#remove stops with strange locations
bus.stops = subset(bus.stops, Virtual_Bus_Stop==0 & Location_Northing!=999999)
bus.seq = subset(bus.seq, Virtual_Bus_Stop==0 & Location_Northing!=999999)


if(!exists("many.rides")){
	many.rides = data.frame()
}
for(i in 1:500){
	many.rides = rbind(many.rides, 
		random.bus.ride(random.walk.max = 500, random.sit.max = 10)
	)
}

#png("mean 10.png")
	hist(many.rides$num.stops, main="Histogram of #Stops with waiting mean 5 stops per bus")
#dev.off()


#bus.stops.visted.order = match(bus.stops$Bus_Stop_Code, stop.seq)
#bus.stops.visted.order = bus.stops.visted.order[!is.na(bus.stops.visted.order)]

#plot(bus.stops.visted$Location_Easting, bus.stops.visted$Location_Northing,, type='l')
