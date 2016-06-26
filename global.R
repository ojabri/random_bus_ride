distanceStops = function(stop1, stop2, print=T){
 stop1 = subset(bus.stops, Stop_Code_LBSL == stop1)
 stop2 = subset(bus.stops, Stop_Code_LBSL == stop2)
 
 d = ((stop1$Location_Easting-stop2$Location_Easting)^2 + (stop1$Location_Northing-stop2$Location_Northing)^2)^0.5 
 if(print) cat(paste("\n  Distance between stops", stop1$Stop_Code_LBSL, "and", stop2$Stop_Code_LBSL, "is", d, "\n"))
 
 d
}

getRandomStop = function(current.Stop_Code_LBSL, random.walk.max){
 stops.within.distance = bus.stops[distanceStops(current.Stop_Code_LBSL, bus.stops$Stop_Code_LBSL, F) <= random.walk.max,]
 random.bus.stop = sample(stops.within.distance[, "Stop_Code_LBSL"], 1) #check if this works when only one stop in radius
	random.bus.stop.row = subset(bus.stops, Stop_Code_LBSL==random.bus.stop )
 
 #cat(paste("Current Bus Stop:", current.Stop_Code_LBSL, "\n"))
	#cat(paste("Random Bus Stop:", random.bus.stop, "\n"))
	distanceStops(current.Stop_Code_LBSL, random.bus.stop, F)
 
 random.bus.stop
}

getRandomSeq = function(random.bus.stop){
 buses.at.stop.index = bus.seq$Stop_Code_LBSL==random.bus.stop
	if(sum(buses.at.stop.index)>1){
		random.route.index = sample(which(buses.at.stop.index), 1)
		random.route = bus.seq[random.route.index,]
	#random.route = bus.seq[buses.at.stop.index ,]
	} else {
		random.route = bus.seq[buses.at.stop.index,]
	}
 random.route
}

ride.bus = function(current.Stop_Code_LBSL, random.walk.max, random.sit.max){
 
 random.bus.stop = getRandomStop(current.Stop_Code_LBSL, random.walk.max)
 random.route = getRandomSeq(random.bus.stop)
	#cat("****\n")
	#print(random.route)
	#cat("****\n")

	random.route.seq = subset(bus.seq, route==random.route$route & Run==random.route$Run)
	
	#cat("****\n")
	#print(random.route.seq)
	#cat("****\n")
	
	if( max(random.route.seq$Sequence) == random.route$Sequence) {
		#cat("End of line. Switching Direction\n")
		random.route = subset(bus.seq, route==random.route$route & Run==ifelse(random.route$Run=="1", "2", "1") & Sequence==1)
		random.route.seq = subset(bus.seq, route==random.route$route & Run==random.route$Run)
  
  while(nrow(random.route.seq)==0){
   print("Bus Route has no return bus. Try looking for another bus route at current stop")
   random.route.new = getRandomSeq(random.bus.stop)
   
   #if you get the same route again then try walking to a new bus stop
   #if(random.route$route == random.route.new$route){
    #random.bus.stop = getRandomStop(current.Stop_Code_LBSL, random.walk.max)
    random.route = getRandomSeq(random.bus.stop)
   #}
	 	random.route.seq = subset(bus.seq, route==random.route$route & Run==random.route$Run)
  }
		# cat("****\n")
		# print(random.route)
		# cat("****\n")
		
		 #cat("****\n")
		 #print(random.route.seq)
		 #cat("****\n")
	
	}
	
	last.stop = max(random.route.seq$Sequence)
	stay.on.num = random.sit.max #ceiling(rnorm(1,random.sit.max ,3))
	switch.stop = min(random.route$Sequence + stay.on.num, last.stop) 
	switch.stop = random.route.seq$Sequence[min(switch.stop, nrow(random.route.seq))]
	
	#cat(paste("\nfirst",random.route$Sequence, "last", switch.stop, "\n"))
	
	first.row = which(random.route.seq$Sequence == random.route$Sequence & random.route.seq$Run==random.route$Run)
	last.row = which(random.route.seq$Sequence == switch.stop & random.route.seq$Run==random.route$Run)
	
	random.route.seq[first.row:last.row, ]

	#data.frame(bus.stops[match(new.stop.seq, bus.stops$Stop_Code_LBSL),], 	route = random.route$route)
}

random.bus.ride = function(random.walk.max = 1000, random.sit.max = 10){
	{
	in.central = F
	init.bus.stop.code = sample(bus.stops$Stop_Code_LBSL, 1)
	first.bus.stop = subset(bus.stops, Stop_Code_LBSL==init.bus.stop.code)
 
	#print(paste("First stop:", first.bus.stop$Stop_Name, first.bus.stop$Stop_Code_LBSL))
	}
	current.bus.code = init.bus.stop.code
	stop.seq = data.frame()
	while(!in.central){
  current.bus.stop = subset(bus.stops, Stop_Code_LBSL == current.bus.code)
 
  #simulate bus ride
  new.stop = ride.bus(current.bus.code, random.walk.max, random.sit.max)
  #print(new.stop)
  #cat(paste("starting at stop:", head(new.stop$Stop_Name,1), head(new.stop$Stop_Code_LBSL,1), ""))
  stops.in.central = point.in.polygon(new.stop$Location_Easting, new.stop$Location_Northing, centralLondonPostcodes.chull$Eastings, centralLondonPostcodes.chull$Northings)
  
  if(any(stops.in.central==1)){
   #print("******Stop is in central********")
   in.central=T
   n = min(which(stops.in.central==1))
   #cat(paste("n=", n, "\n"))
  } else {
   #print("Stop not in central")
   n = nrow(new.stop)
   current.bus.code = new.stop$Stop_Code_LBSL[n] #tail(new.stop$Stop_Code_LBSL,1)
  }
  
  
	 if(n>1){ 
		 stop.seq = rbind(stop.seq, data.frame(Stop_Code_LBSL = new.stop[c(1:n),"Stop_Code_LBSL"], type = c("start", rep("intermediate", n-2), "end") ))
		 #cat(paste("ending at stop:", new.stop$Stop_Name[n], new.stop$Stop_Code_LBSL[n], "via", unique(new.stop$route), "\n"))
  } else {
   stop.seq = rbind(stop.seq, data.frame(Stop_Code_LBSL = new.stop[1,"Stop_Code_LBSL"], type =  "end" ))
  }
	}
	
	ride.summary = data.frame(
		start.stop = first.bus.stop$Stop_Name,
		end.stop = new.stop$Stop_Name[n],
		num.stops = nrow(stop.seq),
		random.walk.max, 
		random.sit.max,
  bus.switches = sum(stop.seq$type=="end")
	)
	stops = cbind(order = 1:nrow(stop.seq), stop.seq)
	
	list(summary = ride.summary, stops = stops)
}

convertToLatLong = function(x, y){
 points = data.frame(x=x,y=y)
 coordinates(points) <- ~x + y
 proj4string(points) <- CRS("+init=epsg:27700")
 points = spTransform(points, CRS("+init=epsg:4326"))

 data.frame(Latitude = points@coords[,2], Longitude = points@coords[,1]) 
}


setwd("C:/Users/hannah.dadds.OD/Documents/london_buses")
bus.seq = read.csv("bus-sequences.csv")
bus.stops = read.csv("bus-stops.csv")
#post.codes = read.csv("London postcodes.csv")

wc = read.csv("codepo_gb/Data/CSV/wc.csv", header=F)
ec = read.csv("codepo_gb/Data/CSV/ec.csv", header=F)
post.code.col.names = read.csv("codepo_gb/Doc/Code-Point_Open_Column_Headers.csv")
centralLondonPostcodes = rbind(wc, ec)
names(centralLondonPostcodes) = apply(post.code.col.names, 2, as.character)

centralLondonPostcodes.chull = centralLondonPostcodes[chull(centralLondonPostcodes[,c("Eastings", "Northings")]),]
centralLondonPostcodes.chull = data.frame(centralLondonPostcodes.chull, convertToLatLong(centralLondonPostcodes.chull$Eastings, centralLondonPostcodes.chull$Northings))



#clean data
names(bus.seq)[1]="route"
names(bus.stops)[1]="Stop_Code_LBSL"
bus.stops$Stop_Area = as.character(bus.stops$Stop_Area)
bus.stops$Stop_Code_LBSL = as.character(bus.stops$Stop_Code_LBSL)
bus.seq$Stop_Code_LBSL = as.character(bus.seq$Stop_Code_LBSL)
bus.seq$route = as.character(bus.seq$route)
#remove stops with strange locations
bus.stops = subset(bus.stops, Virtual_Bus_Stop==0 & Location_Northing!=999999)
bus.seq = subset(bus.seq, Virtual_Bus_Stop==0 & Location_Northing!=999999)

