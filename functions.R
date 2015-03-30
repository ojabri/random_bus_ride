
ride.bus = function(current.Stop_Code_LBSL, random.walk.max, random.sit.max){
	current.bus.stop.code.index = bus.stops$Stop_Code_LBSL==current.Stop_Code_LBSL
	
	x = bus.stops[current.bus.stop.code.index,"Location_Easting"]
	y = bus.stops[current.bus.stop.code.index,"Location_Northing"]
	
	stops.within.distance = with(bus.stops, ((Location_Easting-x)^2 + (Location_Northing-y)^2)^0.5 < random.walk.max)
	
	i=1
	while(sum(stops.within.distance)==0){
		print(paste("No Stops Nearby. Walking ", i*100, "meters more"))
		stops.within.distance = with(bus.stops, ((Location_Easting-x)^2 + (Location_Northing-y)^2)^0.5 < random.walk.max + i * 100)
		
		if(i>50){
			print("something is wrong")
			print(bus.stops[stops.within.distance,])
			print(bus.stops[current.bus.stop.code.index,])
			stop()
		}
		i=i+1
	}


	random.bus.stop = sample(bus.stops[stops.within.distance, "Stop_Code_LBSL"], 1)

	buses.at.stop.index = bus.seq$Stop_Code_LBSL==random.bus.stop

	random.route.index = sample(which(buses.at.stop.index), 1)
	random.route = bus.seq[random.route.index,]
	
	random.route.seq = subset(bus.seq, route==random.route$route & Run==random.route$Run)
	
	last.stop = max(random.route.seq$Sequence)
	
	if(random.route$Sequence == last.stop){
		print("end of the line buddy")
		#print(random.route$Run)
	}
	
	stay.on.num = ceiling(rnorm(1,random.sit.max ,3))
	switch.stop = min(random.route$Sequence + stay.on.num, last.stop)
	
	new.stop = random.route.seq[which.min(random.route.seq$Sequence-switch.stop), "Stop_Code_LBSL"]
	
	
	subset(bus.stops, Stop_Code_LBSL == new.stop)
}

random.bus.ride = function(random.walk.max = 1000, random.sit.max = 10){
	in.central = F
	init.bus.stop.code = sample(bus.stops$Stop_Code_LBSL, 1)
	first.bus.stop = subset(bus.stops, Stop_Code_LBSL==init.bus.stop.code)
	
	print(paste("First stop:", first.bus.stop$Stop_Name, first.bus.stop$Stop_Code_LBSL))
	
	current.bus.code = init.bus.stop.code
	stop.seq = c()
	while(!in.central){
		new.stop = ride.bus(current.bus.code, random.walk.max, random.sit.max)
		#print(new.stop)
		print(paste("    at stop:", new.stop$Stop_Name, new.stop$Stop_Code_LBSL))
		
		stop.seq = c(stop.seq, as.character(new.stop$Stop_Code_LBSL))
		x = new.stop$Location_Easting
		y = new.stop$Location_Northing

		close.central.post.codes = subset(post.codes.central, ((Easting-x)^2 + (Northing-y)^2)^0.5 < 100)
		if(nrow(close.central.post.codes)>0){
			in.central=T
		} else {
			current.bus.code = new.stop$Stop_Code_LBSL
		}
	}
	
	print(paste("Last stop:",new.stop$Stop_Name, new.stop$Stop_Code_LBSL))
	
	data.frame(
		start.stop = first.bus.stop$Stop_Name,
		end.stop = new.stop$Stop_Name,
		num.stops = length(stop.seq),
		random.walk.max, 
		random.sit.max
	)
}