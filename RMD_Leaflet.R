library(leaflet)

##### List of hospital at Sungnam city #####
hosp.list <- matrix( c(
37.434424, 127.128252, "St Mary's will hospital", 
37.433154, 127.129692, "SK healthcare center",
37.432860, 127.130368, "Moran Yeonsei clinic",
37.430488, 127.128846, "Sinui clinic",
37.430714, 127.129783, "Moran mental health clinic",
37.438611, 127.127411, "Saesomang hospital",
37.439907, 127.128970, "Chung Hospital",
37.440874, 127.132449, "Sungnam orthopediatric clinic",
37.441811, 127.135692, "Jeil psychologic clinic",
37.442557, 127.137967, "Clean psychologic clinic",
37.443762, 127.140183, "Jiwoo hospital",
37.440821, 127.130456, "Kwack women's hospital",
37.440633, 127.131486, "Soo clinic",
37.440838, 127.132129, "Yeonsei Internal medicine clinic",
37.446012, 127.134176, "E-med hospital",
37.442571, 127.136386, "Hansol orthopediatric clinic",
37.445280, 127.139068, "Sungnam Clinic",
37.444189, 127.143145, "Ha clinic",
37.438090, 127.140806, "Cham Seoul Radiology",
37.439334, 127.142995, "Happy psychologic clinic",
37.439998, 127.144003, "Huen hospital",
37.439573, 127.145227, "MaumBit psychologic clinic",
37.442605, 127.150956, "Good dream psychologic clinic",
37.448984, 127.145335, "Hyosaran hospital",
37.453127, 127.161924, "Sungnam Joongang hospital",
37.445466, 127.162470, "Barunmadi hospital",
37.416550, 127.120654, "Bundang Yeonsei hospital",
37.410524, 127.126844, "Barunsesang hospital",
37.413127, 127.129211, "Bundang Chuck hospital",
37.410066, 127.125418, "Cha general hospital",
37.412331, 127.129733, "Best Bundang clinic",
37.412741, 127.129156, "Sungmo psychologic clinic",
37.413560, 127.129511, "Yeonsei Modoo clinic",
37.417368, 127.126759, "Joongang Leader's dental clinic",
37.417079, 127.133981, "Kids-M pediatric clinic" ), ncol=3, byrow=TRUE)
hosp.list <- as.data.frame(hosp.list)
colnames(hosp.list) <- c("lat", "lng", "hosp.name")

### adjusting variable types ###
hosp.list$lat <- as.character(hosp.list$lat)
hosp.list$lat <- as.numeric(hosp.list$lat)
hosp.list$lng <- as.character(hosp.list$lng)
hosp.list$lng <- as.numeric(hosp.list$lng)
hosp.list$hosp.name <- as.character(hosp.list$hosp.name)

##### Draw map - popup & cluster #####
hosp.list[, c(1, 2)] %>%
	leaflet() %>%
	addTiles() %>%
	addMarkers(popup=hosp.list$hosp.name, clusterOptions = markerClusterOptions()) %>%
	addRectangles(lat1=37.410, lng1=127.120, 
				  lat2=37.455, lng2=127.163,
				  fillColor="transparent")
