#library(RMySQL)
#setwd('~/RProjects/plot')
setwd('~/processElectricVehicleData')
#eChargingNet <- dbConnect(MySQL(), user='plot', password='plot', dbname='evehicle', host='114.214.198.18')
#pile_info <- dbReadTable(eChargingNet, name='pile_info')
load('site_info.Rda')
load('realtime_record.Rda')
load('pile_info.Rda')

timerange <- c(as.POSIXct("2016-04-13 5:00:00"), as.POSIXct("2016-04-13 20:00:00"))
fromTime <- seq.POSIXt(timerange[1], timerange[2], by='1 hour')
endTime <- seq.POSIXt(timerange[1] + 3600, timerange[2] + 3600, by='1 hour')
result <- data.frame(site_id= numeric(0), used_pile_ratio = numeric(0), longitude = numeric(0), latitude = numeric(0), radius = numeric(0))

library(ggmap)
geoCenter = "Beijing"
#baseMap <- get_map(location = geoCenter, source='stamen', maptype='toner-lines', crop=F)
load("baseMap.Rda")

pileCountVector <- numeric()
for(i in 1:length(site_info$site_id))
{
	currentSiteID <- site_info[i, 'site_id']
	pileCodes <- pile_info[pile_info$site_id == currentSiteID, 'pile_code']
	pileCount <- length(pileCodes)
	pileCountVector <- c(pileCountVector, pileCount)
}
pileCountVector <- floor(pileCountVector / median(pileCountVector - 1, na.rm = FALSE))

produceSourceImage <- function(k)
{
	for(i in 1:length(site_info$site_id))
	{
		currentSiteID <- site_info[i, 'site_id']
		pileCodes <- pile_info[pile_info$site_id == currentSiteID, 'pile_code']
		pileCount <- length(pileCodes)
		realtimeRecordFlag <- realtime_record$pile_code %in% pileCodes
		realtimeRecordBuffer <- realtime_record[realtimeRecordFlag,]
		pileIndex <- 0
		for(currentPile in pileCodes)
		{
			currentTimeRange <- c(fromTime[k], endTime[k])
			timeFlag <- realtimeRecordBuffer$time > as.numeric(currentTimeRange)[1] & realtimeRecordBuffer$time < as.numeric(currentTimeRange)[2]
			pileFlag <- realtimeRecordBuffer$pile_code == currentPile
			currentFlag <- timeFlag & pileFlag
			currentRealtimeRecord <- realtimeRecordBuffer[currentFlag, 'record_id']
			if(!!length(currentRealtimeRecord))
			{
				pileIndex <- pileIndex + 1
			}
		}
		result[i,] <- c(currentSiteID, pileIndex / pileCount, site_info[i, 'longitude'], site_info[i, 'latitude'], pileCountVector[i])
	}
	return(result)
}

dir.create("resultWeekday", FALSE)
scary <- "#2b2b2b"
light <- "#bfbfbf"

for (k in 1:length(fromTime))
{
	result  <- produceSourceImage(k)
	save(result, file = sprintf("./resultWeekday/result_%d.Rda", k))
	png(filename = sprintf("./resultWeekday/Rplot%d.png", k), width=639.5*2, height=544*2, res=144, bg=scary)

	mapResult <- ggmap(baseMap) + theme_dark() + geom_point(aes(x = longitude, y = latitude, color = used_pile_ratio, shape = factor(radius,levels=c(0,1,2,3))),data = result, alpha = .5, size = 3) + 
	scale_color_continuous(low="#00ff00", high="#ff0000", limit=c(0,1)) + 
	labs(x = "Longitude", y ="Latitude", color = "Utilization", shape = "Capacity", title = paste(format(fromTime[k], "%H:%M"), format(endTime[k], "%H:%M"), sep = " ~ "))+ theme(plot.background=element_rect(fill=scary, color=scary),
          panel.background=element_rect(fill=scary, color=scary),
          legend.background=element_rect(fill=scary),
          legend.key=element_rect(fill=scary, color=scary),
          legend.text=element_text(color=light, size=10),
          legend.title=element_text(color=light),
          axis.title=element_text(color=light),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          plot.title=element_text(color=light, face="bold", size=16),
          plot.margin=margin(0, 0, 0, 0))
	print(mapResult)
	dev.off()
}

#library(animation)
#saveVideo({ani.options(interval = 0.05, namx = 300);produceSourceImages()},video.name="chargingStationBeijing.mp4",other.opts="-pix_fmt yuv420p -b 300k")
# Using bash command ffmpeg -y -r 2 -i Rplot%d.png -filter scale="854:trunc(ow/a/2)*2" -pix_fmt yuv420p -b:a 600k chargingStationBeijing.mp4
