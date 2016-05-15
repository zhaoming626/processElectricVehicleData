#library(RMySQL)
#setwd('~/RProjects/plot')
setwd('~/RProjects/processElectricVehicleData')
#eChargingNet <- dbConnect(MySQL(), user='plot', password='plot', dbname='evehicle', host='114.214.198.18')
#pile_info <- dbReadTable(eChargingNet, name='pile_info')
load('site_info.Rda')
load('realtime_record.Rda')
load('pile_info.Rda')

timerange <- c(as.Date("2016-04-01"), as.Date("2016-04-30"))
fromDate <- seq.Date(timerange[1], timerange[2], by='1 day')
endDate <- seq.Date(timerange[1] + 1, timerange[2] + 1, by='1 day')
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
#pileCountVector <- floor(pileCountVector / median(pileCountVector - 1, na.rm = FALSE))
siteUsedRatio <- sum(pileCountVector<=25) / length(pileCountVector)

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
			currentTimeRange <- c(fromDate[k], endDate[k])
			currentTimeRange <- as.POSIXct(currentTimeRange, format = "%Y-%m-%d")
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

dir.create("result", FALSE)
scary <- "#2b2b2b"
light <- "#bfbfbf"

#for (k in 1:length(fromDate))
for (k in 1:1)
{
	result  <- produceSourceImage(k)
	save(result, file = sprintf("./result/result_%d.Rda", k))
	png(filename = sprintf("./result/Rplot%d.png", k), width=639.5*2, height=544*2, res=144, bg=scary)

	mapResult <- ggmap(baseMap) + theme_dark() + geom_point(aes(x = longitude, y = latitude, color = used_pile_ratio, size = radius),data = result, alpha = .5) + 
	scale_color_continuous(low="#00ff00", high="#ff0000", limit=c(0,1)) + scale_radius(name="Capacity", range = c(2,6), limits = c(0,25), breaks = c(3,8,18,25), labels = c("0-2", "3-7", "8-17", "19-25")) +
	labs(x = paste("UsedSiteCount / TotalSiteCount = ", sprintf("%1.2f%%", 100 * siteUsedRatio), sep = ""), color = "Utilization", title = fromDate[k]) + theme(plot.background=element_rect(fill=scary, color=scary),
          panel.background=element_rect(fill=scary, color=scary),
          legend.background=element_rect(fill=scary),
          legend.key=element_rect(fill=scary, color=scary),
          legend.text=element_text(color=light, size=10),
          legend.title=element_text(color=light),
          axis.title=element_text(color=light),
          axis.title.x=element_text(color=light, size=12),
          axis.title.y=element_blank(),
          plot.title=element_text(color=light, face="bold", size=16),
          plot.margin=margin(0, 0, 0, 0))
	print(mapResult)
	dev.off()
}

#library(animation)
#saveVideo({ani.options(interval = 0.05, namx = 300);produceSourceImages()},video.name="chargingStationBeijing.mp4",other.opts="-pix_fmt yuv420p -b 300k")
# Using bash command ffmpeg -y -r 2 -i Rplot%d.png -filter scale="854:trunc(ow/a/2)*2" -pix_fmt yuv420p -b:a 600k chargingStationBeijing.mp4
