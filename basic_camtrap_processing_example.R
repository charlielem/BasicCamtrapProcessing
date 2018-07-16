setwd("C:/Users/rowcliffe.m/Documents/Teaching/Imperial_MSc/Ecological_methods/CameraTrapping/Analysis")
source("Data_tools.R")

##################################################################################################################################
#EXTRACT IMAGE METADATA
##################################################################################################################################
infolder <- "C:/Users/rowcliffe.m/Documents/Teaching/Imperial_MSc/Ecological_methods/CameraTrapping/Analysis/Images"
outfile <- "C:/Users/rowcliffe.m/Documents/Teaching/Imperial_MSc/Ecological_methods/CameraTrapping/Analysis/metadata.csv"
exifolder <- ("C:/Users/Rowcliffe.M/Documents/APPS/ExifTool")
read.exif(infolder, outfile, exifolder)
imgdat <- read.csv(outfile)

##################################################################################################################################
#MAKE SITE AND EVENT DATAFRAMES
##################################################################################################################################
#Read site data
sitedat <- read.csv("site_data.csv")
sitedat$deploy_start <- strptime(sitedat$deploy_start, "%d/%m/%Y")
sitedat$deploy_end <- strptime(sitedat$deploy_end, "%d/%m/%Y")
sitedat$nights <- as.numeric(sitedat$deploy_end - sitedat$deploy_start)
sitedat$group <- as.factor(sitedat$group)
View(sitedat)

#Read tag data
tagdat <- get.tagdat(imgdat, sitedat)
View(tagdat)

#Make row-per-event data
eventdat <- get.eventdat(1, tagdat, sitedat)
View(eventdat)

#Tabulate site-by-species events
events <- event.table(eventdat)
trate <- events/sitedat$nights

#Overall species events and trap rates (per 100 nights)
ev_sp <- apply(events, 2, sum)
tr_sp <- 100 * ev_sp / sum(sitedat$nights)
data.frame(Events=ev_sp, Traprate=round(tr_sp,1))
range(sitedat$nights)
sum(sitedat$nights)
min(sitedat$deploy_start)
max(sitedat$deploy_end)

##################################################################################################################################
#MAPPING TRAP RATES
##################################################################################################################################
source("./mapping.r")
#Set working directory to the that where mapping info is stored 
wd <- getwd()
setwd("./Mapping")

#Load mapping files (jpeg base plus georeferencing and boundary info)
silbase <- loadmap("SilwoodPark.jpg", "ImageCorners.kml")
boundary <- getXMLcoords("Silwood boundary.kml")
setwd(wd)

#Make map with base, boundary and camera points scaled/scaled according to trap rate
par(mar=c(2,2,2,2))
for(sp in colnames(events)){
  tr <- trate[,sp]
  plotmap(silbase)
  mtext(sp)
  addshape(silbase, boundary, "poly", col=2)
  latlong <- sitedat[,c("actual_lat","actual_long")]
  names(latlong) <- c("long", "lat")
  addshape(silbase, latlong, "point", col=ifelse(tr==0, 4, 2), cex=0.5*tr+0.5, pch=16)
}
trrange <- range(trate[trate>0])
tr <- round(c(0,trrange[1], mean(trrange), trrange[2]), 2)
plot.new()
legend(0.5,0.5, tr, col=c(4,2,2,2), pt.cex=0.5*tr+0.5, pch=16, 
       title="Records per night", y.intersp=1.5)

##################################################################################################################################
#OCCUPANCY ANALYSIS
##################################################################################################################################
library(unmarked)

#Extract a detection matrix for occupancy analysis for a given species
dmat <- get.dmatrix("muntjac", eventdat, sitedat)
dmat3 <- condense.matrix(dmat, 3)
dmat3$detection
#Read in and create covariate data structures
weatherdat <- read.csv("./heathrow_weather.csv")
obscovs <- list(temp=matrix(weatherdat$temp.avg, nrow=nrow(dmat), ncol=nrow(weatherdat), byrow=T),
                 precip=matrix(weatherdat$precip.mm.sum, nrow=nrow(dmat), ncol=nrow(weatherdat), byrow=T)
)
sitecovs <- sitedat[match(rownames(dmat), sitedat$site), c("group","habitat")]

#Make unmarked data structure for occupancy analysis
umf <- unmarkedFrameOccu(dmat, siteCovs=sitecovs, obsCovs=obscovs)
summary(umf)

#Fit models
m1 <- occu(~habitat+temp+precip ~habitat, umf)
m2 <- occu(~habitat+temp ~habitat, umf)
m3 <- occu(~habitat+precip ~habitat, umf)
m4 <- occu(~temp+precip ~habitat, umf)
m5 <- occu(~habitat ~habitat, umf)
m6 <- occu(~temp ~habitat, umf)
m7 <- occu(~precip ~habitat, umf)
m8 <- occu(~1 ~habitat, umf)
mods <- fitList(htp=m1, ht=m2, hp=m3, tp=m4, h=m5, t=m6, p=m7, null=m8)
modSel(mods)

m9 <- occu(~habitat ~1, umf)
mods <- fitList(h=m5, null=m9)
modSel(mods)

backTransform(m9, "state")

##################################################################################################################################
#ACTIVITY ANALYSIS
##################################################################################################################################
source("C:/Users/rowcliffe.m/Documents/My_R_Packages/activity1.3/activity_code_1_3.r")
par(mar=c(4,4,2,2))
for(sp in c("badger", "fox", "muntjac", "rabbit", "roedeer", "squirrel")){
  time <- subset(eventdat, species==sp)$time * pi / 12
  amod <- fitact(time)
  plot(amod, tline=list(col=2), main=sp)
}


