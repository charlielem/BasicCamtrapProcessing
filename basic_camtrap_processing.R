library(data.table) #for rbindlist, used in get.eventdat, and dcast (from reshape2), used in get.tagdat

#Download standalone Exif executable from here: http://www.sno.phy.queensu.ca/~phil/exiftool/
#Unzip and rename the exiftool(-k).exe file to exiftool.exe
#Path and file names in infolder and outfile must be free of spaces
#Images in subfolders of infolder are also processed
read.exif <- function(infolder, outfile, exifpath="C:/Users/Rowcliffe.M/Documents/APPS/ExifTool"){
  wd <- getwd()
  setwd(exifpath)
  cmd <- paste("exiftool -r -csv", infolder, ">", outfile)
  shell(cmd)
  read.csv(outfile)
  setwd(wd)
}

#Extracts tag data, plus date, julian day, file name and path, from raw exif data
#INPUT
# imgdat: data frame of raw exif data extracted using read.exif
# sitedat: data frame of site data, row per site with at least columns:
#     site: site ID to be matched in other data frames
#     deploy_start, deploy_end: date  of deployment start/end (DD/MM/YYYY format)
#     Also provide UTM co-ordinates if mapping required
get.tagdat <- function(imgdat, sitedat){
  #list of tag strings
  lst <- strsplit(as.character(imgdat$Keywords), ",")
  #matrix with columns col (ultimate column names), val (column-specific values)
  mtx <- matrix(unlist(lapply(unlist(lst), strsplit, ":")), ncol=2, byrow=T)
  mtx <- sub(" ", "", mtx)
  #get column names consistent
  repeat{
    print(paste("Column names:"))
    print(paste(names(table(mtx[,1]))))
    nm1 <- readline(prompt="Enter a name to change or ENTER to stop: ")
    if(nm1=="") break
    nm2 <- readline(prompt="Enter the replacement name: ")
    mtx[mtx[,1]==nm1,1] <- nm2
  }
  #add col names and row nums (referring to imgdat)
  mtx <- data.frame(mtx)
  names(mtx) <- c("col", "val")
  mtx$row <- rep(1:length(lst), unlist(lapply(lst, length)))
  #rename col names where second species
  msp <- tapply(mtx$col, mtx$row, function(x) sum(x=="species"))
  i <- mtx$row %in% names(msp[msp>1])
  j <- which(mtx[i,]$col=="species")
  k <- as.numeric(row.names(mtx[i,])[j[2*(1:(length(j)/2))]])
  mtx$col <- as.character(mtx$col)
  mtx$col[k] <- "species2"
  #flatten matrix to give column per col type; add date, julian day, path and file names
  dat <- dcast(mtx, row~col, value.var="val")
  dat$date <- strptime(imgdat$DateTimeOriginal, "%Y:%m:%d %H:%M:%S")[dat$row]
  drange <- c(min(sitedat$deploy_start), max(sitedat$deploy_end))
  dat$jday <- as.numeric(trunc(julian(dat$date)) - julian(drange[1]))
  dat$path <- imgdat$Directory[dat$row]
  dat$file <- imgdat$FileName[dat$row]
  #split out secondary species
  spdat <- dat[, -which(names(dat)=="species2")]
  sp2dat <- subset(dat, !is.na(species2))[,-which(names(dat)=="species")]
  names(sp2dat) <- names(spdat)
  dat <- rbind(spdat, sp2dat)
  dat <- dat[order(as.numeric(dat$site), dat$date), ]
  #get species values consistent
  repeat{
    print(paste("Species names:"))
    print(paste(names(table(dat$species))))
    nm1 <- readline(prompt="Enter a name to change (press ENTER to stop): ")
    if(nm1=="") break
    nm2 <- readline(prompt="Enter the replacement name: ")
    dat$species[dat$species==nm1] <- nm2
  }
  dat
}

#Create data frame with a row per independent event, with non-independence defined as
# within a given time of a prior record
#INPUT
# interval.hrs: independence interval in hours
# tagdat: data frame of row per image data created using get.tagdat
# sitedat: data frame of site data (see above for format)
get.eventdat <- function(interval.hrs, tagdat, sitedat){
  get.one <- function(block, interval.hrs, tagdat){
    subdat <- subset(tagdat, sp.site==block)
    dates <- sort(subdat$date)
    i <- c(TRUE, diff(dates) > interval.hrs*60^2)
    subdat$date <- as.POSIXct(subdat$date)
    subdat[i, ]
  }
  sp.site <- paste(tagdat$sp, tagdat$site, sep=".")
  blocks <- sort(unique(sp.site))
  res <- rbindlist(lapply(blocks, get.one, interval.hrs, tagdat))
  res$time <- hour(res$date) + minute(res$date)/60 + second(res$date)/60^2
  res
}

#Create a sites by species matrix of event counts
#INPUT
# eventdat: a row per event dataframe created using get.eventdat
event.table <- function(eventdat){
  events <- table(eventdat$site, eventdat$species)
  events <- events[match(sitedat$site, rownames(events)),]
  events[is.na(events)] <- 0
  rownames(events) <- sitedat$site
  events
}

#Create detection matrix for occupancy analysis of a given species with daily occasions
#INPUT
# sp: text giving species to create matrix for
# eventdat: data frame of event data (row per event)
# sitedat: data frame of site data (see above for format)
get.dmatrix <- function(sp, eventdat, sitedat){
  #add any missing sites (no records in dataset)
  extrasites <- sitedat$site[!sitedat$site %in% unique(eventdat$site)]
  if(length(extrasites)>0){
    r <- 1:length(extrasites)
    eventdat <- rbind(eventdat[r, ],eventdat)
    eventdat[r,] <- NA
    eventdat$site[r] <- extrasites
  }
  #add any missing julian days (no records in dataset)
  drange <- julian(c(min(sitedat$deploy_start), max(sitedat$deploy_end)))
  jdays <- 0:diff(drange)
  extradays <- jdays[!(jdays %in% unique(eventdat$jday))]
  if(length(extradays)>0){
    r <- 1:length(extradays)
    eventdat <- rbind(eventdat[r, ],eventdat)
    eventdat[r,] <- NA
    eventdat$jday[r] <- extradays
  }
  #create matrix  
  detmats <- with(eventdat, table(list(site, jday, species)))
  detmat <- detmats[,,sp]
  detmat[detmat>0] <- 1
  cn <- as.numeric(colnames(detmat))
  jstart <- julian(sitedat$deploy_start)
  jend <- as.numeric(julian(sitedat$deploy_end)-drange[1])
  jstart <- as.numeric(jstart-drange[1])
  for(i in 1:nrow(detmat)){ 
    j <- match(rownames(detmat)[i], sitedat$site)
    detmat[i, cn<jstart[j] | cn>jend[j]] <- NA
  }
  matrix(detmat, nrow=nrow(detmat), dimnames=list(rownames(detmat),NULL))
}

#Condense a detection matrix from daily to occasions of len days
#INPUT
# mat: a detection matrix with daily occasions
# len: the new occasion length over which to condense detection histories
condense.matrix <- function(mat, len){
  
  proc.occasion <- function(i, dr){
    (j <- seq(len*(i-1)+1, len*i))
    c(pres=as.numeric(any(dr[j]==1, na.rm=T)), days=len-sum(is.na(dr[j])))
  }
  
  proc.site <- function(dr){
    t(sapply(1:occs, proc.occasion, dr))
  }
  
  occs <- ceiling(ncol(mat)/len)
  res <- apply(mat, 1, proc.site)
  detmat <- t(res[1:occs,])
  effmat <- t(res[(1:occs)+occs,])
  detmat[effmat==0] <- NA
  list(detection=detmat, effort=effmat)
}
