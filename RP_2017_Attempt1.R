#Thursday 19th July 2018
#Regents Park 2017 Data

#code to clear workspace if needed:
rm(list = ls())

setwd("C:/Users/cml1917/Documents/BasicCamtrapProcessing")
source("basic_camtrap_processing.R")

infolder <- "C:/Users/cml1917/Desktop/IoZ/Analysis/Regents_Park_2017"
outfile <- "C:/Users/cml1917/Desktop/IoZ/Analysis/metadata.csv"
exifolder <- "C:/Users/cml1917/Desktop/IoZ/Analysis"

# Error message after exifolder step:
#153 directories scanned
#55473 image files read
#Warning messages:
# 1: running command 'C:\windows\system32\cmd.exe /c exiftool -r -csv C:/Users/cml1917/Desktop/IoZ/Analysis/Regents_Park_2017 > C:/Users/cml1917/Desktop/IoZ/Analysis/metadata.csv' had status 1 
#2: In shell(cmd) :
# 'exiftool -r -csv C:/Users/cml1917/Desktop/IoZ/Analysis/Regents_Park_2017 > C:/Users/cml1917/Desktop/IoZ/Analysis/metadata.csv' execution failed with error code 1

imgdat <- read.exif(infolder, outfile, exifolder) # = did this line first as was in script I was following but said could not find fucntion read.exif

imgdat #output: C:/Users/cml1917/Desktop/IoZ/Analysis - is it meant to e a file path or should it be a dataframe?

###########################################################################################################
## Done ##
#########################################################################################################
setwd("C:/Users/cml1917/Desktop/IoZ/Analysis")

sitedat <- read.csv("sitedat.csv")
sitedat$deploy_start <- strptime(sitedat$deploy_start, "%d/%m/%Y")
sitedat$deploy_end <- strptime(sitedat$deploy_end, "%d/%m/%Y")
sitedat$nights <- as.numeric(sitedat$deploy_end - sitedat$deploy_start)
View(sitedat)

#all this works

##########################################################################################################
## Done ##
#########################################################################################################

#tagdat <- get.tagdat(imgdat, sitedat) error: $ operator is invalid for atomic vectors


get.tagdat <- function(imgdat, sitedat){
  #list of tag strings
  lst <- strsplit(as.character(imgdat$Keywords), ",")
  #matrix with columns col (ultimate column names), val (column-specific values)
  mtx <- matrix(unlist(lapply(unlist(lst), strsplit, ":")), ncol=2, byrow=T)
  mtx <- sub(" ", "", mtx) }

#runs but not sure if it does anything

  #get column names consistent
  repeat{
    print(paste("Column names:"))
    print(paste(names(table(mtx[,1]))))
    nm1 <- readline(prompt="Enter a name to change or ENTER to stop: ")
    if(nm1=="") break
    nm2 <- readline(prompt="Enter the replacement name: ")
    mtx[mtx[,1]==nm1,1] <- nm2
  }
#Output: Error in table(mtx[, 1]) : object 'mtx' not found

read.csv(metadata) #tried numerous combinations of this command (checked and am using correct working directory)
#Output: Error in read.table(file = file, header = header, sep = sep, quote = quote,  : 
#object 'metadata' not found 





