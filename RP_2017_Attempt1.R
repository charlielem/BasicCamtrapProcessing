#Tuesday 17th July 2018
#Regents Park 2017 Data

#code to clear workspace if needed:
rm(list = ls())

library(data.table)

setwd("C:/Users/cml1917/Desktop/IoZ/Analysis")

#source("Data_tools.R") # this step is erroring, not sure what I need to change to make it work - does this just mean 
#what script was used/where the info came from???

infolder <- "C:/Users/cml1917/Desktop/IoZ/Analysis/Regents_Park_2017"
outfile <- "C:/Users/cml1917/Desktop/IoZ/Analysis/metadata.csv"
exifolder <- ("C:/Users/cml1917/Desktop/IoZ/Analysis/exiftool.exe")

#read.exif(infolder, outfile, exifolder) = did this line first as was in script I was following but said could not find fucntion read.exif

read.exif <- function(infolder, outfile, exifpath="C:/Users/cml1917/Desktop/IoZ/Analysis/exiftool"){
  wd <- getwd()
  setwd(exifpath)
  cmd <- paste("exiftool -r -csv", infolder, ">", outfile)
  shell(cmd)
  read.csv(outfile)
  setwd(wd)
}


imgdat <- read.csv(outfile) #nothing errors until here where it says: Error in file(file, "rt") : cannot open the connection
#In addition: Warning message:
#In file(file, "rt") :
  #cannot open file 'C:/Users/cml1917/Desktop/IoZ/Analysis/metadata.csv': No such file or directory





