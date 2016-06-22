---
#### Sourcedataunzipfiledownlodtimelistfiles, echo=TRUE, cache=TRUE}
#### 1c. Source data with url, download zipfile, unzip, read the .csv
#### data file, record date and time of download, print list of files in this
#### directory
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
zipFile <- "StormData.csv.bz2"
if (!file.exists("StormData.csv.bz2")) {
        message(paste("Downloding", zipFile))
        fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
        download.file(fileUrl, destfile="StormData.csv.bz2", method = "curl")
}else{
        message(paste("File exists;", zipFile))
}
# Record the date & time of the data download
dateDownLoaded <- date()
dateDownLoaded
# Review list of files in the new directory
rawstormdata <<- read.csv(bzfile("StormData.csv.bz2"), header=TRUE, stringsAsFactors = FALSE)

---
# 1h. Select only relevant columns of rawstormdata for the analysis
newstormdata <- rawstormdata[, c(8,23:28)]
newstormdata <<- na.omit(newstormdata)
any(is.na(newstormdata))


---
# List file in working directory
ls()
