

ufo = read.delim(file.choose(), sep="\t", stringsAsFactors=FALSE, header=FALSE, na.strings="")
head(ufo)
names(ufo) <- c("DateOccurred", "DateReported", "Location", "ShortDescription", "Duration", "LongDescription")
head(ufo)

ufo$DateOccurred[1]
nchar(ufo$DateOccurred[1])
ufo$DateOccurred[756]
nchar(ufo$DateOccurred[756])

which(nchar(ufo$DateOccurred)!=8)  # which are the troubled rows?
length(which(nchar(ufo$DateOccurred)!=8))  # how many are there?

head(
  ufo[which(nchar(ufo$DateOccurred)!=8 | nchar(ufo$DateReported)!=8),1]
)  # print the first column of them


good.rows <- ifelse(nchar(ufo$DateOccurred)!=8 | nchar(ufo$DateReported)!=8, FALSE, TRUE)
length(which(!good.rows))
length(good.rows)
ufo<-ufo[good.rows,]

ufo$DateOccurred <- as.Date(as.character(ufo$DateOccurred), format="%Y%m%d")
ufo$DateReported<-as.Date(as.character(ufo$DateReported), format="%Y%m%d")

s1 = 'no comma here'
s2 = 'comma, here'
strsplit(s1,",")[[1]]
strsplit(s2,",")[[1]]

get.location <- function(s) {
  split.location<-tryCatch(strsplit(s,",")[[1]],error=function(e) return(c(NA, NA))) #looks for the split character ","
  clean.location <- gsub("^ ","",split.location)
  if (length(clean.location)>2) {
    return(c(NA,NA))
  }
  else {
    return(clean.location)
  }
}

head(ufo$Location)
strsplit(ufo$Location,",")
city.state<-lapply(ufo$Location, get.location)
head(city.state)
location.matrix<-do.call(rbind, city.state)

ufo<-transform(ufo, USCity=location.matrix[,1], USState=tolower(location.matrix[,2]), stringsAsFactors=FALSE)

us.states<-c("ak","al","ar","az","ca","co","ct","de","fl","ga","hi","ia","id","il","in","ks","ky","la","ma","md","me","mi","mn","mo","ms","mt","nc","nd","ne","nh","nj","nm","nv","ny","oh","ok","or","pa","ri","sc","sd","tn","tx","ut","va","vt","wa","wi","wv","wy")


head(ufo$USState)
match(ufo$USState,us.states)
head(match(ufo$USState, us.states))

ufo$USState<-us.states[match(ufo$USState,us.states)]
head(ufo$USState)
ufo$USCity[is.na(ufo$USState)]<-NA
head(ufo$USCity)

ufo.us<-subset(ufo, !is.na(USState))
head(ufo.us)

summary(ufo.us$DateOccurred)
ufo.us <- subset(ufo.us, !is.na(ufo.us$DateOccurred))

ufo.us<-subset(ufo.us, DateOccurred>=as.Date("1990-01-01"))
nrow(ufo.us)

ufo.us$YearMonth<-strftime(ufo.us$DateOccurred, format="%Y-%m")
head(ufo.us$YearMonth)

library(plyr)
sightings.counts<-ddply(ufo.us,.(USState,YearMonth),nrow)
head(sightings.counts)





