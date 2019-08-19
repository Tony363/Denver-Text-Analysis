setwd("C:/Users/manni/OneDrive/Documents/MASTERS/SEM 01 - MSBX5410/AirBNB-Project/Downloads - Working Set")	#set path to find excel files
DenList <- read.csv("denver_listings.csv", stringsAsFactors=FALSE)							#initialize data set based on denver listings 
DenCal <- read.csv("denver_calendar.csv", stringsAsFactors=FALSE)								#initialize data set based on denver calendar
DenRev <- read.csv("denver_reviews.csv", stringsAsFactors=FALSE)								#initialize data set based on denver reveiw
library (ggplot2)
library (ggmap)
library (gridExtra)
register_google(key = "AIzaSyCgOqOT7wtVHh5QrCXZnKAhFBt4V6XHeJQ")


ListingID <- DenList$id															#initialize vector
Neighborhood <- DenList$neighbourhood												#initialize vector
Zipcode <- DenList$zipcode														#initialize vector
Beds <- DenList$beds															#initialize vector
PricePerNight <- DenList$price													#initialize vector
PricePerWeek <- DenList$weekly_price												#initialize vector
SecDeposit <- DenList$security_deposit												#initialize vector
CleanFee <- DenList$cleaning_fee													#initialize vector

ListingID <- as.character(ListingID)												#set vector as character vector
Neighborhood <- as.character(Neighborhood)											#set vector as character vector
Zipcode <- as.character(Zipcode)													#set vector as character vector
Beds <- as.numeric(Beds)														#set vector as numeric vector

for (i in 1:length(ListingID)){													#for loop for removing "$" sign 
	PricePerNight[i] <- gsub(pattern="$", replace="", PricePerNight[i],fixed=TRUE)				#in all of the vectors containing dollar values
	PricePerNight[i] <- gsub(pattern=",", replace="", PricePerNight[i],fixed=TRUE)	
	PricePerWeek[i] <- gsub(pattern="$", replace="", PricePerWeek[i],fixed=TRUE)
	PricePerWeek[i] <- gsub(pattern=",", replace="", PricePerWeek[i],fixed=TRUE)
	SecDeposit[i] <- gsub(pattern="$", replace="", SecDeposit[i],fixed=TRUE)
	SecDeposit[i] <- gsub(pattern=",", replace="", SecDeposit[i],fixed=TRUE)
	CleanFee[i] <- gsub(pattern="$", replace="", CleanFee[i],fixed=TRUE)
	CleanFee[i] <- gsub(pattern=",", replace="", CleanFee[i],fixed=TRUE)
}

PricePerNight <- as.numeric(PricePerNight)											#set vector as numeric vector
PricePerWeek <- as.numeric(PricePerWeek)												#set vector as numeric vector
SecDeposit <- as.numeric(SecDeposit)												#set vector as numeric vector
CleanFee <- as.numeric(CleanFee)													#set vector as numeric vector

Beds[is.na(Beds)] <- 1	
Beds[Beds==0] <- 1															#change "NA" values to 1

NeighReview <- data.frame(	ListingID, Neighborhood, Zipcode, Beds, PricePerNight, 				#initialize data set with created vectors 
			PricePerWeek, SecDeposit, CleanFee)

NeighReview$NightPricePerBed <- (NeighReview$PricePerNight / NeighReview$Beds)+ CleanFee				#create new column with nightly price per bed plus cleaning fee cost

ZipByNight <- aggregate(NightPricePerBed ~ Zipcode, FUN=mean, data=NeighReview)					#return the mean value of nightly price per bed in each neighborhood
NeiByNight <- aggregate(NightPricePerBed ~ Neighborhood, FUN=mean, data=NeighReview)

ZipByNight <- ZipByNight[order(ZipByNight$NightPricePerBed, decreasing = TRUE),]					#order those mean values descendingly
NeiByNight <- NeiByNight[order(NeiByNight$NightPricePerBed, decreasing = TRUE),]					#order those mean values descendingly

HighestZip <- ZipByNight[1,1]
HighestNeig <- NeiByNight[1,1]
LowestZip <- ZipByNight[nrow(ZipByNight),1]
LowestNeig <- NeiByNight[nrow(NeiByNight),1]

exp_lon<-DenList[DenList$zipcode==HighestZip, "longitude"]
exp_lat<-DenList[DenList$zipcode==HighestZip, "latitude"]									#Longitude and Latitude for 80216

HZLAT <- mean(sort(exp_lat))  
HZLON <- mean(sort(exp_lon)) 														#Find the central longitude and latitude for 80216

least_lon<-DenList[DenList$zipcode==LowestZip, "longitude"]
least_lat<-DenList[DenList$zipcode==LowestZip, "latitude"]									#Longitude and latitude for 80011 

LZLAT <- mean(sort(least_lat))  
LZLON <- mean(sort(least_lon)) 															#Find the central longitude and latitude for 80011

expnei_lon<-DenList[DenList$neighbourhood==HighestNeig, "longitude"]
expnei_lat<-DenList[DenList$neighbourhood==HighestNeig, "latitude"]							#Longitude and Latitude for Neighborhood CBD

HNLAT <- mean(sort(expnei_lat))
HNLON <- mean(sort(expnei_lon))															#Find the central longitude and latitude for CBD neighborhood

leastnei_lon<-DenList[DenList$neighbourhood==LowestNeig, "longitude"]
leastnei_lat<-DenList[DenList$neighbourhood==LowestNeig, "latitude"]							#Longitude and Latitude for Neighborhood Montbello

LNLAT <- mean(sort(leastnei_lat))
LNLON <- mean(sort(leastnei_lon))													#Find the central longitude and latitude for Montbello neighborhood

denv_map <- ggmap(get_map(location = c(-104.9233, 39.71881), zoom = 11, maptype = 'roadmap'))

denv_map + 
geom_point(size=.1, data =DenList, mapping = aes(x=DenList$longitude, y=DenList$latitude))+ 
geom_point(size=4, aes(x=HZLON, y=HZLAT, color='Most Expensive Zipcode 80216'))+ 
geom_point(size=3, aes(x=LZLON, y=LZLAT, color='Least Expensive Zipcode 80011'))+ 
geom_point(size=4, aes(x=HNLON, y=HNLAT, color='Most Expensive Neighborhood CBD'))+ 
geom_point(size=3, aes(x=LNLON, y=LNLAT, color='Least Expensive Neighborhood Montbello'))				#Final map with all data points including Most and Least expensive zip codes and neighborhoods

dev.new()


plot1 <- ggplot(data=NeiByNight, aes(x=NightPricePerBed))+ geom_histogram(binwidth=25,fill=8,color=1)
plot2 <- ggplot(data=ZipByNight, aes(x=NightPricePerBed))+ geom_histogram(binwidth=25,fill=8,color=1)
plot1 <- plot1 + ggtitle("Neighborhood")
plot2 <- plot2 + ggtitle("Zipcode")
grid.arrange(plot1,plot2)



















