
# Clear old virables
rm(list=ls())
gc()

# Load related libraries
#install.packages("corrplot")
library(ggplot2)
library(reshape2)
library(dplyr)
library(corrplot)
library(VIM)

# Load data
melhouse <- read.csv("D:/Drive/Persona/MIT Study/Semester 3/ITEC873 Machine Learning/1. Project Presentation and Proposal/Melbourne_housing_FULL.csv")


### DATA EXPLORATION
# The data set has 21 columns with 34857 rows
dim(melhouse)
str(melhouse)
summary(melhouse)
head(melhouse)

#################################################################################################### 
### DATA PREPROCESSING

## Check missing parttern
melhouse_arr <- aggr(melhouse,sortVars=TRUE)
#names(melhouse_arr$missings)
#melhouse_arr$missings$Count

# Columns with above 55% of missing values
allmissings <- melhouse_arr$missings$Variable[which(melhouse_arr$missings$Count > 0.55*nrow(melhouse))]
allmissings # "BuildingArea" "YearBuilt"
# Remove BuildingArea and YearBuilt
melhouse$BuildingArea <- NULL
melhouse$YearBuilt <- NULL


### Deal with "#N/A" values in Distance, Postcode, CouncilArea, Regionname, Propertycount
str(melhouse)
# Convert to suitable data type and find NA values
# Distance variable
melhouse$Distance <- as.character(melhouse$Distance)
unique(melhouse$Distance)
# Remove rows with Distance value is NA
melhouse$Distance[which((melhouse$Distance %in% c("#N/A")))] <- NA
melhouse$Distance <- as.numeric(melhouse$Distance)

# Postcode variable
melhouse$Postcode <- as.character(melhouse$Postcode)
unique(melhouse$Postcode)
melhouse$Postcode[which((melhouse$Postcode %in% '#N/A'))] <- NA

# CouncilArea varialbe
melhouse$CouncilArea <- as.character(melhouse$CouncilArea)
unique(melhouse$CouncilArea)
melhouse$CouncilArea[which(melhouse$CouncilArea %in% "#N/A")] <- NA


# Regionname variable
melhouse$Regionname <- as.character(melhouse$Regionname)
unique(melhouse$Regionname)
melhouse$Regionname[which(melhouse$Regionname %in% "#N/A")] <- NA

# Propertycount variable
melhouse$Propertycount <- as.character(melhouse$Propertycount)
unique(melhouse$Propertycount)
melhouse$Propertycount[which((melhouse$Propertycount %in% c("#N/A")))] <- NA
melhouse$Propertycount <- as.numeric(melhouse$Propertycount)

## NA in landsize
# If landsize is zero, set its value as NA
melhouse[which((melhouse$Landsize == 0)),]
melhouse$Landsize <- ifelse(melhouse$Landsize == 0,NA,melhouse$Landsize)


# Recheck missing parttern
str(melhouse)
melhouse_arr <- aggr(melhouse,sortVars=TRUE)

#######################################################
## Price (target variable)

boxplot(melhouse$Price)
mis.num <- length(melhouse$Price[which(is.na(melhouse$Price))]) # 7610 missing
mis.num/nrow(melhouse) # 0.2183206 => Account for nearly 22% of original data
# Since Price is the target varialbe, and the number of observation is huge
# Missing data of price will be removed
melhouse <- melhouse[which(!is.na(melhouse$Price)),]
dim(melhouse)
# Review missing parttern
melhouse_arr <- aggr(melhouse,sortVars=TRUE)

#####################
# Missing parttern of bedroom, bathroom and car spot
melhouse_arr <- aggr(melhouse[,c("Rooms","Bedroom2","Bathroom","Car")],sortVars=TRUE)
# Since the number of bathroom and car spot are also important factors when assess a house,
# These columns are kepts. Hence, rows with missing values (25%) are removed
melhouse <- melhouse[which(!is.na(melhouse$Bathroom)),]
melhouse <- melhouse[which(!is.na(melhouse$Car)),]



###################### 
# Landsize 0.228369975
# Imputation Landsize using median values
summary(melhouse$Landsize)
boxplot(melhouse$Landsize~melhouse$Type)
hist(melhouse$Landsize)

# Get median of Landsize for different house's Type and Suburb
medians <- na.omit(melhouse[,c("Type","Suburb","Landsize")]) %>% group_by(Type,Suburb) %>% 
  summarise(MedLandsize = median(Landsize))
melhouse <- melhouse %>% left_join(medians,by=c("Type"="Type","Suburb"="Suburb"))
# Merge
melhouse$Landsize <- ifelse(is.na(melhouse$Landsize), melhouse$MedLandsize, melhouse$Landsize)
# Remove median data
melhouse$MedLandsize <- NULL

# Review missing parttern
melhouse_arr <- aggr(melhouse,sortVars=TRUE)
# Remove missing values left (0.003035793)
melhouse <- melhouse[which(!is.na(melhouse$Landsize)),]

# ADDRESS OUTLIERS
boxplot(melhouse$Landsize~melhouse$Type,main="Boxlot of Landsize by house Type",
        xlab="House Type: (h-house), (t-townhouse), (u-unit)",ylab="Landsize",col="green")
# Which houses with Landsize above 10000m2
melhouse[which(melhouse$Landsize > 10000),c("Type","Suburb","Landsize")] # 43 houses 
# Remove rows with Landsize above 10000m2
melhouse <- melhouse[which(melhouse$Landsize < 10000),]
# Which houses with Landsize bellow 10m2
melhouse[which(melhouse$Landsize <= 10),c("Type","Suburb","Landsize")] # 
# Remove rows with Landsize bellow 10m2
melhouse <- melhouse[which(melhouse$Landsize > 10),]

summary(melhouse$Landsize)
boxplot(melhouse$Landsize~melhouse$Type)

# Save data for later analysis
write.csv(melhouse,row.names=FALSE,file="cleaned_melbourn_housing_data.csv")

# Review missing parttern
melhouse_arr <- aggr(melhouse,sortVars=TRUE)

###################### 
# Impute Longtitude and Lattitude from the address of house
# Since googple map limit the number of geocode queries,
# This imputation code need to run several times to get all Longtitude and Latitude

melhouse <- read.csv(file="cleaned_melbourn_housing_data.csv",header = TRUE)

# Review missing parttern
melhouse_arr <- aggr(melhouse,sortVars=TRUE)

library(ggmap)
library(stringr)

# Get full address
melhouse$full.address <- paste(melhouse$Address,melhouse$Suburb,"Melbourn, Australia", sep=", ")
for(i in 1:nrow(melhouse)){
  if(is.na(melhouse$Longtitude[i]))
  {
    longlat <- geocode(melhouse$full.address[i])
    melhouse$Longtitude[i] <- longlat[1,1]
    melhouse$Lattitude[i] <- longlat[1,2]
  }
}
melhouse$full.address <- NULL

# Save data for later analysis
write.csv(melhouse,row.names=FALSE,file="cleaned_melbourn_housing_data.csv")

# End Long and Lat imputation
####################################################################################################


####################################################################################################

# CORRELATION ANALYSIS
# Reread cleaned data
melhouse <- read.csv(file="cleaned_melbourn_housing_data.csv",header = TRUE)

# Get numeric varialbes
nums <- unlist(lapply(melhouse, is.numeric)) 
melhouseN <- melhouse[,nums]
# Calculate correlation
mhN.corr <- cor(na.omit(melhouseN))
corrplot(mhN.corr, order = "hclust")
mhN.corr

# As the correlation of Bedroom2 and Rooms is above 0.95, so Bedroom2 is redundant and is deleted
melhouse$Bedroom2 <- NULL

# Recheck correlation
nums <- unlist(lapply(melhouse, is.numeric)) 
melhouseN <- melhouse[,nums]
# Calculate correlation
mhN.corr <- cor(na.omit(melhouseN))
corrplot(mhN.corr, order = "hclust")

# Save data for later analysis
write.csv(melhouse,row.names=FALSE,file="cleaned_melbourn_housing_data.csv")


## DISCRIPTIVE EXPLORATION
library(ggplot2)

###	Which suburbs have the most expensive houses?

# Top 10 suburbs have the highest median house price
top10.high.median <- melhouse %>% group_by(Suburb) %>% summarise(MedianPrice = median(Price)) %>%
  arrange(desc(MedianPrice)) %>% mutate(SuburbName=reorder(Suburb,MedianPrice)) %>%
  head(10)
top10.high.median  

ggplot(top10.high.median, aes(x=SuburbName,y=MedianPrice)) + 
  geom_bar(stat = 'identity',colour='white',fill='green') +
  geom_bar(stat='identity',colour="white",fill="green") +
  geom_text(aes(x = SuburbName, y = 1, label = paste0("(",MedianPrice,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Suburb Name', y = 'Median House Price', title = 'Suburb name with the highest median house price') +
  coord_flip() + 
  theme_bw()

# Top 10 suburbs have the lowest median house price
top10.low.median <- melhouse %>% group_by(Suburb) %>% summarise(MedianPrice = median(Price)) %>%
  arrange(desc(MedianPrice)) %>% mutate(SuburbName=reorder(Suburb,MedianPrice)) %>%
  tail(10)
top10.low.median  

ggplot(top10.low.median, aes(x=SuburbName,y=MedianPrice)) + 
  geom_bar(stat = 'identity',colour='white',fill='green') +
  geom_bar(stat='identity',colour="white",fill="green") +
  geom_text(aes(x = SuburbName, y = 1, label = paste0("(",MedianPrice,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Suburb Name', y = 'Median House Price', title = 'Suburb name with the lowest median house price') +
  coord_flip() + 
  theme_bw()

## MAP

boxplot(Price~Type, data=melhouse, xlab="House Type: (h-house), (t-townhouse), (u-unit)",ylab="Price",
        main = "Boxplot of Price with each House Type",
        boxwex=0.25,boxfill="lightblue",pch=20)

boxplot(Price~Regionname, data=melhouse, xlab="Regionname",ylab="Price",
        main = "Boxplot of Price with each Region",
        boxwex=0.25,boxfill="lightblue",pch=20)


hist(melhouse$Price,xlab="Price",main="Histogram of Price",breaks = 30,col = "green")
hist(log(melhouse$Price),xlab="log(Price)",main="Histogram of log(Price)",breaks = 30,col = "green")

hist(melhouse$Landsize,xlab="Landsize",main="Histogram of Landsize",breaks = 50,col = "blue")

hist(melhouse$Distance,xlab="Distance",main="Histogram of Distance",breaks = 30,col = "blue")


library(lattice)
barchart(melhouse$Method,melhouse, 
         xlab="Frequency",main="The frequency of each sold method")

barchart(melhouse$Regionname,melhouse, 
         xlab="Frequency",main="The frequency of each Region")

barchart(as.factor(melhouse$Rooms),melhouse, 
         xlab="Frequency",main="The frequency of each number of rooms")

