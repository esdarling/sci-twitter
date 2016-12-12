library(dplyr)
library(reshape2)
library(gdata)
library(stringr)
library(ggplot2)
library(ggrepel)
library(RColorBrewer)
library(vegan)


## =================
# code to make Twitter follower data
# created: sometime in 2016.. (crap, maybe 2015)
## =================

###CODE TO PULL SINGLE NUVI SHEETS INTO A LONG DATABASE
setwd("/Users/emilydarling/Dropbox/1-On the go/Twitter_Followers/data")
excelFile <- "/Users/emilydarling/Dropbox/1-On the go/Twitter_Followers/Data/110 Tabs.xlsx"   
numSheets <- sheetCount(excelFile, verbose=TRUE)
nameSheets <- sheetNames(excelFile)
nameSheets

# name and number of sheet - pull to match into ESD/IMC data]
# coerce all to characters for easier rbind_all later
for (i in 1:110) {
  mySheet <- read.xls(excelFile, sheet=i)
  split <- strsplit(nameSheets[i], split = "\\ ")  
  id <- sapply(split, function(x) x[1]) 
  handle <- sapply(split, function(x) x[2]) 
  mySheet$id <- id
  mySheet$handle <- handle  
  mySheet$full_name <- mySheet[,1]
  mySheet <- mySheet[,c(6:7,8,2:5)]
  mySheet[]<- lapply(mySheet, as.character)
  setwd("/Users/emilydarling/Dropbox/1-On the go/Twitter_Followers/data/csv_files")  
  write.csv(mySheet, file=paste(nameSheets[i], "csv", sep="."), row.names=FALSE)
}

setwd("/Users/emilydarling/Dropbox/1-On the go/Twitter_Followers/Data/csv_files")
csv_files <- dir(pattern='*.csv$', recursive = T)
csv_files

#setwd("/Users/emilydarling/Dropbox/1-On the go/Twitter_Followers/Data/csv_files")
#duarte <- read.csv("1 @duarteoceans.csv", 
#                   header = TRUE, stringsAsFactors = FALSE) 

#drew <- read.csv("2 @Drew_Lab.csv", 
 #                  header = TRUE, stringsAsFactors = FALSE) 

# keep stringsAsFactors = FALSE in do.call
temp = list.files(pattern="*.csv")

twitter110 = do.call("rbind", 
                  lapply(csv_files, function(x) read.csv(x, stringsAsFactors = FALSE)))

# check when names weren't matching in - two errors
#myFiles <- lapply(csv_files, function(x) read.csv(x, stringsAsFactors = FALSE))
#head(myFiles)
#is.list(myFiles)

#test <- do.call("rbind", lapply(myFiles, names))
#head(test)

#setwd("/Users/emilydarling/Dropbox/1-On the go/Twitter_Followers/Data")
#write.csv(test, "names check.csv")

head(twitter110)
names(twitter110)
nrow(twitter110)
levels(as.factor(twitter110$handle))

twitter110b <- twitter110 %>%
  arrange(id)
  
#might have to pull in all csvs, rechange variables (factors, characters, etc)
setwd("/Users/emilydarling/Dropbox/1-On the go/Twitter_Followers/data")
write.csv(twitter110b, "110_long.csv", row.names = FALSE)    

# read in twitter 100 long compilation
setwd("/Users/emilydarling/Dropbox/1-On the go/Twitter_Followers/Data")
drew <- read.csv("110_long.csv", header = TRUE, stringsAsFactors = FALSE) 

head(twitter110)
nrow(twitter110)
levels(as.factor(twitter110$handle))

## IDEAS FROM LOOKING AT BIOS
# will need to pull all emoticons out
# remove #, @
#check that only strings are left, delete all other spaces




###CODE TO SEND HANDLES to MIKE HAGUE, NUVI

#Randomly select 110 followers from list of 162
setwd("/Users/emilydarling/Dropbox/1-On the go/Twitter_Followers/Data")         
dat <- read.csv("162 Twitter followers for Mike_1April2015.csv", header = TRUE, stringsAsFactors = TRUE) 
head(dat)
nrow(dat) 
names(dat)

#dplyr to randomly select 110 handles
dat_110 <- sample_n(dat, 110)
hist(dat_110$Followers, main = NA, xlab = "# Followers (n = 110 random handles)")
min(dat_110$Followers); max(dat_110$Followers)

write.csv(dat_110, "random 110 handles_16April2015.csv", row.names = FALSE)

#top 110 handles with most followers
top_110 <- dat %>%
  arrange(desc(Followers)) %>%
  top_n(110, wt = Followers)

head(top_110)

hist(top_110$Followers, main = NA, xlab = "# Followers (n = top 110 handles)")
min(top_110$Followers); max(top_110$Followers)
hist(top_110$Tweets)


##For Mike, v1
setwd("/Users/emilydarling/Dropbox/1-On the go/Twitter_Followers/Data")         
dat <- read.csv("NUVI list for R_31Mar2015.csv", header = TRUE, stringsAsFactors = TRUE) 
head(dat)
nrow(dat) 
names(dat)

#Remove DaysSinceLastTweet = 9999 (never tweeted)
dat <- dat[-which(dat$DaysSinceLastTweet == 9999),]                        
hist(dat$DaysSinceLastTweet)     
hist(dat$Followers, main = "No. followers") 

#Subset to handles that have tweeted, retweeted in last 2 weeks
dat_2weeks <- subset(dat,DaysSinceLastTweet <= 14)
hist(dat_2weeks$DaysSinceLastTweet)  
nrow(dat_2weeks)   

op <- par(mfrow = c(1,3))
hist(dat_2weeks$DaysSinceLastTweet, main = "Days since last tweet")   
hist(dat_2weeks$Followers, main = "No. followers")  
hist(dat_2weeks$Tweets, main = "No. tweets")  
par(op)      

write.csv(dat_2weeks, "162 Twitter followers for Mike_1April2015.csv", row.names = FALSE)


sum(dat_2weeks$Followers)


#152 people tweeted in last week
dat_1week <- subset(dat,DaysSinceLastTweet <= 7)
hist(dat_1week$DaysSinceLastTweet)  
nrow(dat_1week)   

op <- par(mfrow = c(1,3))
hist(dat_1week$DaysSinceLastTweet, main = "Days since last tweet")   
hist(dat_1week$Followers, main = "No. followers")  
hist(dat_1week$Tweets, main = "No. tweets")  
par(op)
           

#92 people tweeted in last two days
dat_1day <- subset(dat,DaysSinceLastTweet <= 1)
hist(dat_1day$DaysSinceLastTweet)  
nrow(dat_1day)  

op <- par(mfrow = c(1,3))
hist(dat_1day$DaysSinceLastTweet, main = "Days since last tweet")   
hist(dat_1day$Followers, main = "No. followers")  
hist(dat_1day$Tweets, main = "No. tweets")  
par(op)   


# ============================
# = Histograms of followers  =
# ============================  
head(dat_2weeks)  
hist(dat_2weeks$Followers)  
hist(dat_2weeks$Tweets)  

hist(dat_1week$Followers)  
hist(dat_1week$Tweets) 

hist(dat_1day$Followers)  
hist(dat_1day$Tweets)  
