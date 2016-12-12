## =================
# code for Twitter followers
# created: 28 Nov 2016
# where? Paris! 
## =================

library(dplyr)
library(reshape2)
library(stringr)
library(ggplot2)
library(ggrepel)
library(RColorBrewer)
library(vegan)
library(readxl)

#install.packages("readxl")

## =================
# load 110 scientists info
## =================
setwd("/Users/emilydarling/Dropbox/1-On the go/Twitter_Followers/data/sent to NUVI")
scis110 <- read.csv("random 110 handles_16April2015.csv",
                    header = TRUE, strip.white = TRUE, stringsAsFactors = FALSE)
head(scis110)

min(scis110$Followers)
filter(scis110, Followers < 150)


## =================
# load long followers data
## =================
setwd("/Users/emilydarling/Dropbox/1-On the go/Twitter_Followers/data")
d <- read_excel("110 profiles_long.xlsx", sheet = 1)
head(d)

levels(as.factor(d$handle))

#translate from multiple languages into English -- another time

## =================
# basic string cleaning
## =================
#change all to lower case
d$bio <- tolower(d$Bio)

#remove lists with / and replace with " " (e.g., wife/phd/friend)
d$bio <- gsub("\\/", " ", d$bio)

# forward slash -- escape with a double backslash
sub("\\/", " ", "Peace/Love")
#[1] "Peace Love"


#remove special characters
d$bio <- iconv(d$bio, "UTF-8", "ASCII", sub = "")

#remove punctuations
punct <- '[]\\?!\"\'#$%&(){}+*/:;,._`|~\\[<=>@\\^-]'
d$bio <- gsub(punct, "", d$bio)

d <- d %>% 
  arrange(id,desc(Reach))

#remove extra whitespaces
d$bio <- str_replace_all(d$bio, pattern = "\\s+", " ")

head(d$bio)

## =================
# start testing out with @OConnorLab
## =================
head(d)
oconnor <- filter(d, handle == "@OConnorLab")
oconnor

write.csv(oconnor, file.path(PROJHOME, "outputs","OConnorLab test.csv"), 
          row.names = FALSE)


## =================
# code to assign categories
## =================

## =================
# science student
## =================
#BS, BSc, MSc, PhD, DPhil, postdoc or posdoc, fellow, 
#(student and 
#grad*, *ologist or *ology or science)

pat1 <- "\\bbsc?\\b|\\bmsc?\\b|\\bphd\\b|\\bdphil\\b|post?doc|\\bfellow\\b|\\bpost doc\\b"
pat2 <- "*ologist|*ology\\b|\\bgrad*|science"

test1 <- c("bsc","bs","journal of the","postdoc","posdoc","post doc")
grepl(pat1,test1) 

test2 <- c("entomology", "student entomologist")
grepl("student",test2) & grepl(pat2,test2) 


names(oconnor)
oconnor$sci.student <- ifelse(grepl(pat1,oconnor$bio) | 
                                grepl("\\bstudent\\b",oconnor$bio) & 
                                grepl(pat2,oconnor$bio), 1, 0)

test <- filter(oconnor, sci.student == 1)
test$Username
test$bio

## =================
# science faculty
## =================
#lectur*, prof* 

pat3 <- "\\blectur+|\\bprof+"
test3 <- c("lecturer in marine","university professor",
           "journal of the","university of X biology phd student",
           "project")

grepl(pat3,test3)

names(oconnor)
oconnor$sci.faculty <- ifelse(grepl(pat3,oconnor$bio) , 1, 0)

test <- filter(oconnor, sci.faculty == 1)
test$Username
test$bio


## =================
# scientific associations
## =================

pat1 <- "(scien|research)"
pat2 <- "(conference|associat|synthesis|integrat|interdisciplinary|
network|group|society|project|\\bcentre\\b|\\bcenter\\b)"
#OR
pat3 <- "(observator|\\bmeeting*|symposium|\\bpeer review\\b|\\bjournal\\b)"
oconnor$sci.assoc <- ifelse(grepl(pat1,oconnor$bio) & grepl(pat2,oconnor$bio) | 
                              grepl(pat3,oconnor$bio), 
                            1, 0)
test <- filter(oconnor, sci.assoc == 1)

test$Username
test$bio

## =================
# other scientists
## =================
pat4 <- "(\\btechnician\\b|scien|\\blab)"
pat5 <- "*ologists?\\b|*olog*|*icist\\b|*tician\\b"

#only if not in another science category already 
names(oconnor)
oconnor$check <- rowSums(oconnor[9:11])

oconnor$other.sci <- ifelse(oconnor$check == 0 & 
                              (grepl(pat4,oconnor$bio) | grepl(pat5,oconnor$bio)),
                            1,0)

## =================
# universities, field stations, museums, zoos, aquariums
## =================
pat6 <- "(?i)(\\bmuseum\\b|\\bzoo\\b|\\baquarium\\b)"

names(oconnor)
oconnor$check <- rowSums(oconnor[c(9:11,13)])
oconnor$mza <- ifelse(oconnor$check == 0 & 
                              grepl(pat6,oconnor$Username) | 
                              grepl(pat6,oconnor$full_name)|
                              grepl("botanical garden",oconnor$bio), 1,0)

filter(oconnor, mza > 0)

#second pass into scientific organizations
pat7 <- "\\bfield station|\\buniversit" 
oconnor$sci.assoc <- ifelse(oconnor$check == 0 & 
                        grepl(pat7,oconnor$bio), 1, oconnor$sci.assoc)


## =================
# applied category
## =================
pat8 <- "\\bedf\\b|\\bfund\\b|\\bfoundation\\b|\\bwwf\\b|\\bci\\b|\\bwcs\\b|
\\bsociety\\b|\\bconserv*|\\brestor*|\\brecover*|\\bgrassroots\\b|\\bapplied\\b|
\\borganization\\b|\\btrust\\b|\\bngo\\b" 

names(oconnor)
oconnor$applied <- ifelse(oconnor$check == 0 & 
                              grepl(pat8,oconnor$bio), 1, 0)

filter(oconnor, applied == 1)

## =================
# educators and outreach  
## ================
pat10 <- "\\beducat|\\bteach|\\boutreach\\b" 
test <- c("educator","outreach","teaching the world high school")
grepl(pat10,test)

names(oconnor)
oconnor$check <- rowSums(oconnor[c(9:11,13:15)])
oconnor$outreach <- ifelse(oconnor$check == 0 & 
                            grepl(pat10,oconnor$bio), 1, 0)
filter(oconnor, outreach == 1)


## =================
# media
## =================

## =================
# politicians
## =================


## =================
# unknown
## =================
oconnor$unknown <- ifelse(is.na(oconnor$bio), 1,0)

## =================
# general public
## =================
names(oconnor)
oconnor$check <- rowSums(oconnor[c(9:11,13:17)])

oconnor$public <- ifelse(oconnor$check == 0, 1, 0)
filter(oconnor, public == 1)

write.csv(oconnor, file.path(PROJHOME, "outputs","output - OConnorLab test.csv"), 
          row.names = FALSE)


#could try wordclouds of each of the categories
#check handling and processing of strings in R



