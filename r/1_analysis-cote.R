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

length(unique(d$Username)) * 0.05 / 2

levels(as.factor(d$handle))

#translate from multiple languages into English -- another time

## =================
# basic string cleaning
## =================
#change all to lower case
head(d)
d$bio <- tolower(d$Bio)
d$full_name <- tolower(d$full_name)
d$Username <- tolower(d$Username)

#remove lists with / and replace with " " (e.g., wife/phd/friend)
d$bio <- gsub("\\/", " ", d$bio)

# forward slash -- escape with a double backslash
sub("\\/", " ", "Peace/Love")
#[1] "Peace Love"

length(unique(d$full_name))

#remove punctuations
punct <- '[]\\?!\"\'#$%&(){}+*/:;,._`|~\\[<=>@\\^-]'
d$bio <- gsub(punct, "", d$bio)

d <- d %>% 
  arrange(id,desc(Reach))

############
#make foreign category
##############
#identify bios with special characters (suggests another languages)
# ?? 

#remove special characters
d$bio <- iconv(d$bio, "UTF-8", "ASCII", sub = "")

#remove extra whitespaces
d$bio <- str_replace_all(d$bio, pattern = "\\s+", " ")

head(d$bio)

## =================
# test with @redlipblenny
## =================
head(d)
unique(d$handle)

cote <- filter(d, handle == "@redlipblenny")
cote

write.csv(cote, file.path(PROJHOME, "outputs","redlipblenny test.csv"), 
          row.names = FALSE)


## =================
# code to assign categories
## =================

## =================
# science faculty
## =================
#lectur*, prof* 

pat3 <- "\\blectur+|\\bprof\\b|\\bprofessor+|\\bresearch chair\\b|\\bcrccrc\\b"
test3 <- c("lecturer in marine","university professor",
           "journal of the","university of X biology phd student",
           "project","professora")

grepl(pat3,test3)

names(cote)
cote$sci.faculty <- ifelse(grepl(pat3,cote$bio) , 1, 0)

test <- filter(cote, sci.faculty == 1)
test$Username
test$bio


## =================
# science student
## =================
#BS, BSc, MSc, PhD, DPhil, postdoc or posdoc, fellow, 
#(student and 
#grad*, *ologist or *ology or science)

pat0 <- "(stud+|\\bcandidate\\b)"
pat1 <- "(\\bbsc?\\b|\\bmsc?\\b|\\bphd|\\bdphil\\b|\\bdoctoral\\b|\\masters\\b|\\bgraduate school\\b|\\undergraduate\\b|\\grad+|*ologist|*ology\\b)"

pat2 <- "(\\bpost?doc|fellow|\\bpost doc\\b|\\bgradschool\\b|\\bstudying\\b|\\bundergrad|\\bmasters\\b)"

test1 <- c("ms student","doctoral candidate","bsc studying","with a phd",
           "postdoc","entomology grad student", "student entomologist",
           "science and biology student","undergrad")
(grepl(pat0,test1) & grepl(pat1,test1)) | grepl(pat2,test1) 

names(cote)
cote$sci.student <- ifelse(cote$sci.faculty == 0 & 
                              ((grepl(pat0,cote$bio) & grepl(pat1,cote$bio)) | 
                                 grepl(pat2,cote$bio)), 1, 0)

test <- filter(cote, sci.student == 1)
test$bio

## =================
# universities, field stations, museums, zoos, aquariums
## =================
pat6 <- "(museum|zoo|aquarium|\\botanical gardens\\b|\\bcurator\\b)"

names(cote)
cote$check <- rowSums(cote[9:10])
hist(cote$check)
cote$mza <- ifelse(cote$check == 0 & 
                     (grepl(pat6,cote$Username) | 
                     grepl(pat6,cote$full_name)|
                     grepl(pat6,cote$bio)), 1, 0)

test <- filter(cote, mza > 0)
test$bio


## =================
# other scientists
## =================
#code to find other individual scientists
pat4 <- "(\\btechnician\\b|\\bacademic\\b|\\bdr\\b|\\bresearch associate\\b|\\bresearch scientist\\b|\\lab manager\\b|\\bphd\\b|\\bresearcher\\b)"
pat5 <- "*ologist\\b|*icist\\b|*tician\\b|\\bscientist\\b"


test <- c("research scientist", "marine biologist","biology association")
test2 <- c("documentary filmmaker amp digital media strategist i help share stories that matter opinions mine reachingblue","specialist")

grepl(pat4,test2) | grepl(pat5,test2)

#only if not in another science category already 
names(cote)
cote$check <- rowSums(cote[c(9:10,12)])
hist(cote$check)


cote$other.sci <- ifelse(cote$check == 0 & 
                           (grepl(pat4,cote$bio) | grepl(pat5,cote$bio) |
                              (grepl("director",cote$bio) & 
                                 grepl("research",cote$bio))), 1,0)

test <- filter(cote, other.sci == 1)
test$bio


## =================
# educators and outreach  -- individuals
## ================
pat10 <- "\\beducator|\\bteach+|classrooms" 
test <- c("educator","outreach","teaching the world high school")
grepl(pat10,test)

names(cote)
cote$check <- rowSums(cote[c(9:10,12:13)])
hist(cote$check)

cote$outreach <- ifelse(cote$check == 0 & 
                          grepl(pat10,cote$bio), 1, 0)

test <- filter(cote, outreach == 1)
test$bio


## =================
# scientific associations
## =================
pat1 <- "(\\bresearch\\b|\\bscien+)"
pat2 <- "(\\bassociation\\b|\\bsynthesis|\\binterdisciplinary\\b|\\bnetwork\\b|\\bsociet\\b|\\bdept\\b|\\bdepartment\\b|\\blab+|\\balliance\\b|\\bcentre|\\bcenter|\\balliance\\b|\\binitiative\\b|\\bacademicians\\b|\\brepository\\b)"

#OR
pat3 <- "(observator|\\bsymposi|\\bpeer review\\b|\\bjournal\\b|\\bconference\\b|\\bresearch group\\b|\\bfield station|\\buniversity\\b)"

pat3b <- "(\\bmeeting|\\bsociety|chapter)"

###
#fix -- not "conference call"
###

test <- c("conference call","research center","research centre",
          "hakai field station", "university of british columbia",
          "canadian society for ecology",
          "chapter fishery biologists", "nonprofit journal")

(grepl(pat1,test) & grepl(pat2,test)) | grepl(pat3,test) |
  (grepl(pat3b,test) & 
     grepl("*olog",test))


names(cote)
cote$check <- rowSums(cote[c(9:10,12:13)])
hist(cote$check)

cote$sci.assoc<- ifelse(cote$check == 0 & 
                          (grepl(pat1,cote$bio) & grepl(pat2,cote$bio)) |
                          cote$check == 0 & grepl(pat3,cote$bio) | 
                          cote$check == 0 & (grepl(pat3b,cote$bio) & 
                                               grepl("*olog",cote$bio)), 1, 0)

test <- filter(cote, sci.assoc == 1)
test$bio


## =================
# media
## =================
#let media include people within other scientists, students and profs

pat11 <- "(\\bwriter\\b|\\bjournalis|\\bblog|\\bpublisher\\b|\\bcorresponden|\\bcomms\\b|\\communicator\\b|scicomm|\\bauthor\\b|\\bproducer|\\bproduction|\\baudio\\b|\\bradio\\b|\\bpodcast+|\\bdocumentar+|\\bfilm+|\\bphotograph+|\\breport|\\bshow\\b|movie\\b|\\bcopyeditor\\b|\\bbroadcast|\\btelevision\\b|\\bcommunications\\b|\\bfreelance\\b|\\bvideograph+|\\beditor\\b|\\bfoto+)"

test <- c("author", "blogger","journalist","photojournalist podcaster",
          "covemovieops", "commissioning editor","products", "filmmakers",
          "environmental reporting")
grepl(pat11,test)

names(cote)
cote$check <- rowSums(cote[c(9,10,12,14:15)])
hist(cote$check)

cote$media <- ifelse(cote$check == 0 & grepl(pat11, cote$bio), 1, 0)

test <- filter(cote, media > 0 )
test$bio


## =================
# applied 
## =================
pat8 <- "(\\bedf\\b|\\bfund\\b|\\bfoundation\\b|\\bwwf+|\\bwcs\\b|\\bsociety\\b|\\btrust|\\bngo\\b|\\biucn\\b|\\bpew|\\bnonprofit\\b|\\bnon ?profit\\b|\\bgreenpeace\\b|\\bphilanthropy\\b|\\bconservation scientist\\b|\\bconservation biologist|\\badvoca+|\\bstewardship\\b|\\busaid\\b|\\bpolicy officer\\b|\\bcapacity development\\b|\\international development|\\bsanctuar|\\bpaul ?g ?allen\\b|\\bthe ?nature ?conservancy\\b|\\btnc\\b|\\bintergovernmental\\b|\\bwildaid\\b|\\bzsl\\b|\\bnonpartisan\\b|\\bcommunity organi(s|z)ation\\b|\\bactivis|\\bthink ?tank\\b|\\bvisual\\b|\\bblue ?ventures)" 

pat8b <- "(\\bwwf+|\\bwcs+)" 

test <- c("un wfp", "pewenvironment","nonprofit","organisation","wwfcanada",
          "conservation scientist","conserving nature",
          "wcsfiji", "advocacy","paulgallen","community organization",
          "blueventures")
grepl(pat8,test) | grepl(pat8b,test)

names(cote)
cote$check <- rowSums(cote[c(9:10,12,14:15)])
hist(cote$check)

cote$applied <- ifelse(cote$check == 0 & grepl(pat8,cote$bio) |
                         cote$check == 0 & grepl(pat8b,cote$Username), 1, 0)

test <- filter(cote, applied == 1)
test$bio


## =================
# politicians, decision makers
## =================

##start here

#check canadian MP acounts, US senators, congress
pat12 <- "\\bpublic servant\\b|\\bgovernment agency\\b"
pat13 <- "(usfs|usfws|usgs)"

test <- c("usfwspacific", "usgs","usfs", "mpenvironment")
grepl(pat12,test) | grepl(pat13,test)

names(cote)
cote$check <- rowSums(cote[c(9:10,12:17)])
hist(cote$check)

cote$politician <- ifelse(cote$check == 0 & (grepl(pat12,cote$bio) |
                            grepl(pat13,cote$Username)), 1, 0)

test <- filter(cote, politician > 0 )
test$bio

## =================
# unknown
## =================
cote$unknown <- ifelse(is.na(cote$bio), 1,0)

test <- filter(cote, unknown == 1)
test$bio


## =================
# general public
## =================
names(cote)
cote$check <- rowSums(cote[c(9:10,12:19)])
hist(cote$check)

cote$public <- ifelse(cote$check == 0, 1, 0)

test <- filter(cote, public == 1)
test$bio


names(cote)
cote$check <- rowSums(cote[c(9:10,12:20)])
hist(cote$check)


## =================
# last sweep for leftover scientists
## =================
pat14 <- "(\\bscien+|*olog|*systems|evolution|\\bgrad+|\\bmsc\\b|\\bphd\\b|academi)"
pat15 <- "(\\bmajor\\b|\\bstudent\\b)"

test <- c("biology major", "scientist", "ubc global systems student",
          "marine biology enthusiast","evolutionary","oceanography graduate",
          "the national marine sanctuary system is a network of special places preserving and protecting americas ocean and great lakes", "specialist")
grepl(pat14, test)

names(cote)
cote$sci.student <- ifelse(cote$public == 1 & 
                             grepl(pat14,cote$bio) & grepl(pat15,cote$bio),
                           1,cote$sci.student)

cote$other.sci <- ifelse(cote$public == 1 & 
                             grepl(pat14,cote$bio),
                           1,cote$other.sci)

cote$public <- ifelse(cote$sci.student == 1, 0, cote$public)
cote$public <- ifelse(cote$other.sci == 1, 0, cote$public)

test <- filter(cote, public == 1)
test$bio


## =================
# last sweep for leftover conservation? 
## =================
pat15 <- "(\\bconservation\\b)"

names(cote)
cote$applied <- ifelse(cote$public == 1 & 
                           grepl(pat15,cote$bio),
                         1,cote$applied)

cote$public <- cote$public - cote$applied

#dump extra columns for checking
names(cote)
cote2 <- cote[,c(2:3,8:20)]
write.csv(cote2, file.path(PROJHOME, "outputs","output - cote test.csv"), 
          row.names = FALSE)




#could try wordclouds of each of the categories
#check handling and processing of strings in R



