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
library(textcat)
library(cldr)

citation("stringr")
citation()
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

## =================
# assess language of tweets -- after first classification, to remove from public
## =================

#remove special characters
d$bio <- iconv(d$bio, "UTF-8", "ASCII", sub = "")

#remove and, amp
d$bio <- str_replace_all(d$bio, "(\\band\\b|\\bamp\\b)", "")

#remove extra whitespaces
d$bio <- str_replace_all(d$bio, pattern = "\\s+", " ")


## =================
# test with all handles
## =================
head(d)
unique(d$handle)

length(unique(d$handle))
length(unique(d$Username))
nrow(d)

## =================
# code to assign categories
## =================
all.data <- d

## =================
# faculty
## =================

pat3 <- "\\blectur+|\\bprof\\b|\\bprofessor\\b|\\bprofesseur|\\bprofesor|\\bresearch chair\\b|\\bcrccrc\\b|\\bdean\\b|\\bfaculty\\b|\\bdistinguished chair\\b"

test <- c("i teach at the university of utah",
           "lead research at the university",
          "university news", "journal")

grepl(pat3,test) |
  (grepl("\\bteach|\\bchair\\b",test) & 
     grepl("\\buniversity\\b",test))| 
  (grepl("research",test) 
   & grepl("lead|director",test))

names(all.data)
all.data$faculty <- ifelse(grepl(pat3,all.data$bio) |
                                 (grepl("\\bteach|\\bchair\\b",all.data$bio) & 
                                 grepl("\\buniversity\\b",all.data$bio))| 
                             (grepl("research",all.data$bio) 
                           & grepl("\\blead\\b",all.data$bio)), 1, 0)

test <- filter(all.data, faculty == 1)
test$bio
## =================
# science student
## =================

pat0 <- "(stud(?!ies)|candidat+|major|\\bcompleting\\b|\\blearning\\b|\\bscholar\\b)"
pat1 <- "(\\bbsc?\\b|\\bmsc?\\b|phd|\\bdphil\\b|\\bdoctoral\\b|\\masters\\b|\\undergrad|grad+|ologist|ology\\b|oceanography\\b|biome|systems|\\bma\\b|\\bba\\b|evolution|istry\\b|\\bmres\\b|\\bresources? management\\b|\\beco\\b|scien+)"

pat2 <- "(\\bpost?doc|fellow\\b|\\bpost doc\\b|\\bgradschool\\b|\\bstudying\\b|\\bgraduate school\\b|\\bstudent\\b)"

test1 <- c("parasite enthusiast research scientist ecologist ornithologist bossed around by two labradors never far from a cake")
(grepl(pat0,test1,perl = TRUE) & grepl(pat1,test1)) | grepl(pat2,test1) 

names(all.data)
all.data$student <- ifelse(all.data$faculty == 0 & 
                              ((grepl(pat0,all.data$bio, perl = TRUE) & 
                                  grepl(pat1,all.data$bio)) | 
                                 grepl(pat2,all.data$bio)), 1, 0)

## =================
# universities, field stations, museums, zoos, aquariums
## =================
pat6 <- "(museum|museo|musee|\\bzoo\\b|zoo\\b|aquarium|\\botanical gardens\\b|\\bcurator\\b|smithsonian)"
test <- c("zoo", "zoology","zoologist")

grepl(pat6,test)

names(all.data)
all.data$check <- rowSums(all.data[9:10])
hist(all.data$check)
all.data$mza <- ifelse(all.data$check == 0 & 
                     (grepl(pat6,all.data$Username) | 
                     grepl(pat6,all.data$full_name)|
                     grepl(pat6,all.data$bio)), 1, 0)

## =================
# other scientists
## =================
#code to find other individual scientists
pat4 <- "(\\btechnician\\b|\\bacademic\\b|\\bdr\\b|research ass+|\\bresearch scientist\\b|\\blab manager\\b|chemist\\b|tician\\b|scientist\\b|\\bbotanist\\b|\\bscholar\\b|botanist|physicist\\b|\\b(?<!amateur )naturalist\\b|(?<!\\btechno)logist\\b|(?<!\\bphot)ographer\\b|\\beconomist\\b)"

pat.test <- "(?<!activ)ist"
grepl(pat.test,"activist", perl = TRUE)

test <- c("amateur naturalist")

grepl(pat4,test, perl = TRUE)

#only if not in another science category already 
names(all.data)
all.data$check <- rowSums(all.data[c(9:10,12)])

all.data$other.sci <- ifelse(all.data$check == 0 & 
                           (grepl(pat4,all.data$bio, perl = TRUE) |
                              (grepl("director",all.data$bio) & 
                                 grepl("research",all.data$bio))), 1,0)

## =================
# educators and outreach  -- individuals
## ================
pat10 <- "(\\beducator|\\bteach+|classrooms|\\bscience class\\b)"
test <- c("science teacher","outreach","teaching the world high school",
          "8th grade science class")
grepl(pat10,test)

names(all.data)
all.data$check <- rowSums(all.data[c(9:10,12:13)])
hist(all.data$check)

all.data$outreach <- ifelse(all.data$check == 0 & 
                          grepl(pat10,all.data$bio), 1, 0)

## =================
# scientific associations
## =================
pat1 <- "(\\bresearch\\b|\\bscien|istry\\b|isheries\\b|(?<!\\btechn)olog|\\bstud+|environment|\\bconservation|(?<!techn)olog|\\buniversity\\b)"
#AND
pat2 <- "(\\bassociation\\b|\\bsynthesis|\\binterdisciplinary\\b|\\bnetwork\\b|\\bdept\\b|\\bdepartment\\b|\\bcentre|\\bcenter|\\binitiative\\b|\\bacademicians\\b|\\brepository\\b|\\bforum\\b|\\bsection\\b|\\bmeeting\\b|\\bsociety\\b|\\bchapter\\b|\\bcouncil\\b|\\binstitute\\b|\\bcongress\\b|\\bproject\\b|\\bstudy\\b|\\bextension\\b|\\bvessel\\b)"

#OR
pat3 <- "(\\bobservator|\\bsymposi*|\\bpeer ?review\\b|\\bjournal\\b|\\bresearch group\\b|\\bfield station|\\bmeetings\\b|\\bresearch lab\\b|\\blab group\\b)"

#grepl("^(?=.*\\bconference\\b)(?!.*\\bcall\\b)", "conference", perl=TRUE)

test <- c("ecology synthesis", "conference call",
          "chapter fishery biologists", "nonprofit journal")
(grepl(pat1,test, perl = TRUE) & grepl(pat2,test)) | grepl(pat3,test) | 
  grepl("^(?=.*\\bconference\\b)(?!.*\\bcall\\b)", test, perl=TRUE)
 

names(all.data)
all.data$check <- rowSums(all.data[c(9:10,12:13)])
hist(all.data$check)

all.data$sci.assoc<- ifelse(all.data$check == 0 & 
                          grepl(pat1,all.data$bio, perl = TRUE) & grepl(pat2,all.data$bio) |
                            all.data$check == 0 & grepl(pat3,all.data$bio) | 
                            all.data$check == 0 & 
                            grepl("^(?=.*\\bconference\\b)(?!.*\\bcall\\b)",
                                  all.data$bio, perl=TRUE), 1, 0)

test <- filter(all.data, sci.assoc == 1)
test$bio


## =================
# media
## =================
#let media include people within other scientists, students and profs

pat11 <- "(writer\\b|journalis|blog|\\bpublisher\\b|\\bcorresponden|\\bcomms\\b|\\communicator\\b|scicomm|\\bauthor\\b|\\bproducer|\\bproduction|\\baudio\\b|\\bradio\\b|\\bpodcast+|\\bdocumentar+|\\bfilm+|\\bphotographer|\\breport|\\bshow\\b|movie\\b|\\bcopyeditor\\b|\\bbroadcast|\\btelevision\\b|\\bcommunicati|\\bfreelance\\b|\\bvideograph+|\\beditor\\b|\\bfoto+|\\bpublish+|\\media|news|periodist|\\byoutuber\\b|\\bmedia relations\\b|\\bpublicist\\b|\\bmagazine\\b|\\bwebzine\\b)"

test <- c("author", "blogger","journalist","photojournalist podcaster",
          "covemovieops", "commissioning editor","products", "filmmakers",
          "environmental reporting")
grepl(pat11,test)

names(all.data)
all.data$check <- rowSums(all.data[c(9,10,12,14:15)])
hist(all.data$check)

all.data$media <- ifelse(all.data$check == 0 & grepl(pat11, all.data$bio), 1, 0)


## =================
# applied 
## =================
pat8 <- "(\\bedf\\b|\\bfund(?! raising)\\b|\\b\\bfoundation\\b|\\bwwf+|\\bwcs\\b|\\bsociety\\b|trust\\b|\\bngo\\b|\\biucn\\b|\\bpew|\\bnonprofit\\b|\\bnon ?profit\\b|\\bgreenpeace\\b|\\bphilanthropy\\b|\\bstewardship\\b|\\busaid\\b|\\bpolicy officer\\b|\\bcapacity development\\b|\\international development|\\bsanctuar|\\bpaul ?g ?allen\\b|\\bthe ?nature ?conservancy\\b|\\btnc\\b|\\bintergovernmental\\b|\\bwildaid\\b|\\bzsl\\b|\\bnonpartisan\\b|\\bcommunity organi(s|z)ation\\b|\\bthink ?tank\\b|\\bblue ?ventures|\\bwildlife ?conservation ?society\\b|coalition|\\bcharit+|union of concerned scientists|\\bconservationorg\\b|\\bconservation international\\b|\\b operation ?wallacea\\b|\\boceannetworks\\b|\\bnational parks association\\b|\\bbirdlife|audobon|\\bscience policy\\b|\\bguy harvey\\b|\\bwildlife program\\b|alliance|unesco|conservancy|\\bsynchronicity earth\\b|\\bnot for profit\\b|\\brspb\b|\\bcoral triangle initiative)"

pat8b <- "(\\bwwf+|\\bwcs+|\\bedf|\\bzsl)" 

test <- c("fund raising", "funds")
grepl(pat8,test, perl = TRUE) | grepl(pat8b,test)

names(all.data)
all.data$check <- rowSums(all.data[c(9:10,12,14:15)])
hist(all.data$check)

all.data$applied <- ifelse(all.data$check == 0 & grepl(pat8,all.data$bio, perl = TRUE) |
                         all.data$check == 0 & grepl(pat8b,all.data$Username), 1, 0)


## =================
# politicians, decision makers
## =================

##start here

#check canadian MP acounts, US senators, congress
pat12 <- "\\bpublic servant\\b|\\bgovernment agency\\b|eucommission|congressional|congressman|congresswoman|senator\\b|senate|parliament|municipal|\\blabour candidate\\b|\\bindependent candidate\\b|\\bconservation candidate\\b|\\blegislator\\b|\\beuropean commission\\\b|\\bnmfs\\b|\\bnoaa\\b"
pat13 <- "(usfs|usfws|usgs|nps|\\bmp|noaa|nmfs)"

test <- c("usfwspacific", "usgs","usfs", "mpenvironment","usa","mpenvironment")
grepl(pat12,test) | grepl(pat13,test)

names(all.data)
all.data$check <- rowSums(all.data[c(9:10,12:17)])
hist(all.data$check)

all.data$politician <- ifelse(all.data$check == 0 & (grepl(pat12,all.data$bio) |
                            grepl(pat13,all.data$Username)), 1, 0)

## =================
# unknown
## =================
all.data$unknown <- ifelse(is.na(all.data$bio), 1,0)


## =================
# general public
## =================
names(all.data)
all.data$check <- rowSums(all.data[c(9:10,12:19)])
hist(all.data$check)

all.data$public <- ifelse(all.data$check == 0, 1, 0)


names(all.data)
all.data$check <- rowSums(all.data[c(9:10,12:20)])
hist(all.data$check)

## =================
# check for non-English languages in public, and remove? 
## =================

#textcat is easier, but returns a lot of misclassifications
#cldr has much better classification rate

test <- data.frame(x = all.data[sample(nrow(all.data), 100),"bio"])
cldr.save <- detectLanguage(test$x)
test$detectedLanguage <- cldr.save$detectedLanguage
  
names(all.data)
cldr.all.data <- detectLanguage(all.data$bio)
all.data$language <- cldr.all.data$detectedLanguage
all.data$lang.reliable <- cldr.all.data$isReliable

unique(all.data$language)

#only use reliable == TRUE
lang.test <- dplyr::filter(all.data, language == "ALBANIAN" & 
                             lang.reliable == "TRUE")
lang.test$bio

#condense languages that can be identified re: english
all.data$foreign <- ifelse(all.data$lang.reliable == "TRUE" & 
                             all.data$language != "ENGLISH", 1, 0)
sum(all.data$foreign)

#2579 bios reliably identified within 32 non-english or unknown languages
#overlooked non-roman characters, which didn't download into recognizable formats
#non-ASCII characters removed first in analysis -- not able to be classified anyways
foreign <- filter(all.data, foreign == 1)
unique(foreign$language)
sum(foreign$foreign)

#1727 non-English languages classified into public group
#(and should be removed from analysis)
public <- filter(all.data, public == 1)
sum(public$foreign)


## =================
# final sweep for other scientists
## =================
pat14 <- "(?<!\\btechn)ology\\b|\\boceanography\\b|\\becosystem\\b|\\bresources? management\\b|\\bevolution|\\becotox|\\becoevo\\b|bioinformatics|\\bdynamics\\b|(?<!\\bphot)ography\\b|\\bacademia\\b|\\bastronomy\\b|\\bgenetics\\b|\\bneuroscience\\b|\\bmolecular\\b|\\bbiosciences\\b|\\bpolitical science\\b|statistics|ology|\\bresearcher\\b"

test <- "oceanography"
grepl(pat14, test, perl = TRUE)


all.data$other.sci <- ifelse(all.data$public == 1 & 
                               grepl(pat14,all.data$bio, perl = TRUE), 1, 
                             all.data$other.sci)
all.data$public <- ifelse(all.data$other.sci == 1, 0, all.data$public)

## =================
# EXPORT
## =================
#dump extra columns for checking
names(all.data)
all.data2 <- all.data[,c(1:5,8:10,12:23)]
write.csv(all.data2, file.path(PROJHOME, "outputs","output - all.data_7Dec2016.csv"), 
          row.names = FALSE)

head(all.data2)
check <- filter(all.data2, Username == "@iied")


## =================
# select 5% of followers to validate
## =================
head(all.data2)

# database includes 64,666 unique followers (Usernames)


names(all.data2)
validate <- all.data2 %>% 
  melt(id.vars = c(1:6,18:20)) %>% 
  group_by(Username,language,lang.reliable,foreign,bio,variable) %>% 
  summarize(value = mean(value))

length(unique(validate$Username))
validate <-  dcast(validate, Username+language+lang.reliable+foreign+bio ~ variable)


#remove profiles classified as public that are non-english
#now have 63,348 total unique profiles
names(validate)
validate <- validate[-which(validate$foreign == 1 & 
                              validate$public == 1),]

#select 5% (n = 3161) of followers to validate classifications
0.05 * length(unique(validate$Username))

#make list of unique followers
head(all.data2)
followers <- all.data2 %>% 
  dplyr::distinct(Username)

perc5 <- followers[sample(nrow(followers),3161, replace = FALSE),]
perc5$included <- 1
perc5

#merge back into follower bios and classifications
validate <- left_join(validate, perc5) %>% 
  filter(included == 1)
head(validate)
names(validate)
nrow(validate) / 2

data <- validate[,-c(2:4,17)]

em <- data[sample(1:round(nrow(data)/2)), ]
head(em)
write.csv(em, 
          file.path(PROJHOME, "outputs","emily - tocheck_7Dec2016.csv"), 
          row.names = FALSE)


isa <- data[sample(round(nrow(data)/2)+1:nrow(data)), ]
isa <- na.omit(isa)
head(isa)
write.csv(isa, 
          file.path(PROJHOME, "outputs","isabelle - tocheck_7Dec2016.csv"), 
          row.names = FALSE)


#could try wordclouds of each of the categories
#check handling and processing of strings in R

24/106
25/215
81/805

