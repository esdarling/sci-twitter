library(tidyverse)
library(reshape2)
library(RColorBrewer)
library(grid)
library(extrafont)
library(here)

#-------------------------------------------------------------------------
#load and update packages
# ipak <- function(pkg){
#   new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
#   if (length(new.pkg)) 
#     install.packages(new.pkg, dependencies = TRUE)
#   sapply(pkg, require, character.only = TRUE)
# }

# usage
#packages <- c("reshape2", "RColorBrewer", "scales", "grid")
#ipak(packages)

#-------------------------------------------------------------------------
## SOURCE - load, clean and check data

#-------------------------------------------------------------------------
#load 110 users data
scis110 <- read.csv(file.path(PROJHOME,"sci-twitter","data",
                              "110 handles_with Twitter dates.csv"),
                    header = TRUE, strip.white = TRUE, stringsAsFactors = FALSE)
names(scis110)
names(scis110)[3] <- "handle"
#puttolower to match data analysis
scis110$handle <- tolower(scis110$handle)
unique(scis110$handle)

head(scis110)

#-------------------------------------------------------------------------
## load NUVI data - of all followers
d.original <- read.csv(file.path(PROJHOME,"sci-twitter","data",
                        "output - all.data_7Dec2016.csv"),
                       header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
names(d.original)
head(d.original)
d.original$handle <- tolower(d.original$handle)
unique(d.original$handle)

#rename some NUVI typos.. so they will match into scis110
unique(d.original$handle)
d.original$handle <- recode_factor(d.original$handle, 
                          "@ericsotka" = "@eriksotka", 
                          "@thelibalib" = "@thelibalab", 
                          "@ejmilnesgulland" = "@ejmilnergulland", 
                          "@johnrhutchison"= "@johnrhutchinson", 
                          "@bjeuquist"= "@bjenquist", 
                          "@micheljikaiser" = "@micheljkaiser", 
                          "@jelmcglothlin" = "@joelmcglothlin")

head(d.original)

#manuscript summaries
length(unique(d.original$Username)) #64,666 followers

test <- d.original %>% 
  select(Username) %>% 
  distinct()

#faculty.all.followers is all followers before anyone removed
faculty.all.followers <- d.original %>% 
  select(id, handle, Username) %>% 
  distinct() %>% 
  arrange(Username)

length(unique(faculty.all.followers$handle))
length(unique(faculty.all.followers$Username))

#remove foreign publics, unlikely to be properly classified
#some foreign scientists already well classified :) 
#make dataset d, to start removing Usernames not for analysis
d <- d.original
d <- d[-which(d$foreign == 1 & d$public == 1),]
nrow(d)
length(unique(d$Username))
64666-1709

62958/64666
64666-62958
1709/64666

64666-1708

#62,958 unique followers once foreign public (unclassified) profiles are removed
length(unique(d$Username)) 

#-------------------------------------------------------------------------
#add group2
#recode outreach (educators + mza)
#melt
names(d)
d2 <- d %>% 
  select(c(4:5,7:17)) %>% 
  melt(id.vars = c(1:2), variable.name = "group") %>% 
  filter(value > 0) #filter only where groups are identified
head(d2)

d2$group <- recode_factor(d2$group, 
                          mza = "outreach")
levels(as.factor(d2$group))
# 
# d2$group <- factor(d2$group, levels = c("faculty", "student",
#                                         "other.sci", "sci.assoc",
#                                         "outreach","applied", 
#                                         "media", "public","politician",
#                                         "unknown"))

#combine scientists group
d2$group2 <- recode_factor(d2$group,
                           faculty = "scientists",
                           student = "scientists",
                           other.sci = "scientists",
                           sci.assoc = "scientists")
levels(as.factor(d2$group2))
# 
# #recode for outputs
# d2$group <- recode_factor(d2$group, 
#                           faculty = "Science faculty",
#                           student = "Science students",
#                           'other.sci' = "Other scientists",
#                           'sci.assoc' = "Science associations",
#                           outreach = "Outreach",
#                           applied = "Applied",
#                           media = "Media",
#                           public = "Public",
#                           politician = "Decision makers",
#                           unknown = "Unknown")
# levels(as.factor(d2$group))

d2$group2 <- recode_factor(d2$group2, 
                           scientists = "Scientists",
                           outreach = "Outreach",
                           applied = "Applied",
                           media = "Media",
                           public = "Public",
                           politician = "Decision makers",
                           unknown = "Unknown")
levels(as.factor(d2$group2))

#Drop duplicates if Reach estmate is duplicated
d2 <- d2 %>% 
  group_by(Username, group2) %>% 
  summarize(Reach = max(Reach))

length(unique(d2$Username)) #62,958

#remove Unknown for all analysis
unique(d2$group2)
d2 <- d2 %>%
  filter(group2 != "Unknown")
length(unique(d2$Username)) #51,625 classified

#something is funky with d2 as a listed grouped data.frame
#hack is to save and reload as a flat data.frame
write.csv(d2, here("data", "d2.flat.export.csv"), 
          row.names = FALSE)

d2 <- read.csv(here("data", "d2.flat.export.csv"), 
               strip.white = TRUE, stringsAsFactors = FALSE, header = TRUE)
str(d2)

#remove some profiles that have more than one type and incl. Public
check <- d2 %>% 
  group_by(Username) %>% 
  tally() %>% 
  arrange(desc(n))
head(check)

d2 <- left_join(d2, check)
head(d2)

#12 Usernames have a duplicated Public classification - remove
duplicate.users <- d2 %>% 
  filter(n > 1 & group2 == "Public")
duplicate.users$Username

check <- d2 %>% 
  #sample_n(10, replace = TRUE) %>% 
  mutate(remove = ifelse(Username %in% duplicate.users$Username & 
                           group2 == "Public", 1, 0)) %>% 
  filter(remove == 1) 
#12 rows need to be removed

d2 <- d2 %>% 
  mutate(remove = ifelse(Username %in% duplicate.users$Username & 
                           group2 == "Public", 1, 0)) %>% 
  filter(remove == 0) %>% 
  select(-remove)

head(d2)

#deal with multiple types
count.multiple.type <- d2 %>% 
  filter(n > 1) %>% 
  select(Username) %>% 
  distinct()
nrow(count.multiple.type) 
1564 / 51625

head(d2)

#d2 is now a dataset of 51,625 classified profiles with their Username, group and reach
#rename d2 to followers, 51,625 classified with foreign, unknown removed
head(d2)
followers <- d2
length(unique(followers$Username))


#faculty.all.followers is all followers before anyone removed
length(unique(faculty.all.followers$handle))
length(unique(faculty.all.followers$Username))
head(faculty.all.followers)

#want to match all followers for analysis, this is d2 file
head(faculty.all.followers)
length(unique(faculty.all.followers$Username))

head(followers)
length(unique(followers$Username))

d2 <- inner_join(faculty.all.followers[,-4], followers, by = "Username")
head(d2)
length(unique(d2$Username))         


