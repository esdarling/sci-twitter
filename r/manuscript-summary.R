library(dplyr)
library(reshape2)
library(gdata)
library(stringr)
library(ggplot2)
library(ggrepel)
library(RColorBrewer)
library(vegan)


## =================
# load 110 follower info, summaries
## =================
d <- read.csv(file.path(PROJHOME,"sci-twitter","data",
                        "output - all.data_7Dec2016.csv"),
              header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
names(d)
head(d)
d$handle <- tolower(d$handle)
unique(d$handle)

d.summary <- d %>% 
  select(Username, foreign, 
         faculty:public) %>% 
  #filter(foreign < 1) %>% 
  distinct() %>% 
  melt(id.vars = 1:2) %>% 
  filter(value > 0)

head(d.summary)
length(unique(d.summary$Username))
table(d.summary$foreign)

table(d.summary$variable)
((64260-11359) / 64260) * 100

unique(d.summary$variable)

head(d)
d.distinct <- d %>% 
  group_by(Username) %>%
  summarize(mean.Reach = mean(Reach), 
            foreign = max(foreign))

table(d.distinct$foreign)
summary(d.distinct$mean.Reach); sd(d.distinct$mean.Reach)

head(d.distinct)
table(d.distinct$foreign)
length(unique(d.distinct$Username))



#read scis110
scis110 <- read.csv(file.path(PROJHOME,"sci-twitter","data",
                              "110 handles_with Twitter dates.csv"),
                    header = TRUE, strip.white = TRUE, stringsAsFactors = FALSE)
names(scis110)[3] <- "handle"
head(scis110)
#puttolower to match data analysis
scis110$handle <- tolower(scis110$handle)

head(scis110)

#GENDER
table(scis110$Gender)

#POSITION
head(scis110)
table(scis110$Level)
33/110
27/110
50/110


#UNIVERSITIES
unique(scis110$University)
unique(scis110$Country)

#FOLLOWERS
hist(scis110$Followers)
summary(scis110$Followers); sd(scis110$Followers)

#DATE JOINED
names(scis110)
summary(scis110$Months.total)

subset(scis110, Followers > 5000)

head(scis110)
hist(scis110$Months.total)
hist(scis110$Year.joined)
