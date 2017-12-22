library(dplyr)
library(reshape2)
library(stringr)
library(ggplot2)
library(ggrepel)
library(RColorBrewer)
library(vegan)
library(readxl)
library(grid)
library(gtable)
library(scales)
library(gridExtra)
library(extrafont)

#-------------------------------------------------------------------------
## Twitter followers analysis

## load NUVI data
d <- read.csv(file.path(PROJHOME,"sci-twitter","data",
                        "output - all.data_7Dec2016.csv"),
              header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
names(d)
d$handle <- tolower(d$handle)
unique(d$handle)

#summaries of 64,666 followers
names(d)
decision.makers <- d %>% 
  filter(politician == 1) %>% 
  distinct()
head(decision.makers)
nrow(decision.makers)

191/64666

decision.makers <- d %>% 
  filter(politician == 1) %>% 
  distinct()
head(decision.makers)
nrow(decision.makers)

191/64666

#rename some NUVI typos.. so they will match into scis110
unique(d$handle)
d$handle <- recode_factor(d$handle, 
                          "@ericsotka" = "@eriksotka", 
                          "@thelibalib" = "@thelibalab", 
                          "@ejmilnesgulland" = "@ejmilnergulland", 
                          "@johnrhutchison"= "@johnrhutchinson", 
                          "@bjeuquist"= "@bjenquist", 
                          "@micheljikaiser" = "@micheljkaiser", 
                          "@jelmcglothlin" = "@joelmcglothlin")

head(d)

#manuscript summaries
length(unique(d$Username)) #64,666 followers


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
# 
# filter(scis110, Followers > 5000)
# 
# head(scis110)
# hist(scis110$Months.total)
# hist(scis110$Year.joined)
# 

head(d)
reach.110 <- d %>% 
    group_by(handle) %>% 
    summarize(median.reach = median(Reach, na.rm = TRUE),
    sum.reach = sum(Reach, na.rm = TRUE),
    max.reach = max(Reach, na.rm = TRUE))

hist(reach.110$max.reach)
summary(reach.110$max.reach)

#d2 - reach by 64,666 handles
names(d)
d2 <- d %>% 
  group_by(Username) %>% 
  summarize(median.reach = median(Reach, na.rm = TRUE),
            sum.reach = sum(Reach, na.rm = TRUE),
            max.reach = max(Reach, na.rm = TRUE))

head(d2)
summary(d2)
nrow(d2)

hist(d2$max.reach)
summary(d2$max.reach); sd(d2$max.reach) #0 to 6,716,665

test <- filter(d2, max.reach > 5000000)

#Supplementary Figure 1
ggplot(data = d2, aes(x = max.reach)) + 
  geom_histogram() + 
  scale_x_log10()

#Next, I've calculated the average reach for the followers classified to each group. 
#1. summarize groups for each handle, from d

names(d)
d2 <- d %>% 
  select(c(2,4:5,20,7:17)) %>% 
  melt(id.vars = c(1:4), variable.name = "group") %>% 
  filter(value > 0) #filter only where groups are identified

head(d2)
head(d2[which(is.na(d2$group)),]) #all followers identified to a group - good :) 

#64,666 unique followers
length(unique(d2$Username))

#remove foreign publics, unlikely to be properly classified
#some foreign scientists already well classified :) 
d2 <- d2[-which(d2$foreign == 1 & d2$group == "public"),]
length(unique(d2$Username))
62958/64666
64666-62958
1708/64666

table(d2$group)
192/64666 #politicians / decision makers

head(scis110)

#62,958 unique followers once foreign public (unclassified) profiles are removed
length(unique(d2$Username)) 


#recode outreach (educators + mza)
d2$group <- recode_factor(d2$group, 
                          mza = "outreach")
levels(as.factor(d2$group))

d2$group <- factor(d2$group, levels = c("faculty", "student",
                                        "other.sci", "sci.assoc",
                                        "outreach","applied", 
                                        "media", "public","politician",
                                        "unknown"))

#combine scientists group
d2$group2 <- recode_factor(d2$group,
                           faculty = "scientists",
                           student = "scientists",
                           other.sci = "scientists",
                           sci.assoc = "scientists")
levels(as.factor(d2$group2))

#recode for outputs
d2$group <- recode_factor(d2$group, 
                          faculty = "Science faculty",
                          student = "Science students",
                          'other.sci' = "Other scientists",
                          'sci.assoc' = "Science associations",
                          outreach = "Outreach",
                          applied = "Applied",
                          media = "Media",
                          public = "Public",
                          politician = "Decision makers",
                          unknown = "Unknown")
levels(as.factor(d2$group))

d2$group2 <- recode_factor(d2$group2, 
                           scientists = "Scientists",
                           outreach = "Outreach",
                           applied = "Applied",
                           media = "Media",
                           public = "Public",
                           politician = "Decision makers",
                           unknown = "Unknown")
levels(as.factor(d2$group2))

## how many followers have more than one follower type? 
head(d2)

type.count <- d2 %>% 
  filter(group != "Unknown") %>% 
  group_by(Username, group) %>% 
  tally() %>% 
  ungroup() %>% 
  group_by(Username) %>% 
  summarize(n.group = n()) %>% 
  arrange(desc(n.group))

head(type.count)
nrow(type.count)

test <- filter(type.count, n.group >1)
nrow(test) #1569 have multiple types
1569 / 51625

## ==================================================
#boxplots by group
## ==================================================

##START HERE - STUCK
head(d2)
length(unique(d2$Username))

d3 <- d2 %>% 
  group_by(handle) %>% 
  mutate(n_followers = n()) %>% 
  group_by(handle,group) %>% 
  summarize(n_group = n(),
            n_followers = mean(n_followers),
            prop_followers = n_group / n_followers,
            mean_reach = median(Reach),
            max_reach = max(Reach))

head(d3)

#boxplots by group - all scientists
# The palette with black:
levels(as.factor(d3$group))

group.colours <- c("#E69F00","#E69F00","#E69F00","#E69F00",
                   "darkorchid1", "#009E73",
                   "dodgerblue","deepskyblue",
                   "navyblue","#999999")

group.colours.orig <- c("#0072B2","#0072B2","#0072B2","#0072B2",
                        "#56B4E9", "#009E73","#CC79A7",
                        "#D55E00","#E69F00",
                        "#999999")

unique(d3$group)
head(d3)

#boxplots by group
head(d3)
str(d3)


head(mpg)
ggplot(data = mpg,
       aes(x = class, y = hwy)) + 
  geom_boxplot()

unique(mpg$class)
filter(mpg, class == "compact")


unique(d3$group)
str(d3)
is.data.frame(d3)

filter(d3, class == "Science faculty")

ggplot(data = d3,
       aes(x = as.factor(group), y = as.numeric(mean_reach))) + 
  geom_boxplot()

group.a <- ggplot(data = filter(d3,group != "Unknown"),
                  aes(x = reorder(group, prop_followers), y = mean_reach)) +
  geom_boxplot(aes(colour = group)) + 
  scale_colour_manual(values = group.colours) +
  scale_y_log10(labels = comma,
                breaks = c(10,100,1000,10000,100000,1000000)) +
  theme_bw(base_size = 12) + 
  theme(panel.grid = element_blank(),
        legend.position = "none",
        text=element_text(family="Times"),
        axis.text.x = element_text(angle = 45, hjust=1, vjust=1),
        axis.title.x = element_blank()) + 
  ylab("Median Twitter reach")
group.a

#mean reach by group, averaged across 110 handles
group.b <- ggplot(data = filter(d3,group != "Unknown"), 
                  aes(x = reorder(group, prop_followers), y = prop_followers)) +
  geom_boxplot(aes(colour = group)) + 
  scale_colour_manual(values = group.colours) +
  scale_y_continuous(limits = c(0,0.45)) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill = "transparent"),
        legend.position = "none",
        text=element_text(family="Times"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) + 
  ylab("Mean proportion of followers")
group.b

g <- arrangeGrob(rbind(ggplotGrob(group.b), ggplotGrob(group.a), size = "last"))
ggsave(file.path(PROJHOME,"paper","figures-tables", "outputs",
                 "FigSX - boxplot proportion, reach x group.pdf"), g,
       height = 6, width = 6)


## ==================================================
#boxplots by group -  scientists grouped
## ==================================================
head(d2)
d4 <- d2 %>% 
  group_by(handle) %>% 
  mutate(n_followers = n()) %>% 
  group_by(handle,group2) %>% 
  summarize(n_group = n(),
            n_followers = mean(n_followers),
            prop_followers = n_group / n_followers,
            mean_reach = median(Reach))

head(d4)

levels(as.factor(d4$group2))
group2.colours <- c("#E69F00", "darkorchid1", "#009E73",
                    "dodgerblue","deepskyblue","navyblue",
                    "#999999")

group2.colours.orig <- c("#0072B2", "#56B4E9", "#009E73",
                         "#CC79A7","#D55E00","#E69F00",
                         "#999999")

#create dummy facet of prop followers and reach 
head(d4)

group2.a <- ggplot(data = filter(d4, group2 != "Unknown"), 
                   aes(x = reorder(group2, -prop_followers), y = mean_reach)) +
  geom_boxplot(aes(colour = group2)) + 
  scale_colour_manual(values = group2.colours) + 
  scale_y_log10(labels = comma,
                breaks = c(10,100,1000,10000,100000,1000000)) +
  theme_bw(base_size = 18) + 
  theme(panel.grid = element_blank(),
        legend.position = "none",
        text=element_text(family="Times"),
        axis.text.x = element_text(size = 18, angle = 45, hjust=1, vjust=1),
        axis.title.x = element_blank()) + 
  ylab("Median Twitter reach")

#mean reach by group, averaged across 110 handles
group2.b <- ggplot(data = filter(d4, group2 != "Unknown"), 
                   aes(x = reorder(group2, -prop_followers), y = prop_followers)) +
  geom_boxplot(aes(colour = group2)) + 
  scale_colour_manual(values = group2.colours) +
  scale_y_continuous(limits = c(0,0.8)) + 
  theme_bw(base_size = 18) +
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill = "transparent"),
        legend.position = "none",
        text=element_text(family="Times"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),) + 
  ylab("Mean proportion of followers") 


g <- arrangeGrob(rbind(ggplotGrob(group2.b), ggplotGrob(group2.a), size = "last"))
ggsave(file.path(PROJHOME,"paper","figures-tables", "outputs",
                 "Fig3 - boxplot proportion, reach x group.pdf"), g,
       height = 8, width = 6)


#histograms of reach by group
unique(d2$group)
ggplot(data = dplyr::filter(d2, group != "Unknown")) + 
  geom_histogram(aes(x = Reach, fill = group), colour = "black",
                 boundary = 0) + 
  scale_x_log10(labels = comma,
                breaks = c(10,100,1000,10000,100000,1000000)) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill = "transparent"),
        axis.text.x = element_text(angle = 45, hjust=1, vjust=1),
        legend.position = "none") + 
  facet_wrap(~group, ncol = 3, scales = "free_y") + 
  scale_fill_manual(values = group.colours) +
  ylab("Frequency") + 
  xlab("Twitter reach")

ggsave(file.path(PROJHOME,"paper","figures-tables","outputs",
                 "reach x group log-histogram.pdf"),
       height = 6, width = 8)

#ANOVA of reach by groups
head(d4)
unique(d4$group2)

data <- dplyr::filter(d4, group2 != "Unknown")
model <- lm(data$mean_reach ~ data$group2)
anova(model)
oneway.test(data$mean_reach ~ data$group2)

kruskal.test(data$mean_reach ~ data$group2)
TukeyHSD(aov(data$mean_reach ~ data$group2))

model <- lm(data$prop_followers ~ data$group2)
anova(model)
oneway.test(data$prop_followers ~ data$group2)

kruskal.test(data$prop_followers ~ data$group2)
TukeyHSD(aov(data$prop_followers ~ data$group2))
                                                                                                           ## ==================================================
head(d2)
length(unique(d2$Username))
?anova


## ==================================================
# Fig 2 - follower accumulation plots -  scientists subgroups
## ==================================================
d5 <- d2 %>% 
  group_by(handle,group) %>% 
  summarize(n_group = n(),
            mean_reach = median(Reach),
            max_reach = max(Reach))

d5 <- left_join(scis110[,c(2:4)], d5)
head(d5)

levels(as.factor(d5$group))

#all levels on one plot
ggplot(data = filter(d5, group != "Unknown"), 
       aes(x = Followers, y = n_group)) + 
  stat_smooth(aes(colour = group), se = FALSE) + 
  geom_point(shape = 21, 
             aes(colour = group, fill = group, size = max_reach), 
             alpha = 0.5) + 
  theme_bw(base_size = 18) + 
  scale_x_log10(breaks = c(10,100,500,1000,2500,5000,10000)) +
  scale_y_continuous(breaks = c(0,1000,2000)) +
  scale_colour_manual("Group", values = group.colours) +
  scale_fill_manual("Group", values = group.colours) +
  theme(panel.grid = element_blank(),
        text = element_text(family="Times"), 
        legend.key = element_blank(), #se = true is setting grey background of legend
        legend.background = element_blank()) + 
  ylab("No. of followers by type") +
  xlab("Total followers") +
  scale_size_continuous("Maximum \nreach", labels = comma)

ggsave(file.path(PROJHOME,"paper","figures-tables", "outputs",
                 "follower increase x group.pdf"),
       height = 5.5, width = 10)

#multi plot of follower increase by group
ggplot(data = filter(d5, group != "Unknown"), 
       aes(x = Followers, y = n_group)) + 
  stat_smooth(aes(colour = group), se = FALSE) + 
  geom_point(shape = 21, aes(colour = group, size = max_reach), 
             fill = "white", alpha = 0.5) + 
  theme_bw(base_size = 10) + 
  scale_x_log10(breaks = c(100,1000,10000)) + 
  scale_colour_manual("Group", values = group.colours) +
  facet_wrap(~ group, scales = "free_y") + 
  theme(panel.grid = element_blank(),
        text=element_text(family="Times"), 
        legend.position = "none") + 
  ylab("No. of group followers") +
  xlab("Twitter followers") +
  scale_size_continuous("Maximum \nreach", labels = comma)

ggsave(file.path(PROJHOME,"paper","figures-tables", "outputs",
                 "multiplot - follower increase x group.pdf"),
       height = 3.5, width = 5.5)

## ==================================================
# follower accumulation plots -  scientists combined (group2)
## ==================================================
head(d2)
d6 <- d2 %>% 
  group_by(handle,group2) %>% 
  summarize(n_group2 = n(),
            mean_reach = median(Reach),
            max_reach = max(Reach))

d6 <- left_join(scis110[,c(2:4)], d6)
head(d6)

levels(as.factor(d6$group2))

#all levels on one plot
head(d6)
ggplot(data = filter(d6, group2 != "Unknown"), 
       aes(x = Followers, y = n_group2)) + 
  stat_smooth(aes(colour = group2), se = FALSE) + 
  geom_point(shape = 21, 
             aes(colour = group2, fill = group2, size = max_reach), 
              alpha = 0.5) + 
  theme_bw(base_size = 18) + 
  scale_x_log10(breaks = c(10,100,500,1000,2500,5000,10000)) + 
  scale_colour_manual("Group", values = group2.colours) +
  scale_fill_manual("Group", values = group2.colours) +
  theme(panel.grid = element_blank(),
        text=element_text(family="Times")) + 
  ylab("No. of group followers") +
  xlab("Total followers") +
  scale_size_continuous("Maximum reach", labels = comma)

ggsave(file.path(PROJHOME,"paper","figures-tables", "outputs",
                 "Fig2 - follower increase x group2.pdf"),
       height = 6, width = 10)

#multi plot of follower increase by group2
ggplot(data = filter(d6, group2 != "Unknown"), 
       aes(x = Followers, y = n_group2)) + 
  stat_smooth(aes(colour = group2), se = FALSE) + 
  geom_point(shape = 21, aes(colour = group2, size = max_reach), 
             fill = "white", alpha = 0.5) + 
  theme_bw(base_size = 16) + 
  scale_x_log10(breaks = c(100,1000,10000)) + 
  scale_colour_manual("Group", values = group2.colours) +
  facet_wrap(~ group2, scales = "free_y") + 
  theme(panel.grid = element_blank(),
        text=element_text(family="Times"),
        legend.position = "none",
        axis.title = element_blank()) + 
  ylab("No. ofgroup followers") +
  xlab("Total followers") +
  scale_size_continuous("Maximum \nreach", labels = comma)

ggsave(file.path(PROJHOME,"paper","figures-tables", "outputs",
                 "multiplot - follower increase x group2.pdf"),
       height = 3.5, width = 6.5)

## does maximum reach (e.g., high quality followers) increase with followers? 

## MODELS
#fit reach x group
#public and media significantly accrue higher quality reaches with more followers
library(nlme)
head(d6)
m0 <- gls(max_reach ~ Followers * group2,
          data = d6,
          na.action = na.exclude)
summary(m0)
anova(m0)

ggplot(data = filter(d6, group2 != "Unknown"), 
       aes(x = Followers, y = max_reach)) + 
  geom_point(shape = 21, aes(colour = group2, size = max_reach), 
             fill = "white", alpha = 0.5) + 
  stat_smooth(aes(colour = group2), se = FALSE, method = "lm") + 
  theme_bw(base_size = 10) + 
  scale_x_log10(breaks = c(10,100,500,1000,2500,5000,10000)) + 
  scale_y_continuous(labels = comma) +
  scale_colour_manual("Group", values = group2.colours) +
  theme(panel.grid = element_blank()) + 
  ylab("Maximum reach") +
  xlab("log10 Twitter followers") +
  scale_size_continuous("Maximum \nreach", labels = comma)


# ggsave(file.path(PROJHOME,"paper","outputs",
#                  "model - reach x followers increase.pdf"),
#        height = 5, width = 6.5)      

