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

#rename some NUVI typos..? 
unique(d$handle)
d$handle <- recode_factor(d$handle, 
                          "@ericsotka" = "@eriksotka", 
                          "@thelibalib" = "@thelibalab", 
                          "@ejmilnesgulland" = "@ejmilnergulland", 
                          "@johnrhutchison"= "@johnrhutchinson", 
                          "@bjeuquist"= "@bjenquist", 
                          "@micheljikaiser" = "@micheljkaiser", 
                          "@jelmcglothlin" = "@joelmcglothlin")


#load 110 users data
scis110 <- read.csv(file.path(PROJHOME,"sci-twitter","data",
                              "110 handles_with Twitter dates.csv"),
                    header = TRUE, strip.white = TRUE, stringsAsFactors = FALSE)
names(scis110)[2] <- "handle"
#puttolower to match data analysis
scis110$handle <- tolower(scis110$handle)

# 
# filter(scis110, Followers > 5000)
# 
# head(scis110)
# hist(scis110$Months.total)
# hist(scis110$Year.joined)
# 

d2 <- d %>% 
    group_by(handle) %>% 
    summarize(median.reach = median(Reach, na.rm = TRUE),
    sum.reach = sum(Reach, na.rm = TRUE),
    max.reach = max(Reach, na.rm = TRUE))
head(d2)
summary(d2)

levels(as.factor(d2$handle))

#check big reach numbers, yep, makes sense with 110 followers_long.xlsx
#and summarized in new SOM table
#filter(d, Reach > 4000000)
scis110 <- left_join(scis110, d2)
head(scis110)
names(scis110)

#no reach should be NA - otherwise joining typos
test <- filter(scis110, is.na(median.reach))
names(scis110)

reach.melt <- melt(scis110[,c(2,4,13,17:19)], id.vars = 1:3)
head(reach.melt)
summary(reach.melt)


#plot of followers x total reach of Twitter academic
reach.melt$variable <- factor(reach.melt$variable, 
                              levels = c("median.reach",
                                         "max.reach",
                                         "sum.reach"))

reach.melt$variable <- recode_factor(reach.melt$variable, 
median.reach = "Median reach",
max.reach = "Maximum reach",
sum.reach = "Cumulative reach")

facet_names <- c(`Median reach` = "Median reach (average quality)",
                 `Maximum reach` = "Maximum reach (highest quality)",
                 `Cumulative reach` = "Cumulative reach (sum of quality)")

#plot - followers by reach
head(reach.melt)
ggplot(aes(x = Followers, y = value), data = reach.melt) + 
      geom_point(size = 3, alpha = 0.75) + 
      theme_bw(base_size = 14) + 
      scale_x_log10(breaks = c(0,10,25,50,100,250,500,750,1000,
      1500,2500,3500,5000,7500,10000)) +
      scale_y_continuous(labels = comma) +
      ylab("Twitter reach (follower's followers)") + 
      xlab("No. of followers") +
      stat_smooth(colour = "black", se = TRUE, fill = "grey50") + 
      theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1),
      text = element_text(size = 12),
      panel.grid = element_blank()) +
      facet_wrap(~variable, ncol = 1, scales = "free_y", 
      labeller = as_labeller(facet_names))

ggsave(file.path(PROJHOME,"paper","figures-tables", 
                 "outputs",
                 "followers x reach.pdf"),
       height = 8, width = 5.5)

#histogram of followers
head(scis110)

ggplot(data = scis110) + 
      geom_histogram(aes(x = Followers), colour = "black", fill = "grey60",
      boundary = 0) + 
      scale_x_log10(breaks = c(0,10,25,50,100,250,500,750,1000,
      1500,2500,3500,5000,7500,10000)) +
      scale_y_continuous("Frequency", limits = c(0,13), expand = c(0,0),
      breaks = c(0,5,10)) +
      theme_bw(base_size = 20) +
      theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1),
      axis.ticks.x = element_blank(),
      text = element_text(size = 12),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "transparent")) + 
      annotate("text", label = "n = 110 academics", x = 3000, y = 12)

ggsave(file.path(PROJHOME,"paper","figures-tables", "outputs",
    "followers log-histogram.pdf"),
    height = 3.15, width = 5.12)

#histogram of reach of followers
head(d)
d.reach <- d %>% 
    group_by(Username) %>% 
    summarize(Reach = mean(Reach))
    min(d.reach$Reach); max(d.reach$Reach)
    median(d.reach$Reach); sd(d.reach$Reach)

ggplot(data = d.reach) + 
    geom_histogram(aes(x = Reach), colour = "black", fill = "grey60",
    boundary = 0) + 
    scale_x_log10(labels = comma,
    breaks = c(10,100,1000,10000,100000,1000000)) +
    scale_y_continuous("Frequency", limits = c(0,9000), expand = c(0,0)) + 
    theme_bw(base_size = 14) +
    theme(panel.grid = element_blank(),
    plot.background = element_rect(fill = "transparent")) + 
    annotate("text", label = "n = 64,666 followers", x = 900000, y = 7500)

ggsave(file.path(PROJHOME,"paper","figures-tables", "outputs",
    "reach log-histogram.pdf"),
    height = 3.15, width = 10)

#consider time and followers / reach
#GO BACK TO MARKDOWN FILE 

#Next, I've calculated the average reach for the followers classified to each group. 
#1. summarize groups for each handle, from d

names(d)
d2 <- d %>% 
  select(c(2,4:5,20,7:17)) %>% 
  melt(id.vars = c(1:4), variable.name = "group") %>% 
  filter(value > 0)

head(d2)
head(d2[which(is.na(d2$group)),])

#64,666 unique followers
length(unique(d2$Username))

#remove foreign publics, unlikely to be properly classified
d2 <- d2[-which(d2$foreign == 1 & d2$group == "public"),]
length(unique(d2$Username))
62958/64666

table(d2$group)

#62,958 unique followers once foreign public (unclassified) profiles are removed
length(unique(d2$Username)) 

#plot reach histograms for each group
head(d2)

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

## ==================================================
#boxplots by group
## ==================================================
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
                   "yellow", "#009E73",
                   "dodgerblue","deepskyblue",
                   "navyblue","#999999")

group.colours.orig <- c("#0072B2","#0072B2","#0072B2","#0072B2",
                        "#56B4E9", "#009E73","#CC79A7",
                        "#D55E00","#E69F00",
                        "#999999")

unique(d3$group)
head(d3)
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
group2.colours <- c("#E69F00", "yellow", "#009E73",
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
kruskal.test(data$mean_reach ~ data$group2)
TukeyHSD(aov(data$mean_reach ~ data$group2))

model <- lm(data$prop_followers ~ data$group2)
anova(model)
kruskal.test(data$prop_followers ~ data$group2)
TukeyHSD(aov(data$prop_followers ~ data$group2))
                                                                                                           ## ==================================================
head(d2)
length(unique(d2$Username))



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
  ylab("No. of group followers") +
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

