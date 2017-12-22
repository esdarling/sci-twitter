
#load DATA SOURCE
library(here)
source(here("r", "SOURCE_ANALYSIS.R"))

head(d) #original data 
head(faculty.all.followers) #110 faculty with all their followers
head(followers) #follower informatio -- removed foreign, melted data
head(scis110) #scis110 faculty profile information
head(d2) #faculty with classified followers for analysis



#----------------------------------------------------------------------------------
#FIGURES only include followers that are included in analysis
#not foreign 
#public duplicates removed
#unknown group removed
#leaves a total of 51,625 unique followers, each associated with a faculty handle




#FIGURE 2 

#summarize prop. followers and reach statistics by follower type
head(d2)
length(unique(d2$Username))
unique(d2$group2) #Unknown group removed from analysis
length(unique(d2$Username)) #51,625 profiles

nrow(d2) #53,216 rows, includes #1591 duplicated profiles
nrow(d2) - length(unique(d2$Username))

head(d2)


#calculate some summary statistics by reach
head(d2)
d2.reach <- d2 %>% 
  group_by(group2) %>% 
  summarize(n = n(),
            median.reach = median(Reach),
            max.reach = max(Reach)) %>% 
  arrange(desc(n))
d2.reach

write.csv(d2.reach, 
          file.path(PROJHOME,"paper","figures-tables", "outputs",
                    "reach summary by type.csv"), 
          row.names = FALSE)

#filter(d, Username == "@jebyrnes")
sum(d2.reach$n)





#Profiles with more than one group
sum(d2.reach$n) #53,216 total followers
(53216 - 51625) / 51625 * 100




#check follower numbers
sum(d2.reach$n) #53216 
length(unique(d2$Username)) 
64575 - 62958 #1617 is difference - multiple profiles! 

head(d2)


#----------------------------------------------------------------------------------
#Fig 2 - differences in reach by group
oneway.test(d2$Reach ~ d2$group2)

group2.colours <- c("#E69F00", "darkorchid1", "#009E73",
                    "dodgerblue","deepskyblue","navyblue",
                    "#999999")

#create dummy facet of prop followers and reach 
d2.reach.summary <- d2 %>% 
  group_by(group2) %>% 
  summarize(y0 = min(Reach),
            y25 = quantile(Reach, 0.25),
            y50 = median(Reach),
            y75 = quantile(Reach, 0.75),
            y100 = max(Reach))
d2.reach.summary

ggplot(data = d2.reach.summary, aes(x = group2)) +
  geom_boxplot(aes(ymin = y0, lower = y25, 
                   middle = y50, 
                   upper = y75, ymax = y100),
    stat = "identity")

head(d2)


test <- d2[1:5000,]
head(test)
str(test)
ggplot(data = d2[1:5000,],
       aes(x = as.factor(group2), y = as.numeric(Reach))) +
  geom_violin()
  


  
ggplot(data = d2,
       aes(x = group2, y = Reach)) + 
  geom_point(aes(colour = group2)) + 
  geom_boxplot(aes(colour = group2))

hist(d2$Reach)
boxplot(d2$Reach~ d2$group2)


+ 
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










#-------------------------------------------------------------------------
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

#boxplots by group
head(mpg)
ggplot(data = mpg,
       aes(x = class, y = hwy)) + 
  geom_boxplot()

##START HERE
head(d2)
nrow(d2)

d2.distinct <- d2 %>% 
  group_by(group2, Username) %>% 
  summarize(max.reach = max(Reach))
head(d2.distinct)
  
ggplot(data = filter(d2.distinct, group2 != "Unknown"),
       aes(x = reorder(group2, -max.reach, median), y = max.reach)) + 
  geom_boxplot() +
  scale_y_log10(labels = comma,
                breaks = c(10,100,1000,10000,100000,1000000))

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

