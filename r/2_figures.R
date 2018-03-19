
#load DATA SOURCE
library(here)
source(here("r", "SOURCE_ANALYSIS.R"))

head(d) #original data 
head(faculty.all.followers) #110 faculty with all their followers
head(followers) #follower information -- removed foreign, melted data
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
unique(d2$group2) #Unknown removed 
length(unique(d2$Username)) #51,625 profiles

#calculate some summary statistics by reach
head(followers) #use followers because otherwise pseudoreplication within handles

d2.reach <- followers %>% 
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

#----------------------------------------------------------------------------------
#Fig 2 - differences in reach by group
head(followers)
length(unique(followers$Username))

## FIG 2a -----------------------------------------------------------------------------
#proportion followers by group (n = 110 academics)
head(d2)
levels(as.factor(d2$group2))

prop.followers <- d2 %>% 
  group_by(handle) %>% 
  mutate(total.followers = n()) %>% 
  arrange(id) %>%
  ungroup() %>% 
  group_by(id, handle, group2) %>% 
  summarize(sum.type = sum(n), 
            total.followers = mean(total.followers), 
            prop.type = sum.type / total.followers) %>% 
  ungroup()

prop.followers.summary <- prop.followers %>% 
  group_by(group2) %>% 
  summarize(mean.prop = round(mean(prop.type),4))
prop.followers.summary
sum(prop.followers.summary[1,2])

levels(as.factor(prop.followers$group2))

#set colour palette
group2.colours <- c("deepskyblue", "darkorange1", "#009E73",
                    "dodgerblue","darkorchid1","navyblue")

group2.colours.orig <- c("#0072B2", "#56B4E9", "#009E73",
                         "#CC79A7","#D55E00","#E69F00")

# Colourblind friendly paletter
# From Rcookbook, The palette with black:
cbbPalette <- c("#56B4E9", "#0072B2","#009E73", 
                "#E69F00", "#D55E00", "#CC79A7")

#oneway ANOVA
oneway.test(prop.followers$prop.type ~ prop.followers$group2)

#ggplot Fig 2a
ggplot(data = prop.followers,
       aes(x = reorder(group2, -prop.type, median), y = prop.type)) + 
  geom_violin(trim = FALSE,
              adjust = .75, 
              #draw_quantiles = c(0.25, 0.5, 0.75), 
              draw_quantiles = 0.5, 
              scale = "width", 
              aes(fill = group2)) + 
  geom_point(alpha = 0.25, shape = 21, colour = "black",
             aes(fill = group2)) + 
  scale_fill_manual(values = cbbPalette) +
  theme_bw(base_size = 20) + 
  theme(panel.grid = element_blank(),
        legend.position = "none",
        text=element_text(family="Times"),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) + 
  ylab("Proportion of followers")
p1 <- last_plot()

## FIG 2b -----------------------------------------------------------------------------
#reach by group, n = 51,625 followers
length(unique(followers$Username))
levels(as.factor(followers$group2))

#oneway ANOVA
oneway.test(followers$Reach ~ followers$group2)
kruskal.test(followers$Reach ~ followers$group2)
TukeyHSD(aov(followers$Reach ~ followers$group2))

#ggplot
ggplot(data = followers,
       aes(x = group2, y = Reach)) + 
  geom_violin(trim = FALSE,
              adjust = .75, 
              #draw_quantiles = c(0.25, 0.5, 0.75), 
              draw_quantiles = 0.5, 
              scale = "width", 
              aes(fill = group2)) + 
  geom_point(alpha = 0.25, shape = 21, colour = "black",
             aes(fill = group2)) + 
  scale_fill_manual(values = cbbPalette) +
  scale_y_log10(labels = comma,
                 breaks = c(0,10,100,1000,10000,100000,1000000)) +
  theme_bw(base_size = 20) + 
  theme(panel.grid = element_blank(),
        legend.position = "none",
        text=element_text(family="Times"),
        axis.text.x = element_text(size = 20, angle = 45, hjust=1, vjust=1),
        axis.title.x = element_blank()) + 
  ylab("Twitter reach")
p2 <- last_plot()

g <- arrangeGrob(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))

ggsave(file.path(PROJHOME,"paper","figures-tables", "outputs",
                 "Fig2.pdf"), g,
       height = 9, width = 8)


## ==================================================
# Fig 3 - follower accumulation plots 
head(d2)
head(scis110)

d3 <- left_join(d2, scis110[,c(3,5)]) %>% 
  group_by(handle, group2, Followers) %>% 
  summarize(max.Reach = max(Reach), 
            n.type = n()) %>% 
  ungroup()
head(d3)

#ggplot
ggplot(data = d3, aes(x = Followers, y = n.type), group = group2) + 
  # stat_smooth(aes(colour = group2),
  #             method = "lm", fullrange = TRUE,
  #             formula = y ~ poly(x, 2)) +
  stat_smooth(aes(colour = group2, linetype = group2),
              se = FALSE, fullrange = TRUE) + #method = LOESS
  geom_point(shape = 21, 
             aes(colour = group2, fill = group2, size = max.Reach), 
             alpha = 0.5) + 
  theme_bw(base_size = 18) + 
  scale_x_log10(breaks = c(10,100,500,1000,2500,5000,10000)) +
  scale_y_continuous(breaks = c(0,1000,2000,3000)) +
  scale_colour_manual("Type", values = cbbPalette) +
  scale_linetype_manual("Type", values = c("solid", "dashed",
                                           "dotted", "dotdash",
                                           "longdash", "twodash")) +
  scale_fill_manual("Type", values = cbbPalette) +
  theme(panel.grid = element_blank(),
        text = element_text(family="Times"), 
        legend.key = element_blank(), #se = true is setting grey background of legend
        legend.background = element_blank(), 
        legend.position = "right") + 
  ylab("No. of followers by type") +
  xlab("Total followers") +
  scale_size("Maximum \nreach", labels = comma, 
                        range = c(2,10))

ggsave(file.path(PROJHOME,"paper","figures-tables", "outputs",
                 "Fig 3 follower x group.pdf"),
       height = 8, width = 12)

#INSET of follower increase by group -------------------------------------------
ggplot(data = d3, aes(x = Followers, y = n.type), group = group2) + 
  # stat_smooth(aes(colour = group2),
  #             method = "lm", fullrange = TRUE,
  #             formula = y ~ poly(x, 2)) +
  geom_point(shape = 21, 
             aes(colour = group2, fill = group2, size = max.Reach), 
             alpha = 0.15) +
  stat_smooth(aes(colour = group2, linetype = group2),
              se = FALSE, fullrange = TRUE) + #method = LOESS
  theme_bw(base_size = 12) + 
  scale_x_log10(breaks = c(10,100,1000,5000)) +
  scale_colour_manual("Type", values = cbbPalette) +
  scale_fill_manual("Type", values = cbbPalette) +
  scale_linetype_manual("Type", values = c("solid", "dashed",
                                           "dotted", "dotdash",
                                           "longdash", "twodash")) +
  theme(panel.grid = element_blank(),
        text = element_text(family="Times"), 
        legend.key = element_blank(), #se = true is setting grey background of legend
        legend.background = element_blank(), 
        legend.position = "none", 
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) + 
  ylab("No. of followers by type") +
  xlab("Total followers") +
  scale_size("Maximum \nreach", labels = comma, 
             range = c(2,10)) + 
  facet_wrap(~ group2, scales = "free_y") + 

ggsave(file.path(PROJHOME,"paper","figures-tables", "outputs",
                 "Fig 3 INSET - follower increase x group.pdf"),
       height = 3.5, width = 6)

