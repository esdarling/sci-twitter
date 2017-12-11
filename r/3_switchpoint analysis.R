library(tidyverse)
library(reshape2)
library(stringr)
library(extrafont)
library(segmented)
library(strucchange)
library(broom)

#install.packages("cldr")
citation("strucchange")
citation("stringr")

citation()

#-------------------------------------------------------------------------
## load Twitter data
source(file.path(PROJHOME, "sci-twitter", "r", "2_figures.R"))

head(d6)
unique(d6$handle) #handle are 110 scientists

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

#-------------------------------------------------------------------------
## switchpoint analysis by Twitter followers
head(d6)

d6.scientists <- d6 %>% 
  filter(group2 == "Scientists")

my.lm <- lm(n_group2 ~ Followers, data = d6.scientists)
summary(my.lm)

#STRUCCHANGE PACKAGE, Bayes criteria for breakpoints in slope
ans <- breakpoints(n_group2 ~ Followers, data = d6.scientists)
ans$breakpoints

##START HERE
#breakpoints take a time-series approach and looks for breaks in the time series
#here, time is a 'space for time approach by followere'
head(d6.scientists)

#Scientists
d6.scientists <- d6.scientists %>% 
  arrange(Followers)
bp.scis <- breakpoints(d6.scientists$n_group2 ~ d6.scientists$Followers)
bp.scis
d6.scientists[89,"Followers"] #1916 Followers

ci.nile <- confint(bp.scis)
ci.nile
d6.scientists[61,"Followers"] #735 Followers
d6.scientists[90,"Followers"] #1954 Followers

#Media
unique(d6$group2)
d6.applied <- d6 %>% 
  filter(group2 == "Applied") %>% 
  arrange(Followers)

head(d6.applied)
bp <- breakpoints(d6.applied$n_group2 ~ d6.applied$Followers)
bp
d6.applied[88,"Followers"] #2201 Followers

confint(bp)
breakdates(bp, format.times = TRUE)
breakfactor(bp)

ci.nile <- confint(bp)
ci.nile
d6.applied[65,"Followers"] #961 Followers
d6.applied[89,"Followers"] #2314 Followers

#order dataframe like a time series by Followers
d6 <- d6 %>% 
  arrange(group2, Followers)
unique(d6$group2)
head(d6)

#make results list of breakpoints and confint
result_list <- list()

for(i in levels(d6$group2)){
  print(i)
  data = d6[which(d6$group2 == i),]
  bp = breakpoints(data$n_group2 ~ 1)
  result_list[[i]] <- as.data.frame(confint(bp)[1])[1,]
}
result_final <- do.call(rbind, result_list)
names(result_final) <- c("confint25", "breakpoint", "confint975")
result_final

# data = d6[which(d6$group2 == "Outreach"),]
# is.numeric(data$n_group2)
# breakpoints(data$n_group2 ~ 1)
# 
# data = d6[which(d6$group2 == "Media"),]
# is.numeric(data$n_group2)
# breakpoints(data$n_group2 ~ 1)

result_list2 <- matrix(NA, nrow = 7, ncol = 3)
row.names(result_list2) <- levels(d6$group2)
result_list2

for(i in levels(d6$group2)) {
  data = d6[which(d6$group2 == i),]
  value1 = data[result_final[i,"confint25"],"Followers"]
  value2 = data[result_final[i,"breakpoint"],"Followers"]
  value3 = data[result_final[i,"confint975"],"Followers"]
  print(c(value1, value2, value3))
  result_list2[i,] <- c(value1, value2, value3)
}
result_list2 <- as.data.frame(result_list2)
names(result_list2) <- c("Followers.25", "Followers.breakpoint", "Followers.975")
result_list2

result_final <- cbind(result_final, result_list2)
result_final

write.csv(result_final, 
          file.path(PROJHOME, "paper", "figures-tables", "outputs",
                    "breakpoint strucchange table.csv"), 
          row.names = TRUE)


#==========================================================================
##1000 vs. 1000+ groups
#NAH - median of groups isn't much of a difference, variability past 1000+ crushed by mean
# head(d6)
# 
# follower.split <- d6 %>% 
#   mutate(split1000 = ifelse(Followers < 1000, "<1000", "1000+")) %>% 
#   group_by(split1000, handle, Followers, group2) %>% 
#   summarize(sum.group.followers = sum(n_group2)) %>% 
#   filter(!group2 == "Unknown")
#   
# #stacked bar plot
# head(follower.split)
# ggplot(data = follower.split, aes(x = split1000)) + 
#   geom_bar(aes(fill = group2), position = "fill")
# 
# test <- follower.split %>% 
#   mutate(prop = sum.group.followers / Followers) %>% 
#   group_by(split1000, group2) %>% 
#   summarize(mean.prop = mean(prop), n = n())
# 
# head(follower.split)
# unique(follower.split$group2)


#==========================================================================
##SEGMENTED PACKAGE
?segmented
my.seg <- segmented(my.lm, 
                    seg.Z = ~Followers, 
                    psi = 2000)
#summary(my.seg)
#slope(my.seg)
my.seg$psi

for(i in levels(d6$group2)){
  print(i)
  my.lm = lm(n_group2 ~ Followers,
             data = d6[which(d6$group2 == i),])
  my.seg <- segmented(my.lm)
  print(confint.segmented(my.seg))
  print(my.seg$psi)
}

head(d6)
nrow(d6[which(d6$group2 == "Scientists"),])

# # get the fitted data
# my.fitted <- fitted(my.seg)
# my.model <- data.frame(Followers = d6.scientists$Followers, 
#                        n_group2 = my.fitted)
# 
# # plot the fitted model
# ggplot(my.model, aes(x = Followers, y = n_group2)) + geom_line()

fitted_models <- d6 %>% 
  group_by(group2) %>% 
  do(model = segmented(lm(n_group2 ~ Followers, data = .),
                       seg.Z = ~Followers, 
                       psi = 2000))

summary(fitted_models)
glimpse(fitted_models$model)


fitted_models$model$coefficients



#then STUCK! how to extract psi? 
