
#load DATA SOURCE
library(here)
source(here("r", "SOURCE_ANALYSIS.R"))

head(d) #original data 
head(faculty.all.followers) #110 faculty with all their followers
head(followers) #follower information -- removed foreign, melted data
head(scis110) #scis110 faculty profile information
head(d2) #faculty with classified followers for analysis


#----------------------------------------------------------------------------------
#STRUCCHANGE PACKAGE, Bayes criteria for breakpoints in slope
library(strucchange)

head(d2)
head(scis110)

d3 <- left_join(d2, scis110[,c(3,5)]) %>% 
  group_by(handle, group2, Followers) %>% 
  summarize(max.Reach = max(Reach), 
            n.type = n()) %>% 
  ungroup()
head(d3)

#Scientists
scientists <- d3 %>% 
  filter(group2 == "Scientists") %>% 
  arrange(Followers)
head(scientists)

bp.scis <- breakpoints(scientists$n.type ~ scientists$Followers)
bp.scis
scientists[89,"Followers"] #1916 Followers

ci.nile <- confint(bp.scis)
ci.nile
scientists[62,"Followers"] #735 Followers
scientists[90,"Followers"] #1954 Followers


#order dataframe like a time series by Followers
d3 <- d3 %>% 
  arrange(group2, Followers)
unique(d3$group2)
head(d3)

#make results list of breakpoints and confint
result_list <- list()

for(i in levels(d3$group2)){
  print(i)
  data = d3[which(d3$group2 == i),]
  bp = breakpoints(data$n.type ~ 1)
  result_list[[i]] <- as.data.frame(confint(bp)[1])[1,]
}
result_final <- do.call(rbind, result_list)
names(result_final) <- c("confint25", "breakpoint", "confint975")
result_final

#make matrix that pulls values from dataset
result_list2 <- matrix(NA, nrow = 6, ncol = 3)
row.names(result_list2) <- levels(d3$group2)
result_list2

head(d3)
head(result_final)

data = d3[which(d3$group2 == "Scientists"),]
data[result_final["Scientists","confint25"],"Followers"]
value1 <- 374
value2 <- 450
value3 <- 546
result_list2["Scientists",] <- c(value1, value2, value3)

for(i in levels(d3$group2)) {
  data = d3[which(d3$group2 == i),]
  value1 = data[result_final[i,"breakpoint"],"Followers"]
  value2 = data[result_final[i,"confint25"],"Followers"]
  value3 = data[result_final[i,"confint975"],"Followers"]
  print(c(i, value1, value2, value3))
  #result_list2[i,] <- c(value1, value2, value3)
}

##STUCK 
##Can't figure out how to put back into result_list? 
##Used print to confirm Table 2..


result_list2[i,]

result_list2 <- as.data.frame(result_list2)
names(result_list2) <- c("Followers.25", "Followers.breakpoint", "Followers.975")
result_list2

result_final <- cbind(result_final, result_list2)
result_final

write.csv(result_final, 
          file.path(PROJHOME, "paper", "figures-tables", "outputs",
                    "breakpoint strucchange table.csv"), 
          row.names = TRUE)


