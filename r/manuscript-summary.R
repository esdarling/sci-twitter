
#load DATA SOURCE
library(here)
source(here("r", "SOURCE_ANALYSIS.R"))

head(d.original) #original data 
head(faculty.all.followers) #110 faculty with all their followers
head(followers) #follower informatio -- removed foreign, melted data
head(scis110) #scis110 faculty profile information
head(d2) #faculty with classified followers for analysis

##Manuscript summaries in text

#Number of followers for each faculty
head(faculty.all.followers)

count <- faculty.all.followers %>% 
  group_by(handle) %>% 
  tally()

head(count)
summary(count$n); sd(count$n)

#Reach of faculty
reach.all <- d.original %>% 
  select(Username, Reach) %>% 
  group_by(Username) %>% 
  summarize(Reach = max(Reach, na.rm = TRUE)) %>% 
  distinct()

summary(reach.all$Reach)
length(unique(d.original$Username))

faculty.all.followers <- left_join(faculty.all.followers, reach.all) %>% 
  arrange(id)
head(faculty.all.followers)
summary(faculty.all.followers$Reach)

filter(reach.all, Username == "@_oceanidas")
filter(d.original, Username == "@_oceanidas")

faculty.reach <- faculty.all.followers %>% 
  group_by(handle) %>% 
  summarize(n.followers = n(), 
            median.follower.reach = median(Reach), 
            max.follower.reach = max(Reach), 
            cumulative.follower.reach = sum(Reach)) %>% 
  arrange(n.followers)
faculty.reach
summary(faculty.reach) #good, no NAs in reach now
sd(faculty.reach$cumulative.follower.reach)

filter(faculty.reach, cumulative.follower.reach == 5470)
#@bugdog_scott has the lowest reach 
#he has 10 followers
#his 10 followers have a total reach of 5470 (there are 5470 followers of his followers)

PROJHOME

write.csv(faculty.reach, file.path(PROJHOME, "paper", 
                                   "figures-tables", "outputs", 
                                   "Twitter reach summary for 110 faculty.csv"), 
          row.names = FALSE)

## -------------------------------------------------------------------------------------
#GENDER
table(scis110$Gender)

## -------------------------------------------------------------------------------------
#gender analysis
head(scis110)
names(scis110)

#is there a difference in tweeting activity by gender? 
m0 <- lm(log10(Tweets) ~ Gender * Months.total, data = scis110)
anova(m0)
plot(m0)

q <- residuals(m0)
hist(q)

m0.raw <- lm(Tweets ~ Gender * Months.total, data = scis110)
anova(m0.raw)
plot(m0.raw)

library(stargazer)
test <- anova(m0.raw)
stargazer(as.matrix(test), type = "html")
?stargazer
getwd()

q.raw <- residuals(m0.raw)
hist(q.raw)


#is there a difference in followers by gender? 
m1 <- lm(log10(Followers) ~ Gender * Months.total, data = scis110)
anova(m1)
plot(m1)

m1.raw <- lm(Followers ~ Gender * Months.total, data = scis110)
anova(m1.raw)




## -------------------------------------------------------------------------------------
#POSITION
head(scis110)
table(scis110$Level)
33/110
27/110
50/110

#UNIVERSITIES
unique(scis110$University)
unique(scis110$Country)

country.supp <- scis110 %>% 
  group_by(Country) %>% 
  tally() %>% 
  arrange(desc(n))
country.supp

write.csv(country.supp, 
          file.path(PROJHOME,"paper","figures-tables", "outputs",
                    "Supp table - country by academics.csv"), 
          row.names = FALSE)

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

#SUMMARY For SUPPLEMENTARY TABLE 1
names(scis110)

supp <- scis110 %>% 
  select(handle, Followers, Name, University, Gender, Level) %>% 
  arrange(Followers) %>% 
  select(-c(Followers))
supp

faculty.supp <- left_join(faculty.reach, supp) %>% 
  select(handle, Name:Level, 
         n.followers:cumulative.follower.reach)
head(faculty.supp)

write.csv(faculty.supp, 
          file.path(PROJHOME,"paper","figures-tables", "outputs",
                    "Supp table - 110 scis info.csv"), 
          row.names = FALSE)
