
#---
#title: "Calcolate for statistics course in thesis"
#date: "7/3/2022"
#input: A data frame file containing data on colorectal cancer in both Jewish and Arab sectors
#output: Display data on table 1
#---

# Upload packages

install.packages("foreign")
library(foreign)
library(dplyr)
library(stringr)
library(ltm)
library(tidyverse)
# ------- read the file --------


T2 <- read.spss("/Users/odedsabah/Desktop/temp.sav",to.data.frame=TRUE)
cronbach.alpha(T2, CI=TRUE)

select_T2 <- T2

# Select columns of the dataframe
b= dplyr::select(select_T2,Gender,Ethnicity,Health)
m = subset(b,Gender == "Male")
m
m_A = subset(m,Ethnicity == "Arab" )
m_A
m_J = subset(m,Ethnicity == "Jew" )
wilcox_test(Health ~ Ethnicity, data = m)
wilcox_test(Health ~ Ethnicity, data = T2)
res <- wilcox.test(VSA ~ group, data = my_data, paired = TRUE)
res
hist(m_A$Health)
hist(m_J$Health)

shapiro.test(m_J$Healt); shapiro.test(m_A$Health)
ks.test(m_A$Health, "pnorm")
ks.test(m_J$Health, "pnorm")
hist(T2$VAS.b)
hist(T2$VAS.a)

# chack normalision in VSA
shapiro.test(T2$VAS.b); shapiro.test(T2$VAS.a)# without segnificent
ks.test(T2$VAS.b, "pnorm") #within segnificent
ks.test(T2$VAS.a, "pnorm")

## Plot using a qqplot
qqnorm(T2$VAS.b);qqline(T2$VAS.a, col = 2)
qqnorm(T2$VAS.b);qqline(T2$VAS.a, col = 2)
mean(T2$VAS.b)
mean(T2$VAS.a)
o1 = as.vector(T2$VAS.b)
o2 = as.vector(T2$VAS.a)
my_data <- data.frame(
                group = rep(c("befor", "after"), each=16 ),
                VSA = c(o1,  o2)
                )
print(my_data)

library(coin)

wilcox_test(VSA ~ group, data = my_data)
res <- wilcox.test(VSA ~ group, data = my_data, paired = TRUE)
res

h = as.numeric(T2$Health)

typeof(h)

factor(w) = group_by(T2$Ethnicity)
mean(T2$VAS.b)
mean(T2$VAS.a)
hist(T2$Health)
hist(T2$VAS.a)

hist(T2$VAS.b)
summary(T2$Wellbeing)
summary(T2$Social.func)
summary(T2$Energy)
T2$Gender
# solver nean & SD for age & count the women with\out kinds & frequencies
library(dplyr)
T2 %>% group_by(Ethnicity) %>%
  summarise(mean.Health = mean(Health, na.rm = TRUE),
            sd.Health = sd(Health, na.rm = TRUE), m.median = median(Health),
            n.Health = n()) %>%
  mutate(se.Health = sd.Health / sqrt(n.Health),
         lower.ci.Health = mean.Health - qt(1 - (0.05 / 2), n.Health - 1) * se.Health,
         upper.ci.Health = mean.Health + qt(1 - (0.05 / 2), n.Health - 1) * se.Health)
m %>%  group_by(Ethnicity)
tt <- wilcox.test(Health ~ Ethnicity, data = m, paired = TRUE)
res
library(dplyr)
T2 %>%
  summarise(mean.vsa.a = mean(VAS.b, na.rm = TRUE),
            sd.VSA.a = sd(VAS.b, na.rm = TRUE), m.median = median(VAS.a),
            n.VSA.a = n()) %>%
  mutate(se.VSA.a = sd.VSA.a / sqrt(n.VSA.a),
         lower.ci.Health = mean.vsa.a - qt(1 - (0.05 / 2), n.VSA.a - 1) * se.VSA.a,
         upper.ci.Health = mean.vsa.a + qt(1 - (0.05 / 2), n.VSA.a - 1) * se.VSA.a)
tt <- wilcox.test(VSA ~ group, data = my_data, paired = TRUE)
res
# count by Kids group residence_form(place)
group_split_Kids = T2 %>% group_by(Kids) %>% group_split(kids)

by_without_kids_residence_form =group_split_Kids[[1]] %>% group_by(residence_form, add = T) %>%
summarise(residence_form, n = n()/nrow(group_split_Kids[[1]])*100)
count(by_without_kids_residence_form)

by_with_kids_residence_form = group_split_Kids[[2]] %>% group_by(residence_form, add = T) %>%
summarise(residence_form, n = n()/nrow(group_split_Kids[[2]])*100)
count(by_with_kids_residence_form)

# count by Kids group W_employment
by_without_kids_W_employment =group_split_Kids[[1]] %>% group_by(W_employment, add = T) %>%
summarise(W_employment, n = n()/nrow(group_split_Kids[[1]])*100)
count(by_without_kids_W_employment)

by_with_kids_W_employment = group_split_Kids[[2]] %>% group_by(W_employment, add = T) %>%
summarise(W_employment, n = n()/nrow(group_split_Kids[[2]])*100)
count(by_with_kids_W_employment)

# count by Kids group Spouse_employment
by_without_kids_Spouse_employment =group_split_Kids[[1]] %>% group_by(Spouse_employment, add = T) %>%
summarise(Spouse_employment, n = n()/nrow(group_split_Kids[[1]])*100)
count(by_without_kids_Spouse_employment)

by_with_kids_Spouse_employment = group_split_Kids[[2]] %>% group_by(Spouse_employment, add = T) %>%
summarise(Spouse_employment, n = n()/nrow(group_split_Kids[[2]])*100)
count(by_with_kids_Spouse_employment)

# count by Kids group Marital_Status
by_without_kids_Marital_Status =group_split_Kids[[1]] %>% group_by(Marital_Status, add = T) %>%
summarise(Marital_Status, n = n()/nrow(group_split_Kids[[1]])*100)
count(by_without_kids_Marital_Status)

by_with_kids_Marital_Status = group_split_Kids[[2]] %>% group_by(Marital_Status, add = T) %>%
summarise(Marital_Status, n = n()/nrow(group_split_Kids[[2]])*100)
count(by_with_kids_Marital_Status)

#mean & sd by Kids group relationsip_duration
summary(group_split_Kids[[1]]$relationship_duration)
summary(group_split_Kids[[2]]$relationship_duration)
sd(group_split_Kids[[1]]$relationship_duration)
sd(group_split_Kids[[2]]$relationship_duration)
group_split_Kids[[2]]$relationship_duration

# find valus in table and chenge val cell to num
T2$T3_BE_1
x = (T2$T2_BE_1 : T2$T2_BE_23)
for (x in T2){str_replace_all(x, "אף פעם", "0") +
str_replace_all(x, "תמיד" , "4")+
str_replace_all(x, "לפעמים"  , "4")+
str_replace_all(x, "לעיתים קרובות"  , "4")+
str_replace_all(x, "לעיתים רחוקות"  , "4")
}
new_r = factor(T2$T2_BE_1)
new_r
levels(new_r)
new_lev = levels(new_r)
new_lev[c(5,1,2,3,4)]
new_r = factor(new_r,levels = new_lev[c(5,1,2,3,4)],labels = c(0,1,2,3,4))
new_r
T2$T2_BE_1

