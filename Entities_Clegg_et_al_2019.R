library(readr)
library(dplyr)
library(ggplot2)
library(tibble)
library(tidyr)

library(psych)
library(corrr)
library(ggcorrplot)
library(ltm)
library(car)
library(onewaytests)
library(AID)
library(nlme)
library(lme4)
library(MuMIn)
library(car)
library(GLMMadaptive)
library(emmeans)
library(afex)
library(lavaan)

library(tidyverse)
library(ggpubr)
library(rstatix)
library(gridExtra)



Rel_and_SC_excluded <- read_csv("C:/Users/annag/Desktop/thesis 2/R/Rel_and_SC_excluded.csv")


#18 Entities 

Entities_18 <- subset(Rel_and_SC_excluded[,c("Entities_2", "Entities_3", "Entities_4", 
                                             "Entities_7", "Entities_9", "Entities_10", "Entities_11",
                                             "Entities_14", "Entities_15", "Entities_16", 
                                             "Entities_17", "Entities_18", "Entities_20", 
                                             "Entities_21", "Entities_22",  "Entities_25", "Entities_26", 
                                             "Entities_28")])

#rename
Entities_18 <- Entities_18 %>%
  rename(
    Gravity = Entities_2,
    Germs = Entities_3, 
    Electricity = Entities_4,
    Oxygen = Entities_7,
    Cells = Entities_9,
    Black_Holes = Entities_10,
    Aliens = Entities_11,
    Global_warming = Entities_14,
    Evolution = Entities_15,
    Heaven = Entities_16,
    Hell = Entities_17,
    Angels = Entities_18,
    Karma = Entities_20,
    Fate = Entities_21,
    Soul = Entities_22,
    Reincarnation = Entities_25,
    God = Entities_26,
    Creation = Entities_28)


#save entities_18 exel 
write.csv(Entities_18, file = "C:/Users/annag/Desktop/thesis 2/R/Entities_18.csv", row.names = FALSE)




#CONFIRMATORY FACTOR ANALYSIS (Clegg et al., 2019)
model_Clegg <- '
  F1 =~ Heaven  + God  + Hell + Angels  + Creation  + Soul 
  F2 =~ Electricity  + Germs  + Gravity + Oxygen + Cells 
  F3 =~ Black_Holes  + Aliens  + Evolution + Global_warming 
  F4 =~ Karma  + Fate  + Reincarnation 
'


fit_both <- cfa(model_Clegg, data = Entities_18)

summary(fit_both, fit.measures = TRUE, standardized = TRUE)




#EXPLORATORY FACTOR ANALISYS 
#correlation matrix (18)
Entities_corr_matrix_18 <- cor(Entities_18)
ggcorrplot(Entities_corr_matrix_18)

#Determining number of factors (18)
#eigen values 
Entities_eigen_18 <- eigen(Entities_corr_matrix_18)
Entities_eigen_18$values

#visualization (18)
plot(Entities_eigen_18$values, type='b', ylab='Eigenvalues', xlab='Factor')


#ENTITIES LOADING PER COUNTRY 
#Australia (C1) 
C1 <- subset(Rel_and_SC_excluded, Country == "1")

Entities_18_c1<- subset(C1[,c("Entities_2", "Entities_3", "Entities_4", 
                              "Entities_7", "Entities_9", "Entities_10", "Entities_11",
                              "Entities_14", "Entities_15", "Entities_16", 
                              "Entities_17", "Entities_18", "Entities_20", 
                              "Entities_21", "Entities_22",  "Entities_25", "Entities_26", 
                              "Entities_28")])

#rename
Entities_18_c1 <- Entities_18_c1 %>%
  rename(
    Gravity = Entities_2,
    Germs = Entities_3, 
    Electricity = Entities_4,
    Oxygen = Entities_7,
    Cells = Entities_9,
    Black_Holes = Entities_10,
    Aliens = Entities_11,
    Global_warming = Entities_14,
    Evolution = Entities_15,
    Heaven = Entities_16,
    Hell = Entities_17,
    Angels = Entities_18,
    Karma = Entities_20,
    Fate = Entities_21,
    Soul = Entities_22,
    Reincarnation = Entities_25,
    God = Entities_26,
    Creation = Entities_28)


#entities 18, C1, 3, varimax
Entities_corr_matrix_18_c1 <- cor(Entities_18_c1)
ggcorrplot(Entities_corr_matrix_18_c1)
Entities_18_c1_eigen <- eigen(Entities_corr_matrix_18_c1)
Entities_18_c1_eigen$values
plot(Entities_18_c1_eigen$values, type='b', ylab='Eigenvalues', xlab='Factor')


Entities_18_c1_varimax <-factanal(x=Entities_18_c1, factors=3, rotation='varimax')
print(Entities_18_c1_varimax)


#Malaysia (C2)
C2 <- subset(Rel_and_SC_excluded, Country == "2")

Entities_18_c2<- subset(C2[,c("Entities_2", "Entities_3", "Entities_4", 
                              "Entities_7", "Entities_9", "Entities_10", "Entities_11",
                              "Entities_14", "Entities_15", "Entities_16", 
                              "Entities_17", "Entities_18", "Entities_20", 
                              "Entities_21", "Entities_22",  "Entities_25", "Entities_26", 
                              "Entities_28")])

#rename
Entities_18_c2 <- Entities_18_c2 %>%
  rename(
    Gravity = Entities_2,
    Germs = Entities_3, 
    Electricity = Entities_4,
    Oxygen = Entities_7,
    Cells = Entities_9,
    Black_Holes = Entities_10,
    Aliens = Entities_11,
    Global_warming = Entities_14,
    Evolution = Entities_15,
    Heaven = Entities_16,
    Hell = Entities_17,
    Angels = Entities_18,
    Karma = Entities_20,
    Fate = Entities_21,
    Soul = Entities_22,
    Reincarnation = Entities_25,
    God = Entities_26,
    Creation = Entities_28)


#entities 18, C2
Entities_corr_matrix_18_c2 <- cor(Entities_18_c2)
ggcorrplot(Entities_corr_matrix_18_c2)
Entities_18_c2_eigen <- eigen(Entities_corr_matrix_18_c2)
Entities_18_c2_eigen$values
plot(Entities_18_c2_eigen$values, type='b', ylab='Eigenvalues', xlab='Factor')

#entities 18, C2, 2, varimax
#optimization problem --> how to solve? --> lower argument (see chat with Luke)
Entities_18_c2_varimax <-factanal(x=Entities_18_c2, factors=2, rotation='varimax', lower = 0.01)
print(Entities_18_c2_varimax)



#Kaiser-Meyer-Olkin criterion 
KMO(Entities_18) #okay
KMO(Entities_18_c1) #okay 
KMO(Entities_18_c2) #okay 


#Malaysia (C2) REMOVING PROBLEMATIC ITEMS 
Entities_18_c2_removed <- subset(C2[,c("Entities_2", "Entities_3", "Entities_4", 
                                       "Entities_7", "Entities_9", "Entities_10",
                                       "Entities_14",  "Entities_16", 
                                       "Entities_17", "Entities_18",  
                                       "Entities_21", "Entities_22", "Entities_26", 
                                       "Entities_28")])

#rename
Entities_18_c2_removed <- Entities_18_c2_removed %>%
  rename(
    Gravity = Entities_2,
    Germs = Entities_3, 
    Electricity = Entities_4,
    Oxygen = Entities_7,
    Cells = Entities_9,
    Black_Holes = Entities_10,
    Global_warming = Entities_14,
    Heaven = Entities_16,
    Hell = Entities_17,
    Angels = Entities_18,
    Fate = Entities_21,
    Soul = Entities_22,
    God = Entities_26,
    Creation = Entities_28)


#entities 18_removed, C2, 2, varimax
#optimization problem --> how to solve? --> lower argument (see chat with Luke)
Entities_18_removed_c2_varimax <-factanal(x=Entities_18_c2_removed, factors=2, rotation='varimax', lower = 0.01)
print(Entities_18_removed_c2_varimax)



#REGRESSION ANALYSIS 


#religiosity score (Clegg et al., 2019)
#1) self-identify as religious
print(Rel_and_SC_excluded$Religiosity)
Religiosity_na <- is.na(Rel_and_SC_excluded$Religiosity)
table(Religiosity_na) #19 

#2) self-identify as a religious denomination  
table(Rel_and_SC_excluded$Religion)
print(Rel_and_SC_excluded$Religion_score)
table(Rel_and_SC_excluded$Religion_score)
Religion_score_na <- is.na(Rel_and_SC_excluded$Religion_score)
table(Religion_score_na) #19 

#3) religious service one time per month or more
print(Rel_and_SC_excluded$Rel_frequent)
Rel_frequent_na <- is.na(Rel_and_SC_excluded$Rel_frequent)
table(Rel_frequent_na) #371 #19NAs
table(Rel_and_SC_excluded$Rel_freq)
table(Rel_and_SC_excluded$Rel_frequent)#okay (Once a month + More than once a week + Once a week)

#calculating Religiosity_score_2
Religiosity_score_2 <- Rel_and_SC_excluded$Religiosity + Rel_and_SC_excluded$Rel_frequent + Rel_and_SC_excluded$Religion_score

#add Religiosity column to the data frame 
Rel_and_SC_excluded <- add_column(Rel_and_SC_excluded, Religiosity_score_2 = Religiosity_score_2, .after = "Religion_score")
table(Rel_and_SC_excluded$Religiosity_score_2)

#Religiosity score per Country 
Country1 <- subset(Rel_and_SC_excluded, Country == "1")
table(Country1$Religiosity_score_2)
#0-1 = 134 ; 2-3 = 49

Country2 <- subset(Rel_and_SC_excluded, Country == "2")
table(Country2$Religiosity_score_2)
#0-1 = 30 ; 2-3 = 158



#AUstralia (C1) FACTORS 
#SCIENCE : Gravity, Germs, Electricity, Oxygen, Cells, Black_holes, Global Warning
#RELIGION: Heaven, Hell, Angels, God, Creation
#SPIRITUALISM: Aliens, Evolution, Karma, Fate, Soul, Reincarnation

#Religion
#Entities_16
#Entities_17
#Entities_18
#Entities_26
#Entities_28


#Factor 1, C1
c1_Factor_1 <- c(Country1$Entities_16, 
                 Country1$Entities_17,
                 Country1$Entities_18, 
                 
                 Country1$Entities_26,
                 
                 Country1$Entities_28)

mean(c1_Factor_1, na.rm = TRUE)
#3.551913
sd(c1_Factor_1, na.rm = TRUE)
#2.169584


#SCIENCE  : Gravity, Germs, Electricity, Oxygen, Cells, Black_holes, Global Warning
#Entities_2
#Entities_3
#Entities_4
#Entities_7
#Entities_9
#Entities_10
#Entities_14


#Factor 2, C1 
c1_Factor_2 <- c(Country1$Entities_2, 
                 Country1$Entities_3,
                 Country1$Entities_4, 
                 Country1$Entities_7,
                 Country1$Entities_9,
                 Country1$Entities_10,
                 Country1$Entities_14)

mean(c1_Factor_2, na.rm = TRUE)
#6.758782
sd(c1_Factor_2, na.rm = TRUE)
#0.7062576



#SPIRITUALITY : Aliens, Evolution, Karma, Fate, Soul, Reincarnation

#Entities_11
#Entities_15
#Entities_20
#Entities_21
#Entities_22
#Entities_25

#Factor 3, C1 
c1_Factor_3 <- c(
  Country1$Entities_11,
  
  Country1$Entities_15,
  
  Country1$Entities_20,
  Country1$Entities_21,
  Country1$Entities_22,
  Country1$Entities_25
)


mean(c1_Factor_3, na.rm = TRUE)
#4.562842
sd(c1_Factor_3, na.rm = TRUE)
#2.06286




#Malaysia (C2) FACTORS: 
#SCIENCE: Gravity, Germs, Electricity, Oxygen, Cells, Black_holes, Global Warning
#RELIGION: Heaven, Hell, Angels, Fate, Soul, God, Creation 


#RELIGION
#Entities_16
#Entities_17
#Entities_18
#Entities_21
#Entities_22
#Entities_26
#Entities_28

#Factor 1, C2 
c2_Factor_1 <- c(Country2$Entities_16, 
                 Country2$Entities_17,
                 Country2$Entities_18, 
                 Country2$Entities_21,
                 Country2$Entities_22, 
                 
                 Country2$Entities_26,
                 
                 Country2$Entities_28)

mean(c2_Factor_1, na.rm = TRUE)
#6.430642
sd(c2_Factor_1, na.rm = TRUE)
#1.449728





#SCIENCE : Gravity, Germs, Electricity, Oxygen, Cells, Black_holes, Global Warning
#Entities_2
#Entities_3
#Entities_4
#Entities_7
#Entities_9
#Entities_10
#Entities_14


#Factor 2, C2 
c2_Factor_2 <- c(
  Country2$Entities_2,
  Country2$Entities_3,
  Country2$Entities_4,
  
  Country2$Entities_7,
  
  Country2$Entities_9,
  Country2$Entities_10,
  
  Country2$Entities_14)

mean(c2_Factor_2, na.rm = TRUE)
#6.741891
sd(c2_Factor_2, na.rm = TRUE)
#0.7985948



#CALCULATING MEANS

Rel_and_SC_excluded <- Rel_and_SC_excluded %>%
  group_by(PID) %>%
  mutate(
    Religion_Mean_c1 = ifelse(Country == 1, mean(c(Entities_16, Entities_17, Entities_18, Entities_26, Entities_28)), NA),
    Science_Mean_c1 = ifelse(Country == 1, mean(c(Entities_2, Entities_3, Entities_4, Entities_7, Entities_9, Entities_10, Entities_14)), NA),
    Spirituality_Mean_c1 = ifelse(Country == 1, mean(c(Entities_11, Entities_15, Entities_20, Entities_21, Entities_22, Entities_25)), NA),
    Religion_Mean_c2 = ifelse(Country == 2, mean(c(Entities_16, Entities_17, Entities_18, Entities_21, Entities_22, Entities_26, Entities_28)), NA),
    Science_Mean_c2 = ifelse(Country == 2, mean(c(Entities_2, Entities_3, Entities_4, Entities_7, Entities_9, Entities_10, Entities_14)), NA))


Rel_and_SC_excluded <- Rel_and_SC_excluded %>%
  relocate(Religion_Mean_c1, .after = "Entities_29") %>%
  relocate(Science_Mean_c1, .after = "Religion_Mean_c1") %>%
  relocate(Spirituality_Mean_c1, .after = "Science_Mean_c1") %>%
  relocate(Religion_Mean_c2, .after = "Spirituality_Mean_c1") %>%
  relocate(Science_Mean_c2, .after = "Religion_Mean_c2")




#ENTITIES REGRESSION ANALYSIS 

#Australia (C1) 
#flip 
columns_of_C1 <- c("PID", "Religiosity_score_2", "Religion_Mean_c1", "Science_Mean_c1", "Spirituality_Mean_c1",
                   "Gender",  "SES_1", "Adjusted_ages", "Country")

columns_of_C1 <- Rel_and_SC_excluded[, columns_of_C1]


subset_data_columns_of_C1 <- columns_of_C1 %>%
  pivot_longer(cols = c(Religion_Mean_c1, Science_Mean_c1, Spirituality_Mean_c1), names_to = "Factor", values_to = "Mean")

#dataframe with no NAS 
subset_data_columns_of_C1_na <- na.omit(subset_data_columns_of_C1)

#check dataset 
table(subset_data_columns_of_C1_na$Country) #okay
table(subset_data_columns_of_C1_na$Factor) #okay


#plot means
ggplot(data = subset_data_columns_of_C1_na, aes(x = Mean)) +
  geom_bar(aes(y = ..count..), stat = "count", position = "stack", fill = "blue") +
  facet_wrap(~ Factor)


ggplot(data = subset_data_columns_of_C1_na, aes(x = Mean)) +
  geom_bar(aes(y = ..count..))


ggplot(data = subset_data_columns_of_C1_na, aes(x = Factor, y = Mean)) +
  geom_violin()+
  scale_x_discrete(labels = c("Religion", "Science", "Spirituality")) +
  labs(title = "Australia")+
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 3, fill = "red", color = "black", show.legend = FALSE)



#plot regression

ggplot(subset_data_columns_of_C1_na, aes(x = Religiosity_score_2, y = Mean, color = Factor)) +
  geom_smooth(method = "lm", se = TRUE) +  
  labs(title = "Australia",
       x = "Religiosity score",
       y = "Mean endorsement") +
  scale_color_manual(values = c("red", "blue", "green"), 
                     labels = c("Religion", "Science", "Spirituality"))+
  ylim(1, 7) +
  theme_minimal()


#check variables 
is.factor(subset_data_columns_of_C1_na$Religiosity_score_2)#okay


#models 

#o. null model 
nullModelTest_c1 <- lmer(Mean  ~ 1 + (1| PID),
                         data = subset_data_columns_of_C1_na) #boundary (singular) fit: see help('isSingular')

summary(nullModelTest_c1)

isSingular(nullModelTest_c1, tol = 1e-4)#TRUE 




#1. model with all variables 
model_c1_all_control <- lmer(Mean  ~ Religiosity_score_2 + Factor +  Gender + Adjusted_ages + SES_1  +(1| PID),
                             data = subset_data_columns_of_C1_na)

model_c1_all <- lmer(Mean  ~ Religiosity_score_2 + Factor  +(1| PID),
                     data = subset_data_columns_of_C1_na)

#2. model with interaction 
model_c1_all_interaction_control <- lmer(Mean  ~ Religiosity_score_2 * Factor + Gender + Adjusted_ages + SES_1 + (1| PID),
                                         data = subset_data_columns_of_C1_na)


model_c1_all_interaction <- lmer(Mean  ~ Religiosity_score_2 * Factor + (1| PID),
                                 data = subset_data_columns_of_C1_na)



#comparing to the  null model 
anova(nullModelTest_c1, model_c1_all)#significant 
anova(nullModelTest_c1, model_c1_all_interaction)#significant 

#comparing models 
anova(model_c1_all, model_c1_all_interaction)#int
anova(model_c1_all_interaction, model_c1_all_interaction_control)

#main effect 
anova(model_c1_all_interaction)



#slopes and post-hoc
summary(model_c1_all_interaction)

confint(emtrends(model_c1_all_interaction, var="Religiosity_score_2"))

emmeans(model_c1_all_interaction, specs = pairwise ~ Religiosity_score_2, at = list(Religiosity_score_2 = c(0, 1, 2, 3)))

emmeans(model_c1_all_interaction, specs = pairwise ~ Factor)

emmeans(model_c1_all_interaction, "Religiosity_score_2", by = "Factor")






#Malaysia (C2) 
#flip 

columns_of_C2 <- c("PID", "Religiosity_score_2", "Religion_Mean_c2", "Science_Mean_c2", 
                   "Gender",  "SES_1", "Adjusted_ages", "Country")

columns_of_C2 <- Rel_and_SC_excluded[, columns_of_C2]


subset_data_columns_of_C2 <- columns_of_C2 %>%
  pivot_longer(cols = c(Religion_Mean_c2, Science_Mean_c2), names_to = "Factor", values_to = "Mean")

#dataframe with no NAS 
subset_data_columns_of_C2_na <- na.omit(subset_data_columns_of_C2)

#check dataset 
table(subset_data_columns_of_C2_na$Country) #okay 
table(subset_data_columns_of_C2_na$Factor)#okay


#plot means
ggplot(data = subset_data_columns_of_C2_na, aes(x = Mean)) +
  geom_bar(aes(y = ..count..), stat = "count", position = "stack", fill = "blue") +
  facet_wrap(~ Factor)


ggplot(data = subset_data_columns_of_C2_na, aes(x = Mean)) +
  geom_bar(aes(y = ..count..))


ggplot(data = subset_data_columns_of_C2_na, aes(x = Factor, y = Mean)) +
  geom_violin()+
  scale_x_discrete(labels = c("Religion", "Science")) +
  labs(title = "Malaysia")+
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 3, fill = "red", color = "black", show.legend = TRUE)



#plot regression

ggplot(subset_data_columns_of_C2_na, aes(x = Religiosity_score_2, y = Mean, color = Factor)) +
  geom_smooth(method = "lm", se = TRUE) +  
  labs(title = "Malaysia",
       x = "Religiosity score",
       y = "Mean endorsement") +
  scale_color_manual(values = c("red", "blue"), 
                     labels = c("Religion", "Science"))+
  ylim(1, 7) +
  theme_minimal()



#check variables 
is.factor(subset_data_columns_of_C2_na$Religiosity_score_2)#okay


#models 

#o. null model 
nullModelTest_c2 <- lmer(Mean  ~ 1 + (1| PID),
                         data = subset_data_columns_of_C2_na) #boundary (singular) fit: see help('isSingular')

summary(nullModelTest_c2)

isSingular(nullModelTest_c2, tol = 1e-4)#TRUE 



#1. model with all variables 
model_c2_all_control <- lmer(Mean  ~ Religiosity_score_2 + Factor + Gender + Adjusted_ages + SES_1  + (1| PID),
                             data = subset_data_columns_of_C2_na)

model_c2_all <- lmer(Mean  ~ Religiosity_score_2 + Factor + (1| PID),
                     data = subset_data_columns_of_C2_na)

#2. model with interaction 
model_c2_all_interaction_control <- lmer(Mean  ~ Religiosity_score_2 * Factor + Gender + Adjusted_ages + SES_1  + (1| PID),
                                         data = subset_data_columns_of_C2_na)


model_c2_all_interaction <- lmer(Mean  ~ Religiosity_score_2 * Factor + (1| PID),
                                 data = subset_data_columns_of_C2_na)

model_c2_all_interaction_gender <- lmer(Mean  ~ Religiosity_score_2 * Factor + Gender + (1| PID),
                                        data = subset_data_columns_of_C2_na)


#comparing to the  null model 
anova(nullModelTest_c2, model_c2_all)#significant 
anova(nullModelTest_c2, model_c2_all_interaction)#significant 

#comparing models 
anova(model_c2_all, model_c2_all_interaction)#int 
anova(model_c2_all_interaction_gender, model_c2_all_interaction)#gender

#anova
anova(model_c2_all_interaction_gender)

#slopes and post-hoc
summary(model_c2_all_interaction_gender)

confint(emtrends(model_c2_all_interaction_gender, var="Religiosity_score_2"))

emmeans(model_c2_all_interaction_gender, specs = pairwise ~ Religiosity_score_2, at = list(Religiosity_score_2 = c(0, 1, 2, 3)))

emmeans(model_c2_all_interaction_gender, specs = pairwise ~ Factor)

emmeans(model_c2_all_interaction_gender, specs = pairwise ~ Gender)

emmeans(model_c2_all_interaction_gender, "Religiosity_score_2", by = "Gender")

emmeans(model_c2_all_interaction_gender, specs = pairwise ~ Gender : Religiosity_score_2)

emmeans(model_c2_all_interaction_gender, specs = pairwise ~ Gender : Factor)







#Malaysia (C2) and Australia (C1) comparison
#flip 
columns_of_both <- c("PID", "Religiosity_score_2", "Religion_Mean_c1", "Science_Mean_c1", "Religion_Mean_c2", "Science_Mean_c2", 
                     "Gender",  "SES_1", "Adjusted_ages", "Country")

columns_of_both <- Rel_and_SC_excluded[, columns_of_both]


subset_data_columns_of_both <- columns_of_both %>%
  pivot_longer(cols = c(Religion_Mean_c1, Science_Mean_c1,Religion_Mean_c2, Science_Mean_c2), names_to = "Factor", values_to = "Mean")

#dataframe with no NAS 
subset_data_columns_of_both_na <- na.omit(subset_data_columns_of_both)

#check dataset 
table(subset_data_columns_of_both_na$Country) #okay 
table(subset_data_columns_of_both_na$Factor)#okay


#plot means
ggplot(data = subset_data_columns_of_both_na, aes(x = Mean)) +
  geom_bar(aes(y = ..count..), stat = "count", position = "stack", fill = "blue") +
  facet_wrap(~ Factor)


ggplot(data = subset_data_columns_of_both_na, aes(x = Mean)) +
  geom_bar(aes(y = ..count..))


ggplot(data = subset_data_columns_of_both_na, aes(x = Factor, y = Mean)) +
  geom_violin()+
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 3, fill = "red", color = "black", show.legend = FALSE)


#recode factor                                                                                                                                                                                      
subset_data_columns_of_both_na_factor <- data.frame(subset_data_columns_of_both_na)

subset_data_columns_of_both_na_factor$Factor <- gsub("Science_Mean_c1", "Science_Mean", subset_data_columns_of_both_na_factor$Factor)
subset_data_columns_of_both_na_factor$Factor <- gsub("Science_Mean_c2", "Science_Mean", subset_data_columns_of_both_na_factor$Factor)
subset_data_columns_of_both_na_factor$Factor <- gsub("Religion_Mean_c1", "Religion_Mean", subset_data_columns_of_both_na_factor$Factor)
subset_data_columns_of_both_na_factor$Factor <- gsub("Religion_Mean_c2", "Religion_Mean", subset_data_columns_of_both_na_factor$Factor)


#plot regression

subset_data_columns_of_both_na_factor$Country <- factor(subset_data_columns_of_both_na_factor$Country)
subset_data_columns_of_both_na_factor$Factor <- factor(subset_data_columns_of_both_na_factor$Factor)

ggplot(subset_data_columns_of_both_na_factor, aes(x = Religiosity_score_2, y = Mean, color = Factor, linetype = Country)) +
  geom_smooth(method = "lm", se = TRUE) +  
  labs(
    x = "Religiosity score",
    y = "Mean endorsement") +
  scale_color_manual(labels = c("Religion", "Science"), values = c("red", "blue")) +
  scale_linetype_manual(labels = c("Australia", "Malaysia"), values = c("dashed", "solid")) +
  ylim(1, 7) +
  theme_minimal()


#check variables
is.factor(subset_data_columns_of_both_na_factor$Religiosity_score_2)#okay
is.factor(subset_data_columns_of_both_na_factor$Country)#to fix 
is.numeric(subset_data_columns_of_both_na_factor$Country)# to fix 
is.numeric(subset_data_columns_of_both_na_factor$Mean)

#fix variable country
subset_data_columns_of_both_na_factor$Country <- as.factor(subset_data_columns_of_both_na_factor$Country)
is.factor(subset_data_columns_of_both_na_factor$Country)


#models 

#o. null model 
nullModelTest_both <- lmer(Mean  ~ 1 + (1| PID),
                           data = subset_data_columns_of_both_na_factor) #boundary (singular) fit: see help('isSingular')

summary(nullModelTest_both)

isSingular(nullModelTest_both, tol = 1e-4)#TRUE 


#1. model with all variables
model_both_all_control <- lmer(Mean  ~ Religiosity_score_2 + Factor + Country + Gender + Adjusted_ages + SES_1  + (1| PID),
                               data = subset_data_columns_of_both_na_factor)

model_both_all <- lmer(Mean  ~ Religiosity_score_2 + Factor + Country + (1| PID),
                       data = subset_data_columns_of_both_na_factor)


#2. model with interaction 
model_both_all_interaction_2w_control <- lmer(Mean  ~ Factor * Country  + Religiosity_score_2 * Country  + Gender + Adjusted_ages + SES_1 +(1| PID),
                                              data = subset_data_columns_of_both_na_factor)

model_both_all_interaction_2w <- lmer(Mean  ~ Factor * Country  + Religiosity_score_2 * Country  +(1| PID),
                                      data = subset_data_columns_of_both_na_factor)

model_both_all_interaction_3w_control <- lmer(Mean  ~ Religiosity_score_2 * Factor * Country + Gender + Adjusted_ages + SES_1 + (1| PID),
                                              data = subset_data_columns_of_both_na_factor)


model_both_all_interaction_3w <- lmer(Mean  ~ Religiosity_score_2 * Factor * Country + (1| PID),
                                      data = subset_data_columns_of_both_na_factor)



model_both_all_interaction_3w_gender <- lmer(Mean  ~ Religiosity_score_2 * Factor * Country + Gender + (1| PID),
                                             data = subset_data_columns_of_both_na_factor)




#comparing to the  null model 
anova(nullModelTest_both, model_both_all)#significant 
anova(nullModelTest_both, model_both_all_interaction_2w)#significant 
anova(nullModelTest_both, model_both_all_interaction_3w)#significant

#comparing models 
anova(model_both_all, model_both_all_interaction_2w, model_both_all_interaction_3w)#3w
anova(model_both_all_interaction_3w, model_both_all_interaction_3w_gender, model_both_all_interaction_3w_control)# gender 

#main effect
anova(model_both_all_interaction_3w_gender)


#slopes and post-hoc
summary(model_both_all_interaction_3w_gender)

emmeans(model_both_all_interaction_3w_gender, specs = pairwise ~ Religiosity_score_2, at = list(Religiosity_score_2 = c(0, 1, 2, 3)))

emmeans(model_both_all_interaction_3w_gender, specs = pairwise ~ Religiosity_score_2 : Country, at = list(Religiosity_score_2 = c(0, 1, 2, 3)))

emmeans(model_both_all_interaction_3w_gender, specs = pairwise ~ Religiosity_score_2 : Factor, at = list(Religiosity_score_2 = c(0, 1, 2, 3)))

emmeans(model_both_all_interaction_3w_gender, specs = pairwise ~ Religiosity_score_2 : Factor : Country, at = list(Religiosity_score_2 = c(0, 1, 2, 3)))

emmeans(model_both_all_interaction_3w_gender, specs = pairwise ~ Factor)

emmeans(model_both_all_interaction_3w_gender, specs = pairwise ~ Country)

emmeans(model_both_all_interaction_3w_gender, specs = pairwise ~ Religiosity_score_2 : Factor)

emmeans(model_both_all_interaction_3w_gender, specs = pairwise ~ Country : Factor)

emmeans(model_both_all_interaction_3w_gender, specs = pairwise ~ Gender)

emmeans(model_both_all_interaction_3w_gender, specs = pairwise ~ Gender : Religiosity_score_2)

confint(emtrends(model_both_all_interaction_3w_gender, var="Religiosity_score_2"))







