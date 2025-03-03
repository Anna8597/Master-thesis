library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lme4)
library(tibble)
library(car)
library(psych)
library(ltm)
library(emmeans)
library(marginaleffects)
library(effectsize)

Rel_and_SC_excluded <- read_csv("C:/Users/annag/Desktop/thesis 2/R/Rel_and_SC_excluded.csv")

#columns of valuation
for (i in 1:ncol(Rel_and_SC_excluded)) {
  cat("Column Name:", names(Rel_and_SC_excluded)[i], "\n")
  cat("Column Number:", i, "\n\n")
}

#factor analysis valuation item list 

valuation <- subset(Rel_and_SC_excluded, select = c(34:44, 76:86))

valuation_na <- is.na(valuation)
table(valuation_na) #1 nas

#removing nas 
valuation_no_na <- na.omit(valuation)

valuation_factors <- factanal(valuation_no_na, factors = 2, rotation = "varimax")

print(valuation_factors)


#cronbach alpha
cronbach.alpha(valuation_no_na, CI=TRUE)#to run 
#.883

#Countries 
C1 <- subset(Rel_and_SC_excluded, Country == "1")
C2 <- subset(Rel_and_SC_excluded, Country == "2")

C1_valuation <-  subset(C1, select = c(34:44, 76:86))
C2_valuation <-  subset(C2, select = c(34:44, 76:86))

C1_valuation <- na.omit(C1_valuation)
C2_valuation <- na.omit(C2_valuation)

cronbach.alpha(C1_valuation, CI=TRUE)
#0.8

cronbach.alpha(C2_valuation, CI=TRUE)
#0.831





#Religiosity score column (sum) (Payir et al., 2021)
#rel_frequent more than once a month 

Rel_frequent_month <- Rel_and_SC_excluded$Rel_frequent 
Rel_and_SC_excluded <- add_column(Rel_and_SC_excluded, Rel_frequent_month = Rel_frequent_month, .after = "Rel_frequent")
Rel_and_SC_excluded$Rel_frequent_month[Rel_and_SC_excluded$Rel_freq == "Once a month"] <- 0


table(Rel_and_SC_excluded$Rel_frequent)

table(Rel_and_SC_excluded$Rel_frequent_month)

Religiosity_score <- Rel_and_SC_excluded$Worship_frequent + Rel_and_SC_excluded$Rel_frequent_month + Rel_and_SC_excluded$Religiosity
print(Religiosity_score)
table(Religiosity_score)

#Religiosity_score
#  0   1   2   3 
#149  47  101  74

#add Religiosity_score column to the data frame 
Rel_and_SC_excluded <- add_column(Rel_and_SC_excluded, Religiosity_score = Religiosity_score, .after = "Religiosity")
table(Rel_and_SC_excluded$Religiosity_score)






#Religiosity_score means

mean(Rel_and_SC_excluded$Religiosity_score, na.rm = TRUE)
#1.269542

sd(Rel_and_SC_excluded$Religiosity_score, na.rm = TRUE)
#1.18441


#Australia
mean(C1$Religiosity_score, na.rm = TRUE)
#0.5191257

sd(C1$Religiosity_score, na.rm = TRUE)
#0.9068817


#Malaysia
mean(C2$Religiosity_score, na.rm = TRUE)
#2

sd(C2$Religiosity_score, na.rm = TRUE)
#0.9421786


#PLOT 
ggplot(Rel_and_SC_excluded, aes(x = factor(Country), y = Religiosity_score)) +
  geom_violin() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "red", 
               position = position_dodge(width = 0.75))+
  labs(y = "Religiosity score", x = NULL)+
  scale_x_discrete(labels = c("1" = "Australia", "2" = "Malaysia"))


ggplot(Rel_and_SC_excluded, aes(x = factor(Country), y = Religiosity_score)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "red", 
               position = position_dodge(width = 0.75))



#ONE WAY ANOVA religiosity 

#checking assumptions
#homogeneity of variance 
bartlett.test(Religiosity_score ~ Country, data = Rel_and_SC_excluded) #okay 

#normality 
by(Rel_and_SC_excluded$Religiosity_score, Rel_and_SC_excluded$Country, shapiro.test)

#check variables
is.factor(Rel_and_SC_excluded$Country)#fix 
Rel_and_SC_excluded$Country <- as.factor(Rel_and_SC_excluded$Country)

#one way anova 
anova_result_religiosity <- aov(Religiosity_score ~ Country, data = Rel_and_SC_excluded)
summary(anova_result_religiosity)#significant


#kruskal test (non parametric)
kruskal_result_religiosity <- kruskal.test(Religiosity_score ~ Country, data = Rel_and_SC_excluded)
print(kruskal_result_religiosity)#same as one way anova 


#Bonferroni corrected comparison (post-hoc test)
pairwise_comparison_religiosity <- pairwise.t.test(Rel_and_SC_excluded$Religiosity_score, 
                                                   Rel_and_SC_excluded$Country, 
                                                   p.adjust.method = "bonferroni")


print(pairwise_comparison_religiosity)

emmeans(anova_result_religiosity, pairwise ~ Country, adjust = "bonferroni")





#VALUATION OF SCIENCE

#valuation of science range 
val_science <- apply(Rel_and_SC_excluded[, 34:44], 1, sum)
table(val_science)

#range: 19 - 55


#add val science as a column in a dataframe
Rel_and_SC_excluded <- add_column(Rel_and_SC_excluded, val_science = val_science, .after = "SVC11")
table(Rel_and_SC_excluded$val_science)


#country 1 2 with val science

C1_science <- subset(Rel_and_SC_excluded, Country == "1")
C2_science <- subset(Rel_and_SC_excluded, Country == "2")


#mean and sd
mean(Rel_and_SC_excluded$val_science, na.rm = TRUE)
#42.81026

sd(Rel_and_SC_excluded$val_science, na.rm = TRUE)
#5.947058


mean(C1_science$val_science, na.rm = TRUE)
#41.36612

sd(C1_science$val_science, na.rm = TRUE)
#5.981873


mean(C2_science$val_science, na.rm = TRUE)
#44.08696

sd(C2_science$val_science, na.rm = TRUE)
#5.628652



#plot
ggplot(Rel_and_SC_excluded, aes(x = factor(Country), y = val_science)) +
  geom_violin() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "red", 
               position = position_dodge(width = 0.75))+
  labs(y = "Valuation of science", x = NULL)+
  ylim(11, 55) +
  scale_x_discrete(labels = c("1" = "Australia", "2" = "Malaysia"))



ggplot(Rel_and_SC_excluded, aes(x = factor(Country), y = val_science)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "red", 
               position = position_dodge(width = 0.75))




#REGRESSION ANALYSIS


#FIX NAS 
is.na(Rel_and_SC_excluded$Religiosity_score)#yes 

Rel_and_SC_excluded_complete <- Rel_and_SC_excluded[complete.cases(Rel_and_SC_excluded$Religiosity_score), ]
is.na(Rel_and_SC_excluded_complete$Religiosity_score)#no

is.na(Rel_and_SC_excluded_complete$val_science)#okay

is.na(Rel_and_SC_excluded_complete$Gender)#no
Rel_and_SC_excluded_complete <- Rel_and_SC_excluded[complete.cases(Rel_and_SC_excluded$Gender), ]


is.na(Rel_and_SC_excluded_complete$Adjusted_ages)#no
Rel_and_SC_excluded_complete <- Rel_and_SC_excluded[complete.cases(Rel_and_SC_excluded$Adjusted_ages), ]

is.na(Rel_and_SC_excluded_complete$SES_1)#no
Rel_and_SC_excluded_complete <- Rel_and_SC_excluded[complete.cases(Rel_and_SC_excluded$SES_1), ]


#relation between religiosity and valuation of science


#plot regression 
Rel_and_SC_excluded_complete$Country <- as.factor(Rel_and_SC_excluded_complete$Country)

ggplot(Rel_and_SC_excluded_complete, aes(x = Religiosity_score, y = val_science, color = Country)) +
  geom_smooth(method = "lm", se = TRUE) +  
  labs(
    x = "Religiosity score",
    y = "Valuation of Science") +
  scale_color_manual(values = c("red", "blue"), 
                     labels = c("Australia", "Malaysia")) +
  ylim(11, 55) +
  theme_minimal()


#check variables
is.factor(Rel_and_SC_excluded_complete$Country)#okay
is.factor(Rel_and_SC_excluded_complete$Religiosity_score)#okay


#o. null model 
nullModelTest_science <- lm (val_science  ~ 1,
                             data = Rel_and_SC_excluded_complete) #boundary (singular) fit: see help('isSingular')

summary(nullModelTest_science)


#1. model with moderator
model_science_mod_control <- lm(val_science  ~ Religiosity_score + Country + Gender + Adjusted_ages + SES_1,
                                data = Rel_and_SC_excluded_complete)

model_science_mod <- lm(val_science  ~ Religiosity_score + Country,
                        data = Rel_and_SC_excluded_complete)

summary(model_science_mod_control)
summary(model_science_mod)

#2. model with interaction 
model_science_int_conrol <- lm(val_science  ~ Religiosity_score * Country + Gender + Adjusted_ages + SES_1,
                               data = Rel_and_SC_excluded_complete)

model_science_int <- lm(val_science  ~ Religiosity_score * Country,
                        data = Rel_and_SC_excluded_complete)


#comparing to null model
anova(nullModelTest_science, model_science_mod)
anova(nullModelTest_science, model_science_int)


#comparing models 
anova(model_science_mod, model_science_int)#not significant 
anova(model_science_mod, model_science_mod_control)#not significant 


#main effect
anova(model_science_mod)
anova(model_science_int)


#slopes and post hoc 
summary(model_science_mod)

confint(emtrends(model_science_mod, var="Religiosity_score"))

emmeans(model_science_mod, specs = pairwise ~ Religiosity_score, at = list(Religiosity_score = c(0, 1, 2, 3)))

emmeans(model_science_mod, specs = pairwise ~ Country)

emmeans(model_science_mod, specs = pairwise ~ Religiosity_score : Country, at = list(Religiosity_score = c(0, 1, 2, 3)))

#cohen's d

em_country_science <- emmeans(model_science_mod, specs = pairwise ~ Country)
eff_size(em_country_science, sigma = sigma(model_science_mod), edf = df.residual(model_science_mod))




#VALUATION OF RELIGION

val_rel <- apply(Rel_and_SC_excluded_complete[, 77:87], 1, sum)  
table(val_rel)

#range: 11 - 55

#add val science as a column in a dataframe
Rel_and_SC_excluded_complete <- add_column(Rel_and_SC_excluded_complete, val_rel = val_rel, .after = "SVC11")
table(Rel_and_SC_excluded_complete$val_rel)


#country 1 2 with val science
C1_rel <- subset(Rel_and_SC_excluded_complete, Country == "1")
C2_rel <- subset(Rel_and_SC_excluded_complete, Country == "2")


#mean and sd
mean(Rel_and_SC_excluded_complete$val_rel, na.rm = TRUE)
#39.4096

sd(Rel_and_SC_excluded_complete$val_rel, na.rm = TRUE)
#10.94819


mean(C1_rel$val_rel, na.rm = TRUE)
#32.3427

sd(C1_rel$val_rel, na.rm = TRUE)
#9.002227


mean(C2_rel$val_rel, na.rm = TRUE)
#46.55682

sd(C2_rel$val_rel, na.rm = TRUE)
#7.592264



#GGPPLOT OF MEANS 
ggplot(Rel_and_SC_excluded_complete, aes(x = factor(Country), y = val_rel)) +
  geom_violin() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "red", 
               position = position_dodge(width = 0.75))+
  labs(y = "Valuation of religion", x = NULL)+
  ylim(11, 55) +
  scale_x_discrete(labels = c("1" = "Australia", "2" = "Malaysia"))



ggplot(Rel_and_SC_excluded_complete, aes(x = factor(Country), y = val_rel)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "red", 
               position = position_dodge(width = 0.75))





#relation between religiosity and valuation of religion

is.na(Rel_and_SC_excluded_complete$val_rel)#okay

Rel_and_SC_excluded_complete <- Rel_and_SC_excluded_complete[complete.cases(Rel_and_SC_excluded_complete$val_rel), ]
is.na(Rel_and_SC_excluded_complete$val_rel)

is.na(Rel_and_SC_excluded_complete$Religiosity_score)#okay

is.na(Rel_and_SC_excluded_complete$Gender)

is.na(Rel_and_SC_excluded_complete$Adjusted_ages)

Rel_and_SC_excluded_complete <- Rel_and_SC_excluded_complete[complete.cases(Rel_and_SC_excluded_complete$Adjusted_ages), ]



#plot regression 
Rel_and_SC_excluded_complete$Country <- as.factor(Rel_and_SC_excluded_complete$Country)

ggplot(Rel_and_SC_excluded_complete, aes(x = Religiosity_score, y = val_rel, color = Country)) +
  geom_smooth(method = "lm", se = TRUE) +  
  labs(
    x = "Religiosity score",
    y = "Valuation of Religion") +
  scale_color_manual(values = c("red", "blue"), 
                     labels = c("Australia", "Malaysia")) +
  ylim(11, 55) +
  theme_minimal()


#check variables
is.factor(Rel_and_SC_excluded_complete$Country)#okay
is.numeric(Rel_and_SC_excluded_complete$val_rel)#okay


#o. null model 
nullModelTest_rel <- lm (val_rel  ~ 1,
                         data = Rel_and_SC_excluded_complete) 

summary(nullModelTest_rel)


#1. model with moderator
model_rel_mod_control <- lm(val_rel  ~ Religiosity_score + Country + Gender + Adjusted_ages + SES_1,
                            data = Rel_and_SC_excluded_complete)

model_rel_mod <- lm(val_rel  ~ Religiosity_score + Country,
                    data = Rel_and_SC_excluded_complete)



#2. model with interaction 
model_rel_int_control <- lm(val_rel  ~ Religiosity_score * Country + Gender + Adjusted_ages + SES_1,
                            data = Rel_and_SC_excluded_complete)

model_rel_int <- lm(val_rel  ~ Religiosity_score * Country,
                    data = Rel_and_SC_excluded_complete)



#comparing to null model 
anova(nullModelTest_rel, model_rel_mod)
anova(nullModelTest_rel, model_rel_int)


#comparing models 
anova(model_rel_mod, model_rel_int)#int
anova(model_rel_int_control, model_rel_int)#int

#main effect
anova(model_rel_int)


#slopes and post hoc
summary(model_rel_int)

confint(emtrends(model_rel_int, var="Religiosity_score"))

emmeans(model_rel_int, specs = pairwise ~ Religiosity_score, at = list(Religiosity_score = c(0, 1, 2, 3)))

emmeans(model_rel_int, specs = pairwise ~ Country)

emmeans(model_science_mod, specs = pairwise ~ Religiosity_score : Country, at = list(Religiosity_score = c(0, 1, 2, 3)))


#cohen's d for country effect 

em_country_rel <- emmeans(model_rel_int, specs = pairwise ~ Country)
eff_size(em_country_rel, sigma = sigma(model_rel_int), edf = df.residual(model_rel_int))


