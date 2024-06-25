library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lme4)
library(tibble)
library(performance)
library(MuMIn)
library(car)
library(emmeans)
library(marginaleffects)
library(broom.mixed)


Rel_and_SC_excluded <- read_csv("C:/Users/annag/Desktop/thesis 2/R/Rel_and_SC_excluded.csv")

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




#AUSTRALIAN VS MALAYSIAN NON PARENTS 
Non_parents <- subset(Rel_and_SC_excluded, Parent == "No")

table(Non_parents$Religiosity_score) #19 na



#endorsement NON PARENTS 
#dataset to run linear regression analysis 

columns_of_endorsment_non_both <- c("PID", "Religiosity_score", "Country", "Retro_Sc_p_CODED", 
                                    "Retro_rel_p_CODED", "Gender",  "SES_1", "Adjusted_ages")

subset_endorsment_non_both <- Non_parents[, columns_of_endorsment_non_both]

subset_data_long_endorsment_non_both <- subset_endorsment_non_both %>%
  pivot_longer(cols = c(Retro_Sc_p_CODED, Retro_rel_p_CODED), names_to = "Domain", values_to = "Endorsement")

table(subset_data_long_endorsment_non_both$Endorsement)#31 ambiguous 

#Drop ambiguous responses
subset_data_long_endorsment_non_both <- subset(subset_data_long_endorsment_non_both, Endorsement != "ambiguous")

#turn endorsement in numbers 
subset_data_long_endorsment_non_both$numerical_endorsement <- ifelse(subset_data_long_endorsment_non_both$Endorsement == "affirmation", 1, 0)

#dataframe with no NAS (Religiosity)
subset_data_long_endorsment_non_both_na <- na.omit(subset_data_long_endorsment_non_both)


#plot percentages 
ggplot(subset_data_long_endorsment_non_both_na, aes(x = Endorsement)) +  
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  facet_wrap(~ Domain)

ggplot(subset_data_long_endorsment_non_both_na, aes(x = Endorsement)) +  
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  facet_wrap(~ Domain + Country, scales = "free_x")



ggplot(subset_data_long_endorsment_non_both_na, aes(x = Endorsement)) +  
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  facet_wrap(~ Country)


#PERCENTAGES 
data_percentage_affirmation <- subset_data_long_endorsment_non_both_na %>%
  group_by(Country, Domain) %>%
  summarise(
    total = n(),
    affirm_count = sum(Endorsement == "affirmation"),
    .groups = 'drop'
  ) %>%
  mutate(percentage = affirm_count / total) %>%
  ungroup()


data_percentage_denial <- subset_data_long_endorsment_non_both_na %>%
  group_by(Country, Domain) %>%
  summarise(
    total = n(),
    affirm_count = sum(Endorsement == "denial"),
    .groups = 'drop'
  ) %>%
  mutate(percentage = affirm_count / total) %>%
  ungroup()


#PLOT 

is.factor(data_percentage_affirmation$Country)
data_percentage_affirmation$Country <- as.factor(data_percentage_affirmation$Country)


ggplot(data_percentage_affirmation, aes(x = Country, y = percentage, fill = Domain)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(y = "% of affirmation", x = NULL, fill = "Domain") +  # Remove "Country" from x-axis label
  scale_fill_manual(values = c("Retro_rel_p_CODED" = "#0000FF80", "Retro_Sc_p_CODED" = "#FF000080"),  # Specify colors for each domain
                    labels = c("Retro_rel_p_CODED" = "Religion", "Retro_Sc_p_CODED" = "Science")) +  # Rename legend labels
  scale_x_discrete(labels = c("Australia", "Malaysia")) +  # Rename x-axis labels
  theme_minimal() +
  theme()

#check variables
is.numeric(subset_data_long_endorsment_non_both_na$Religiosity_score)#okay
is.factor(subset_data_long_endorsment_non_both_na$Country)#to fix 
is.factor(subset_data_long_endorsment_non_both_na$numerical_endorsement)#to fix 

#fix variables
subset_data_long_endorsment_non_both_na$Country <- as.factor(subset_data_long_endorsment_non_both_na$Country)
subset_data_long_endorsment_non_both_na$numerical_endorsement <- as.factor(subset_data_long_endorsment_non_both_na$numerical_endorsement)


#o. null model 
nullModelTest_end_both_non <- glmer(numerical_endorsement ~ 1 + (1|PID), data = subset_data_long_endorsment_non_both_na, family = binomial) 

summary(nullModelTest_end_both_non)


#1. model with moderator
model_mod_end_both_non_control <- glmer(numerical_endorsement ~ Domain + Religiosity_score + Country + Gender + Adjusted_ages + SES_1 + (1|PID), data = subset_data_long_endorsment_non_both_na, family = binomial)


model_mod_end_both_non <- glmer(numerical_endorsement ~ Domain + Religiosity_score + Country + (1|PID), data = subset_data_long_endorsment_non_both_na, family = binomial)




#2. model with interaction 

model_int_end_both_non_2W_control <- glmer(numerical_endorsement ~ Domain * Country  + Religiosity_score * Domain  + Religiosity_score * Country + Gender + Adjusted_ages + SES_1  
                                           + (1|PID), data = subset_data_long_endorsment_non_both_na, family = binomial)

model_int_end_both_non_2W <- glmer(numerical_endorsement ~ Domain * Country  + Religiosity_score * Domain  + Religiosity_score * Country +
                                     (1|PID), data = subset_data_long_endorsment_non_both_na, family = binomial)


model_int_end_both_non_3W_control <- glmer(numerical_endorsement ~ Domain * Religiosity_score * Country + Gender + Adjusted_ages + SES_1  
                                           + (1|PID), data = subset_data_long_endorsment_non_both_na, family = binomial)

model_int_end_both_non_3W <- glmer(numerical_endorsement ~ Domain * Religiosity_score * Country   
                                   + (1|PID), data = subset_data_long_endorsment_non_both_na, family = binomial)


#comparing to null model
anova(nullModelTest_end_both_non, model_mod_end_both_non)#significant 
anova(nullModelTest_end_both_non, model_int_end_both_non_2W)#significant 
anova(nullModelTest_end_both_non, model_int_end_both_non_3W)#significant 

#comparing models 
anova(model_mod_end_both_non, model_int_end_both_non_2W, model_int_end_both_non_3W)#2w
anova(model_int_end_both_non_2W, model_int_end_both_non_2W_control)#2w 

#main effec
Anova(model_int_end_both_non_2W, type = "3")

#slopes and post hoc
summary(model_int_end_both_non_2W)

confint(emtrends(model_int_end_both_non_2W, var="Religiosity_score"))

emmeans(model_int_end_both_non_2W, specs = pairwise ~ Religiosity_score, type = "response", at = list(Religiosity_score = c(0, 1, 2, 3)))






#SOURCES NON PARENTS 
#dataset to run linear regression analysis 

columns_of_sources_non_both <- c("PID", "Religiosity_score", "Country", "Retro_Sc_p_SOURCE", 
                                 "Retro_rel_p_SOURCE", "Gender",  "SES_1", "Adjusted_ages")

subset_sources_non_both <- Non_parents[, columns_of_sources_non_both]

subset_data_long_sources_non_both <- subset_sources_non_both %>%
  pivot_longer(cols = c(Retro_Sc_p_SOURCE, Retro_rel_p_SOURCE), names_to = "Domain", values_to = "Source")

table(subset_data_long_sources_non_both$Source)#242

#Drop uninformative responses
subset_data_long_sources_non_both <- subset(subset_data_long_sources_non_both, Source != "uninformative")

#turn sources in numbers 
subset_data_long_sources_non_both$numerical_sources <- ifelse(subset_data_long_sources_non_both$Source == "parent-only source", 1, 0)

#dataframe with no NAS (Religiosity)
subset_data_long_sources_non_both_na <- na.omit(subset_data_long_sources_non_both)


#plot
ggplot(subset_data_long_sources_non_both, aes(x = Source), ) +  
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  facet_wrap(~ Domain)

ggplot(subset_data_long_sources_non_both, aes(x = Source), ) +  
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  facet_wrap(~ Domain + Country, scales = "free_x")


#PERCENTAGES 

data_percentage_only_parents <- subset_data_long_sources_non_both_na %>%
  group_by(Country, Domain) %>%
  summarise(
    total = n(),
    affirm_count = sum(Source == "parent-only source"),
    .groups = 'drop'
  ) %>%
  mutate(percentage = affirm_count / total) %>%
  ungroup()

data_percentage_multi_parents <- subset_data_long_sources_non_both_na %>%
  group_by(Country, Domain) %>%
  summarise(
    total = n(),
    affirm_count = sum(Source == "multiple source"),
    .groups = 'drop'
  ) %>%
  mutate(percentage = affirm_count / total) %>%
  ungroup()


#PLOT

is.factor(data_percentage_only_parents$Country)
data_percentage_only_parents$Country <- as.factor(data_percentage_only_parents$Country)


ggplot(data_percentage_only_parents, aes(x = Country, y = percentage, fill = Domain)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(y = "% of parent-only source", x = NULL, fill = "Domain") +  # Remove "Country" from x-axis label
  scale_fill_manual(values = c("Retro_rel_p_SOURCE" = "#0000FF80", "Retro_Sc_p_SOURCE" = "#FF000080"),  # Specify colors for each domain
                    labels = c("Retro_rel_p_SOURCE" = "Religion", "Retro_Sc_p_SOURCE" = "Science")) +  # Rename legend labels
  scale_x_discrete(labels = c("Australia", "Malaysia")) +  # Rename x-axis labels
  theme_minimal() +
  theme()



#check variables
is.numeric(subset_data_long_sources_non_both_na$Religiosity_score)#okay
is.factor(subset_data_long_sources_non_both_na$Country)#to fix 
is.factor(subset_data_long_sources_non_both_na$numerical_sources)#to fix 


#fix variables
subset_data_long_sources_non_both_na$Country <- as.factor(subset_data_long_sources_non_both_na$Country)
subset_data_long_sources_non_both_na$numerical_sources <- as.factor(subset_data_long_sources_non_both_na$numerical_sources)


#o. null model 
nullModelTest_source_both_non <- glmer(numerical_sources ~ 1 + (1|PID), data = subset_data_long_sources_non_both_na, family = binomial) 

summary(nullModelTest_source_both_non)


#1. model with moderator
model_mod_source_both_non_control <- glmer(numerical_sources ~ Domain + Religiosity_score + Country + Gender + Adjusted_ages + SES_1 
                                           + (1|PID), data = subset_data_long_sources_non_both_na, family = binomial)


model_mod_source_both_non <- glmer(numerical_sources ~ Domain + Religiosity_score + Country 
                                   + (1|PID), data = subset_data_long_sources_non_both_na, family = binomial)

#2. model with interaction 
model_int_source_both_non_2w_control <- glmer(numerical_sources ~ Domain * Country  + Religiosity_score * Country + Religiosity_score * Domain + Gender + Adjusted_ages + SES_1 
                                              + (1|PID), data = subset_data_long_sources_non_both_na, family = binomial)

model_int_source_both_non_2w <- glmer(numerical_sources ~ Domain * Country  + Religiosity_score * Country + Religiosity_score * Domain
                                      + (1|PID), data = subset_data_long_sources_non_both_na, family = binomial)

model_int_source_both_non_3w_control <- glmer(numerical_sources ~ Domain * Religiosity_score * Country + Gender + Adjusted_ages + SES_1 
                                              + (1|PID), data = subset_data_long_sources_non_both_na, family = binomial)

model_int_source_both_non_3w <- glmer(numerical_sources ~ Domain * Religiosity_score * Country 
                                      + (1|PID), data = subset_data_long_sources_non_both_na, family = binomial)


#comparing to null 
anova(nullModelTest_source_both_non, model_mod_source_both_non)# significant
anova(nullModelTest_source_both_non, model_int_source_both_non_2w)# significant
anova(nullModelTest_source_both_non, model_int_source_both_non_3w)# significant

#comparing models 
anova(model_mod_source_both_non, model_int_source_both_non_2w, model_int_source_both_non_3w)#2w
anova(model_int_source_both_non_2w, model_int_source_both_non_2w_control)#2w

#main effect
Anova(model_int_source_both_non_2w, type = "3")


#slopes and post hoc
summary(model_int_source_both_non_2w)

confint(emtrends(model_int_source_both_non_2w, var="Religiosity_score"))

emmeans(model_int_source_both_non_2w, specs = pairwise ~ Domain, type = "response")

emmeans(model_int_source_both_non_2w, specs = pairwise ~ Domain : Country, type = "response")





#CONFLICT NON PARENTS 
#dataset to run linear regression analysis 

columns_of_conflict_non_both <- c("PID", "Religiosity_score",  
                                  "Retro_sc_conf_type", "Retro_rel_confl_type",
                                  "Country", "Retro_sc_conf_score", "Retro_rel_confl_score", 
                                  "Gender",  "SES_1", "Adjusted_ages")

subset_conflict_non_both <- Non_parents[, columns_of_conflict_non_both]

subset_data_long_conflict_non_both <- subset_conflict_non_both %>%
  pivot_longer(cols = c(Retro_sc_conf_type, Retro_rel_confl_type), names_to = "Domain", values_to = "Type")

#turn types in numbers 
subset_data_long_conflict_non_both$numerical_conflict <- ifelse(subset_data_long_conflict_non_both$Type == "supportive", 1, 0)

table(subset_data_long_conflict_non_both$Retro_sc_conf_score)#108
table(subset_data_long_conflict_non_both$Retro_rel_confl_score)#104

# Replace zero values with NA
subset_data_long_conflict_non_both$Retro_sc_conf_score[subset_data_long_conflict_non_both$Retro_sc_conf_score == 0] <- NA
subset_data_long_conflict_non_both$Retro_rel_confl_score[subset_data_long_conflict_non_both$Retro_rel_confl_score == 0] <- NA

#dataframe with no NAS (Religiosity + 0 score)
subset_data_long_conflict_non_both_na <- na.omit(subset_data_long_conflict_non_both)


#plot
ggplot(subset_data_long_conflict_non_both_na, aes(x = Type), ) +  
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  facet_wrap(~ Domain)

ggplot(subset_data_long_conflict_non_both_na, aes(x = Type), ) +  
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  facet_wrap(~ Domain + Country, scales = "free_x")


#PERCENTAGES 

data_percentage_supportive <- subset_data_long_conflict_non_both_na %>%
  group_by(Country, Domain) %>%
  summarise(
    total = n(),
    affirm_count = sum(Type == "supportive"),
    .groups = 'drop'
  ) %>%
  mutate(percentage = affirm_count / total) %>%
  ungroup()


data_percentage_directive <- subset_data_long_conflict_non_both_na %>%
  group_by(Country, Domain) %>%
  summarise(
    total = n(),
    affirm_count = sum(Type == "directive"),
    .groups = 'drop'
  ) %>%
  mutate(percentage = affirm_count / total) %>%
  ungroup()


#PLOT 

is.factor(data_percentage_supportive$Country)
data_percentage_supportive$Country <- as.factor(data_percentage_supportive$Country)


ggplot(data_percentage_supportive, aes(x = Country, y = percentage, fill = Domain)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(y = "% of supportive responses", x = NULL, fill = "Domain") +  # Remove "Country" from x-axis label
  scale_fill_manual(values = c("Retro_rel_confl_type" = "#0000FF80", "Retro_sc_conf_type" = "#FF000080"),  # Specify colors for each domain
                    labels = c("Retro_rel_confl_type" = "Religion", "Retro_sc_conf_type" = "Science")) +  # Rename legend labels
  scale_x_discrete(labels = c("Australia", "Malaysia")) +  # Rename x-axis labels
  theme_minimal() +
  theme()


#check variables
is.numeric(subset_data_long_conflict_non_both_na$Religiosity_score)#okay
is.factor(subset_data_long_conflict_non_both_na$Country)#to fix 
is.factor(subset_data_long_conflict_non_both_na$numerical_conflict)#to fix 

#fix variables
subset_data_long_conflict_non_both_na$Country <- as.factor(subset_data_long_conflict_non_both_na$Country)
subset_data_long_conflict_non_both_na$numerical_conflict <- as.factor(subset_data_long_conflict_non_both_na$numerical_conflict)



#o. null model 
nullModelTest_conf_both_non <- glmer(numerical_conflict ~ 1 + (1|PID), data = subset_data_long_conflict_non_both_na, family = binomial) 

summary(nullModelTest_conf_both_non)


#1. model with moderator
model_mod_conf_both_non_control <- glmer(numerical_conflict ~ Domain + Religiosity_score + Country +  Gender + Adjusted_ages + SES_1 
                                         + (1|PID), data = subset_data_long_conflict_non_both_na, family = binomial)

model_mod_conf_both_non <- glmer(numerical_conflict ~ Domain + Religiosity_score + Country 
                                 + (1|PID), data = subset_data_long_conflict_non_both_na, family = binomial)

#2. model with interaction 
model_int_conf_both_non_2w_control <- glmer(numerical_conflict ~ Domain * Country  + Religiosity_score * Country + Religiosity_score * Domain + Gender + Adjusted_ages + SES_1 
                                            + (1|PID), data = subset_data_long_conflict_non_both_na, family = binomial)

model_int_conf_both_non_2w <- glmer(numerical_conflict ~ Domain * Country  + Religiosity_score * Country + Religiosity_score * Domain
                                    + (1|PID), data = subset_data_long_conflict_non_both_na, family = binomial)


model_int_conf_both_non_3w_control <- glmer(numerical_conflict ~ Domain * Religiosity_score * Country +  Gender + Adjusted_ages + SES_1 
                                            + (1|PID), data = subset_data_long_conflict_non_both_na, family = binomial)

model_int_conf_both_non_3w <- glmer(numerical_conflict ~ Domain * Religiosity_score * Country 
                                    + (1|PID), data = subset_data_long_conflict_non_both_na, family = binomial)



#comparing to null model 
anova(nullModelTest_conf_both_non, model_mod_conf_both_non)#significant
anova(nullModelTest_conf_both_non, model_int_conf_both_non_2w)#significant
anova(nullModelTest_conf_both_non, model_int_conf_both_non_3w)#significant


#comparing models 
anova(model_mod_conf_both_non, model_int_conf_both_non_2w, model_int_conf_both_non_3w)#2w 
anova(model_int_conf_both_non_2w, model_int_conf_both_non_2w_control)#2w 

#main effect
Anova(model_int_conf_both_non_2w, type = "3")

#slopes and post-hoc
summary(model_int_conf_both_non_2w)

confint(emtrends(model_int_conf_both_non_2w, var="Religiosity_score"))

emmeans(model_int_conf_both_non_2w, specs = pairwise ~ Domain, type = "response")

emmeans(model_int_conf_both_non_2w, specs = pairwise ~ Country, type = "response")

emmeans(model_int_conf_both_non_2w, specs = pairwise ~ Religiosity_score, type = "response", at = list(Religiosity_score = c(0, 1, 2, 3)))

emmeans(model_int_conf_both_non_2w, specs = pairwise ~ Country : Domain, type = "response")

