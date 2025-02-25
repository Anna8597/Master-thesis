library(readr)
library(dplyr)
library(ggplot2)
library(writexl)

Rel_and_SC_excluded <- read_csv("C:/Users/annag/Desktop/thesis 2/R/Rel_and_SC_excluded.csv")




#SAMPLE SIZE 

#Country1: Australia 

Country1 <- subset(Rel_and_SC_excluded, Country == "1")
Country1_Finished <- subset(Rel_and_SC_excluded, Country == "1" & Finished == "TRUE")
percentage_Country1 <- (183 / 390) * 100
print(percentage_Country1)
#Country 1 total sample =  183

Country1_parents <- subset(Country1, Parent == "Yes")
#parents Country 1 = 3 
#non parents Country 1 = 180



#Country2: Malaysia 

Country2 <- subset(Rel_and_SC_excluded, Country == "2")
Country2_Finished <- subset(Rel_and_SC_excluded, Country == "2" & Finished == "TRUE")
percentage_Country2 <- (207 / 390) * 100
print(percentage_Country2)
#Country total sample = 207, Finished = 180 --> 27 NAs 


#Country 2 parents
Country2_parents <- subset(Country2, Parent == "Yes")
Country2_parents_Finished <- subset(Country2_Finished, Parent == "Yes")
#parents Country 2 = 90


#Country 2 non parents
Country2_non_parents <- subset(Country2, Parent == "No")
Country2_non_parents_Finished <- subset(Country2_Finished, Parent == "No")

#non parents Country 2 = 117



#Adjusted Age total 

mean(Rel_and_SC_excluded$Adjusted_ages, na.rm = TRUE)
#26.75775

sd(Rel_and_SC_excluded$Adjusted_ages, na.rm = TRUE)
#12.36968

range(Rel_and_SC_excluded$Adjusted_ages, na.rm = TRUE)
#17 - 65 

table(Rel_and_SC_excluded$Adjusted_ages)

Age_NAs <- is.na(Rel_and_SC_excluded$Adjusted_ages)
table(Age_NAs) #35

ggplot(Rel_and_SC_excluded, aes(x = Adjusted_ages)) +
  geom_bar() 

#under 30 
# 82 + 39 + 29 + 13 + 11 + 10 + 13 + 25 + 21 + 8 + 9 + 3 + 6 + 4 = 273
percentage_under30 <- (273 / 390) * 100
print(percentage_under30)

#over 50
#5+8+11+7+7+4+3+1+1 = 47
percentage_over50 <- (47 / 390) * 100
print(percentage_over50)


#age country 1
Country1_excluded <- subset(Rel_and_SC_excluded, Country == "1")

mean(Country1_excluded$Adjusted_ages, na.rm = TRUE)
#18.97207 

sd(Country1_excluded$Adjusted_ages, na.rm = TRUE)
#3.722736

range(Country1_excluded$Adjusted_ages, na.rm = TRUE)
#17 - 39 

table(Country1_excluded$Adjusted_ages)

ggplot(Country1_excluded, aes(x = Adjusted_ages)) +
  geom_bar() 

#under 18
82+36 = 118 
percentage_under18_c1 <- (118 /183) * 100
print(percentage_under18_c1)


#age Country 2 
Country2_excluded <- subset(Rel_and_SC_excluded, Country == "2")

mean(Country2_excluded$Adjusted_ages, na.rm = TRUE)
#34.67614 

sd(Country2_excluded$Adjusted_ages, na.rm = TRUE)
#13.0521

range(Country2_excluded$Adjusted_ages, na.rm = TRUE)
#18 - 65

table(Country2_excluded$Adjusted_ages)

ggplot(Country2_excluded, aes(x = Adjusted_ages)) +
  geom_bar() 



#GENDER
table(Rel_and_SC_excluded$Gender)
Gender_NA <- is.na(Rel_and_SC_excluded$Gender)
table(Gender_NA)#27 NAs --> all unfished Country 2

percentage_Female <- (249 / 390) * 100
print(percentage_Female)

percentage_Male <- (107 / 390) * 100
print(percentage_Male)

#GENDER C1 
table(Country1_excluded$Gender)

Gender_NA_C1 <- is.na(Country1_excluded$Gender)
table(Gender_NA_C1)#183 FALSE

percentage_Female_C1 <- (129 / 183) * 100
print(percentage_Female_C1)

percentage_Male_C1 <- (48 / 183) * 100
print(percentage_Male_C1)

#GENDERE C2 
table(Country2_excluded$Gender)

Gender_NA_C2 <- is.na(Country2_excluded$Gender)
table(Gender_NA_C2)#180 + 27 NAs

percentage_Female_C2 <- (120 / 180) * 100
print(percentage_Female_C2)

percentage_Male_C2 <- (59 / 180) * 100
print(percentage_Male_C2) 



#ses
SES_NA <- is.na(Rel_and_SC_excluded$SES_1)
table(SES_NA)# 27 NAs 

mean(Rel_and_SC_excluded$SES_1, na.rm = TRUE)
#5.741047

range(Rel_and_SC_excluded$SES_1, na.rm = TRUE)

SES_tot_freq_table <- table(Rel_and_SC_excluded$SES_1)
SES_total_perc_table <- prop.table(SES_tot_freq_table) * 100
print(SES_total_perc_table)

ggplot(Rel_and_SC_excluded, aes(x = SES_1)) +
  geom_bar()

#SES C1
mean(Country1_excluded$SES_1, na.rm = TRUE)

sd(Country1_excluded$SES_1, na.rm = TRUE)

range(Country1_excluded$SES_1, na.rm = TRUE)

SES_C1_freq_table <- table(Country1_excluded$SES_1)
SES_C1_perc_table <- prop.table(SES_C1_freq_table) * 100
print(SES_C1_perc_table)

ggplot(Country1_excluded, aes(x = SES_1)) +
  geom_bar()

#SES C2 
mean(Country2_excluded$SES_1, na.rm = TRUE)

sd(Country2_excluded$SES_1, na.rm = TRUE)

range(Country2_excluded$SES_1, na.rm = TRUE)

SES_C2_freq_table <- table(Country2_excluded$SES_1)
SES_C2_perc_table <- prop.table(SES_C2_freq_table) * 100
print(SES_C2_perc_table)

ggplot(Country2_excluded, aes(x = SES_1)) +
  geom_bar()




#ethnicity total 
table(Rel_and_SC_excluded$Ethnicity)
#Asian: 63 
#White: 110 
#Indian: 12
#Malay: 150 
#Chinese: 10 

#calculate "others" 

excluded_Ethnicity <- c("Asian", "Caucasian/White", "Indian India", "Malay", "Chinese")
count_excluded_Ethnicity <- nrow(subset(Rel_and_SC_excluded, !Ethnicity %in% excluded_Ethnicity))

print(paste("Number of rows excluding specified values:", count_excluded_Ethnicity))
#45 others #okay 


#Ethnicity NAs
Ethnicity_NA <- is.na(Rel_and_SC_excluded$Ethnicity)
table(Ethnicity_NA)# 27 NAs 

#Ethnicity percentages

percentage_Asian <- (63 / 390) * 100
print(percentage_Asian)

percentage_White <- (110 / 390) * 100
print(percentage_White)

percentage_Indian <- (12 / 390) * 100
print(percentage_Indian)

percentage_Malay <- (150 / 390) * 100
print(percentage_Malay)

percentage_Chinese <- (10 / 390) * 100
print(percentage_Chinese)

percentage_Other <- (45 / 390) * 100
print(percentage_Other)

#Ethnicity C1

table(Country1_excluded$Ethnicity)
#Asian: 63 
#White: 110 


#calculate "others" 

excluded_Ethnicity_C1 <- c("Asian", "Caucasian/White")
count_excluded_Ethnicity_C1 <- nrow(subset(Country1_excluded, !Ethnicity %in% excluded_Ethnicity_C1))
print(paste("Number of rows excluding specified values:", count_excluded_Ethnicity_C1))
#10 others #okay 

Ethnicity_NA_C1 <- is.na(Country1_excluded$Ethnicity)
table(Ethnicity_NA_C1)

#Ethnicity percentages

percentage_Asian_C1 <- (63 / 183) * 100
print(percentage_Asian_C1)

percentage_White_C1 <- (110 / 183) * 100
print(percentage_White_C1)

percentage_Other_C1 <- (10 / 183) * 100
print(percentage_Other_C1)


#ETHNICITY C2 

table(Country2_excluded$Ethnicity)
#Malay: 150
#Indian: 12
#Chinese: 10 


#calculate "others" 

excluded_Ethnicity_C2 <- c("Indian India", "Malay", "Chinese")
count_excluded_Ethnicity_C2 <- nrow(subset(Country2_excluded, !Ethnicity %in% excluded_Ethnicity_C2))
print(paste("Number of rows excluding specified values:", count_excluded_Ethnicity_C2))
#35 others #okay 

Ethnicity_NA_C2 <- is.na(Country2_excluded$Ethnicity)
table(Ethnicity_NA_C2) #180 + 27 NAs 

#Ethnicity percentages

percentage_Malay_C2 <- (150 / 207) * 100
print(percentage_Malay_C2)

percentage_Indian_C2 <- (12 / 207) * 100
print(percentage_Indian_C2)

percentage_Chinese_C2 <- (10 / 207) * 100
print(percentage_Chinese_C2)

percentage_Other_C2 <- (35 / 207) * 100
print(percentage_Other_C2)



#EDUCATION
table(Rel_and_SC_excluded$Edu_level)

Education_NA <- is.na(Rel_and_SC_excluded$Edu_level)
table(Education_NA)#27 NAs 

#High school completed, Certificate, year 12 
# 7 + 3 + 62 = 72 
percentage_High_school <- (72 / 390) * 100
print(percentage_High_school)

#pre-university/ matriculation 
#4
percentage_immatriculation <- (4 / 390) * 100
print(percentage_immatriculation)

#other
#3
percentage_other_edu <- (3 / 390) * 100
print(percentage_other_edu)

#vocational
#9 
percentage_vocational <- (9 / 390) * 100
print(percentage_vocational)

#Post-graduate degree
#51
percentage_Post_graduate <- (51 / 390) * 100
print(percentage_Post_graduate)

#Undergraduate degree
#116
percentage_Undergraduate_graduate <- (116 / 390) * 100
print(percentage_Undergraduate_graduate)

#Some University 
#97 + 11 = 108
percentage_Some <- (108 / 390) * 100
print(percentage_Some)




#EDUCATION C1 
table(Country1_excluded$Edu_level)


#High school completed, Certificate, year 12 
# 7 + 62 = 69
percentage_High_school_c1 <- (69 / 183) * 100
print(percentage_High_school_c1)

#Post-graduate degree
#2
percentage_Post_graduate_c1 <- (2 / 183) * 100
print(percentage_Post_graduate_c1)

#Undergraduate degree
#15
percentage_Undergraduate_graduate_c1 <- (15 / 183) * 100
print(percentage_Undergraduate_graduate_c1)

#Some University 
#97
percentage_Some_c1 <- (97 / 183) * 100
print(percentage_Some_c1)




#EDUCATION C2
table(Country2_excluded$Edu_level)

#High school completed, Certificate, year 12 
# 3
percentage_High_school_c2 <- (3 / 207) * 100
print(percentage_High_school_c2)

#other
#3
percentage_other_edu_c2 <- (3 / 207) * 100
print(percentage_other_edu_c2)

#vocational
#9 
percentage_vocational_c2 <- (9 / 207) * 100
print(percentage_vocational_c2)

#pre-university/ matriculation 
#4
percentage_immatriculation_c2 <- (4 / 207) * 100
print(percentage_immatriculation_c2)

#Post-graduate degree
#49
percentage_Post_graduate_c2 <- (49 / 207) * 100
print(percentage_Post_graduate_c2)

#Undergraduate degree
#101
percentage_Undergraduate_graduate_c2 <- (101 / 207) * 100
print(percentage_Undergraduate_graduate_c2)

#Some University 
#11
percentage_Some_c2 <- ( 11 / 207) * 100
print(percentage_Some_c2)




#RELIGIOUS AFFILIATION
table(Rel_and_SC_excluded$Religion)

Religion_NA <- is.na(Rel_and_SC_excluded$Religion)
table(Religion_NA)#19 NAs

#Other 12
percentage_Other_rel <- (12 / 390) * 100
print(percentage_Other_rel)

#Atheist 16 + none 77
percentage_None <- (93 / 390) * 100
print(percentage_None)

#Agnostic 7
percentage_Agnostic <- (7 / 390) * 100
print(percentage_Agnostic)

#Anglican 3
percentage_Anglican <- (3 / 390) * 100
print(percentage_Anglican)

#Buddhism 12
percentage_Buddhism <- (12 / 390) * 100
print(percentage_Buddhism)

#Catholic 36 
percentage_Catholic <- (36 / 390) * 100
print(percentage_Catholic)

#Christian 10
percentage_Christian <- (10 / 390) * 100
print(percentage_Christian)

#Hinduism 16
percentage_Hinduism <- (16 / 390) * 100
print(percentage_Hinduism)

#Islam 171 
percentage_Other <- (171 / 390) * 100
print(percentage_Other)

#Protestant 11
percentage_Protestant  <- (11 / 390) * 100
print(percentage_Protestant)



#religion c1
table(Country1_excluded$Religion)

#Agnosticism 7
percentage_Agnosticism_c1 <- (7/ 183) * 100
print(percentage_Agnosticism_c1)

#Anglican 3 
percentage_Anglican_c1 <- (3/ 183) * 100
print(percentage_Anglican_c1)

#Atheism  16 + None 73
percentage_Atheism_c1 <- (89/ 183) * 100
print(percentage_Atheism_c1)

#Buddhism 7
percentage_Buddhism_c1 <- (7/ 183) * 100
print(percentage_Buddhism_c1)

#Catholic 33
percentage_Catholic_c1 <- (33/ 183) * 100
print(percentage_Catholic_c1)

#Christian 10
percentage_Christian_c1 <- (10/ 183) * 100
print(percentage_Christian_c1)

#Hinduism 8 
percentage_Hinduism_c1 <- (8/ 183) * 100
print(percentage_Hinduism_c1)

#Islam 9 
percentage_Islam_c1 <- (9/ 183) * 100
print(percentage_Islam_c1)

#Protestant 8
percentage_Protestant_c1 <- (8/ 183) * 100
print(percentage_Protestant_c1)

#Other 9 
percentage_Other_c1 <- (9/ 183) * 100
print(percentage_Other_c1)




#religion c2
table(Country2_excluded$Religion)
Rel_C2_NA <- is.na(Country2_excluded$Religion)
table(Rel_C2_NA)#na = 19 

#Buddhism 5 
percentage_Buddhism_c2 <- (5/ 207) * 100
print(percentage_Buddhism_c2)

#Catholic 3
percentage_Catholic_c2 <- (3/ 207) * 100
print(percentage_Catholic_c2)

#Islam 162 
percentage_Islam_c2 <- (162/ 207) * 100
print(percentage_Islam_c2)

#None 4 
percentage_None_c2 <- (4/ 207) * 100
print(percentage_None_c2)

#Protestant 3
percentage_Protestant_c2 <- (3/ 207) * 100
print(percentage_Protestant_c2)

#Hindu 8 
percentage_Hindu_c2 <- (8/ 207) * 100
print(percentage_Hindu_c2)

#other 3 + no reply 19 
percentage_other_c2 <- (22/ 207) * 100
print(percentage_other_c2)





#OCCUPATION + EMPLOYMENT --> occupation: open question not coded; employment not in C2 
table(Rel_and_SC_cleaned_excluded$Occupation)

table(Rel_and_SC_cleaned_excluded$Employment)


#CHILD AGE (Country 2, parents)
Country2_excluded_combined_parents <- subset(Country2_excluded_combined, Parent == "Yes")
print(Country2_excluded_combined_parents$Child_age)

Child_age_NA <- is.na(Country2_excluded_combined_parents$Child_age)
table(Child_age_NA) #13 NAs 



#NON PARENTS 

Non_parents <- subset(Rel_and_SC_excluded, Parent == "No")

Non_parents_c1 <- subset(Non_parents, Country == "1")

Non_parents_c2 <- subset(Non_parents, Country == "2")


#age country 1
mean(Non_parents_c1$Adjusted_ages, na.rm = TRUE)
sd(Non_parents_c1$Adjusted_ages, na.rm = TRUE)
range(Non_parents_c1$Adjusted_ages, na.rm = TRUE)



#age Country 2 
mean(Non_parents_c2$Adjusted_ages, na.rm = TRUE)
sd(Non_parents_c2$Adjusted_ages, na.rm = TRUE)
range(Non_parents_c2$Adjusted_ages, na.rm = TRUE)



#SES C1
mean(Non_parents_c1$SES_1, na.rm = TRUE)

sd(Non_parents_c1$SES_1, na.rm = TRUE)

range(Non_parents_c1$SES_1, na.rm = TRUE)


#SES C2 
mean(Non_parents_c2$SES_1, na.rm = TRUE)

sd(Non_parents_c2$SES_1, na.rm = TRUE)

range(Non_parents_c2$SES_1, na.rm = TRUE)


#GENDER C1 
table(Non_parents_c1$Gender)

Gender_NA_NON_C1 <- is.na(Non_parents_c1$Gender)
table(Gender_NA_NON_C1)#180 FALSE



#GENDERE C2 
table(Non_parents_c2$Gender)

Gender_NA_NON_C2 <- is.na(Non_parents_c2$Gender)
table(Gender_NA_NON_C2)#102 + 15 NAs


#Ethnicity C1
table(Non_parents_c1$Ethnicity)
Ethnicity_NA_NON_C1 <- is.na(Non_parents_c1$Ethnicity)
table(Ethnicity_NA_NON_C1)#okay 180

#ETHNICITY C2 
table(Non_parents_c2$Ethnicity)
Ethnicity_NA_NON_C2 <- is.na(Non_parents_c2$Ethnicity)
table(Ethnicity_NA_NON_C2)#102 


#religion c1
table(Non_parents_c1$Religion)
Religion_na_non_c1 <- is.na(Non_parents_c1$Religion)
table(Religion_na_non_c1)#okay


#religion c2
table(Non_parents_c2$Religion)
Religion_na_non_c2 <- is.na(Non_parents_c2$Religion)
table(Religion_na_non_c2)#106







