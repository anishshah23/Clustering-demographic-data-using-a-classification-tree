library(ElemStatLearn)
rm(list = ls(all = T))
data("marketing")
head(marketing)
# constructing a table from text book data. 
demographic = c("Sex", "Martial_Status", "Age", "Education", "Occupation", "Income", "Years_In_BayArea", "Dual_Incomes", "Numbers_in_Household", 
                "Number_of_Children", "Householder_Status", "Type_of_Home", "Ethinic_Classification", "Language_in_Home")
N = 9409
Sex = sample(c(1,2), N,replace = T)
Martial_Status = sample(seq(1,5), N, replace = T)
Age = sample(seq(1,7), N, replace = T)
Education = sample(seq(1,6), N, replace = T)
Occupation = sample(seq(1,9), N, replace = T)
Income = sample(seq(1,9), N, replace = T)
Years_In_BayArea = sample(seq(1,5), N, replace = T)
Dual_Incomes = sample(seq(1,3), N, replace = T)

Numbers_in_Household = sample(seq(1,9), N, replace = T)
Number_of_Children = sample(seq(1,9), N, replace = T)
Householder_Status = sample(seq(1,3), N, replace = T)
Type_of_Home = sample(seq(1,5), N, replace = T)
Ethinic_Classification = sample(seq(1,8), N, replace = T) 
Language_in_Home = sample(seq(1,3), N, replace = T)

training_sample = data.frame(Sex, Martial_Status, Age, Education, Occupation, Income, Years_In_BayArea, Dual_Incomes, Numbers_in_Household, 
                             Number_of_Children, Householder_Status, Type_of_Home, Ethinic_Classification, Language_in_Home)
names(training_sample) = demographic
training_sample$target = 1
rm(Sex, Martial_Status, Age, Education, Occupation, Income, Years_In_BayArea, Dual_Incomes, Number_of_Children, Numbers_in_Household, 
   Householder_Status, Type_of_Home, Ethinic_Classification, Language_in_Home)

ref_sample = training_sample
for(i in 1:ncol(ref_sample)){
  ref_sample[,i] = sample(ref_sample[,i], nrow(ref_sample), replace = F)
}
ref_sample$target = 0
acombined_data = rbind(ref_sample, training_sample); rm(ref_sample, training_sample)
# few cols are categorical. Changing them. 
acombined_data$Sex = as.factor(as.character(acombined_data$Sex))
acombined_data$Martial_Status = as.factor(as.character(acombined_data$Martial_Status))
acombined_data$Occupation = as.factor(as.character(acombined_data$Occupation))
acombined_data$Dual_Incomes = as.factor(as.character(acombined_data$Dual_Incomes))
acombined_data$Householder_Status = as.factor(as.character(acombined_data$Householder_Status))
acombined_data$Type_of_Home = as.factor(as.character(acombined_data$Type_of_Home))
acombined_data$Ethinic_Classification = as.factor(as.character(acombined_data$Ethinic_Classification))
acombined_data$Language_in_Home = as.factor(as.character(acombined_data$Language_in_Home))

library(rpart)
acombined_data$target = as.factor(as.character(acombined_data$target))
model = rpart(target~., acombined_data)
summary(model)
predicted = predict(model, acombined_data[,-c(15)])
predicted

