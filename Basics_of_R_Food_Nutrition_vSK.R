# Getting Set to Start, let's install the packages, if not already done so.
# install.packages("readxl")

#Since I have already installed the package, I am not going to do now.
#I will just load the package 

#1-Set up working directory
setwd("C:/Users/hp/Desktop/02_Next Steps/BABI Mentoring/01_May19 Batch/01_R/Session 2/")
getwd() #Checking the working directory


#2-2.	Import the dataset in the console in the .xls(x) format
library(readxl)
Food_Nutrition1 <- read_excel("C:/Users/hp/Desktop/02_Next Steps/BABI Mentoring/01_May19 Batch/01_R/Session 2/Dataset_Food Nutrition.xlsx")
Food_Nutrition2 <- read_excel("Dataset_Food Nutrition.xlsx")
Food_Nutrition_wMissing <- read_excel("C:/Users/hp/Desktop/02_Next Steps/BABI Mentoring/01_May19 Batch/01_R/Session 2/Food Nutrition2.xlsx")

#3-View the top 10 rows of the data
head(Food_Nutrition1,20)
head(Food_Nutrition2,15)

#4-View the last 20 rows of the data
tail(Food_Nutrition1,20)

#5-Show the summary of the data
summary(Food_Nutrition1)
summary(Food_Nutrition2)
summary(Food_Nutrition_wMissing)

#5a-Structure of the data
str(Food_Nutrition1)
str(Food_Nutrition2)

#5b-Dimension of the data

dim(Food_Nutrition1)
dim(Food_Nutrition2)

#5c-Check for missing values
sum(is.na(Food_Nutrition1$`Lipid_Tot_(g)`))
sum(is.na(Food_Nutrition2))
sum(is.na(Food_Nutrition_wMissing))

#6-Create a vector "test" using the top 10 values of variable Protein_(g)
test<-head(Food_Nutrition1$`Protein_(g)`,10)
test

#7-Select the top 5 rows of initial 5 variables in a matrix format
Food_Nutrition1[1:5,1:5]

#8-What is the class of the Sodium_(mg) variable
class(Food_Nutrition1$`Sodium_(mg)`)

#9-Create a new variable "EPW" by dividing Energ_Kcal with the Water; what is the 
# dimension of the new dataset?
Food_Nutrition1$EPW<-Food_Nutrition1$Energ_Kcal/Food_Nutrition1$`Water_(g)`
head(Food_Nutrition1$EPW)
dim(Food_Nutrition1)

#Please note that the number of columns have increased by 1 and now is 21

#10.	Create a subset of the dataset, where the Energ_Kcal is less than 500, what 
# is the dimension of this new dataset?
food_new<-Food_Nutrition1[Food_Nutrition1$Energ_Kcal<500, ]
food_new1<-Food_Nutrition1[Food_Nutrition1$Energ_Kcal<500, c(1,2,3,15) ]

food_new
dim(food_new)

food_new1
dim(food_new1)


#11-Plot a Plot between Enrg_Kcal and Water using the new subset created
plot(food_new$Energ_Kcal,food_new$`Water_(g)`, main = "Plot between Energ_KCal vs Water")
plot(food_new$`Water_(g)`, food_new$Energ_Kcal, main = "New Plot between Energy and Water")

#11a-Plot a chart between Energy and Carbohydratyes - intuitively Energy should be related to Carbs 
plot(food_new$Energ_Kcal,food_new$`Carbohydrt_(g)`, main = "Plot between Energ_KCal vs Carbohydrates", xlab = "Energy (Kcal)", ylab = "Carbohydrates (g)")
plot(food_new$`Carbohydrt_(g)`,food_new$Energ_Kcal, main = "Plot between Energ_KCal vs Carbohydrates", ylab = "Energy (Kcal)", xlab = "Carbohydrates (g)", col="blue")

#11b-Plot a chart between Carbs and Sugar - intuitively Carbs should be related to Sugar - lets explore
plot(food_new$`Sugar_Tot_(g)`,food_new$`Carbohydrt_(g)`, type = "p", main = "Plot between Carbs vs Sugar", ylab = "Carbs (g)", xlab = "Sugar (g)", col="darkblue")

#12-Plot a histogram of Sugar_tot variable using the new subset
hist(food_new$`Sugar_Tot_(g)`)
hist(food_new$`Sugar_Tot_(g)`, col = "purple")

#12a-Plot a histogram of a few other variables using the new subset
hist(food_new$`Lipid_Tot_(g)`, main = "Histogram for Lipids", xlab = "Bin", ylab = 
       "Frequency", col = "lightblue")
hist(food_new$`Potassium_(mg)`, main = "Histogram for Potassium", xlab = "Bin", col = "lightgreen")

#13-	Find the top 10 products based on following:
#13.1-	Higher the Energy_Kcal, higher the ranking
x<-Food_Nutrition1
y<-x[order(-x$Energ_Kcal),]
head(y$Shrt_Desc,10)

#13.2-	Lower the water content, higher the ranking
z<-x[order(x$`Water_(g)`),]
head(z$Shrt_Desc,10)

#14-Create a subset of the data where product_desc contains "CHEESE" and 
# list down the summary statistics of the subset
library(dplyr)
m<-filter(x,grepl("CHEESE",x$Shrt_Desc))
head(m$Shrt_Desc)
summary(m)
str(m)
dim(m)

#Pattern Matching and Replacement

#Description
#grep, grepl, regexpr, gregexpr and regexec search for matches to argument pattern within each element of a character vector: they differ in the format of and amount of detail in the results.
#sub and gsub perform replacement of the first and all matches respectively.

#Usage
#grep(pattern, x, ignore.case = FALSE, perl = FALSE, value = FALSE,
#     fixed = FALSE, useBytes = FALSE, invert = FALSE)

#grepl(pattern, x, ignore.case = FALSE, perl = FALSE,
#     fixed = FALSE, useBytes = FALSE)

#Arguments
#pattern - character string containing a regular expression (or character string for fixed = TRUE) to be matched in the given character vector. Coerced by as.character to a character string if possible. If a character vector of length 2 or more is supplied, the first element is used with a warning. Missing values are allowed except for regexpr and gregexpr.
#x, text - a character vector where matches are sought, or an object which can be coerced by as.character to a character vector. Long vectors are supported.
#ignore.case - if FALSE, the pattern matching is case sensitive and if TRUE, case is ignored during matching


#14a-Lets try to see if there is a difference if we ignore the case
m1<-filter(x,grepl("CHEESE",x$Shrt_Desc, ignore.case = TRUE))
head(m1$Shrt_Desc)
summary(m1)
dim(m1)

#15.	Using the cut function on water variable divide the whole data into 6 bins, list 
# down the summary statistics of all the 6 bins

#Lets first understand the cut function

?cut
#cut divides the range of x into intervals and codes the values in x according to which interval they fall. The leftmost interval corresponds to level one, the next leftmost to level two and so on.
#equivalent to Lookup in Excel - which helps in bucketizing the data


l<-Food_Nutrition1
l$bins<-cut(x$`Water_(g)`, 6, labels=c("A", "B", "C","E","F","G"))
View(l)
dim(l)
bin1<-filter(l,l$bins=="A")
View(bin1)
summary(bin1)
dim(bin1)

