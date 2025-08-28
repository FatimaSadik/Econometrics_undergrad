#Lab 1
#Note: Update the R
#In this course we will use the companion package for Wooldridge's textbook
#Install Packages and loading them in R
install.packages("wooldridge")
library(wooldridge)
#load dataframe from the package. Note, the command will not work if the package wooldridge is not installed and unpacked properly
data("Fertility")
#To see the summary statistics for all columns
summary(Fertility)
#To see summary stats for one particular column
summary(Fertility$morekids)
#To generate a new variable/column and store it in the Fertility dataframe
Fertility$age2<-Fertility$age^2

