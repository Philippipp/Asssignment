install.packages("dplyr")
install.packages("corrplot")
library(dplyr)
library(ggplot2)
library(corrplot)


#A)
# Read the CSV 
x <- read.csv(file = "HR_comma_sep.csv", header = TRUE, sep = ",")

#Check the first rows of the data 
head(x)
#How many Attributes and rows are in the Data set
length(x)
length(x[,1])

#Showes the name of the attributes 
names(x)

#shows the datatyp of the attributes
str(x)

# Here you can see how many empolyees have left 
sum(x$left)

# The employess who are still working in the company
length(x[,1]) - sum(x$left)

# Show Information about the data mean, maximum, min, 
summary(x)

#AVG Day working hours 
mean(x$average_montly_hours)/20

# Here we are printing all the unique values in sales columns
aggdata <-aggregate(x, by=list(x$sales), FUN=mean)

#Find some Correlations 
correlations <- x %>% select(satisfaction_level:promotion_last_5years)
w <- cor(correlations)
corrplot(w, method="number")

#D
# look for na' values --> 0 na values 
is.na.data.frame(x)
sum(is.na.data.frame(x))

# look for outliers values --> 0 na values 
boxplot.stats(x[,1])$out
# Also you can look at the summary 

# look for wrong values --> 0 na values 

is.special <- function(x){ if (is.numeric(x)) !is.finite(x) else is.na(x) }
sapply(x, is.special)

#E
# if there is a missing value we can impute it with the mean the average_monthly_hours
library(Hmisc)
x$average_montly_hours <- impute(x$average_montly_hours, mean)

# Here we can change the average monthly working hours to daily working hours
# it will give an better overview  and a better understanding
x$average_montly_hours <- x$average_montly_hours/20
head(x$average_montly_hours)

#reduce the dimensions set an attribute null 
x$promotion_last_5years <- NULL



