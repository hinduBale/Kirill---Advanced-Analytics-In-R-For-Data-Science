getwd()
setwd("C://Users//Rahul//Desktop//R Programming//Kirill---Advanced-Analytics-In-R-For-Data-Science")

# This was basic import: fin <- read.csv(file.choose())
fin <- read.csv("P3-Future-500-The-Dataset.csv", na.strings = "")

head(fin, 10)
tail(fin, 20)

str(fin)

summary(fin)

fin$ID <- factor(fin$ID)
fin$Inception <- factor(fin$Inception)

summary(fin)
str(fin)
#---------------------------Learning to deal with Factor Value Trap(FVT)--------------------------------------

ex1 <- c("Hello", "my", "name", "is", "khan", "oh", "my", "my")
ex1
typeof(ex1)
            #----------- Converting this to a R factor (cause that is what will happen when you'll import such data!!!)-----------
ex1 <- factor(ex1)
typeof(ex1)
ex1
summary(ex1)
str(ex1)
            #------------To convert this ex1 from factor to normal double or integer...we'll do the following-------------------
ex2 <- as.character(ex1)
ex2
            #---------------------Now, try something different shall we---------------------------------------------------
ex3 <- c("14", "12", "13", "14", "12", "12")
ex3
ex4 <- as.numeric(ex3)
ex4 #-------------Yaaay----------------

ex3 <- factor(ex3)
ex3

typeof(ex3)

ex5 <- as.numeric(ex3)
ex5    #-------------Hawwww, yaha to kata hai apna!!-----------------------

ex5 <- as.numeric(as.character(ex3))
ex5

    #-------------------That's it...Thank You!!---------------------------------
head(fin)

#-----------Coming back to our dataset, the reason Revenue, Expenses and Growth are not recognised as integers is beacuse of $, % and Dollars..So, we'll replace that-----------------

fin$Expenses <- gsub(" Dollars", "", fin$Expenses)
fin$Expenses <- gsub(",", "", fin$Expenses)
head(fin, 10)

fin$Revenue <- gsub(",", "", fin$Revenue)  
fin$Revenue <- gsub("\\$", "", fin$Revenue)
head(fin, 20)

fin$Growth <- gsub("%", "", fin$Growth)
head(fin, 20)
tail(fin, 20)

head(fin, 24)
fin[!complete.cases(fin), ]

fin[is.na(fin$Expenses),]


#------------------------Time to remove some rows, that we cannot do anything else with(NA values in Industry colum)----------------------------------------------------

    #----------------------------------Let's take a back-up before removing anything---------------------------------------------------------

fin_backup <- fin
fin_backup

fin[!complete.cases(fin$Industry), ]
fin <- fin[!is.na(fin$Industry), ]

head(fin, 20)
fin[!complete.cases(fin), ]


#--------------------------------Correcting the serial numbers of the data frame--------------------------------------------------------------------------------------

fin_backup1 <- fin
rownames(fin) <- 1: nrow(fin)
fin


#-----------------------------------Putting in missing data where it can be filled logically------------------------------------------------------------------------

fin[is.na(fin$State), ]
fin[is.na(fin$State) & fin$City == "New York", "State"] <- "NY" #Just like arr[2,3]; Here the filter(is.na(fin$State) & fin$City == "New York") was used to specify the rows and "State" is used to specify the column and by using "<-", we are assigning that block some value..

fin[is.na(fin$State), ]

fin[is.na(fin$State) & fin$City == "San Francisco", "State"] <- "CA"

fin[c(11,82,265,377), ]

fin[!complete.cases(fin), ]


#-------------------------------Median imputation in NAs of Employee column of the data frame-------------------------------------------------------------------------------
  #It should be noted that, we could have used the mean to fill in the missing records of employee, but median is better, because it is not affected much by outliers..

median_emp_retail <- median(fin[fin$Industry == "Retail","Employees"], na.rm = TRUE)
median_emp_retail
fin[is.na(fin$Employees) & fin$Industry == "Retail", "Employees"] <- median_emp_retail

median_emp_finserv <- median(fin[fin$Industry == "Financial Services", "Employees"], na.rm = TRUE)
median_emp_finserv
fin[is.na(fin$Employees) & fin$Industry == "Financial Services","Employees"] <- median_emp_finserv

fin[c(3,330), ]

#-----------------------------------Median imputation for Growth, Revenue and Expenses---------------------------------------------------------------------------------------
str(fin)
fin$Growth <- as.numeric(fin$Growth)
fin[!complete.cases(fin), ]
median_growth_const <- median(fin[fin$Industry== "Construction","Growth"], na.rm = TRUE)
median_growth_const
fin[is.na(fin$Growth) & fin$Industry== "Construction", "Growth"] <- median_growth_const

fin[8, ]

fin[!complete.cases(fin), ]
fin$Revenue <- as.numeric(fin$Revenue)
fin$Expenses <- as.numeric(fin$Expenses)

str(fin)

#----------------------------------------Median imputation for Revenue-------------------------------------------------

median_revenue_const <- median(fin[fin$Industry == "Construction", "Revenue"], na.rm = TRUE)
median_revenue_const
fin[is.na(fin$Revenue) & fin$Industry == "Construction", "Revenue"] <- median_revenue_const

fin[c(8,42),  ]

#----------------------------------------Median imputation for Expenses---------------------------------------------------

median_expense_const <- median(fin[fin$Industry == "Construction", "Expenses"], na.rm = TRUE)
median_expense_const
fin[is.na(fin$Expenses) & fin$Industry == "Construction", "Expenses"] <- median_expense_const

fin[c(8, 42), ]

#----------------------------------------Filling in the logical entries------------------------------------------------------
fin[is.na(fin$Profit), ]
fin[is.na(fin$Profit), "Profit"] <- fin[is.na(fin$Profit), "Revenue"] - fin[is.na(fin$Profit), "Expenses"]

fin[c(8,42), ]

fin[!complete.cases(fin), ]

fin[is.na(fin$Expenses), "Expenses"] <- fin[is.na(fin$Expenses), "Revenue"] - fin[is.na(fin$Expenses), "Profit"] 
fin[15, ]

fin[!complete.cases(fin), ] #As the inception year is not important for our analysis, we could just leave that field vacant.


#-------------------------Done with the data cleaning part, now onto the visualisation part----------------------------------------
  #-----------------------------Scatterplot(bubbleplot)--------------------------------------
library("ggplot2")
p <- ggplot(data = fin)
p + geom_point(aes(x = Revenue, y = Expenses, color = Industry, size = Profit))

#-------------------------Scatterplot #2----------------------------------------------------
q <- ggplot(data = fin, aes(x = Revenue, y = Expenses, colour = Industry))
q + geom_point()
q + geom_point() + geom_smooth(fill = NA, size = 1.2)

#----------------------------Boxplot-----------------------------------------------
r <- ggplot(data = fin, aes(x = Industry, y = Growth, colour = Industry))
r + geom_boxplot(size = 1)
r + geom_jitter() + geom_boxplot(size = 1, alpha = 0.5, outlier.colour = NA) 
