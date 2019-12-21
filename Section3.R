#Delivaerable:

#Character: Machine Name
#Vector: (min, mean, max) Utilization for the month(exclude unknown hours)
#Logical: Has utilisation ever fallen below 90%
#Vector: All hours where utilization is unknown(NA)
#Dataframe: For this machine
#Plot: For all machines

getwd()

util <- read.csv("P3-Machine-Utilization.csv")

head(util, 20)
str(util)
summary(util)

#-------------------Deriving the Utilization column---------------------------------
util$Utilization <- 1 - util$Percent.Idle
head(util, 25)

#-----------------------Handling Date-Time------------------------------------

util$PosixTime <- as.POSIXct(util$Timestamp, format = "%d/%m/%Y %H:%M")
head(util, 20)
summary(util)

#--------------------------------Rearranging the columns---------------------------------------
util$Timestamp <- NULL
head(util, 20)
util <- util[, c(4,1,2,3)]
head(util, 20)

#-------------------------------Subsetting the list(Since we are only to work with RL1)----------------------------
RL1 <- util[util$Machine == "RL1", ]
summary(RL1)
RL1$Machine <- factor(RL1$Machine)
summary(RL1)

#-----------------------------------Starting off with building the list---------------------------------
RL1_stats <- c(min(RL1$Utilization, na.rm = TRUE),
               mean(RL1$Utilization, na.rm = TRUE),
               max(RL1$Utilization, na.rm = TRUE))
RL1_stats

util_under_90 <- as.logical(length(which(RL1$Utilization < 0.90)))

list_RL1 <- list("RL1", RL1_stats, util_under_90)
list_RL1

names(list_RL1)
names(list_RL1) <- c("Machine","Stats","LowThreshold")
names(list_RL1)

#---------------------------Extracting components of a list---------------------------------

list_RL1
list_RL1[1]
list_RL1[[1]]
list_RL1$Machine

list_RL1[2] #Still a list
list_RL1[[2]] #Returns the object
list_RL1$Stats

typeof(list_RL1[2])
typeof(list_RL1[[2]])
typeof(list_RL1$Stats)

list_RL1$Stats
list_RL1$Stats[2]
list_RL1$Stats[1]
list_RL1$Stats[3]

list_RL1[[3]]

#------------------------Adding and deleting list components------------------------------------
list_RL1$UnknownHours <- RL1[is.na(RL1$Utilization), "PosixTime"]
list_RL1
list_RL1[5] <- "New Information"
list_RL1[8] <- "Testing out new stuff"
list_RL1


#------------Deleting
list_RL1[5: 7] <- NULL
list_RL1

list_RL1$DataFrame <- RL1
list_RL1
summary(list_RL1)

list_RL1[[4]][1]
#----------------------Plotting time-plots--------------------------------------
library(ggplot2)
p <- ggplot(data = util)
myplot <- p + geom_point(aes(x = PosixTime, y = Utilization, colour = Machine), size = 1.2) + 
          facet_grid(Machine~.)+
          geom_hline(yintercept = 0.90, color = "DarkGreen",linetype = 3)

myplot

list_RL1$Plot <- myplot
list_RL1


str(list_RL1)
