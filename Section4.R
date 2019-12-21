getwd()
Chicago <- read.csv("Chicago-F.csv", row.names = 1)
NewYork <- read.csv("NewYork-F.csv", row.names = 1)
Houston <- read.csv("Houston-F.csv", row.names = 1)
SanFrancisco <- read.csv("SanFrancisco-F.csv", row.names = 1)

Chicago

is.data.frame(Chicago)
#-----------------Converting data.frames to matrix--------------------------
Chicago     <-as.matrix(Chicago)
NewYork     <-as.matrix(NewYork)
Houston     <-as.matrix(Houston)
SanFrancisco<-as.matrix(SanFrancisco)

Weather <- list(Chicago, NewYork, Houston, SanFrancisco)

Weather

names(Weather) <- c("Chicago", "NewYork", "Houston", "SanFrancisco")
names(Weather)

#--------------------------Getting started with the apply function-------------------------
apply(Chicago, 1, mean)
apply(Chicago, 2, mean)

Weather
lapply(Weather, rowMeans)
lapply(Weather, max)
lapply(Weather, function(x) x[1, ])
lapply(Weather, function(z) round((z[1, ] - z[2, ])/z[2, ], 2))
lapply(Weather, function(z) round((z[2, ] - z[1, ])/z[2, ],2))     

#-------------------------Setting up the deliverables------------------------------------------
lapply(Weather, rowMeans)
sapply(Weather, rowMeans)
round(sapply(Weather, rowMeans), 2) #Deliverable 1

sapply(Weather, function(z) round((z[1, ] - z[2, ])/z[2, ],2)) #Deliverable 2


sapply(Weather, apply, 1, max) #Deliverable 3
sapply(Weather, apply, 1, min) #Deliverable 4


Chicago
max(Chicago[1, ])
which.max(Chicago[1, ])
names(which.max(Chicago[1, ]))

apply(Chicago, 1, function(x) names(which.max(x)))
lapply(Weather, function(y) apply(y, 1, function(x) names(which.max(x))))
sapply(Weather, function(y) apply(y, 1, function(x) names(which.max(x))))
