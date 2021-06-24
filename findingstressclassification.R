
install.packages("normtest")
install.packages("pastecs")
install.packages("sp")
install.packages("smooth")
install.packages("writexl")
library(sp)
library(pastecs)
library(normtest)
library(readxl)
library(writexl)
library(readxl)
install.packages("kohonen")
library(kohonen)
set.seed(222)
#zscoredata<- read_excel("F:/OneDrive - University of Tasmania/Structuring_thesis/Chapters1,2,3/PLOTS DY/RResults/zscoresclaed.xls", 
#                           col_names = FALSE)
#View(zscoredata)
#data<- zscoredata
library(readxl)
data <- read_excel("F:/OneDrive - University of Tasmania/Structuring_thesis/Chapters1,2,3/PLOTS DY/RResults/spDY12rollingfrom2.xlsx", 
                   col_names = FALSE)

data2<-as.numeric(unlist(data))
str(data2)
summary(data2)
data<-matrix(data2, nrow=5042, ncol=31)
str(data)
summary(data)

# 10 crisis classification approach

g<- somgrid(xdim = 10, ydim = 1, topo = "rectangular")
map <- som(data, grid= g, alpha = c(0.05,0.01), radius=1)
plot(map, type= 'changes' )
plot(map)
class10<- map$unit.classif
head(class10)
head(data)
map$codes
map$grid
map$distances
map$data
map$unit.classif
write.csv(class10, "F:/OneDrive - University of Tasmania/Structuring_thesis/Chapters1,2,3/PLOTS DY/RResults/stressvector10.csv", row.names=F)
plot(map, type='codes', palette.name=rainbow, main= "31 by 31 matrix of application data")
plot(map, type ='count')
plot(map, type= 'mapping')
plot(map, type= 'dist.neigh')


#What I did in matlab is
g<- somgrid(xdim = 31, ydim = 31, topo = "rectangular")
map <- som(data, grid= g, alpha = c(0.05,0.01), radius=1)
plot(map, type= 'changes' )
plot(map)
class<- map$unit.classif
head(class)
head(data)
map$codes
map$grid
map$distances
map$data
map$unit.classif
write.csv(class, "F:/OneDrive - University of Tasmania/Structuring_thesis/Chapters1,2,3/PLOTS DY/RResults/stressvector1.csv", row.names=F)
plot(map, type='codes', palette.name=rainbow, main= "31 by 31 matrix of application data")
plot(map, type ='count')
plot(map, type= 'mapping')
plot(map, type= 'dist.neigh')

#small crisis subset
data2 <- read_excel("F:/OneDrive - University of Tasmania/Structuring_thesis/Chapters1,2,3/PLOTS DY/RResults/spDY12rollingfrom2.xlsx", 
                    col_names = FALSE)

data2<-as.numeric(unlist(data))
str(data2)
summary(data2)
data<-matrix(data2, nrow=5042, ncol=31)
str(data)
summary(data)
#What I did in matlab is
g<- somgrid(xdim = 10, ydim = 1, topo = "rectangular")
map <- som(data, grid= g, alpha = c(0.05,0.01), radius=1)
plot(map, type= 'changes' )
plot(map)
class2<- map$unit.classif
head(class2)
head(data)
map$codes
map$grid
map$distances
map$data
map$unit.classif
write.csv(class2, "F:/OneDrive - University of Tasmania/Structuring_thesis/Chapters1,2,3/PLOTS DY/RResults/stressvector2.csv", row.names=F)
plot(map, type='codes', palette.name=rainbow, main= "31 by 31 matrix of application data")
plot(map, type ='count')
plot(map, type= 'mapping')
plot(map, type= 'dist.neigh')

#duffie crisis subset 10 crises
data2 <- read_excel("F:/OneDrive - University of Tasmania/Structuring_thesis/Chapters1,2,3/PLOTS DY/RResults/spDY12rollingfrom2.xlsx", 
                    col_names = FALSE)

data2<-as.numeric(unlist(data))
str(data2)
summary(data2)
data<-matrix(data2, nrow=5042, ncol=31)
str(data)
summary(data)

