install.packages("normtest")
install.packages("pastecs")
install.packages("sp")
install.packages("smooth")
install.packages("writexl")
install.packages("pkgconfig")
install.packages("vctrs")
library(sp)
library(pastecs)
library(normtest)
library(dplyr)
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

data <- read_excel("C:/Users/islamr/Desktop/DATA/GHDFROM.xlsx", col_names = FALSE)
data2<-as.numeric(unlist(data))
str(data2)
summary(data2)
data<-matrix(data2, nrow=5040, ncol=31)
str(data)
summary(data)
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
write.csv(class, "C:/Users/islamr/Desktop/DATA/GHDFROM900.csv", row.names=F)