library(imager)

#a
data("iris")
ind1 <- iris$Species==levels(iris$Species)[1]
ind2 <- iris$Species==levels(iris$Species)[2]
ind3 <- iris$Species==levels(iris$Species)[3]
boxplot(iris$Sepal.Length[ind1],iris$Sepal.Length[ind2],iris$Sepal.Length[ind3],names =levels(iris$Species),main= "Boxplot of Sepal Length")
boxplot(iris$Sepal.Width[ind1],iris$Sepal.Width[ind2],iris$Sepal.Width[ind3],names =levels(iris$Species),main="Boxplot of Sepal Width")
boxplot(iris$Petal.Length[ind1],iris$Petal.Length[ind2],iris$Petal.Length[ind3],names =levels(iris$Species),main="Boxplot of Petal Length")
boxplot(iris$Petal.Width[ind1],iris$Petal.Width[ind2],iris$Petal.Width[ind3],names =levels(iris$Species),main="Boxplot of Petal Width")
plot(iris$Sepal.Length,iris$Petal.Length,pch =16,col = iris$Species,xlab = "Sepal Length",ylab = "Petal Length", main = "Petal.Length Vs Sepal.Length")

##############################
#b
flip <- function(img){
  col.img <- as.array(img[,,1,])
  dims <- dim(col.img)
  flipped.img.arr <- array(0,dim = dims)
  for(i in 1:dims[1]){
    flipped.img.arr[i,,] <- col.img[dims[1]-i+1,,]
  }
  return(as.cimg(flipped.img.arr))
}

##################################
#c
library(MASS)
data("ships")
plot(ships$service,ships$incidents,pch =16, col = ships$type,xlab = "Aggregate Months of Service",ylab = "# Damage Incidents", main = "Incidents Vs Service")
legend("bottomright",levels(ships$type), col = c(1,2,3,4,5),pch=16)
#Agree that Ship Type B had the most accidents

#############################
#d
library(xml2)
library(dplyr)
library(rvest)
library(tidyverse)
html1 <- read_html("https://stats.stackexchange.com/questions?tab=Votes")
question.titles <- html1 %>% html_elements(".s-post-summary--content-title a") %>% html_text()
views <- html1 %>% html_elements(".s-post-summary--stats-item-number") %>% html_text() %>% matrix(ncol =3,byrow = T) %>% as.data.frame()
names(views) =c("Number of votes","Number of Answers","Number of Views")
data1 <- cbind("Title of the questios"= question.titles,views)

#################################
#e

number.of.days=numeric(1000)
for(j in 1:1000){
  bottle <- array(1,dim = 100)
  for (i in 1:200) {
  
    ind <- sample(1:length(bottle),size = 1)
    bottle[ind] <- bottle[ind]-0.5
    if(bottle[ind]==0){
      number.of.days[j]=i-1
      break
    }
  }
}
mean(number.of.days)
