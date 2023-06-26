library(imager)


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