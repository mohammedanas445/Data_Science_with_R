#Q1 & Q2
library(imager)
dog <- load.image("dog.jpeg")
col.mat <- as.array(dog[,,1,])
#c(1,0,0) for pure red and c(0,0,1) for pure blue
dims <- apply(col.mat,c(1,2),function(x) norm(x-c(0,1,0),"2") )
ind <- which(dims==min(dims),arr.ind = T)
plot(dog)
points(ind,col = "red")


############################
#Q3
col1 <- load.image("col1.png")
col1 <- as.cimg(col1[,,,1:3])
col2 <- load.image("col2.png")
col3 <- load.image("col3.png")
diff_col <- function(img,col){
  img <- as.array(img[,,1,])
  dists <- apply(img,c(1,2),function(x) norm(x-col,"2"))
  return(dists)
}
which.color <- function(img){
  dist_col <- numeric(3)
  p_cols <- diag(3)
  for(i in 1:3){
    dist_col[i] <- mean(diff_col(img, col = p_cols[,i]))
  }
  inds <- which.min(dist_col)
  return(c("red","green","blue")[inds])
}
which.color(col1)
which.color(col2)
which.color(col3)


#######################
#Q4
land1 <- load.image("land1.jpeg")
land2 <- load.image("land2.jpeg")
snow_test <- function(pic1,pic2){
  dist1 <- mean(diff_col(pic1,c(1,1,1)))
  dist2 <- mean(diff_col(pic2,c(1,1,1)))
  paste(ifelse(dist1<dist2,"pic1","pic2"))
}
snow_test(land1,land2)


#############################
#Q5
dog <- load.image("dog.jpeg")
reverse.image <- function(img){
  img.arr <- as.array(img[,,1,])
  dims <- dim(img.arr)
  rotated.img <- array(0,dim = dims)
  for (i in 1:dims[1]) {
    for (j in 1:dims[2]) {
      rotated.img[i,j,] <- img.arr[dims[1]-i+1,dims[2]-j+1,]
    }
  }
  return(as.cimg(rotated.img))
}
plot(reverse.image(dog))
## Let's plot size by side
#par(mfrow = c(1,2))
#plot(dog)
#plot(as.cimg(rot))

#########################
#Q6
rotate.by.ninty.image <- function(img){
  img.arr <- as.array(img[,,1,])
  dims <- dim(img.arr)
  rotated.img <- array(0,dim = c(dims[2],dims[1],dims[3]))
  for (i in 1:dims[2]) {
    for (j in 1:dims[1]) {
      rotated.img[i,j,] <- img.arr[j,dims[2]-i+1,]
    }
  }
  return(as.cimg(rotated.img))
}
plot(rotate.by.ninty.image(dog))

########################
#Q7
rotate.by.ninty.anti.image <- function(img){
  img.arr <- as.array(img[,,1,])
  dims <- dim(img.arr)
  rotated.img <- array(0,dim = c(dims[2],dims[1],dims[3]))
  for (i in 1:dims[2]) {
    for (j in 1:dims[1]) {
      rotated.img[i,j,] <- img.arr[dims[1]-j+1,i,]
    }
  }
  return(as.cimg(rotated.img))
}
plot(rotate.by.ninty.anti.image(dog))

##############################
#Q8
dog <- load.image("dog.jpeg")
#78.6KB
col.dog <- as.array(dog[,,1,])
col.dog <- col.dog[1:600,1:600,]
reduced <- array(0,dim = c(300,300,3))
for (i in 1:300) {
  for(j in 1:300){
    reduced[i,j,1] <- mean(col.dog[((2*i-1):(2*i)),((2*j-1):(2*j)),1])
    reduced[i,j,2] <- mean(col.dog[((2*i-1):(2*i)),((2*j-1):(2*j)),2])
    reduced[i,j,3] <- mean(col.dog[((2*i-1):(2*i)),((2*j-1):(2*j)),3])
  }
}
reduced.dog <- as.cimg(reduced)
#19.2KB
save.image(reduced.dog,"dog_300.jpeg")  

###################################
#Q9
dog <- load.image("dog.jpeg")
#78.6KB
col.dog <- as.array(dog[,,1,])
col.dog <- col.dog[1:600,1:600,]
reduced <- array(0,dim = c(60,60,3))
for (i in 1:60) {
  for(j in 1:60){
    reduced[i,j,1] <- mean(col.dog[((10*i-9):(10*i)),((10*j-9):(10*j)),1])
    reduced[i,j,2] <- mean(col.dog[((10*i-9):(10*i)),((10*j-9):(10*j)),2])
    reduced[i,j,3] <- mean(col.dog[((10*i-9):(10*i)),((10*j-9):(10*j)),3])
  }
}
reduced.dog <- as.cimg(reduced)
save.image(reduced.dog,"dog_60.jpeg")
#1.7KB
plot(reduced.dog)

##############################################
#Q10

#######################
#Basic Visualization in R

#Q1
load("IMDB_movies.Rdata")

#Q2
#a
hist(dat$rating,main = paste("Histogram of Rating"),xlab = "Rating")
#b
hist(dat$rating,main = paste("Histogram of Rating"),xlab = "Rating",col = "white")

#Q3
boxplot(dat$rating,main = "Boxplot of Ratings")
boxplot(dat$rating,main = "Boxplot of Ratings",col = "pink")

#Q4
boxplot(dat$men_rating,dat$women_rating, names = c("Men","Women"), main = "Boxplot of Ratings by gender")

#Q5
hist(dat$men_rating, main = "Histogram of Ratings",col = adjustcolor("blue",alpha.f = .5),xlab = "Rating",xlim = range(c(dat$men_rating,dat$women_rating)))
hist(dat$women_rating,add = T, col = adjustcolor("red",alpha.f = .5))
legend("topright",c("Men","Women"),fill = c(adjustcolor("blue",alpha.f = .5),adjustcolor("red",alpha.f = .5)))

#Q6
plot(dat$over.votes,dat$rating,xlab = "# Votes",ylab = "Rating")
plot(dat$over.votes,dat$rating,xlab = "# Votes",ylab = "Rating",pch = 16)

#Q7
inds <- which(dat$rating>8.9) 
#or inds <- dat$rating>8.9 give logical output, but can be use here
text(dat$over.votes[inds],dat$rating[inds],labels = dat$name[inds])

#Q8
year_category <- (dat$year>1996)+1
plot(dat$over.votes[before_1996],dat$rating[before_1996],xlab = "# Votes",ylab = "Rating",pch = 16,col = year_category)
legend("bottomright",c("after 1996","before 1996"), col = c(2,1), pch =16)

#Q9
plot(dat$year, dat$over.votes, ylab = "# votes", xlab = "Year",pch =16)
# we see that for movies with highest number of votes, the ratings are
# high. Also, from the above plot we see that older movies 
# naturally don't have a lot of votes
# there is some response bias naturally then, in that when someone
# likes a movie a lot, they are more compelled to vote

#10
plot(dat$over.votes,dat$rating,xlab = "# Votes",ylab = "Rating",pch = 16, type = "n")
for (i in 1:length(dat$rating)) {
  #gives .1 second pause before running next line
  Sys.sleep(.1)
  #ith point
  points(dat$over.votes[i],dat$rating[i],pch = 16,col = year_category[i])
}
