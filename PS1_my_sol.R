Factorial <- function(N)
{
  if(N==0){
    return(1)
  }
  return(N*Factorial(N-1))
}
Factorial(5)


FACtorial <- function(N){
  if(N==0){return(1)}
  
  return(prod(1:N))
}
FACtorial(6)


####################
RHL_exp <- function(n){
  return((1+1/n)^n)
}



#########################
data <- read.csv("SOC171A_Survey_Responses.csv")
head(data) # to see the top of the data
str(data) # structure of the data
dim(data) # dimensions
k= (data$Gender=="Male")
Male_data = data[k,]
Male_data
indices=which((data$Category=="OBC-NCL")&(data$Marriage.preference=="Love Marriage"))
OBC_Mariage_data=data[indices,]
OBC_Mariage_data



#############################
primes <- 2
count <- 1
m <- 3
while( count <= 1000){
  m.is.prime <- T
  for(p in primes){
    if(m%%p==0){
      m.is.prime <- F
      break
    }
  }
  if(m.is.prime){
    primes <- c(primes,m)
    count <- count+1
  }
  m = m+2
}
primes


##########################
fibonacci.numbers=c(1,1)

for(count in 3:500){
  fibonacci.numbers[count]=fibonacci.numbers[count-1]+fibonacci.numbers[count-2]
}
fibonacci.numbers


#####################################
cricket <- read.csv("https://dvats.github.io/assets/course/mth208/battingbowling.csv")
allrounder <-cricket[which((cricket$Bowling<40)&(cricket$Batting>25)), ]
no_allrounders <- as.data.frame(table(allrounder$Team))

no_allrounders[which(no_allrounders$Freq==max(no_allrounders$Freq)),1]
no_allrounders[which(no_allrounders$Freq==min(no_allrounders$Freq)),1]

###################################
#Q1
#a
random <- rbinom(n=1000,size=1, prob = 0.5)
heads <- sum(random)/1000

#b
toses <- rbinom(n=1000, size=1, prob =0.5)
mean(toses)


#####################################
#Q2
#a
sample(x=c("red","red","red","green","green","blue","blue"),size = 1, prob = rep(1/7,7))
#b
runif(n =1, min = 0, max = 5)
#c
A <- matrix(c(3,1,-2,4,5,3,-1,2,-2),nrow = 3)
pi <- c(norm(A[,1],type = "2"),norm(A[,2], type = "2"),norm(A[,3], type = "2"))/(norm(A[,1],type = "2")+norm(A[,2], type = "2")+norm(A[,3], type = "2"))
sample(1:3,size = 1, prob = pi)

#########################################
#Q3
#a
count <- function(){
  randomdraws <- runif(1, min = 0, max = 1)
while(sum(randomdraws)<1){
  randomdraws <- c(randomdraws,runif(1, min = 0, max = 1))
}
return(length(randomdraws))
}
#b
vec <- numeric(length = 1000)
for(i in 1:1000){
  vec[i]=count()
}
#c
mean(vec)


#########################
#Q4
#a
attempts <- function(age){
  candlesleft <- age
  numberofattempts=0
  while(candlesleft){
    candlesleft=candlesleft-sample(1:candlesleft,size = 1)
    numberofattempts = numberofattempts+1
  }
  return(numberofattempts)
}
#b
num1 <- numeric(length = 1000)
for (i in 1:1000) {
  num1[i]=attempts(25)
}
#c
mean(num1)
#d
num2 <- numeric(length = 1000)
for (i in 1:1000) {
  num2[i]=attempts(30)
}
mean(num2)