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
