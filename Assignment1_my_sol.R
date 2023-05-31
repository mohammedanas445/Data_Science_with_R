#a
html1 <- read_html("https://www.moneyworks4me.com/best-index/nse-stocks/top-nifty50-companies-list/")
allstocks <- html_table(html1)[[1]] %>% as.data.frame()
allstocks <- allstocks[2:13]
allstocks$`Company Name (M.Cap)` <- str_remove_all(allstocks$`Company Name (M.Cap)`,"\n\n")
allstocks

##############################
#b
allstocks_urls <- html1 %>% html_elements(".company-ellipses a") %>% html_attr("href")
allstocks_urls <- paste("https://WWW.moneyworks4me.com",allstocks_urls,sep = '')
random_five_stocks <- allstocks_urls[17:21]
data_sets <- c()
for (i in 1:5) {
  stock_html <- read_html(random_five_stocks[i])
  stock_details <- html_table(stock_html,header = T)
  
  names(stock_details[[1]])=stock_details[[1]][1,]
  names(stock_details[[3]])=stock_details[[3]][1,]
  
  stock_details <- rbind.data.frame(stock_details[[1]][6:13,1:11],stock_details[[3]][2:7,1:11])
  data_sets <- c(data_sets,stock_details)
}

###################################
#c
##1.
tennis <- function(p){
  success = 0
  for(i in 1:5){
    success <- success + rbinom(1,size = 1,prob = p)
    if((success>=3)|(i-success>=3)){
      break
    }
  }
  return(i)
}

##2.
matches <- numeric(1000)
for (j in 1:1000) {
  matches[j] <- tennis(0.7)
}
ans <- mean(matches)

##################################
#d
##1.
MontyHall <- function(){
  # door1=car,door2 & door3 =goat
  x <- sample(1:3,size=1)
  if(x==1){
    #Monty opens door3
    #contestant switches
    x <- 2
    return(x==1)
  }
  if(x==2){
    #Monty opens door3
    #contestant switches
    x <- 1
    return(x==1)
  }
  if(x==3){
    #Monty opens door2
    #contestant switches
    x <- 1
    return(x==1)
  }
}

##2.
game_if_switches <- numeric(1000)
for(k in 1:1000){
  game_if_switches[k] = MontyHall()
}
prob_of_wining_if_switches <- mean(game_if_switches)
prob_of_wining_if_switches

################
#e
html2 <- read_html("https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-right-now/")
rankings <- html2 %>% html_elements(".countdown-index-resposive") %>% html_text()
movie_name <- html2 %>% html_elements(".article_movie_title a") %>% html_text()
percentage_score <- html2 %>% html_elements(".tMeterScore") %>% html_text()
movie_year <- html2 %>% html_elements(".subtle.start-year") %>% html_text() %>% substr(2,5) %>% as.numeric()
topmovies_data <- data.frame("Ranking"=rankings,"Name of Movie"=movie_name,"Tomato % score"=percentage_score,"Year of the movie"=movie_year,check.names = F)
