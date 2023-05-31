#Q1
htmlpostdoc <- read_html("https://www.iitk.ac.in/math/visitors-post-doctoral-fellow")
postdocnames <- htmlpostdoc %>% html_elements(".head2") %>% html_text()
postdocnames

#Q2
html <- read_html("https://www.imdb.com/chart/top/")
movienames <- html %>% html_elements(".titleColumn a") %>% html_text()
movienames

#Q3
movie_year <- html %>% html_elements(".secondaryInfo") %>% html_text() %>% substr(2,5) %>% as.numeric()
movie_rating <- html %>% html_elements(".ratingColumn.imdbRating strong") %>% html_text() %>% as.numeric()
movie_votes <- html %>% html_elements(".ratingColumn.imdbRating strong") %>% html_attr("title")
for (i in 1:250) {
  movie_votes[i] = str_split(movie_votes[i]," ")[[1]][4]
  
}
movie_votes= str_remove_all(movie_votes,",") %>% as.numeric()

movies_data <- data.frame("Name"=movienames,"Year"=movie_year,"Rating"=movie_rating,"Votes"=movie_votes)


#Q4


#Q5
poster_links <- html %>% html_elements(".posterColumn a img") %>% html_attr("src")
img <- load.image(poster_links[1])