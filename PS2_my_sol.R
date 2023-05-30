library(tidyverse)
library(rvest)
html <- read_html("https://www.iitk.ac.in/math/faculty")
name <- html %>% html_elements(".head3 a") %>% html_text()
