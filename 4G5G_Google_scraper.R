# Scraper for Google to get historical 4G/5G articles summaries

library(rvest)
library(tidyverse)
library(magrittr)

q1 <- "4G"
q1_startdate <- as.Date("2009-01-01")
q1_enddate <- as.Date("2014-04-30")

q2 <- "5G"
q2_startdate <- as.Date("2017-10-01")
q2_enddate <- as.Date("2019-10-31")

# Clearly: Gnews.io is inadequate
# Bing News Search is inadequate too as data ranges are not possible
# scraaaape then Google News

pagination_google <- "&start=" #need to add start where

google_news_url <- "https://www.google.com/search?"



# Gnews_APIToken <- "88f5a00fda8a15b6c1b7273a166a478b"
# token <- paste0("&token=",Gnews_APIToken)
# url <- "https://gnews.io/api/v3/search?q="

# create fortnight vector
date_diff <- as.numeric(q2_enddate - q1_startdate, units="days")
fortnight_diff <- date_diff %/% 14

fortnightly_news_sample <- as.data.frame(1:fortnight_diff)
colnames(fortnightly_news_sample)[1] <- "fortnights"

fortnightly_news_sample <- fortnightly_news_sample  %>% 
  mutate(fortnightstart = as.Date(q1_startdate + fortnights * 14)) %>% 
  mutate(fortnightend = fortnightstart + 13)



# Create function to see if 4G should be quried or not

fourG_yesno <- function(fortnight_start) {
  
  if (fortnight_start >= q1_startdate & fortnight_start <= q1_enddate) {
    return(1)
  } else return(0)
  
}

# Create function to see if 5G news should be queried

fiveG_yesno <- function(fortnight_start) {
  
  if (fortnight_start >= q2_startdate & fortnight_start <= q2_enddate) {
    return(1)
  } else return(0)
  
}

# Create query table
fortnightly_news_sample$'4G' <- unlist(lapply(fortnightly_news_sample$fortnightstart, fourG_yesno))
fortnightly_news_sample$'5G' <- unlist(lapply(fortnightly_news_sample$fortnightstart, fiveG_yesno))

write_csv(fortnightly_news_sample, "fortnights_4G5G.csv")

get_google_news <- function(fortnightstart, keyword, pages) {
  query <- paste("q=",keyword,"+before:",fortnightstart+13,"+after:",fortnightstart,sep='')
  my_google_url <- paste(google_news_url, query, "&cr=countryUS&tbs=ctr:countryUS&tbm=nws",sep='')
  sites <- 1:pages
  for (i in sites) {
    
    paginated_url <- paste(my_google_url, pagination_google, (i-1)*10, sep='')
    
    t <- read_html(paginated_url)
    
    news_titles <- t %>% html_nodes('.g') %>% html_nodes('.r') %>% html_text()
    
    temp <- t %>% html_nodes('.g') %>% html_nodes('.slp') %>% html_text()
    news_source <- sapply(strsplit(temp, " - "), `[`, 1)
    news_date <- sapply(strsplit(temp, " - "), `[`, 2)
    
    news_desc <- t %>% html_nodes('.g') %>% html_nodes('.st') %>% html_text()
    
    if (i == 1) {
      output <- data.frame("headline" = news_titles, "description" = news_desc, "source" = news_source, "date" = news_date)
    } else {
      output <- rbind(output, data.frame("headline" = news_titles, "description" = news_desc, "source" = news_source, "date" = news_date))
    }
  }
  Sys.sleep(runif(1,1,3))
  status <- paste(fortnightstart, " has been DONE for ", keyword)
  print(status)
  return(output)
}

# for each fortnight scrape google news :-> 4G | IP change after 90 reqs

temp <- fortnightly_news_sample %>%
  filter(`4G` == 1) %$%
  fortnightstart[1:90] %>%
  data.table() %>%
  mutate(articles_FourG = lapply(., get_google_news, keyword = q1, pages = 1 )) 

colnames(temp)[1] <- "fortnightstart"

fortnightly_news_sample <- right_join(temp,fortnightly_news_sample)

fourG_articles <- rbindlist(fortnightly_news_sample$articles_FourG, fill = TRUE, idcol = TRUE)
write_csv(fourG_articles, "fortnightlynews_4g_1.csv")

# Second batch for 4G articles

temp <- fortnightly_news_sample %>%
  filter(`4G` == 1) %$%
  fortnightstart[91:138] %>%
  data.table() %>%
  mutate(articles_FourG = lapply(., get_google_news, keyword = q1, pages = 1 )) 

colnames(temp)[1] <- "fortnightstart"

fortnightly_news_sample <- right_join(temp,fortnightly_news_sample)

fourG_articles <- rbindlist(fortnightly_news_sample$articles_FourG, fill = TRUE, idcol = TRUE)
write_csv(fourG_articles, "fortnightlynews_4g_2.csv")

# for each fortnight scrape google news :-> 5G

temp <- fortnightly_news_sample %>%
  filter(`5G` == 1) %$%
  fortnightstart %>%
  data.table() %>%
  mutate(articles_FiveG = lapply(., get_google_news, keyword = q2, pages = 1 )) 

colnames(temp)[1] <- "fortnightstart"

fortnightly_news_sample <- right_join(temp,fortnightly_news_sample)

fiveG_articles <- rbindlist(fortnightly_news_sample$articles_FiveG, fill = TRUE, idcol = TRUE)
write_csv(fiveG_articles, "fortnightlynews_5g.csv")





