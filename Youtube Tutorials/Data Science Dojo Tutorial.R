"Video link: https://www.youtube.com/watch?v=smriIBG08ok"

library(rvest)
library(dplyr)

link <- "https://www.marketwatch.com/story/bitcoin-jumps-after-credit-scare-2018-10-15"
page_mw <- read_html(link)

#Grabbing the title
title <- page_mw %>% html_node("title") %>% html_text()


#If we use html_nodes instead the output is a list with 3 elements
page_mw %>% html_nodes("title") %>% html_text()

#Grabbing the body
body <- page_mw %>% html_nodes("p") %>% html_text()
page_mw %>% html_node("body") %>% html_text()

"We take every single paragraph in the HTML source code. If we take the body
node instead it will output the whole body"
rm(page_mw, title, body)

--------------------------------------------------------------------------------

#Scraping the most recent articles about bitcoin
link <- "https://www.marketwatch.com/search?q=bitcoin&Link=MW_SearchAC&mod=keyword_search"
page_bitcoin_article <- read_html(link)

#Grabbing the article titles
title_article <- page_bitcoin_article %>%
                  html_nodes(".searchresult a") %>%
                  html_text()

#Grabbing their links
link_article <- page_bitcoin_article %>%
                  html_nodes(".searchresult a") %>%
                  html_attr("href")

#Grabbing the date time publish
datetime_article <- page_bitcoin_article %>%
                    html_nodes(".resultlist span") %>%
                    html_text()

#Cleaning the datetime into the correct format
library(lubridate)

datetime_article <- datetime_article %>%
                    gsub(pattern = "\\.", replacement = "") %>%
                    parse_date_time(orders = "%I:%M %p %m/%d/%Y") %>%
                    ymd_hms(tz = "US/Eastern") %>%
                    with_tz(tzone = "GMT")

#Calculating how long the article has been up
age_article <- difftime(Sys.time(), datetime_article, units = "hours")

#Grabbing the writer
get_writer <- function(link){
  page <- read_html(link)
  writer <- page %>% html_node(".hasMenu h4") %>% html_text()
}

"For some reason, if the type of content is 'article', the link does not
include the http part. if the type is 'story', the link works just fine.
We will have to correct the links first"

for (i in 1:length(link_article)) {
  if (!grepl(pattern = "http://www.marketwatch.com", link_article[i])) {
    link_article[i] <- paste("http://www.marketwatch.com",
                             link_article[i],
                             sep = "")
  }
}
rm(i)

writer_article <- sapply(link_article, get_writer, USE.NAMES = FALSE)

#Creating a dataframe to hold all the values
bitcoin_articles <- data.frame(Title = title_article,
                               Writer = writer_article,
                               Time = datetime_article,
                               Hours_ago = age_article,
                               Link = link_article)

#Sending the result in an email
library(mailR)

password <- as.character("nurqajummfqxzgbi")

from <- "jonathanryan576@gmail.com"
to <- c("jonathanryan576@gmail.com")
subject <- "Market Watch Bitcoin Articles Scraping Result"
body <- "bitcoin_articles"

smtp <- list(host.name = "smtp.gmail.com", 
             port = 465,
             html = TRUE,
             user.name = "jonathanryan576@gmail.com",
             passwd = password,
             ssl = TRUE)

send.mail(from, to, subject, body, smtp, authenticate = TRUE, send = TRUE)

#Trying again
library(mailR)
sender <- "jonathanryan576@gmail.com"
recipients <- c("jonathanryan576@gmail.com")
send.mail(from = sender,
          to = recipients,
          subject = "Subject of the email",
          body = "Body of the email",
          smtp = list(host.name = "smtp.gmail.com", port = 587, 
                      user.name = "jonathanryan576@gmail.com",            
                      passwd = "nurqajummfqxzgbi", tls = TRUE),
          authenticate = TRUE,
          send = TRUE)


#Trying the sendmailR library
library(sendmailR)

from <- "jonathanryan576@gmail.com"
to <- c("jonathanryan576@gmail.com")
subject <- "Market Watch Bitcoin Articles Scraping Result"
body <- "bitcoin_articles"
mailcontrol <- list(smtpServer = "ASPMX.L.GOOGLE.COM")
                  

sendmail(from = from,
         to = to,
         subject = subject,
         msg = body,
         control = mailcontrol)

"It does not work."
