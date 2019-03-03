###String Data
install.packages("jsonlite")
library(jsonlite)

trump <- fromJSON('http://trumptwitterarchive.com/data/realdonaldtrump/2019.json')

head(trump)
str(trump)
trump[1,'text']

"I'm a student. He says 'it is ok'!" #works
"I'm a student. He says \"it is ok\"!" 
cat("I'm a student. He says \"it is ok\"!") #to prevent quotes from causing errors

test <- "This is the first line. \nThis the \t second line with a tab." #new line and add tab
cat(test)

tweet1 <- "MAKE AMERICA GREAT AGAIN!"
tweet2 <- 'Congratulations @ClemsonFB! https://t.co/w8viax0OWY'

str_length(tweet1) #length of tweet
str_split(tweet1,pattern = " ") -> words #count words but list form
str_c(unlist(words),collapse = " ") #all words in dataset form

tolower(tweet1) #lowercase vs toUpper()

str_sub(tweet1,start=1,end = 4) #subsetting pieces

grepl(pattern = "test", "test this") #search for pattern in string

table(trump$source)
hist(str_length(trump$text), main= "Trump Tweet Length")
plot(str_length(trump$text), main= "Trump Tweet Length",type = "o")

trumpslit <- str_split(trump$text,pattern=" ")
lengths(trumpslit)
hist(lengths(trumpslit[!trump$is_retweet]))

unlist(trumpslit)

trump.split <- unlist(str_split(trump$text, pattern = " "))
wordfreq <- sort(table(trump.split), decreasing = TRUE) #frequency of words
barplot(wordfreq[1:10],las=2)

str_view_all(trump$text,"MAKE AMERICA") #find words in texts

tweet <- '{"source": "Twitter for iPhone", "id_str": "1085519375224983552", "text": "It is becoming more and more obvious that the Radical Democrats are a Party of open borders and crime. They want nothing to do with the major Humanitarian Crisis on our Southern Border. #2020!", "created_at": "Wed Jan 16 12:49:21 +0000 2019", "retweet_count": 22738, "in_reply_to_user_id_str": null, "favorite_count": 88796, "is_retweet": false}'

str_view_all(tweet,"\\bis\\b") #whole word
str_view_all(tweet,"\\Bis\\b") #not whole word

str_view_all(tweet,"[tT]hey")  #multiple cases
str_view_all(tweet,"(?i)they") #ignore case

trump2018 <- stream_in(file("trump2018.json"))
head(trump2018)
str(trump2018)
#trump tweets 3510 times during the year of 2018

trump2018 %>% filter(is_retweet==TRUE) %>% summarise(n(),n()/3510)
#464 of trump's tweets are retweets i.e. 13.22%

trump2018 %>% filter(is_retweet==FALSE) %>% sample_n(size=100, replace = FALSE) -> tt18

strptime(tt18$created_at, "%a %b %d %H:%M:%S %z %Y") -> ttdate
length(unique(format(ttdate,"%Y-%m-%d"))) #the number of days the sample of tweets were sent over

table(format(ttdate,"%a")) #tweets grouped by days of the week

ntweet <- lengths(str_split(tt18$text,pattern = " ")) #number of words per tweet
ntweet


###Language Analysis
trump.api <- tibble(id=1:nrow(tt18),text=tt18$text)
trump.json <- toJSON(list(documents=trump.api))

cogapikey <- ##insert API key here in "string format"
apiurl <- "https://westus.api.cognitive.microsoft.com/text/analytics/v2.0/sentiment"

#Detect language: https://westus.api.cognitive.microsoft.com/text/analytics/v2.0/languages
#Key phrases: https://westus.api.cognitive.microsoft.com/text/analytics/v2.0/keyPhrases
#Linked entities: https://westus.api.cognitive.microsoft.com/text/analytics/v2.0/entities
#Sentiment: https://westus.api.cognitive.microsoft.com/text/analytics/v2.0/sentiment

install.packages("httr")
library(httr)

tj.out <- POST(apiurl,add_headers(`Ocp-Apim-Subscription-Key` = cogapikey, 
                                  `Content-Type` = "application/json"), body = trump.json)

tj.out %>% content() %>% flatten_df() -> sentiment #sentiment scores
sentiment

tcor <- cbind(sentiment$score,ntweet,tt18$retweet_count,tt18$favorite_count)
colnames(tcor) <- c("sentiment","words","retweets","favourites")

library(Hmisc)
tcor.m <- rcorr(tcor)
tcor.m$r #correlation matrix

