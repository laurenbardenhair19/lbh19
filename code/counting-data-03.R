
#Need to use Hash in this section 
#install.packages("hash")
library(hash)

#load the data from previous script
load("./data/flat_data.RData")

###COUNTING - Now we want to know how many times a keyword, location, or language is used across all claim observations, in aggregate. 

### --- LOCATION - number of disinfo events referencing individual countries (country is mentioned in the article) ###

#Here I will calculate the number of disinfo events on a per-country basis and Create a dictionary/hash to keep track of how many times each country is referenced

#initializing an empty hash
countrycount <- hash()

# this loop looks through each observation looking at locations. If the location has not been named before, it adds it and gives it a count value of 1. If the location has been named before, it adds a value of 1 to that location's existing count. 

for (locs in flat_data$locations) {
  for (loc in locs) {
    if (is.null(countrycount[[loc]])) {
      countrycount[[loc]] <- 1
    } else{
      countrycount[[loc]] <- countrycount[[loc]] + 1
    }
  }
}

#Now printing out the countries and associated counts with a colon between for ease of reading. The function 'cat' prints multiple items:
for (k in keys(countrycount)) {
  cat(k, ":",  countrycount[[k]], "\n")
}

###---KEYWORD--- Number of Disinfo Events by Keyword####

#Calculate the number of disinfo events by keyword topic
#Create a dictionary/hash to keep track of how many times each keyword is referenced

#initializing an empty hash
keywordcount <- hash()

#Again, this loop goes through all the keywords. If the keyword has not been named before, it adds it and gives it a count value of 1. If it has already been named, it adds a tally of 1 to its existing value.
for (keywords in flat_data$keywords) {
  for (keyword in keywords) {
    if (is.null(keywordcount[[keyword]])) {
      keywordcount[[keyword]] <- 1
    } else{
      keywordcount[[keyword]] <- keywordcount[[keyword]] + 1
    }
  }
}

#Initialize three lists to parse out keyword and count variables 
keyword_tracker <- list()
keyword_tracker$key <- list()
keyword_tracker$count <- list()

# transfering the original dictionary/hash table into the two lists. 
#Iterate and assign these to the list so they are easier to work with
#Too many keywords to print, so will need to visualize
for (k in ls(keywordcount)) {
  keyword_tracker$key <- c(keyword_tracker$key, unlist(k))
  keyword_tracker$count <- c(keyword_tracker$count, unlist(keywordcount[[k]]))
  #cat(k, ":",  keywordcount[[k]], "\n")
}

# R seems to have trouble looking at individual values vs lists so easier to "unlist" the variables. 'List of lists' to 'list of numbers.
keyword_tracker$key <- unlist(keyword_tracker$key)
keyword_tracker$count <- unlist(keyword_tracker$count)

#run summary to get some summary statistics on the count variable
summary <- summary(keyword_tracker$count)
print(summary)

#Since a lot of keywords are only mentioned once, I only want print the keywords mentioned more than the mean number of times, otherwise
#there are too many. Below I calculated the median and mean to see what would be the best threshold to use. 
median <- median(keyword_tracker$count)
mean <- mean(keyword_tracker$count)
#I decided on using the mean 

#loop goes through the vector keyword tracker to look at all keywords. Prints the keyword and its corresponding number of counts if the count number is greater than the mean number of counts. In this case the mean is 29.6 counts in the data (since 2015). 
for (i in 1:length(keyword_tracker$key)) {
  if (keyword_tracker$count[[i]] > mean) {
    cat(keyword_tracker$key[[i]], ":", keyword_tracker$count[[i]], "\n")
  }
  

}

## LANGUAGE - Number of Disinfo Events by Language###

#Here I am again generating a dictionary/hash table 
langcount <- hash()


for (langs in flat_data$languages) {
  for (lang in unlist(langs)) {
    #print(lang)

    #Again, if the language has already been listed, a 1 value is added to the existing total. If the language has not been listed, it is added and given a tally of 1. 
    if (is.null(langcount[[lang]])) {
      langcount[[lang]] <- 1
    } else{
      langcount[[lang]] <- langcount[[lang]] + 1
    }
  }
}

#Here I am printing out the counts in text form. The function cat prints multiple items:
for (lang in keys(langcount)) {
  cat(lang, ":",  langcount[[lang]], "\n")
}


save(keywordcount, file="./data/keywordcount.RData")
save(langcount, file="./data/langcount.RData")
save(flat_data, file="./data/flat_data.RData")

