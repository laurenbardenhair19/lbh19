#Install API and load
#install.packages("devtools")
#devtools::install_github("corriebar/euvsdisinfoR")
library(euvsdisinfoR)

#Load some other libraries that will be useful
#install.packages("hash")
library(hash)

#Run API, will take some time to download everything
d <- disinfo()
alldata <- d %>%
  add_all("all")
get_creative_works(pages="all")

#Flatten the data by pulling from 'all data' frame and then the sub dataframes to put desired variables into one list
flat_data <- list()
flat_data$id <- alldata$claims$claims_id
flat_data$date <- alldata$claims$claim_published
flat_data$keyword_ids <- alldata$claims$keywords
flat_data$location_ids <- alldata$claims$content_locations
flat_data$title <- alldata$reviews$claim_reviewed
flat_data$creativeworks_ids <- alldata$claims$appearances


#I am specifically looking to make the data more manageable by linking keywords and keyword ids, locations and location ids, languages and language ids, and dates together so analysis is easier. 

#-----KEYWORDS-------###

#Change keyword_id into keywords using alldata$keywords as conversion lookup table to retrieve keywords
getkeyword <- alldata$keywords$keyword_name
names(getkeyword) <- alldata$keywords$keyword_id

#create conversion function using getkeyword
convert_keyword_id <- function(k, lookup = getkeyword) {
  return(unname(lookup[k]))
}

#deal with some observations having multiple keywords
#process one at a time using lapply
convert_keyword_id_list <- function(ks, convert_func = convert_keyword_id) {
  return(lapply(ks, convert_keyword_id))
}

#now we feed the original keyword id data into the function so instead of lists of numbers associated with each observation, we have a list of key words associated with each observation 
flat_data$keywords = lapply(flat_data$keyword_ids, convert_keyword_id_list)

#-------LOCATIONS------####Locations refers to the nations mentioned in the news articles

#Change location_id into locations using alldata$locations as lookup table
getlocation <- alldata$countries$country_name
names(getlocation) <- alldata$countries$country_id

#And now we complete the same process we did with the previous two variables. 
convert_location_id <- function(loc, lookup = getlocation) {
  return(unname(lookup[loc]))
}

convert_location_id_list <- function(locs, convert_func = convert_location_id) {
  return(lapply(locs, convert_location_id))
}

#apply to all data locations using lapply
flat_data$locations <- lapply(flat_data$location_ids, convert_location_id_list)


###COUNTING - Now we want to know how many times a keyword, location, or language is used in the articles, in aggregate. One at a time, we need to generate code to count the number of times each unique term is used. 

###  LOCATION - number of disinfo events on a per-country basis (country is mentioned in the article) ###

#Calculate the number of disinfo events on a per-country basis
#Create a dictionary/hash to keep track of how many times each country is referenced
countrycount <- hash()

#below, this loop will go through each observation looking at location. If the location has not been named before, it adds it and gives it a count value of 1. If the location has been named before, it adds a value of 1 to that location's existing count. 

for (locs in flat_data$locations) {
  for (loc in locs) {
    if (is.null(countrycount[[loc]])) {
      countrycount[[loc]] <- 1
    } else{
      countrycount[[loc]] <- countrycount[[loc]] + 1
    }
  }
}

#Now printing out the counts in text form. The function 'cat' prints multiple items:
for (k in keys(countrycount)) {
  cat(k, ":",  countrycount[[k]], "\n")
}

###---KEYWORD--- Number of Disinfo Events by Keyword####

#Calculate the number of disinfo events by keyword topic
#Create a dictionary/hash to keep track of how many times each keyword is referenced
keywordcount <- hash()

for (keywords in flat_data$keywords) {
  for (keyword in keywords) {
    if (is.null(keywordcount[[keyword]])) {
      keywordcount[[keyword]] <- 1
    } else{
      keywordcount[[keyword]] <- keywordcount[[keyword]] + 1
    }
  }
}

keyword_tracker <- list()
keyword_tracker$key <- list()
keyword_tracker$count <- list()

# transfering the original dictionary/hash table into two lists. 
#Iterate and assign these to the list so they are easier to work with
#Too many keywords to print, so will need to visualize
for (k in ls(keywordcount)) {
  keyword_tracker$key <- c(keyword_tracker$key, unlist(k))
  keyword_tracker$count <- c(keyword_tracker$count, unlist(keywordcount[[k]]))
  #cat(k, ":",  keywordcount[[k]], "\n")
}

#Something weird was going on with the count variable. R has trouble looking at individual values vs lists so easier to "unlist" the variables. 'List of lists' to 'list of numbers'.  
keyword_tracker$key <- unlist(keyword_tracker$key)
keyword_tracker$count <- unlist(keyword_tracker$count)

#run summary to get some summary statistics
summary <- summary(keyword_tracker$count)
print(summary)

#Since a lot of keywords are only mentioned once, I only want print the keywords mentioned more than the mean number of times, otherwise
#there are too many
median <- median(keyword_tracker$count)
mean <- mean(keyword_tracker$count)

#print the keyword and its corresponding number of counts if the count number is greater than the mean number of counts. In this case the mean is 29.6 counts in the data (since 2015). 
for (i in 1:length(keyword_tracker$key)) {
  if (keyword_tracker$count[[i]] > mean) {
    cat(keyword_tracker$key[[i]], ":", keyword_tracker$count[[i]], "\n")
  }
}

#----LANGUAGES---Number of disinfo events by target language###
getlanguage_id <- alldata$creative_works$in_language
names(getlanguage_id) <- alldata$creative_works$creative_work_id

getlanguage <- alldata$languages$language_name
names(getlanguage) <- alldata$languages$language_id

#create conversion function
convert_creativeworks_id <- function(cw, lookup1 = getlanguage_id, lookup2 = getlanguage) {
  #print(lookup1[[cw]])
  if (is.na(lookup1[[cw]])) {
    return(list("None"))
  } else{
    lang_id <- unname(lookup1[[cw]])
    lang_name <- unname(lookup2[[lang_id]])
    return(lang_name)
  }
}

convert_creativeworks_id_list <- function(cw, convert_func = convert_creativeworks_id) {
  return(lapply(cw, convert_creativeworks_id))
}

flat_data$languages <- lapply(flat_data$creativeworks_ids, convert_creativeworks_id_list)
flat_data$languages <- unlist(flat_data$languages)


langcount <- hash()

for (langs in flat_data$languages) {
  for (lang in langs) {
    print(lang)
    if (is.null(langcount[[lang]])) {
      langcount[[lang]] <- 1
    } else{
      langcount[[lang]] <- langcount[[lang]] + 1
    }
  }
}

#Now printing out the counts in text form. The function cat prints multiple items:
for (lang in keys(langcount)) {
  cat(lang, ":",  langcount[[lang]], "\n")
}

##Again there are too many languages here, so I will group them by region, and then also look at the languages with total counts in the upper quartile. This will allow me to look at which languages are targeted the most. 

MiddleEastLanguages <- list("Arabic")
EuropeanLanguages <- list("Spanish", "German", "French")
SlavicLanguages <- list("Russian", "Ukrainian")

langcountdata <- data.frame("lang" = keys(langcount), "count" = values(langcount))
langcountdata[langcountdata$lang %in% MiddleEastLanguages, "region"] <- 1
langcountdata[langcountdata$lang %in% EuropeanLanguages, "region"] <- 2
langcountdata[langcountdata$lang %in% SlavicLanguages, "region"] <- 3


