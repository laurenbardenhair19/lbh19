#Load some other libraries that will be useful
#install.packages("hash")
library(hash)

#Load dplyr for visulization preparation
library(dplyr)

#
#install.packages("tidyverse")
library(tidyverse)

#Load ggplot for datavisualizations
library(ggplot2)

#Color scheme for data visualizations 
library(RColorBrewer)


#Load data
load("./data/alldata.RData")

#Flatten the data by pulling from 'all data' frame and then the sub dataframes to put desired variables into one list
flat_data <- list()
flat_data$id <- alldata$claims$claims_id
flat_data$keyword_ids <- alldata$claims$keywords
flat_data$location_ids <- alldata$claims$content_locations
flat_data$title <- alldata$reviews$claim_reviewed
flat_data$creativeworks_ids <- alldata$claims$appearances
flat_data$year <- substr(alldata$claims$claim_published,1, 4)
flat_data$month <- substr(alldata$claims$claim_published,6, 7)

#I am specifically looking to make the data more manageable by linking keywords and keyword ids, locations and location ids, languages and language ids, and dates together so analysis is easier. I'm going to go through each type of variable  one at a time, starting with keywords. 

#-----KEYWORDS-------###

#Create lookup table "getkeyword" that matches keyword ids with keyword names
getkeyword <- alldata$keywords$keyword_name
names(getkeyword) <- alldata$keywords$keyword_id

#create conversion function that takes in the keyword ID numbers and the getkeyword lookup table. It will match the keyword id to the associated keyword name
convert_keyword_id <- function(k, lookup = getkeyword) {
  return(unname(lookup[k]))
}

#deal with some observations having multiple keywords
#process one at a time using lapply. This function takes in keyword ids and the convert keyword id function and returns the list of keyword names associated with each observation claim. 
convert_keyword_id_list <- function(ks, convert_func = convert_keyword_id) {
  return(lapply(ks, convert_keyword_id))
}

#now we feed the original keyword id data into the function so instead of lists of numbers associated with each observation, we have a list of string keywords associated with each observation 
flat_data$keywords = lapply(flat_data$keyword_ids, convert_keyword_id_list)

print(flat_data$keywords)

#-------LOCATIONS------####Locations refers to the nations mentioned in the news articles

#Change location_id into locations using alldata$locations as lookup table "get location"
getlocation <- alldata$countries$country_name
names(getlocation) <- alldata$countries$country_id

#This function takes a "loc" or location ID and a lookup table "get location" and returns the associated location name in lookup
convert_location_id <- function(loc, lookup = getlocation) {
  return(unname(lookup[loc]))
}

#This function takes in a list of locations "locs" and the above, convert location id function, which converts individual location IDs to names. 
convert_location_id_list <- function(locs, convert_func = convert_location_id) {
  return(lapply(locs, convert_location_id))
}

#Here we are running the aforementioned functions. We are inputting the original location ids and using "convert location id list" to return all the text names associated with each observation
flat_data$locations <- lapply(flat_data$location_ids, convert_location_id_list)

#----LANGUAGES---Now I do the same for languages of publication###

#There is an extra step to generating the lookup table here because language data is referenced through another table, 'Creative Works'. Thus, I first need two lookup tables. 

#This first lookup table allows me to reference creative works IDs and their associated language IDs
getlanguage_id <- alldata$creative_works$in_language
names(getlanguage_id) <- alldata$creative_works$creative_work_id

#This lookup table allows me to reference language IDs and their associated names 
getlanguage <- alldata$languages$language_name
names(getlanguage) <- alldata$languages$language_id

# Now I can create another conversion function. This function takes in a creative work ID and the two lookup tables. It returns the langugage names associated with each creative work. 
convert_creativeworks_id <- function(cw, lookup1 = getlanguage_id, lookup2 = getlanguage) {
  #print(lookup1[[cw]])
  # This catches errors if the creative work lookup fails
  if (is.na(lookup1[[cw]])) {
    return(list("None"))
  } else{
    lang_id <- unname(lookup1[[cw]])
    lang_name <- unname(lookup2[[lang_id]])
    return(lang_name)
  }
}

#This function takes in the list of creative works associated with each claim observation and returns the associated list of language names. 
convert_creativeworks_id_list <- function(cw, convert_func = convert_creativeworks_id) {
  return(lapply(cw, convert_creativeworks_id))
}

#This applies the function "convert creative works id list" to each observation in the data
flat_data$languages <- lapply(flat_data$creativeworks_ids, convert_creativeworks_id_list)

#"unlist" formats the data properly
flat_data$all_languages <- unlist(flat_data$languages)

#save the flat data
save(flat_data, file = "./data/flat_data.RData")