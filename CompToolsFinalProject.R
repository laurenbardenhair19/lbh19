#Install API and load
#install.packages("devtools")
#devtools::install_github("corriebar/euvsdisinfoR")
#install.packages("RColorBrewer")
library(euvsdisinfoR)

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


#Run API, will take some time to download everything
d <- disinfo()
alldata <- d %>%
  add_all("all")
get_creative_works(pages="all")

#Flatten the data by pulling from 'all data' frame and then the sub dataframes to put desired variables into one list
flat_data <- list()
flat_data$id <- alldata$claims$claims_id
flat_data$keyword_ids <- alldata$claims$keywords
flat_data$location_ids <- alldata$claims$content_locations
flat_data$title <- alldata$reviews$claim_reviewed
flat_data$creativeworks_ids <- alldata$claims$appearances
flat_data$year <- substr(alldata$claims$claim_published,1, 4)
flat_data$month <- substr(alldata$claims$claim_published,6, 7)

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
flat_data$all_languages <- unlist(flat_data$languages)

#Here I am again generating a dictionary/hash table that displays all the languages and the number of times they have been a published language for the disinformation articles. 
langcount <- hash()

for (langs in flat_data$languages) {
  for (lang in unlist(langs)) {
    #print(lang)
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

 

####------ANALYZING THE DATA-----####

## (KEYWORDS) There are too many keywords to say something meaningful with the data. Thus, I will generate general 'topic' categories where multiple related keywords can be grouped and analyzed. 

RevisionistHisroty <- list("Adolf Hitler", "Historical Revisionism", "Molotov-Ribbentrop Pact", "Nazi/Fascist", "USSR", "WWII")
EasternEurope <- list("Baltic States", "Colour revolutions", "Eastern Partnership")
Ukraine <- list("Abandoned Ukraine", "Azov Sea", "Crimea", "Crimean Tartars", "Donbas", "Eastern Ukraine", "Euromaidan", "illegal annexation", "Kerch", "Minsk agreements", "Ukraine", "Ukrainian disintegration", "Ukrainian statehood", "Volodymyr Zelensky", "War in Ukraine")


## (LANGUGAGES) Again there are too many languages here, so I will group them by region. This will allow me to look at which areas of the world are targeted the most.
MiddleEastLanguages <- list("Arabic")
USandUK <- list("English")
WesternEuropeanLanguages <- list("Spanish", "German", "French", "Italian", "Finnish", "Spanish, Castilian", "Swedish")
EasternEuropeanLanguages <- list("Russian", "Ukrainian", "Czech", "Belarusian", "blr", "bgr", "Bosnian", "Croation", "Estonian", "Latvian", "Hungarian", "Lithuanian", "ltu", "lva", "Macedonian", "mda", "mne", "Moldavian", "Polish", "rou", "Serbian", "srb")
CentralAsiaLanguages <- list("Abkhazian", "Armenian", "Azerbaijani")

langcountdata <- data.frame("lang" = keys(langcount), "count" = values(langcount))
langcountdata[langcountdata$lang %in% MiddleEastLanguages, "region"] <- "Middle East"
langcountdata[langcountdata$lang %in% EuropeanLanguages, "region"] <- "Western Europe"
langcountdata[langcountdata$lang %in% EasternEuropeanLanguages , "region"] <- "Eastern Europe"
langcountdata[langcountdata$lang %in% USandUK , "region"] <- "USA UK"
langcountdata[langcountdata$lang %in% CentralAsiaLanguages , "region"] <- "Central Asia"

#Just looking at the numbers for the new variable 'region'. This will tell me the frequency with which different regions have been targeted since 2015. 
dft_lang <- as.tbl(langcountdata)
#Here I'm using dyplr to summarise a subset of the data (in this case, all the regions)
dft_lang %>% group_by(region) %>% summarise(count=sum(count))
#dropping the "NA" observations here 
dft_lang <- dft_lang %>% drop_na()
langfreq <- dft_lang %>% group_by(region) %>% summarise(count=sum(count))


###----- Data Visualization (Pie Chart) for frequency of language use by language region ---##

#Caluclate the percents and figure out the order of regions that needs to be listed in the legend
piepercent<- round(100*langfreq$count/sum(langfreq$count), 1)
print(piepercent)
print(langfreq$count)
print(langfreq$region)

# I used the Brewer color package to give me more color selection in my visualizations
display.brewer.all()
brewer.pal(n = 8, name = 'BuGn')

#Below I create the pie chart and legend. 
regionspie <-pie(piepercent, labels = piepercent, clockwise = TRUE, radius = .5, main="Pie Chart of Region Frequencies by Percent", col=brewer.pal(n=5, name ="BuGn"))
legend("topright", c("Central Asia", "Eastern Europe", "Middle East", "US and UK", "Western Europe"), fill=brewer.pal(n=5, name = "BuGn"))


## Back to the other parts of the analysis...

### Now I want to look at the number of references per year, per keyword, per location/region, or published language over time ###
match_country_location <-function(locs, country="None") {
  return(country %in% locs)
}

match_language <-function(langs, language="None") {
  return(language %in% langs)
}

#Below, the lines are going through all the data (for either locations or languages) looking for matches to the specified string term, espcially in the observations with multiple locations or languages. It's putting the findings in a flat data format. 
flat_data$ukr_loc <- lapply(flat_data$locations, country="Ukraine", match_country_location)
flat_data$latvia_loc <- lapply(flat_data$locations, country="Latvia", match_country_location)
flat_data$rus_lang <- lapply(flat_data$languages, language="Russian", match_language)
flat_data$arb_lang <- lapply(flat_data$languages, language="Arabic", match_language)
flat_data$eng_lang <- lapply(flat_data$languages, language="English", match_language)

# Now I'm creating a new dataframe for these new variables using dyplr. 
dft <- as.tbl(data.frame("year"=unlist(flat_data$year), "month"=unlist(flat_data$month), "ukr"=unlist(flat_data$ukr_loc), "latvia"=unlist(flat_data$latvia_loc), "russian"=unlist(flat_data$rus_lang), "arabic"=unlist(flat_data$arb_lang), "english"=unlist(flat_data$eng_lang)))

#Here I am generating dummy variables for the countries or languages I want to review over time and then counting (tallying) the number of times the dummy turns up 1 in each year
dft %>% filter(latvia == TRUE) %>% group_by(year) %>% tally()
dft %>% filter(russian == TRUE) %>% group_by(year) %>% tally()
dft %>% filter(arabic == TRUE) %>% group_by(year) %>% tally()
dft %>% filter(english == TRUE) %>% group_by(year) %>% tally()
dft %>% filter(ukr == TRUE) %>% group_by(year) %>% tally()


#Below, I group the dataframe by year acros each dummy variable 
dft %>% group_by(year) %>% tally()
#Here is define dft_year as the grouping for Ukraine only 
dft_year <-dft %>% filter(ukr == TRUE) %>% group_by(year) %>% tally()


####----DATA VISUALIZATION ---- this is indicating the number of times Ukraine was mentioned in a disinformation article each year since 2015. 

ukrainetime<-ggplot(data=dft_year, aes(x=dft_year$year, y=dft_year$n)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()
#Now I will relabel the axis to 'Year' and 'Number of Articles'. I also add a heading. 
newukrainetime <- ukrainetime + ggtitle("Disinformation Articles About Ukraine by Year") +
  xlab("Year (2015-2019)") + ylab("Number of Articles") 
print(newukrainetime)

#Now, to see if there is a trend of increasing disinformation articles over time, I look at some target languages that have likely been monitored since 2015. I see if there is an increasing trend. 

#dft %>% group_by(year) %>% tally()
#Here I do the same thing I did for Ukraine here for articles printed in English
#dft_arabic <- dft %>% filter(arabic == TRUE) %>% group_by(year) %>% tally()

#arabictime<-ggplot(data=dft_arabic, aes(x=dft_arabic$year, y=dft_arabic$n)) +
  #geom_bar(stat="identity", fill="steelblue")+
  #theme_minimal()

#print(arabictime)

