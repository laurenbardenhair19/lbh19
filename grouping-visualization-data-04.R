
#Load dplyr for visulization preparation
library(dplyr)

#install.packages("tidyverse")
library(tidyverse)

#Load ggplot for datavisualizations
library(ggplot2)

#Color scheme for data visualizations 
library(RColorBrewer)


load("./results/keywordcount.RData")
load("./results/langcount.RData")
load("./results/flat_data.RData")

####------GROUPING THE DATA-----####

## (KEYWORDS) There are too many keywords to say something meaningful with the data. Thus, I will generate general 'topic' categories where multiple related keywords can be grouped and analyzed. 

RevisionistHistory <- list("Adolf Hitler", "Historical Revisionism", "Molotov-Ribbentrop Pact", "Nazi/Fascist", "USSR", "WWII")
EasternEurope <- list("Baltic States", "Colour revolutions", "Eastern Partnership")
Ukraine <- list("Abandoned Ukraine", "Azov Sea", "Crimea", "Crimean Tartars", "Donbas", "Eastern Ukraine", "Euromaidan", "illegal annexation", "Kerch", "Minsk agreements", "Ukraine", "Ukrainian disintegration", "Ukrainian statehood", "Volodymyr Zelensky", "War in Ukraine")
Russia <- list("Anti-Russian", "Destabilising Russia", "Encircling Russia", "Ethnic Russians", "Russian expansionism", "Russian language", "Russian superiority", "Russian world", "Russo-Georgian War", "Russophobia", "Vladimir Putin")
EU_NATO <- list("AA/DCFTA", "Brexit", "EU disintegration", "EU elections 2019", "EU/NATO enlargement", "Euro-scepticism", "Europe", "European Parliament", "European Union", "European Values", "migration", "Migration crisis", "NATO", "Refugees")
US <- list("Barack Obama", "CIA", "Deep state", "Donald Trump", "Hillary Clinton", "INF Treaty", "Robert Muelller", "US", "US presence in Europe")
Elections <- list("election meddling", "Elections", "Manipulated elections/referendum")
Syria_ME <- list("Bashar al-Assad", "Biological weapons", "Chemical weapons/attack", "Daesh", "Douma", "Middle East", "Muslim/Islam", "Syrian War", "White Helmets")

#Now I will generate a dataframe for these categories composed of keywords and associated count tallies. The new category for these variables will be called "Topics". 
topiccountdata <- data.frame("key" = keys(keywordcount), "count" = values(keywordcount))
print(topiccountdata)

#Assigns a topic name to topiccountdata$topic, if the keyword is in one of the defined lists
topiccountdata[topiccountdata$key %in% RevisionistHistory, "topic"] <- "Revisionist History"
topiccountdata[topiccountdata$key %in% EasternEurope, "topic"] <- "Eastern Europe"
topiccountdata[topiccountdata$key %in% Ukraine, "topic"] <- "Ukraine"
topiccountdata[topiccountdata$key %in% Russia, "topic"] <- "Russia"
topiccountdata[topiccountdata$key %in% EU_NATO, "topic"] <- "EU NATO"
topiccountdata[topiccountdata$key %in% US, "topic"] <- "US"
topiccountdata[topiccountdata$key %in% Elections, "topic"] <- "Elections"
topiccountdata[topiccountdata$key %in% Syria_ME, "topic"] <- "Syria and the Middle East"

#Now I want to look at the frequency by topic by first making a table, dft_topic
dft_topic <- as.tbl(topiccountdata)
print(dft_topic)

#Now I want to group by topic once the "NA" responses are dropped and tally
topic_freq <- dft_topic %>% drop_na()
topic_freq_filtered <- (topic_freq %>% group_by(topic) %>% summarise(count=sum(count)))


#####-----Data Visualization (Pie Chart, Topic) -----####

#Here I want to generate a pie chart that shows the frequency of each topic in relation to all the topics. First I calculate the percents
piepercentt<- round(100*topic_freq_filtered$count/sum(topic_freq_filtered$count), 1)


# I used the Brewer color package to give me more color selection in my visualizations
display.brewer.all()
brewer.pal(n = 8, name = 'Paired')


#Here I generate a pie chart with labels and a legend included. 
topicspie <-pie(piepercentt, labels = piepercentt, clockwise = TRUE, radius = .5, main="Pie Chart of Topic Frequencies by Percent", col=brewer.pal(n=8, name ="Paired"))
legend("bottomright", c("Eastern Europe", "Elections", "EU NATO", "Revisionist History", "Russia", "Syria and the Middle East", "Ukraine", "US"), fill=brewer.pal(n=8, name = "Paired"), cex=.7) 


## ----(LANGUGAGES) Again there are too many languages here, so I will group them by region. This will allow me to look at which areas of the world are targeted the most.

MiddleEastLanguages <- list("Arabic")
USandUK <- list("English")
WesternEuropeanLanguages <- list("Spanish", "German", "French", "Italian", "Finnish", "Spanish, Castilian", "Swedish")
EasternEuropeanLanguages <- list("Russian", "Ukrainian", "Czech", "Belarusian", "blr", "bgr", "Bosnian", "Croation", "Estonian", "Latvian", "Hungarian", "Lithuanian", "ltu", "lva", "Macedonian", "mda", "mne", "Moldavian", "Polish", "rou", "Serbian", "srb")
CentralAsiaLanguages <- list("Abkhazian", "Armenian", "Azerbaijani")

#generate a dataframe for these categories. The categories will be listed in new variable 'region'. 
langcountdata <- data.frame("lang" = keys(langcount), "count" = values(langcount))

#Again, assigns a region name to langcountdata$lang, if the language is in one of the defined regions above
langcountdata[langcountdata$lang %in% MiddleEastLanguages, "region"] <- "Middle East"
langcountdata[langcountdata$lang %in% WesternEuropeanLanguages, "region"] <- "Western Europe"
langcountdata[langcountdata$lang %in% EasternEuropeanLanguages , "region"] <- "Eastern Europe"
langcountdata[langcountdata$lang %in% USandUK , "region"] <- "USA UK"
langcountdata[langcountdata$lang %in% CentralAsiaLanguages , "region"] <- "Central Asia"

#Just putting this data in table called dft_lang.
dft_lang <- as.tbl(langcountdata)

#Here I'm using dyplr to summarise a subset of the data (in this case, all the regions)
dft_lang %>% group_by(region) %>% summarise(count=sum(count))
#dropping the "NA" observations here 
dft_lang <- dft_lang %>% drop_na()
langfreq <- dft_lang %>% group_by(region) %>% summarise(count=sum(count))


###----- Data Visualization (Pie Chart, Language)  ---##

#Caluclate the percents and figure out the order of regions that needs to be listed in the legend
piepercent<- round(100*langfreq$count/sum(langfreq$count), 1)
print(piepercent)
print(langfreq$count)
print(langfreq$region)

# I used the Brewer color package to give me more color selection in my visualizations
display.brewer.all()
brewer.pal(n = 8, name = 'BuGn')

#Below I create the pie chart with labels and legend. 
regionspie <-pie(piepercent, labels = piepercent, clockwise = TRUE, radius = .5, main="Pie Chart of Region Frequencies by Percent", col=brewer.pal(n=5, name ="BuGn"))
legend("topright", c("Central Asia", "Eastern Europe", "Middle East", "US and UK", "Western Europe"), fill=brewer.pal(n=5, name = "BuGn"))


### ---ANALYSIS BY YEAR----- Now I want to look at frequencies of all these variables over time ###

# . Here I create two functions that take one observation and assign a true or false value if the condition is met (whether it matches the country or language given). 

#This function takes one country name and a list of countries. It returns 'True' if the country is in the list. 
match_country_location <-function(locs, country) {
  return(country %in% locs)
}

#This function takes one language and a list of languages. It returns 'True' if the language is in the list. 
match_language <-function(langs, language) {
  return(language %in% langs)
}

#Below, the functions above are used to go through all the data (for either locations or languages) looking for matches to the specified countries and languages. I also put the findings in a flat data format. 
flat_data$ukr_loc <- lapply(flat_data$locations, country="Ukraine", match_country_location)
flat_data$latvia_loc <- lapply(flat_data$locations, country="Latvia", match_country_location)
flat_data$rus_lang <- lapply(flat_data$languages, language="Russian", match_language)
flat_data$arb_lang <- lapply(flat_data$languages, language="Arabic", match_language)
flat_data$eng_lang <- lapply(flat_data$languages, language="English", match_language)

# Now I'm creating a new dataframe called "dft" for these new variables. I'm also unlisting them. 
dft <- as.tbl(data.frame("year"=unlist(flat_data$year), "month"=unlist(flat_data$month), "ukr"=unlist(flat_data$ukr_loc), "latvia"=unlist(flat_data$latvia_loc), "russian"=unlist(flat_data$rus_lang), "arabic"=unlist(flat_data$arb_lang), "english"=unlist(flat_data$eng_lang)))

#Here I am filtering the dataframe for the countries or languages I want to review over time and then counting (tallying) the number of times it was referenced by year. 
dft %>% filter(latvia == TRUE) %>% group_by(year) %>% tally()
dft %>% filter(russian == TRUE) %>% group_by(year) %>% tally()
dft %>% filter(arabic == TRUE) %>% group_by(year) %>% tally()
dft %>% filter(english == TRUE) %>% group_by(year) %>% tally()
dft %>% filter(ukr == TRUE) %>% group_by(year) %>% tally()


#Get a count by year, no filters here
dft %>% group_by(year) %>% tally()

#Here I am doing the same thing for Ukraine but defining the result as dft_year for ease of visualization coding. 
dft_year <-dft %>% filter(ukr == TRUE) %>% group_by(year) %>% tally()


####----DATA VISUALIZATION  (Histogram, Ukraine over Time) ---- this is indicating the number of times Ukraine was mentioned in a disinformation article each year since 2015. 

ukrainetime<-ggplot(data=dft_year, aes(x=year, y=n)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()

#Now I will relabel the axis to 'Year' and 'Number of Articles'. I also add a heading. 
newukrainetime <- ukrainetime + ggtitle("Disinformation Articles About Ukraine by Year") +
  xlab("Year (2015-2019)") + ylab("Number of Articles") 
print(newukrainetime)

#Ukrainian elections took place in 2016 and 2019. Lines could be added to designate these dates but I don't think it's necessary here. 

#Now, to see if there is a trend of increasing disinformation articles over time, I tell the code to look back at dft but not to filter by country or language. This should give me all the number of disinformation articles across years. 

dft_year_all <-dft  %>% group_by(year) %>% tally()

#I then visualize the results with a histogram

#This one is a test, will add labels below 
Frequency_all<-ggplot(data=dft_year_all, aes(x=year, y=n)) +
  geom_bar(stat="identity", fill="lightgray")+
  theme_minimal()
print(Frequency_all)

allovertime <- Frequency_all + ggtitle("Number of Disinformation Articles Over Time") +
  xlab("Year (2015-2019)") + ylab("Number of Articles") 
print(allovertime)