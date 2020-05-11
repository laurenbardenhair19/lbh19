
## Short Description:

This project loads data using the EUvsDisinfo API. It merges/flattens the data and cleans it for analysis. 
It uses various functions to loop through each observation, tallying lists of outcomes associated with each 
observation. These tallies are presented in dictionary/hash tables for easy interpretation. 

This project also analyzes and visualizes the EUvsDisinfo data by grouping outcomes in general categories. These
categories are drawn up according to language region, topic, and year. The visualizations reflect the summary 
statistics associated with these groupings. 


## Dependencies/Installation:

I used RStudio Version 1.1.463

This project requires accessing the data through downloading the following package. 

install.packages("devtools")
devtools::install_github("corriebar/euvsdisinfoR")

For cleaning, analysis, and visualization, the following packages are needed. 

#install.packages("RColorBrewer")
#install.packages("hash")
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("ggplot2")


## Files in this repository:


### Code\
This repository contains four files for my code
1. download-data-01.R  Loads data fron EUvsDisinfo API
2. process-data-02.R             Flattens the data and changes ID#s to strings 
3. counting-data-03.R                   Loops through each observation tallying times each outcome 
4. grouping-visualization-data-04.R      Groups outcomes (keywords, lang area, year) for analysis and visualization

### Data\
EUvsDisinfo website. This represents the data collection of a disinformation task force based in Europe. 
Dataset began in 2015 and is updated weekly. https://euvsdisinfo.eu/

1. alldata.RData is the data directly from the API 
2. flat_data.RData is the saved data after it has been flattened, cleaned, and merged
3. keywordcount.RData has the saved keywords and tallies
4. langcount.RData has the saved languages and tallies 

### Results\
1. pie_regions.png             percentage of regional publication languages as percentage of all publications
2. pie_topics.png               percentage of topic keywords as percentage of all general topics
3. ukraine.png                  number of times Ukraine is a keyword in each year 
4. frequency_all.png             number of total claims in each year 





