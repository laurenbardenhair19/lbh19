
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
This repository contains three files for my code
1. download-data-01.R  Loads data fron EUvsDisinfo API
2. process-data-02.R             Flattens the data and changes ID#s to strings 
3. counting-data-03.R                   Loops through each observation tallying times each outcome 
4. grouping-visualization-data-04.R      Groups outcomes (keywords, lang area, year) for analysis and visualization

### Data\
EUvsDisinfo website. This represents the data collection of a disinformation task force based in Europe. 
Dataset began in 2015 and is updated weekly. https://euvsdisinfo.eu/

### Results\
regional-language-target-01.jpg     percentage of regional publication languages as percentage of all publications
keyword-target-02.jpg               percentage of topic keywords as percentage of all general topics
ukraine-over-time-03.jpg            number of times Ukraine a keyword in each year 
all-over-time-04.jpg                number of total claims in each year 





