#Install API and load
#install.packages("devtools")
#devtools::install_github("corriebar/euvsdisinfoR")
#install.packages("RColorBrewer")
library(euvsdisinfoR)


#Run API, will take some time to download everything
d <- disinfo()
alldata <- d %>%
  add_all("all")

save(alldata, file="./data/alldata.RData")
#also need to include the get_creative_works table
#get_creative_works(pages="all")

