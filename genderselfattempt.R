# Curating a database of names and gender from various government data sources

# reads in packages needed for the rest of the script
library(readxl)
library(babynames)
library(openxlsx)
library(dplyr)
library(stringr)
library(ukbabynames)

# SSA data

# reads in baby names from babynames package that includes historical babyname
# from the Social Security Administration (SSA)
SSA <- babynames
SSA <- data.frame("name" = SSA$name, "gender" = SSA$sex, "n" = SSA$n)

# condenses down the SSA dataset and groups each observation by name and gender
# this was needed to be done because the SSA included each year the name appeared
# in their records and the count
SSA <- SSA %>% group_by(name, gender) %>%
summarise(n = sum(n))
# After this is run, we have the total number of observations by name and gender

SSA[which(SSA$name == "John"),]
# This line grabs the rows where the name is John
# As we can see, there were 21,676 female Johns and 5,115,466 male Johns
# that were filed for a social security number


# UK baby names

# reads in babynames from the UK's Office of National Statistics
# follows the same process as the SSA babynames
ukbn <- ukbabynames
ukbn <- data.frame("name" = ukbn$name, "gender" = ukbn$sex, "n" = ukbn$n)
ukbn <- ukbn %>% group_by(name, gender) %>%
  summarise(n = sum(n))

# Ontario data 
# From data that I found for Ontario's babynames
# https://data.ontario.ca/dataset/ontario-top-baby-names-male
# https://data.ontario.ca/dataset/ontario-top-baby-names-female/resource/0c4aec56-b2b8-499b-9739-68ab8a56e69a
# reads in Male and Female babynames into seperate objects
OntarioM <- read.csv(file = "OntarioMales.csv", header = TRUE)
OntarioF <- read.csv(file = "OntarioFemales.csv", header = TRUE)

# Because a gender column was not included in the csv files, I added a new column
# for gender for each observation in the new objects holding the data
OntarioM$gender <- "M"
OntarioF$gender <- "F"

# combines the data from the female and male Ontario objects into the same object
Ontario <- bind_rows(OntarioF, OntarioM)

Ontario$Name <- str_to_title(Ontario$Name)
# changes each name in the Ontario dataset to proper, so that we can assign genders 
# from our Author dataset which has names in the proper form

Ontario <- Ontario %>% group_by(Name, gender) %>%
  summarise(freq = sum(Frequency))

# renames the columns so that we can bind the rows later
colnames(Ontario) <- c("name", "gender", "n")



# Alberta

# this comes from data I found on babynames in Alberta
# https://www.alberta.ca/top-baby-names.aspx
# this follows the same process as SSA and UK names
Alberta <- read_excel("Albertanames.xlsx", col_names = TRUE)
Alberta <- data.frame("name" = Alberta$name, "n" = Alberta$Frequency, 
                      "gender" = Alberta$gender)
Alberta <- Alberta %>% group_by(name, gender) %>%
  summarise(n = sum(n))

# this for loop runs through each value in the Alberta dataset and 
# reassigns the gender from Boy to M and Girl to F
# I did this to keep gender format consistent across the data
for(val in 1:nrow(Alberta))
{if(Alberta$gender[val] == "Boy") {Alberta$gender[val] <- "M"}
  else {Alberta$gender[val] <- "F"}}

# British Columbia

# This comes from data for babynames in British Columbia 
# https://www2.gov.bc.ca/gov/content/life-events/statistics-reports/bc-s-most-popular-baby-names
# follows the same process as Ontario 
BCG <- read.csv(file = "BCnamesgirl.csv", header = TRUE)
BCM <- read.csv(file = "BCnames.csv", header = TRUE)
BCG$gender <- "F"
BCM$gender <- "M"
BC <- bind_rows(BCG, BCM)
BC <- data.frame("name" = BC$name, "gender" = BC$gender, "n" = BC$n)

# here, the count of babies with a certain name was read in as a string
# so we have to get rid of the commas and then convert the column from a 
# chr to an integer 
BC$n <- gsub(",", "", BC$n)
BC$n <- as.integer(BC$n)

# Australia

# this comes from data from the Australian govt
# https://data.gov.au/dataset/ds-sa-9849aa7f-e316-426e-8ab5-74658a62c7e6/details?q=
# Follows the same process as Ontario 
AF <- read_excel("AustraliaF.xlsx", col_names = TRUE)
AM <- read_excel("AustraliaM.xlsx", col_names = TRUE)
AF$gender <- "F"
AM$gender <- "M"
Australia <- bind_rows(AF, AM)
Australia$name <- str_to_title(Australia$name)
Australia <- Australia %>% group_by(name, gender) %>%
  summarise(n = sum(n))

# Belgium

# data from Belgian govt
# https://statbel.fgov.be/nl/themas/bevolking/namen-en-voornamen/voornamen-van-meisjes-en-jongens#figures
# follows same process as Ontario
BelgM <- read_xls("BelgiumM.xls", col_names = TRUE)
BelgF <- read_xls("BelgiumF.xls", col_names = TRUE)
BelgM$gender <- "M"
BelgF$gender <- "F"
Belgium <- bind_rows(BelgF, BelgM)
Belgium <- Belgium %>% group_by(name, gender) %>%
  summarise(n = sum(n))

# Brazil

# this data comes from the Brazilian govt 
#  https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/ORH029
# the data includes the name, female count, and male count
Brazil <- read.csv(file = "BrazilNames.csv", header = TRUE)
Brazil$name <- str_to_title(Brazil$name)

# here, we want to separate the male counts from the females and create new observations
# this line creates a new dataframe with the names and counts of only females
BrazilF <- data.frame("name" = Brazil$name, "n" = Brazil$nfemale)
BrazilF$gender <- "F"

# this line removes any of the names that have no females
BrazilF <- BrazilF[which(BrazilF$n != 0),]

# repeat for males
BrazilM <- data.frame("name" = Brazil$name, "n" = Brazil$nmale)
BrazilM$gender <- "M"
BrazilM <- BrazilM[which(BrazilM$n != 0),]

# combines male and female data
Brazil <- bind_rows(BrazilF, BrazilM)

# Italy

# data comes from Italy govt
# https://github.com/mrblasco/genderNamesITA/blob/master/gender_firstnames_ITA.csv 
# (SOURCE: Anagrafe amministratori locali 1985-2014)
# follows similar process as Brazil 
Italy <- read.csv(file = "ItalyNames.csv", header = TRUE)
colnames(Italy) <- c("name", "n", "nmale", "nfemale")
ItalyF <- data.frame("name" = Italy$name, "n" = Italy$nfemale)
ItalyF$gender <- "F"
ItalyF <- ItalyF[which(ItalyF$n != 0),]
ItalyM <- data.frame("name" = Italy$name, "n" = Italy$nmale)
ItalyM$gender <- "M"
ItalyM <- ItalyM[which(ItalyM$n != 0),]
Italy <- bind_rows(ItalyF, ItalyM)
Italy$name <- str_to_title(Italy$name)

# Turkey

# data from Turkish govt
# https://github.com/mkozturk/turkishnames
# follows similarly to Ontario 
TurkishF <- read.delim(file = "female_name_tally", sep = "", header = FALSE)
TurkishM <- read.delim(file = "male_name_tally", sep = "", header = FALSE)
colnames(TurkishF) <- c("name", "n")
colnames(TurkishM) <- c("name", "n")
TurkishF$gender <- "F"
TurkishM$gender <- "M"
Turkish <- bind_rows(TurkishF, TurkishM)
Turkish <- Turkish %>% group_by(name, gender) %>%
  summarise(n = sum(n))
Turkish$name <- str_to_title(Turkish$name)

# Der Schweiz 

# data from the Swiss govt 
# https://www.bfs.admin.ch/bfs/de/home/statistiken/bevoelkerung/geburten-todesfaelle/vornamen-schweiz.assetdetail.13927395.html
Switz <- read_excel("Schweiz.xlsx", col_names = TRUE)

# count of males and females includes asteriks for zeros
# replaces asteriks from the count of females and males with zeros
Switz$nfemale <- gsub("[*]", "0", Switz$nfemale)
Switz$nmale <- gsub("[*]", "0", Switz$nmale)

# converts chr to int so we can compute
Switz$nfemale <- as.integer(Switz$nfemale)
Switz$nmale <- as.integer(Switz$nmale) 

# the rest of the data follows similarly to Brazil and Italy
SwitzF <- data.frame("name" = Switz$name, "n" = Switz$nfemale)
SwitzF$gender <- "F"
SwitzF <- SwitzF[which(SwitzF$n != 0),]
SwitzM <- data.frame("name" = Switz$name, "n" = Switz$nmale)
SwitzM$gender <- "M"
SwitzM <- SwitzM[which(SwitzM$n != 0),]
Switz <- bind_rows(SwitzF, SwitzM)

# France

# data comes from French government
# https://www.kaggle.com/haezer/french-baby-names?select=national_names.csv
# data already how we want it, so nothing needs to be done besides grouping
France <- read_excel("French.xlsx", col_names = TRUE)
France <- France %>% group_by(name, gender) %>%
  summarise(n = sum(n))

# All countries data

# now we need to combine all the data from the different countries into the same 
# data set
alldata <- bind_rows(SSA, ukbn, Ontario, Alberta, BC, Australia, Belgium,
                     Brazil, Italy, Turkish, Switz, France)

# groups the observations by name and gender so that we can get the total count
# for each name that may have 2 genders
alldata <- alldata %>% group_by(name, gender) %>%
  summarise(n = sum(n))

# this creates a new object that calculates the total count for each name
alltotal <- alldata %>% group_by(name) %>% 
  summarise(ntotal = sum(n))

# this new object assigns the count for each name to each name
# keep in mind that this will include names with more than one gender 
alltrial <- merge(alldata, unique(alltotal), by = "name")

# creates a new column in our original dataset that gives the proportion 
# male or female
alldata$prop = alltrial$n / alltrial$ntotal

# this line makes it to where the data only includes the gender for the name with 
# a prop over .50 
# this makes it to where each name with more than one gender only appears once
alldata <- alldata[which(alldata$prop > .50),]

# I wasn't sure how to do this faster, but I wanted to remove the observations
# from our dataset of names that are single characters
# for whatever reason, some names that we collected were just single letters
# and because our dataset of authors sometimes only includes the author's first
# initial as a name, we don't want to assign a gender to it
alldata <- alldata[which(alldata$name != "A" & alldata$name != "B" & 
                         alldata$name != "C" & alldata$name != "D" &
                         alldata$name != "E" & alldata$name != "F" &
                         alldata$name != "G" & alldata$name != "H" &
                         alldata$name != "I" & alldata$name != "J" &
                         alldata$name != "K" & alldata$name != "L" &
                         alldata$name != "M" & alldata$name != "N" &
                         alldata$name != "O" & alldata$name != "P" &
                         alldata$name != "Q" & alldata$name != "R" &
                         alldata$name != "S" & alldata$name != "T" &
                         alldata$name != "U" & alldata$name != "V" &
                         alldata$name != "W" & alldata$name != "X" &
                         alldata$name != "Y" & alldata$name != "Z"),]

write.csv(alldata, file = "ListofNamesandGend.csv")

