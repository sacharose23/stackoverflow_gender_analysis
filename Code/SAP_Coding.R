
# SAP CODING

# import packages
library(readr)
library(readxl) # not sure if I'm using this package here
library(ggplot2)
library(tidyverse)
library(knitr)

# import raw datasets
data18 <- read.csv("~/Desktop/Team6/developer_survey_2018/survey_results_public.csv")
data17 <- read.csv("~/Desktop/Team6/developer_survey_2017/survey_results_public.csv")
data16 <- read_csv("2016StackOverflowSurveyResults/2016StackOverflowSurveyResponses.csv")
data15 <- read_csv("2015StackOverflowDeveloperSurveyResponses.csv", skip = 1)
data14 <- read_csv("data14.csv")

# Focus on full-time employed developers 
data18 <- data18 %>% filter(Employment == "Employed full-time")
# 2018 : 3.5% missingness in Employment field
data17 <- data17 %>% filter(EmploymentStatus == "Employed full-time")
# 2017 : 0% missingness in Employment field
data16 <- data16 %>% filter(employment_status == "Employed full-time")
# 2016 : 11.5% missingness in Employment field
data15 <- data15 %>% filter(`Employment Status` == "Employed full-time")
# 2015 : 18.3% missingness in Employment field
data14 <- data14 %>% filter(WeeklyWorkHours > 29)
# 2014 : ~0% missingness in WeeklyWorkHours field

# Focus on gender and country columns
data18 <- data18 %>% select(grep("Gender", names(data18)), grep("Country", names(data18)))
data17 <- data17 %>% select(grep("Gender", names(data17)), grep("Country", names(data17)))
data16 <- data16 %>% select(grep("gender", names(data16)), grep("country", names(data16)))
data15 <- data15 %>% select(grep("Gender", names(data15)), grep("Country", names(data15)))

# Recode Gender variable in each dataset
temp <- as.character(data18$Gender)
temp[ which(temp == "Male") ] <- 1
temp[ grep("Female",temp) ] <- 0
temp[ grep("gender",temp) ] <- 0
data18$Gender <- as.numeric(temp)

temp <- as.character(data17$Gender)
temp[ which(temp == "Male") ] <- 1
temp[ grep("Female",temp) ] <- 0
temp[ grep("ender",temp) ] <- 0
temp[ grep("Other",temp) ] <- 0
data17$Gender <- as.numeric(temp)

temp <- as.character(data16$gender)
temp[ which(temp == "Male") ] <- 1
temp[ grep("Female",temp) ] <- 0
temp[ grep("Other",temp) ] <- 0
# Make prefer not to disclose as NA
temp[ grep("Prefer not to disclose",temp) ] <- NA
data16$gender <- as.numeric(temp)

temp <- as.character(data15$Gender)
temp[ which(temp == "Male") ] <- 1
temp[ grep("Female",temp) ] <- 0
temp[ grep("Other",temp) ] <- 0
# Make prefer not to disclose as NA
temp[ grep("Prefer not to disclose",temp) ] <- NA
data15$Gender <- as.numeric(temp)

rm(temp)

# find marginal proportions of males, F or not-only-males, & NAs
a <- as.numeric(summary(factor(data18$Gender))) / nrow(data18) * 100
b <- as.numeric(summary(factor(data17$Gender))) / nrow(data17) * 100
c <- as.numeric(summary(factor(data16$gender))) / nrow(data16) * 100
d <- as.numeric(summary(factor(data15$Gender))) / nrow(data15) * 100
e <- as.numeric(summary(factor(data14$Gender))) / nrow(data14) * 100

table <- rbind(a,b,c,d,e)
colnames(table) <- c("Other", "Males", "NA") 
row.names(table) <- c("2018", "2017", "2016", "2015", "2014")
dput(table, "tableGenderProps")

# Eliminate respondents who have missingness in Gender or Country
data18 <- data18[-which(is.na(data18$Gender)),] 
data17 <- data17[-which(is.na(data17$Gender)),] 
data16 <- data16[-which(is.na(data16$gender)),] 
data16 <- data16[-which(is.na(data16$country)),] 
data15 <- data15[-which(is.na(data15$Gender)),] # no missingness in Country 

write.table(data17, file="data17.csv",sep=",",row.names=F)
write.table(data18, file="data18.csv",sep=",",row.names=F)
# cleaned up duplicates by hand using google sheets
data17 <- read_csv("data17.csv")
data18 <- read_csv("data18.csv") 

# Clean up the duplicates (refer to txt file "Countries")
# couldn't create a function for some reason; it wouldn't work. 
# So, change countryColumn to appropriate data column for each dataset
countryColumn <- data14$Country
countryColumn[grep("Macedonia", countryColumn)] <- "Macedonia"
countryColumn[grep("Myanmar", countryColumn)] <- "Myanmar"
countryColumn[grep("Azerbaidjan", countryColumn)] <- "Azerbaijan"
countryColumn[grep("Tadjikistan", countryColumn)] <- "Tajikistan"
countryColumn[grep("Viet", countryColumn)] <- "Vietnam"
countryColumn[grep("Antigua", countryColumn)] <- "Antigua and Barbuda"
countryColumn[grep("Bosnia", countryColumn)] <- "Bosnia-Herzegovina"
countryColumn[grep("Brunei", countryColumn)] <- "Brunei Darussalam"
countryColumn[grep("Burkina", countryColumn)] <- "Burkina Faso"
countryColumn[grep("d'Ivoire", countryColumn)] <- "Ivory Coast (Cote D'Ivoire)"
countryColumn[grep("Ivory Coast", countryColumn)] <- "Ivory Coast (Cote D'Ivoire)"
countryColumn[grep("Hong Kong", countryColumn)] <- "Hong Kong"
countryColumn[grep("Iran", countryColumn)] <- "Iran"
countryColumn[grep("Ireland", countryColumn)] <- "Ireland"
countryColumn[grep("Korea South", countryColumn)] <- "South Korea"
countryColumn[grep("Republic of Korea", countryColumn)] <- "South Korea"
countryColumn[grep("Democratic People's Republic of Korea", countryColumn)] <- "North Korea"
countryColumn[grep("Korea North", countryColumn)] <- "North Korea"
countryColumn[grep("Moldova", countryColumn)] <- "Moldova"
countryColumn[grep("Russian", countryColumn)] <- "Russia"
countryColumn[grep("Slovak", countryColumn)] <- "Slovakia"
countryColumn[grep("Syrian", countryColumn)] <- "Syria"
countryColumn[grep("Trinidad", countryColumn)] <- "Trinidad and Tobago"
countryColumn[grep("Tanzania", countryColumn)] <- "Tanzania"
countryColumn[grep("Vatican", countryColumn)] <- "Vatican"
countryColumn[grep("Venezuela", countryColumn)] <- "Venezuela"
data14$Country <- countryColumn
  
# after cleaning all datasets to have uniform names on countries with 
# more than 2 observations, then recalculate the # of observations 
# per country for each year

# Finding the Common Countries Among 2014-2018 Surveys
temp <- sort(union(data18$Country, data17$Country)) 
temp <- sort(union(data16$country, temp)) 
temp <- sort(union(data15$Country, temp)) 
countries <- sort(union(data14$Country, temp))
extra <- c("I prefer not to say", "N/A", "Other", "Other (please specify)",
           "Other Country (Not Listed Above)")
countries <- countries[-which(str_detect(countries, paste(extra, collapse="|")))]


# Create a table that shows if each country is included in the survey 
# for each year
table <- cbind(as.numeric(is.element(countries, data14$Country)),
               as.numeric(is.element(countries, data15$Country)),
               as.numeric(is.element(countries, data16$country)),
               as.numeric(is.element(countries, data17$Country)), 
               as.numeric(is.element(countries, data18$Country)))
colnames(table) <- c("2014", "2015", "2016", "2017", "2018")
row.names(table) <- countries

write.table(table, file="country_obs_present.csv",sep=",",row.names=F)

# then eliminate all countries with less than 3 observations
temp <- apply(table[], 1, function(x) sum(x))
deleteCountry <- c()
for(i in 1:length(temp)){
  if(temp[i] < 3) deleteCountry <- append(deleteCountry, names(temp)[i])
}
table <- table[-which(is.element(row.names(table), deleteCountry)),]

# Eliminate rows that do not have countries on the list
data18 <- data18 %>% filter(Country %in% row.names(table))
data17 <- data17 %>% filter(Country %in% row.names(table))
data16 <- data16 %>% filter(country %in% row.names(table))
data15 <- data15 %>% filter(Country %in% row.names(table))
data14 <- data14 %>% filter(Country %in% row.names(table))

# Create a table with number of respondents 
tempTable <- cbind(row.names(table), matrix(NA, nrow = length(row.names(table)), ncol = 5))
colnames(tempTable) <- c("countries", "2014", "2015", "2016", "2017", "2018")

freq18 <- table(data18$Country)
freq17 <- table(data17$Country)
freq16 <- table(data16$country)
freq15 <- table(data15$Country)
freq14 <- table(data14$Country)

for(i in 1:length(freq18)){
  for(j in 1:nrow(tempTable)){
    if( names(freq18)[i]==tempTable[j,1] ) tempTable[j,6] <- freq18[i]
  }
}

for(i in 1:length(freq17)){
  for(j in 1:nrow(tempTable)){
    if( names(freq17)[i]==tempTable[j,1] ) tempTable[j,5] <- freq17[i]
  }
}

for(i in 1:length(freq16)){
  for(j in 1:nrow(tempTable)){
    if( names(freq16)[i]==tempTable[j,1] ) tempTable[j,4] <- freq16[i]
  }
}

for(i in 1:length(freq15)){
  for(j in 1:nrow(tempTable)){
    if( names(freq15)[i]==tempTable[j,1] ) tempTable[j,3] <- freq15[i]
  }
}

for(i in 1:length(freq14)){
  for(j in 1:nrow(tempTable)){
    if( names(freq14)[i]==tempTable[j,1] ) tempTable[j,2] <- freq14[i]
  }
}


write.table(tempTable, file="country_no_of_obs.csv",sep=",",row.names=F)


## Build the dataset to use for analysis
countryRep <- rep(tempTable[,1], each=5)
years <- rep(c("2014", "2015", "2016", "2017", "2018"), nrow(tempTable))
nRespondents <- as.vector(t(tempTable[,2:6]))

countries <- row.names(table)
nMales <- c()
for(i in 1:length(countries)){
  temp14 <- data14 %>% filter(Country == countries[i])
  temp15 <- data15 %>% filter(Country == countries[i])
  temp16 <- data16 %>% filter(country == countries[i])
  temp17 <- data17 %>% filter(Country == countries[i])
  temp18 <- data18 %>% filter(Country == countries[i])
  nMales <- append( nMales, c(sum(temp14$Gender), sum(temp15$Gender),
                 sum(temp16$gender), sum(temp17$Gender),
                 sum(temp18$Gender)) )
}

finalData <- cbind(countryRep, years, nRespondents, nMales)

#eliminate rows with NA
finalData <- finalData[-which(is.na(finalData[,3])),]
propMales <- as.numeric(finalData[,4])/as.numeric(finalData[,3])
finalData <- cbind(finalData, propMales)

deleteThese <- c("Afghanistan", "Albania", "Algeria", "Andorra", "Armenia", "Azerbaijan", "Bahrain",
"Barbados", "Bolivia", "Bosnia-Herzegovina", "Cambodia", "Cameroon", "Costa Rica", "Cuba", 
"Cyprus", "Ecuador", "El Salvador", "Ethiopia", "Fiji", 
"Georgia", "Ghana", "Guatemala", "Honduras", "Iceland", "Iraq", "Ivory Coast (Cote D'Ivoire)", "Jamaica", "Jordan",
"Kazakhstan", "Kenya", "Kuwait", "Kyrgyzstan", "Lebanon", "Liechtenstein",
"Luxembourg", "Macedonia", "Madagascar", "Malta", "Mauritius", "Moldova", "Mongolia", 
"Montenegro", "Morocco", "Mozambique", "Myanmar", "Namibia", "Nepal", "Nicaragua", 
"Nigeria", "Oman", "Panama", "Paraguay", "Peru", "Qatar", "Rwanda", "Saudi Arabia", 
"Senegal", "Somalia", "Sudan", "Suriname", "Syria", "Tajikistan", "Trinidad and Tobago", 
"Tunisia", "Uganda", "Uzbekistan", "Venezuela", "Yemen", "Zimbabwe")

finalData <- finalData[-which(finalData[,1] %in% deleteThese),]

write.table(finalData, file="finalData.csv",sep=",",row.names=F)
countries <- unique(finalData[,1])
write.table(countries, file="countries.csv",sep=",",row.names=F)




