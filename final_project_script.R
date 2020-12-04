# PHP 1510 Final Project
# ----------------------------------------

# Load in relevant packages
library(dplyr)
library(ggplot2)
library(readr)

# Load in dataset
# NOTE: categorical data being read in as factors
hospitals <- read.csv("/Users/MOON/Documents/browncs/PHP1510 /hospital.csv", 
                      stringsAsFactors=TRUE)
str(hospitals)

# Check for any rows with missing data
anyNA(hospitals)

# Examine patient-related variables
#-------------------------------------------
# Examine "age" and "sex" vars
summary(hospitals$age)
ggplot(hospitals, aes(x=age, fill=sex)) +
  geom_histogram(bins=20) + facet_wrap(~company)

hospitals %>% group_by(sex) %>% summarise(count = n())

# Examine "race" var
hospitals %>% group_by(race) %>% summarise(count=n())
hospitals_race <- hospitals %>% group_by(company, race) %>% summarise(count=n())
hospitals_race

ggplot(hospitals_race, aes(x=race, y=count)) +
  geom_col() + facet_wrap(~company) + scale_x_discrete(labels=abbreviate)

# Test for independence between race and company type
table_race <- table(hospitals$company, hospitals$race)
table_race %>% prop.table()
chisq.out <- chisq.test(table_race, correct=FALSE) # Turn off Yates'
chisq.out$residuals

# Examine "complic" var
hospitals %>% group_by(complic) %>% summarise(count=n())
hospitals %>% group_by(company,complic) %>% summarise(count=n())

# Test for significant difference in complications between companies
table_complics <- table(hospitals$company, hospitals$complic)
table_complics <- table_complics[,c("Yes", "No")]
table_complics %>% prop.test()

# Examine the "los" var
summary(hospitals$los)
sd(hospitals$los)
ggplot(hospitals, aes(x=age, y=los)) + geom_bin2d(bins=10) + facet_wrap(~company)
ggplot(hospitals, aes(x=los)) + geom_histogram(bins=10) + facet_wrap(~company)
hospitals %>% group_by(company) %>% summarise(med = median(los), sd = sd(los))
ggplot(hospitals, aes(x=company, y=los)) + geom_boxplot()

# NOTE: this is the primary research question
# Test for significant difference in average LOS between two companies
t.test(los~company, data=hospitals, var.equal=F)

# Examine hospital-related characteristics
# ----------------------------------------------------------
hospitals %>% group_by(hosp.id) %>% summarise(count=n())

# Average number of beds for each hospital
avg_beds <- hospitals %>% select(hosp.id, beds) %>% group_by(hosp.id) %>%
  summarise(avg = mean(beds))

avg_beds[which.max(avg_beds$avg), ]
avg_beds[which.min(avg_beds$avg), ]

# Test for significant difference in average number of beds for patients
# insured by each company
t.test(beds~company, hospitals, var.equal=F)

# Examine "type" var
ggplot(hospitals, aes(x=type, y=los, fill=company)) + geom_boxplot(varwidth=T)

type_table <- table(hospitals$company, hospitals$type)

# Test for significant difference in proportions of patients treated at private
# hospitals (vs. public) between companies
type_table %>% prop.test()
