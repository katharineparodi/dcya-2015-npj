# load libraries
library(weights)
library(car)
library(ryouready)
library(psych)
library(dplyr)
library(finalfit)
library(gmodels)
library(lmtest)
library(multiwayvcov)
library(ggplot2)
library(data.table)
library(moments)
library(interactions)
library(gridExtra)
library(sjPlot)

# read in file
dcya_raw = read.csv("DCYA_HS_2015.csv", header = TRUE)

# inspect data
class(dcya_raw)
dim(dcya_raw)
dplyr::sample_n(dcya_raw, 3)

# store length of dcya_raw df
dcya.length.original <- nrow(dcya_raw)
 
# report weighted sample size 
sum(dcya_raw$Weight)
range(dcya_raw$Weight) #max/min

# examine number of schools/buildings
table(dcya_raw$Building)
length(unique(dcya_raw$Building))


# unweighted sociodemographics
prop.table(table(dcya_raw$Sex, exclude = NULL)) #Sex
prop.table(table(dcya_raw$Race, exclude = NULL)) #Race
prop.table(table(dcya_raw$GLBQ, exclude = NULL)) #GLBQ
prop.table(table(dcya_raw$Transgender, exclude = NULL)) #Transgender

# unweighted mean, median, and sd for age in years in raw data
dcya_raw$AgeRecode <- dcya_raw$Age + 13
mean(dcya_raw$AgeRecode, na.rm = TRUE)
median(dcya_raw$AgeRecode, na.rm = TRUE)
sd(dcya_raw$AgeRecode, na.rm = TRUE)

# descriptive stats on raw data
# Age: 
wpct(dcya_raw$Age, weight = dcya_raw$Weight)
# Grade: 
wpct(dcya_raw$Grade, weight = dcya_raw$Weight)
# Sex: 
wpct(dcya_raw$Sex, weight = dcya_raw$Weight)
# Race: 
wpct(dcya_raw$Race, weight = dcya_raw$Weight)
# GLBQ: 
wpct(dcya_raw$GLBQ, weight = dcya_raw$Weight)
# Transgender: 
wpct(dcya_raw$Transgender, weight = dcya_raw$Weight)

# new dataset to include NAs for reporting weighted sociodemographics in raw data
dcya_raw_nona <- dcya_raw

# descriptive stats on raw data with NAs included
# Age:
dcya_raw_nona$Age[is.na(dcya_raw_nona$Age)] <- "NA"
wpct(dcya_raw_nona$Age, weight = dcya_raw_nona$Weight)

# Grade: 
dcya_raw_nona$Grade[is.na(dcya_raw_nona$Grade)] <- "NA"
wpct(dcya_raw_nona$Grade, weight = dcya_raw_nona$Weight)

# Sex: 
dcya_raw_nona$Sex[is.na(dcya_raw_nona$Sex)] <- "NA"
wpct(dcya_raw_nona$Sex, weight = dcya_raw_nona$Weight)

# Race: 
dcya_raw_nona$Race[is.na(dcya_raw_nona$Race)] <- "NA"
wpct(dcya_raw_nona$Race, weight = dcya_raw_nona$Weight)

# GLBQ: 
dcya_raw_nona$GLBQ[is.na(dcya_raw_nona$GLBQ)] <- "NA"
wpct(dcya_raw_nona$GLBQ, weight = dcya_raw_nona$Weight)

# Transgender: 
dcya_raw_nona$Transgender[is.na(dcya_raw_nona$Transgender)] <- "NA"
wpct(dcya_raw_nona$Transgender, weight = dcya_raw_nona$Weight)

# weighted mean, median, and sd for age in years in raw data
wtd.mean(dcya_raw$AgeRecode, dcya_raw$Weight, na.rm = TRUE)
wtd_var <- wtd.var(dcya_raw$AgeRecode,dcya_raw$Weight, na.rm = TRUE)
sqrt(wtd_var) 

###########################################################################
## data management and cleaning of raw data set
###########################################################################
# filter out SchoolID 26 
dcya_raw <- subset(dcya_raw, Building < 26)
# check data
length(unique(dcya_raw$Building))
dim(dcya_raw)

# store length of dcya_raw df after excluding building
dcya.length.no26 <- nrow(dcya_raw)

# how many removed due to building exclude? 
# place n in participant selection flow diagram
dcya.length.original - dcya.length.no26

# Filter data for mischievous respondents
# Functions to filter respondents from sample
# function1: factorize a column, then run a prop.table, then search if a category < prescribed threshold, then output results through Boolean operator of True/False.
checkColumn <- function(col, thresh) {
	factorized <- factor(col)
	freq <- prop.table(table(factorized))
	under.thresh = freq[freq < thresh]
	print(under.thresh)
	length(under.thresh) > 0
}

# function2: loops over dataset and output results
checkDf <- function(df, thresh) {
	for (n in names(df)) {
		col <- df[,n]
		factorized <- factor(col)
		freq <- prop.table(table(factorized))
		under.thresh = freq[freq < thresh]
		
		if (length(under.thresh) > 0) {
			print(n)
			print(under.thresh)
		}
	}
}

# Remove specified variables
Todrop <- c("Building", "RespondentID", "Weight", "Grade", "Race", "GLBQ", "GenderID", "Transgender", "q0007", "q0007_other", "MMSDzip1", "MMSDzip2", "MMSDzip3", "q0030_0001", "TagTotal")
dcya_for_screener <- dcya_raw[,!(names(dcya_raw) %in% Todrop)]
# Call the function
checkDf(dcya_for_screener, .025)
# examine variables for theoretically-unrelated items

# Screener, assign tags for each item
# convert 2 items for height in feet and inches to 1 item: height in inches, and convert to numeric
dcya_raw$Height_in_Inches <- suppressWarnings(
        (as.numeric(as.character(dcya_raw$q0029_0001_0001)) + 3) * 12 +
        (as.numeric(as.character(dcya_raw$q0029_0001_0002)) - 1)) 
table(dcya_raw$Height_in_Inches, exclude = NULL)
# histogram
hist(dcya_raw$Height_in_Inches)


# Identify top/bottom 2.5% of height
quantile(dcya_raw$Height_in_Inches, 0.975, na.rm = TRUE)
quantile(dcya_raw$Height_in_Inches, 0.025, na.rm = TRUE)
dcya_raw$tag1 <- dcya_raw$Height_in_Inches
dcya_raw$tag1 <- 0
dcya_raw$tag1[dcya_raw$Height_in_Inches < 60 | dcya_raw$Height_in_Inches > 76] <- 1
table(dcya_raw$tag1)

# convert weight in pounds variable to numeric
dcya_raw$Weight_in_lbs <- suppressWarnings(as.numeric(as.character(dcya_raw$q0030_0001))) 
table(dcya_raw$Weight_in_lbs, exclude = NULL)
# histogram
hist(dcya_raw$Weight_in_lbs)

# Identify top/bottom 2.5% of weight
quantile(dcya_raw$Weight_in_lbs, 0.975, na.rm = TRUE)
quantile(dcya_raw$Weight_in_lbs, 0.025, na.rm = TRUE)
dcya_raw$tag2 <- dcya_raw$Weight_in_lbs
dcya_raw$tag2 <- 0
dcya_raw$tag2[dcya_raw$Weight_in_lbs < 98 | dcya_raw$Weight_in_lbs > 250] <- 1
table(dcya_raw$tag2)

# Never seen a dentist
dcya_raw$tag3 <- dcya_raw$q0042
dcya_raw$tag3 <- 0
dcya_raw$tag3[dcya_raw$q0042 == 4] <- 1
table(dcya_raw$tag3)

# Rarely or never wear seatbelt
dcya_raw$tag4 <- dcya_raw$q0044
dcya_raw$tag4 <- 0
dcya_raw$tag4[dcya_raw$q0044 <= 2] <- 1
table(dcya_raw$tag4)

# Gang member
dcya_raw$tag5 <- dcya_raw$q0103
dcya_raw$tag5 <- 0
dcya_raw$tag5[dcya_raw$q0103 >= 3] <- 1
table(dcya_raw$tag5)

# Never eat fruit or vegetables in past week
dcya_raw$tag6 <- dcya_raw$q0032
dcya_raw$tag6 <- 0
dcya_raw$tag6[dcya_raw$q0032 == 1] <- 1
table(dcya_raw$tag6)

# sum items for Tagtotal
dcya_raw$newtagtotal <- dcya_raw$tag1 + dcya_raw$tag2 + dcya_raw$tag3 + dcya_raw$tag4 + dcya_raw$tag5 + dcya_raw$tag6 
table(dcya_raw$newtagtotal)
prop.table(table(dcya_raw$newtagtotal))

# filters cases with >= 4 extreme responses on the screener (retains 0, 1, 2, 3)
# new data.frame = "dcya"
dcya_filtered <- filter(dcya_raw, dcya_raw$newtagtotal<4)

# check data 
dim(dcya_filtered)

# Calculate n identified as potential "mischievous respondents" and removed from the sample
n.removed <- dcya.length.no26 - nrow(dcya_filtered)
n.removed
# this is % removed from original sample (dcya_raw)
(n.removed/dcya.length.original)*100


## subset dataframe to analytic variables
dcya_sub <- dcya_filtered[, c(1:4, 6:8, 10, 55:62, 88:89, 97, 99:101, 198:203, 219, 224:235, 237)]
dim(dcya_sub)

#### updated 2-5-2022 for cca ###
# missing data on age
# Age
table(dcya_sub$Age, exclude = NULL)
prop.table(table(dcya_sub$Age, exclude = NULL))*100

# missing data on biological sex
# Sex
table(dcya_sub$Sex, exclude = NULL)
prop.table(table(dcya_sub$Sex, exclude = NULL))*100

# missing data on race/ethnicity
# Race/Ethnicity
table(dcya_sub$Race, exclude = NULL)
prop.table(table(dcya_sub$Race, exclude = NULL))*100

# missing data on sexual orientation
# Sexual Orientation
table(dcya_sub$GLBQ, exclude = NULL)
prop.table(table(dcya_sub$GLBQ, exclude = NULL))*100

# how many missing data on age or sex or race/ethnicity or sexual orientation? [set disjunction]
missing_sociodemo <- is.na(dcya_sub$Age) | is.na(dcya_sub$Sex) | is.na(dcya_sub$Race) | is.na(dcya_sub$GLBQ)
table(missing_sociodemo)

# subset dataframe to students with complete sociodemographic data
dcya_sub2 <- dcya_sub[!is.na(dcya_sub$Age) & !is.na(dcya_sub$Sex) & !is.na(dcya_sub$Race) & !is.na(dcya_sub$GLBQ), ]
dim(dcya_sub2)


### gender modality and perceived gender expression ###

# how many responded "I don't know what transgender means" on gender modality item in dcya_sub sample?
table(dcya_sub$Transgender)

# in dcya_sub2
table(dcya_sub2$Transgender)

# missing data on transgender (called "gender modality") in dcya_sub sample
# Transgender
table(dcya_sub$Transgender, exclude = NULL)
prop.table(table(dcya_sub$Transgender, exclude = NULL))*100

# in dcya_sub2 
table(dcya_sub2$Transgender, exclude = NULL)


# missing data on perceived gender expression in dcya_sub sample
# Perceived gender expression
table(dcya_sub$q0100, exclude = NULL)
prop.table(table(dcya_sub$q0100, exclude = NULL))*100

# in dcya_sub2
table(dcya_sub2$q0100, exclude = NULL)

# how many missing data on transgender OR perceived gender expression? [set disjunction] in dcya_sub sample
missing_exposure <- is.na(dcya_sub$Transgender) | is.na(dcya_sub$q0100) 
table(missing_exposure)

# how many missing data on transgender AND perceived gender expression in dcya_sub sample?
missing_exposure_both <- is.na(dcya_sub$Transgender) & is.na(dcya_sub$q0100) 
table(missing_exposure_both)

# how many missing data on transgender OR transgender = "I don't know what transgender means" AND missing data on
# perceived gender expression in dcya_sub2 sample?
answered_trans = (!is.na(dcya_sub2$Transgender) & (dcya_sub2$Transgender == 1 | dcya_sub2$Transgender == 2)) & !is.na(dcya_sub2$q0100)
table(answered_trans)

# subset dataframe to students 
dcya_sub3 <- dcya_sub2[answered_trans, ]
dim(dcya_sub3)



### victimization variables ###
# missing data on peer victimization in dcya_sub sample
table(dcya_sub$q0105_0002, exclude = NULL)
prop.table(table(dcya_sub$q0105_0002, exclude = NULL))*100
table(dcya_sub$q0105_0004, exclude = NULL)
prop.table(table(dcya_sub$q0105_0004, exclude = NULL))*100
table(dcya_sub$q0105_0009, exclude = NULL)
prop.table(table(dcya_sub$q0105_0009, exclude = NULL))*100
table(dcya_sub$q0105_0011, exclude = NULL)
prop.table(table(dcya_sub$q0105_0011, exclude = NULL))*100

# in dcya_sub3 sample
table(dcya_sub3$q0105_0002, exclude = NULL)
table(dcya_sub3$q0105_0004, exclude = NULL)
table(dcya_sub3$q0105_0009, exclude = NULL)
table(dcya_sub3$q0105_0011, exclude = NULL)

# missing data on bias-based harassment in dcya_sub sample
# BB Harassment
table(dcya_sub$q0106_0002, exclude = NULL)
prop.table(table(dcya_sub$q0106_0002, exclude = NULL))*100

# in dcya_sub3 sample
table(dcya_sub3$q0106_0002, exclude = NULL)

# how many missing data on peer victimization survey items or bias-based harassment? [set disjunction]
missing_vic <- is.na(dcya_sub3$q0105_0002) | is.na(dcya_sub3$q0105_0004) | is.na(dcya_sub3$q0105_0009) | is.na(dcya_sub3$q0105_0011) |
  is.na(dcya_sub3$q0106_0002) 
table(missing_vic)

# subset dataframe to students with complete victimization data
dcya_sub4 <- dcya_sub3[!is.na(dcya_sub3$q0105_0002) & !is.na(dcya_sub3$q0105_0004) & !is.na(dcya_sub3$q0105_0009) &
                         !is.na(dcya_sub3$q0105_0011) & !is.na(dcya_sub3$q0106_0002), ]
dim(dcya_sub4)



### mental health variables ###
# missing data on SI in dcya_sub sample
# Suicidal Ideation (SI)
table(dcya_sub$q0051, exclude = NULL)
prop.table(table(dcya_sub$q0051, exclude = NULL))*100

# in dcya_sub4 sample
table(dcya_sub4$q0051, exclude = NULL)

# missing data on SA in dcya_sub sample
# Suicide Attempt (SA)
table(dcya_sub$q0052, exclude = NULL)
prop.table(table(dcya_sub$q0052, exclude = NULL))*100

# in dcya_sub4 sample
table(dcya_sub4$q0052, exclude = NULL)

# missing data on NSSI in dcya_sub sample
# Nonsuicidal Self-injury (NSSI)
table(dcya_sub$q0053, exclude = NULL)
prop.table(table(dcya_sub$q0053, exclude = NULL))*100

# in dcya_sub4 sample
table(dcya_sub4$q0053, exclude = NULL)

# missing data on depression in dcya_sub sample
# Depression
table(dcya_sub$q0049, exclude = NULL)
prop.table(table(dcya_sub$q0049, exclude = NULL))*100

# in dcya_sub4 sample
table(dcya_sub4$q0049, exclude = NULL)

# missing data on on anxiety questions in dcya_sub sample
table(dcya_sub$q0046_0001, exclude = NULL)
prop.table(table(dcya_sub$q0046_0001, exclude = NULL))*100
table(dcya_sub$q0046_0002, exclude = NULL)
prop.table(table(dcya_sub$q0046_0002, exclude = NULL))*100

# in dcya_sub4 sample
table(dcya_sub4$q0046_0001, exclude = NULL)
table(dcya_sub4$q0046_0002, exclude = NULL)


# how many missing data on SI, SA, NSSI, depression, anxiety1 or anxiety 2? [set disjunction]
missing_mental_health <- is.na(dcya_sub4$q0051) | is.na(dcya_sub4$q0052) | is.na(dcya_sub4$q0053) | is.na(dcya_sub4$q0049) |
  is.na(dcya_sub4$q0046_0001) | is.na(dcya_sub4$q0046_0002)
table(missing_mental_health)

# subset dataframe to students with complete mental health data
dcya_sub5 <- dcya_sub4[!is.na(dcya_sub4$q0051) & !is.na(dcya_sub4$q0052) & !is.na(dcya_sub4$q0053) &
                         !is.na(dcya_sub4$q0049) & !is.na(dcya_sub4$q0046_0001) & !is.na(dcya_sub4$q0046_0002), ]
dim(dcya_sub5)


### moderators ###
# missing data on school-connectedness in dcya_sub sample
table(dcya_sub$q0095_0002, exclude = NULL)
prop.table(table(dcya_sub$q0095_0002, exclude = NULL))*100
table(dcya_sub$q0095_0004, exclude = NULL)
prop.table(table(dcya_sub$q0095_0004, exclude = NULL))*100
table(dcya_sub$q0095_0005, exclude = NULL)
prop.table(table(dcya_sub$q0095_0005, exclude = NULL))*100
table(dcya_sub$q0095_0006, exclude = NULL)
prop.table(table(dcya_sub$q0095_0006, exclude = NULL))*100

### moderators ###
# in dcya_sub5 sample
table(dcya_sub5$q0095_0002, exclude = NULL)
table(dcya_sub5$q0095_0004, exclude = NULL)
table(dcya_sub5$q0095_0005, exclude = NULL)
table(dcya_sub5$q0095_0006, exclude = NULL)

# how many missing data on any SCS items?
missing_scs <- is.na(dcya_sub5$q0095_0002) | is.na(dcya_sub5$q0095_0004) | is.na(dcya_sub5$q0095_0005) |
  is.na(dcya_sub5$q0095_0006) 
table(missing_scs)

# missing data on family support and monitoring in dcya_sub sample
# Family Support & Monitoring scale, 7 items
table(dcya_sub$q0025_0001, exclude = NULL) 
prop.table(table(dcya_sub$q0025_0001, exclude = NULL))*100
table(dcya_sub$q0025_0002, exclude = NULL)
prop.table(table(dcya_sub$q0025_0002, exclude = NULL))*100
table(dcya_sub$q0025_0003, exclude = NULL)
prop.table(table(dcya_sub$q0025_0003, exclude = NULL))*100
table(dcya_sub$q0025_0004, exclude = NULL)
prop.table(table(dcya_sub$q0025_0004, exclude = NULL))*100
table(dcya_sub$q0025_0005, exclude = NULL)
prop.table(table(dcya_sub$q0025_0005, exclude = NULL))*100
table(dcya_sub$q0025_0006, exclude = NULL)
prop.table(table(dcya_sub$q0025_0006, exclude = NULL))*100
table(dcya_sub$q0025_0007, exclude = NULL)
prop.table(table(dcya_sub$q0025_0007, exclude = NULL))*100


# in dcya_sub5 sample
table(dcya_sub5$q0025_0001, exclude = NULL) 
table(dcya_sub5$q0025_0002, exclude = NULL)
table(dcya_sub5$q0025_0003, exclude = NULL)
table(dcya_sub5$q0025_0004, exclude = NULL)
table(dcya_sub5$q0025_0005, exclude = NULL)
table(dcya_sub5$q0025_0006, exclude = NULL)
table(dcya_sub5$q0025_0007, exclude = NULL)

# how many missing data on any FSM items?
missing_fsm <- is.na(dcya_sub5$q0025_0001) | is.na(dcya_sub5$q0025_0002) | is.na(dcya_sub5$q0025_0003) |
  is.na(dcya_sub5$q0025_0004) | is.na(dcya_sub5$q0025_0005) | is.na(dcya_sub5$q0025_0006) | is.na(dcya_sub5$q0025_0007)
table(missing_fsm)


# how many missing data on any moderator? [set disjunction]
missing_modr <- is.na(dcya_sub5$q0095_0002) | is.na(dcya_sub5$q0095_0004) | is.na(dcya_sub5$q0095_0005) |
  is.na(dcya_sub5$q0095_0006) | is.na(dcya_sub5$q0025_0001) | is.na(dcya_sub5$q0025_0002) | is.na(dcya_sub5$q0025_0003) |
  is.na(dcya_sub5$q0025_0004) | is.na(dcya_sub5$q0025_0005) | is.na(dcya_sub5$q0025_0006) | is.na(dcya_sub5$q0025_0007)
table(missing_modr)


# subset dataframe to final analytic sample
dcya <- dcya_sub5[!is.na(dcya_sub5$q0095_0002) & !is.na(dcya_sub5$q0095_0004) & !is.na(dcya_sub5$q0095_0005) &
                    !is.na(dcya_sub5$q0095_0006) & !is.na(dcya_sub5$q0025_0001) & !is.na(dcya_sub5$q0025_0002) &
                    !is.na(dcya_sub5$q0025_0003) & !is.na(dcya_sub5$q0025_0004) & !is.na(dcya_sub5$q0025_0005) &
                    !is.na(dcya_sub5$q0025_0006) & !is.na(dcya_sub5$q0025_0007), ]
dim(dcya)


### data transformation ###

# create measures for analyses
# Transgender
# examine variable, unweighted
table(dcya$Transgender)
prop.table(table(dcya$Transgender))
# 1 = Yes, 2 = No
(wpct(dcya$Transgender, weight = dcya$Weight))*100

## A binary indication of Transgender
dcya_transgender <- dcya$Transgender
dcya_transgender[dcya$Transgender == 2] <- 1
dcya_transgender[dcya$Transgender == 1] <- 2 
dcya["Transgender_bin"] <- dcya_transgender
# Transgender: 1 = No, 2 = Yes
dcya$Transgender_bin[dcya$Transgender_bin == 1] <- "1_Cisgender"
dcya$Transgender_bin[dcya$Transgender_bin == 2] <- "2_Transgender"

# factorize Transgender variable
dcya$Transgender_bin <- factor(dcya$Transgender_bin)
is.factor(dcya$Transgender_bin)
levels(dcya$Transgender_bin)

# examine new binary variable
table(dcya$Transgender_bin)
prop.table(table(dcya$Transgender_bin))
(wpct(dcya$Transgender_bin, weight = dcya$Weight))*100

# Gender Conformity with Transgender included 
## recoded on 11/15/20 with new ordering 
dcya$TGNC[dcya$Sex == 1 & dcya$q0100 <= 3] <- 1
dcya$TGNC[dcya$Sex == 2 & dcya$q0100 >= 5] <- 1
dcya$TGNC[dcya$Sex == 1 & dcya$q0100 == 4] <- 2
dcya$TGNC[dcya$Sex == 2 & dcya$q0100 == 4] <- 2
dcya$TGNC[dcya$Sex == 1 & dcya$q0100 == 5] <- 3
dcya$TGNC[dcya$Sex == 2 & dcya$q0100 == 3] <- 3
dcya$TGNC[dcya$Sex == 1 & dcya$q0100 >= 6] <- 4
dcya$TGNC[dcya$Sex == 2 & dcya$q0100 <= 2] <- 4

tmp = subset(dcya, Transgender == 1)

table(dcya$TGNC, exclude = NULL)
table(dcya$TGNC[!is.na(dcya$TGNC)])
tgnc.length <-length(dcya$TGNC[!is.na(dcya$TGNC)])
tgnc.length

tmp = dcya[, c("Transgender", "TGNC")]
table(tmp, exclude = NULL)
prop.table(table(tmp, exclude = NULL))


# Gender Conformity, with Transgender youth excluded 
## recoded with new ordering
dcya$GNC[dcya$Sex == 1 & dcya$q0100 <= 3 & dcya$Transgender != 1] <- 1  # If sex = female & perceived gender expression (PGE) = somewhat, mostly or very feminine & NOT transgender
dcya$GNC[dcya$Sex == 2 & dcya$q0100 >= 5 & dcya$Transgender != 1] <- 1  # If sex = male & PGE = somewhat, mostly or very masculine & NOT transgender
dcya$GNC[dcya$Sex == 1 & dcya$q0100 == 4 & dcya$Transgender != 1] <- 2  # If sex = female & PGE = equally feminine and masculine & NOT transgender
dcya$GNC[dcya$Sex == 2 & dcya$q0100 == 4 & dcya$Transgender != 1] <- 2  # If sex = male & PGE = equally feminine and masculine & NOT transgender
dcya$GNC[dcya$Sex == 1 & dcya$q0100 == 5 & dcya$Transgender != 1] <- 3  # If sex = female & PGE = somewhat masculine & NOT transgender
dcya$GNC[dcya$Sex == 2 & dcya$q0100 == 3 & dcya$Transgender != 1] <- 3  # If sex = male & PGE = somewhat feminine & NOT transgender
dcya$GNC[dcya$Sex == 1 & dcya$q0100 >= 6 & dcya$Transgender != 1] <- 4  # If sex = female & PGE = mostly or very masculine & NOT transgender
dcya$GNC[dcya$Sex == 2 & dcya$q0100 <= 2 & dcya$Transgender != 1] <- 4  # If sex = male & PGE = mostly or very feminine & NOT transgender


tmp = dcya[, c("Transgender", "GNC")]
table(tmp, exclude = NULL)

gnc.length <-length(dcya$GNC[!is.na(dcya$GNC)])
gnc.length
# how many respondents excluded when removing transgender?
tgnc.length - gnc.length

# 1 = Gender conforming, 2 = Androgynous, 3 = Moderately GNC, 4 = Highly GNC
# excluded respondents who responded "yes" to transgender item to prevent overlap
dcya$GNC[dcya$GNC == 1] <- "1_GenderConforming"
dcya$GNC[dcya$GNC == 2] <- "2_Androgynous"
dcya$GNC[dcya$GNC == 3] <- "3_ModeratelyGNC"
dcya$GNC[dcya$GNC == 4] <- "4_HighlyGNC"

### update 2/9/22 create new composite variable ###
### re-set NAs back to transgender for gender modality/expression variable###
dcya$GNCT[dcya$GNC == "1_GenderConforming"] <- "1_GenderConforming"
dcya$GNCT[dcya$GNC ==  "2_Androgynous"] <- "2_Androgynous"
dcya$GNCT[dcya$GNC == "3_ModeratelyGNC"] <- "3_ModeratelyGNC"
dcya$GNCT[dcya$GNC == "4_HighlyGNC"] <- "4_HighlyGNC"
dcya$GNCT[is.na(dcya$GNC)] <- "5_Transgender"

# factorize GNCT (composite) variable
dcya$GNCT <- factor(dcya$GNCT)
is.factor(dcya$GNCT)
levels(dcya$GNCT)

# examine new variable
table(dcya$GNCT, exclude = NULL)
prop.table(table(dcya$GNCT, exclude = NULL))
# 1 = Gender conforming, 2 = Androgynous, 3 = Moderately GNC, 4 = Highly GNC, 5 = Transgender
(wpct(dcya$GNCT, weight = dcya$Weight))*100


### go back and update GNC variable (that excludes transgender youth)

# factorize GNC variable
dcya$GNC <- factor(dcya$GNC)
is.factor(dcya$GNC)
levels(dcya$GNC)

# examine new variable
table(dcya$GNC, exclude = NULL)
prop.table(table(dcya$GNC, exclude = NULL))
# 1 = Gender conforming, 2 = Androgynous, 3 = Moderately GNC, 4 = Highly GNC
(wpct(dcya$GNC, weight = dcya$Weight))*100


# examine sociodemographic variables
# Age
table(dcya$Age)
prop.table(table(dcya$Age))
# quantitative variable, recode to numerical values
dcya$AgeRecode <- dcya$Age + 13

# examine new variable
table(dcya$AgeRecode)
prop.table(table(dcya$AgeRecode))
summary(dcya$AgeRecode)
sd(dcya$AgeRecode)

# weighted percentages 
(wpct(dcya$AgeRecode, weight = dcya$Weight))*100

### center variable for regression analyses
dcya["AgeRecode.c"] <- scale(dcya$AgeRecode, center = TRUE, scale = FALSE)
class(dcya$AgeRecode.c)
# coerce to vector
dcya$AgeRecode.c <- as.vector(dcya$AgeRecode.c)
summary(dcya$AgeRecode.c)
class(dcya$AgeRecode.c)


# Sex
table(dcya$Sex)
prop.table(table(dcya$Sex))
# weighted percentages 
wpct(dcya$Sex, weight = dcya$Weight)
class(dcya$Sex)

dcya$Sex[dcya$Sex == 1] <- "2_Female"
dcya$Sex[dcya$Sex == 2] <- "1_Male"
dcya$Sex <- factor(dcya$Sex)
# 1 = female, 2 = male
#check variable
is.factor(dcya$Sex)
levels(dcya$Sex)

table(dcya$Sex)
# re-run weighted percentages 
wpct(dcya$Sex, weight = dcya$Weight)

# Race/Ethnicity
table(dcya$Race)
prop.table(table(dcya$Race))
dcya$RaceRecode <- dcya$Race
# collapse Asian + Hmong
dcya$RaceRecode[dcya$Race == 1] <- 4
# recode
dcya$RaceRecode[dcya$Race == 7] <- 1
dcya$RaceRecode[dcya$Race == 2] <- 2
dcya$RaceRecode[dcya$Race == 3] <- 3
# collapse Other + Middle Eastern + Native
dcya$RaceRecode[dcya$Race == 5] <- 6
dcya$RaceRecode[dcya$Race == 9] <- 6
# recode
dcya$RaceRecode[dcya$Race == 8] <- 5
# Race/ethnicity: 1 = White, 2 = Black, 3 = Latino, 4 = Asian including Hmong, 5 = Multi-racial, 6 = Other
table(dcya$RaceRecode, exclude = NULL)
prop.table(table(dcya$RaceRecode, exclude = NULL))
# weighted percentages 
wpct(dcya$RaceRecode, weight = dcya$Weight)

#### update: re-parameterize so only 1 dummy variable for regression analyses:  1 = white, 2 = non-white (POC)
dcya_race <- dcya$RaceRecode
dcya_race[dcya$RaceRecode == 1] <- "1_White"
dcya_race[dcya$RaceRecode >= 2] <- "2_Non-white"
dcya$RaceRecode.p <- dcya_race
dcya$RaceRecode.p <-factor(dcya$RaceRecode.p)


dcya$RaceRecode[dcya$RaceRecode == 1] <- "1_White"
dcya$RaceRecode[dcya$RaceRecode == 2] <- "2_Black"
dcya$RaceRecode[dcya$RaceRecode == 3] <- "3_Latino"
dcya$RaceRecode[dcya$RaceRecode == 4] <- "4_Asian"
dcya$RaceRecode[dcya$RaceRecode == 5] <- "5_Multiracial"
dcya$RaceRecode[dcya$RaceRecode == 6] <- "6_Other"

dcya$RaceRecode <-factor(dcya$RaceRecode)
is.factor(dcya$RaceRecode)
levels(dcya$RaceRecode)
table(dcya$RaceRecode)
# weighted percentages 
(wpct(dcya$RaceRecode, weight = dcya$Weight))*100

# Sexual Orientation
table(dcya$GLBQ)
prop.table(table(dcya$GLBQ))
# weighted percentages 
wpct(dcya$GLBQ, weight = dcya$Weight)
class(dcya$GLBQ)

#### update: re-parameterize so only 1 dummy variable for regression analyses:  1 = heterosexual, 2 = non-heterosexual
dcya_glbq <- dcya$GLBQ
dcya_glbq[dcya$GLBQ == 1] <- "Heterosexual"
dcya_glbq[dcya$GLBQ >= 2] <- "Non-Heterosexual"
dcya$GLBQ.p <- dcya_glbq
dcya$GLBQ.p <-factor(dcya$GLBQ.p)



dcya$GLBQ[dcya$GLBQ == 1] <- "1_Straight"
dcya$GLBQ[dcya$GLBQ == 2] <- "2_Gay_lesbian"
dcya$GLBQ[dcya$GLBQ == 3] <- "3_Bisexual"
dcya$GLBQ[dcya$GLBQ == 4] <- "4_Questioning"
dcya$GLBQ[dcya$GLBQ == 5] <- "5_Other"

dcya$GLBQ <- factor(dcya$GLBQ)
is.factor(dcya$GLBQ)
levels(dcya$GLBQ)
str(dcya$GLBQ)
table(dcya$GLBQ)
(wpct(dcya$GLBQ, weight = dcya$Weight))*100
# 1 = Straight/heterosexual, 2 = Gay or Lesbian, 3 = Bisexual, 4 = Questioning, 5 = Other

# Suicidal Ideation (SI)
table(dcya$q0051)
prop.table(table(dcya$q0051))

## A binary indication of SI
dcya_SI <- dcya$q0051
dcya_SI[dcya$q0051 == 1] <- 0
dcya_SI[dcya$q0051 == 2 | dcya$q0051 == 3 | dcya$q0051 == 4 ] <- 1
dcya["SI_bin"] <- dcya_SI
# SI: 0 = No, 1 = Yes

# examine new binary SI variable
table(dcya$SI_bin)
prop.table(table(dcya$SI_bin))


# Suicide Attempt (SA)
table(dcya$q0052)
prop.table(table(dcya$q0052))

## A binary indication of SA
dcya_SA <- dcya$q0052
dcya_SA[dcya$q0052 == 1] <- 0
dcya_SA[dcya$q0052 == 2 | dcya$q0052 == 3] <- 1
dcya["SA_bin"] <- dcya_SA
# SA: 0 = No, 1 = Yes

# examine new binary SA variable
table(dcya$SA_bin)
prop.table(table(dcya$SA_bin))


# Nonsuicidal Self-injury (NSSI)
table(dcya$q0053)
prop.table(table(dcya$q0053))

## A binary indication of NSSI
dcya_NSSI <- dcya$q0053
dcya_NSSI[dcya$q0053 == 1] <- 0
dcya_NSSI[dcya$q0053 == 2 | dcya$q0053 == 3 | dcya$q0053 == 4] <- 1
dcya["NSSI_bin"] <- dcya_NSSI
# NSSI: 0 = None, 1 = Any

# examine new binary NSSI variable
table(dcya$NSSI_bin)
prop.table(table(dcya$NSSI_bin))

# Depression
table(dcya$q0049)
prop.table(table(dcya$q0049))

## A binary indication of Depression
dcya_depression <- dcya$q0049
dcya_depression[dcya$q0049 == 2] <- 0
dcya_depression[dcya$q0049 == 1] <- 1
dcya["Depression_bin"] <- dcya_depression
# Depression: 0 = No, 1 = Yes

# examine new binary Depression variable
table(dcya$Depression_bin)
prop.table(table(dcya$Depression_bin))


# GAD-2 Anxiety
# Recode anxiety questions to be on consistent scale 0-3
dcya["FeltAnxious"] <- abs(dcya$q0046_0001 - 4)
dcya["CantStopWorry"] <- abs(dcya$q0046_0002 - 4)

# Add a column that is the combined anxiety score (GAD2)
dcya["GAD2"] = dcya$FeltAnxious + dcya$CantStopWorry

# A binary indication of anxiety based on GAD2 threshold (clinical cutoff)
dcya_anxious <- dcya$GAD2
dcya_anxious[dcya$GAD2 < 3] <- 0
dcya_anxious[dcya$GAD2 >= 3] <- 1
dcya["Anxious_bin"] = dcya_anxious

# examine new binary Anxiety variable
table(dcya$Anxious_bin)
prop.table(table(dcya$Anxious_bin))

# examine alpha coefficient in aggregate sample
gad2_scale <- data.frame(dcya[c('FeltAnxious', 'CantStopWorry')])
psych::alpha(gad2_scale)

# Peer Victimization
dcya$PeerVic_mean <- rowMeans(subset(dcya, select = c('q0105_0002', 'q0105_0004', 'q0105_0009', 'q0105_0011')))
# Examine mean
m <- mean(dcya$PeerVic_mean)
m
# examine median of 4-item peer vic subscale
Peervic.mdn <- dcya[, c('q0105_0002', 'q0105_0004', 'q0105_0009', 'q0105_0011')]
Peervic.mdn2 <- data.matrix(Peervic.mdn)
median(Peervic.mdn2)
hist(dcya$PeerVic_mean)

## test for skewness/kurtosis
skewness(dcya$PeerVic_mean, na.rm = TRUE)
kurtosis(dcya$PeerVic_mean, na.rm = TRUE)

## check internal consistency in aggregate sample
psych::alpha(Peervic.mdn2)

### compute alpha across trans and GNC groups (2 groups in which moderation is tested)
## Trans youth
dcya_transgender = dcya %>%
  filter(Transgender_bin == "2_Transgender")

## GNC youth
dcya_GNC = dcya %>%
  filter(GNC == "2_Androgynous" | GNC == "3_ModeratelyGNC" | GNC == "4_HighlyGNC")

# alpha among transgender youth
PV_Scale_trans <- data.frame(dcya_transgender[c('q0105_0002', 'q0105_0004', 'q0105_0009', 'q0105_0011')])
psych::alpha(PV_Scale_trans)
# alpha among GNC youth
PV_Scale_GNC <- data.frame(dcya_GNC[c('q0105_0002', 'q0105_0004', 'q0105_0009', 'q0105_0011')])
psych::alpha(PV_Scale_GNC)

### binary indication of peer victimization (dichotimize based on distribution of mean value of 4-item
### peer victimization subscale; note extreme skew in distribution, thus, use median value = 1. Note this also lines
### up with qualitative interpretation of scale, in which 1 = Never.)
dcya_pv <- dcya$PeerVic_mean
dcya_pv[dcya$PeerVic_mean == 1] <- 0
dcya_pv[dcya$PeerVic_mean > 1] <- 1
dcya["PeerVic_bin"] = dcya_pv

# PeerVic Binary: 0 = No Peer Vic, 1 = Any Peer Vic 
table(dcya$PeerVic_bin)
prop.table(table(dcya$PeerVic_bin))


# BB Harassment
table(dcya$q0106_0002, exclude = NULL)
prop.table(table(dcya$q0106_0002, exclude = NULL))

## A binary indication of BB Harassment
dcya_BB_bin <- dcya$q0106_0002
dcya_BB_bin[dcya$q0106_0002 == 1] <- 0
dcya_BB_bin[dcya$q0106_0002 == 2 | dcya$q0106_0002 == 3 | dcya$q0106_0002 == 4] <- 1
dcya["BB_bin"] <- dcya_BB_bin
# BB Harassment : 0 = None, 1 = Any

# examine new binary variable
table(dcya$BB_bin)
prop.table(table(dcya$BB_bin))


#################
## Moderators ##
#################
# School-connectedness scale, 4 items, see Poteat et al 2011
# recode items from 1-4 (Always-Never) to 4-1 (Never-Always)
dcya["FeelClose"] <- car::recode(dcya$q0095_0002, "1 = 4; 2 = 3; 3 = 2; 4 = 1")
dcya["TreatFairly"] <- car::recode(dcya$q0095_0004, "1 = 4; 2 = 3; 3 = 2; 4 = 1")
dcya["AdultsTalk"] <- car::recode(dcya$q0095_0005, "1 = 4; 2 = 3; 3 = 2; 4 = 1")
dcya["Belong"] <- car::recode(dcya$q0095_0006, "1 = 4; 2 = 3; 3 = 2; 4 = 1")

# sum items, create scale
dcya$SC_Scale <- dcya$FeelClose + dcya$TreatFairly + dcya$AdultsTalk + dcya$Belong 
# examine scale
table(dcya$SC_Scale, exclude = NULL)
prop.table(table(dcya$SC_Scale, exclude = NULL))
hist(dcya$SC_Scale, xlim = c(4,16), col = "blue")
# calculate mean and store values in new variable
dcya$SC_Scale_Mean <- rowMeans2(dcya[c('FeelClose', 'TreatFairly', 
                                       'AdultsTalk', 'Belong')], min = 2, na.rm = TRUE)
# examine means
table(dcya$SC_Scale_Mean, exclude = NULL)
prop.table(table(dcya$SC_Scale_Mean, exclude = NULL))
hist(dcya$SC_Scale_Mean, col = "lightblue")

# report Cronbach's alpha (measure of internal consistency), recommeded alpha btwn. 0.65 and 0.8+
# raw score = based on covariance matrix; standardized = based on correlation matrix
SC_Scale <- data.frame(dcya[c('FeelClose', 'TreatFairly', 'AdultsTalk', 'Belong')])
psych::alpha(SC_Scale)

## by groups
## Trans youth
dcya_transgender = dcya %>%
  filter(Transgender_bin == "2_Transgender")
## GNC youth
dcya_GNC = dcya %>%
  filter(GNC == "2_Androgynous" | GNC == "3_ModeratelyGNC" | GNC == "4_HighlyGNC")


## Transgender youth
SC_Scale_trans <- data.frame(dcya_transgender[c('FeelClose', 'TreatFairly', 'AdultsTalk', 'Belong')])
psych::alpha(SC_Scale_trans)

SC_Scale_GNC <- data.frame(dcya_GNC[c('FeelClose', 'TreatFairly', 'AdultsTalk', 'Belong')])
psych::alpha(SC_Scale_GNC)

### center variable for regression analyses
dcya["SC_Scale_Mean.c"] <- scale(dcya$SC_Scale_Mean, center = TRUE, scale = FALSE)
class(dcya$SC_Scale_Mean.c)
# coerce to vector
dcya$SC_Scale_Mean.c <- as.vector(dcya$SC_Scale_Mean.c)
summary(dcya$SC_Scale_Mean.c, exclude = NULL)
class(dcya$SC_Scale_Mean.c)

# Family Support & Monitoring scale, 7 items
# recode items from 1-4 (Always-Never) to 4-1 (Never-Always)
dcya["WhereIam"] <- car::recode(dcya$q0025_0001, "1 = 4; 2 = 3; 3 = 2; 4 = 1")
dcya["ClearRules"] <- car::recode(dcya$q0025_0002, "1 = 4; 2 = 3; 3 = 2; 4 = 1")
dcya["FuturePlans"] <- car::recode(dcya$q0025_0003, "1 = 4; 2 = 3; 3 = 2; 4 = 1")
dcya["Consequences"] <- car::recode(dcya$q0025_0004, "1 = 4; 2 = 3; 3 = 2; 4 = 1")
dcya["DoWell"] <- car::recode(dcya$q0025_0005, "1 = 4; 2 = 3; 3 = 2; 4 = 1")
dcya["Progress"] <- car::recode(dcya$q0025_0006, "1 = 4; 2 = 3; 3 = 2; 4 = 1")
dcya["Talk"] <- car::recode(dcya$q0025_0007, "1 = 4; 2 = 3; 3 = 2; 4 = 1")

# sum items, create scale
dcya$FSM_Scale <- dcya$WhereIam + dcya$ClearRules + dcya$FuturePlans + dcya$Consequences + dcya$DoWell + dcya$Progress + dcya$Talk
# examine scale
table(dcya$FSM_Scale, exclude = NULL)
hist(dcya$FSM_Scale)
# calculate mean and store values in new variable
dcya$FSM_Scale_Mean <- rowMeans2(dcya[c('WhereIam', 'ClearRules', 'FuturePlans', 'Consequences',
                                        'DoWell', 'Progress', 'Talk')], min = 4, na.rm = TRUE)
# examine means
table(dcya$FSM_Scale_Mean, exclude = NULL)
hist(dcya$FSM_Scale_Mean, col = "lightblue")
# report Cronbach's alpha (measure of internal consistency), recommeded alpha btwn. 0.65 and 0.8+
FSM_Scale <- data.frame(dcya[c('WhereIam', 'ClearRules', 'FuturePlans', 'Consequences', 'DoWell', 'Progress', 'Talk')])
psych::alpha(FSM_Scale)

## by groups
## Trans youth
dcya_transgender = dcya %>%
  filter(Transgender_bin == "2_Transgender")
## GNC youth
dcya_GNC = dcya %>%
  filter(GNC == "2_Androgynous" | GNC == "3_ModeratelyGNC" | GNC == "4_HighlyGNC")

# Trans youth
FSM_Scale_trans <- data.frame(dcya_transgender[c('WhereIam', 'ClearRules', 'FuturePlans', 'Consequences', 'DoWell', 'Progress', 'Talk')])
psych::alpha(FSM_Scale_trans)
# GNC youth
FSM_Scale_GNC <- data.frame(dcya_GNC[c('WhereIam', 'ClearRules', 'FuturePlans', 'Consequences', 'DoWell', 'Progress', 'Talk')])
psych::alpha(FSM_Scale_GNC)

### center variable for regression analyses
dcya["FSM_Scale_Mean.c"] <- scale(dcya$FSM_Scale_Mean, center = TRUE, scale = FALSE)
class(dcya$FSM_Scale_Mean.c)
# coerce to vector
dcya$FSM_Scale_Mean.c <- as.vector(dcya$FSM_Scale_Mean.c)
summary(dcya$FSM_Scale_Mean.c, exclude = NULL)
class(dcya$FSM_Scale_Mean.c)

	
###########################################################################
# check data
dim(dcya)

###########################################################################
## Analyses

## Step 1: calculate weighted proportions & conduct chi-squared test 
## Transgender relative to Cisgender youth
# SI
## 0 = NO, 1 = YES
## need to apply survey weights
#a <- CrossTable(xtabs(dcya$Weight ~ dcya$SI_bin + dcya$Transgender_bin), chisq = TRUE)


# SA
#CrossTable(xtabs(dcya$Weight ~ dcya$SA_bin + dcya$Transgender_bin), chisq = TRUE)

# NSSI
#CrossTable(xtabs(dcya$Weight ~ dcya$NSSI_bin + dcya$Transgender_bin), chisq = TRUE)

# Anxiety
#CrossTable(xtabs(dcya$Weight ~ dcya$Anxious_bin + dcya$Transgender_bin), chisq = TRUE)

# Depression 
#CrossTable(xtabs(dcya$Weight ~ dcya$Depression_bin + dcya$Transgender_bin), chisq = TRUE)

# Peer Victimization
#CrossTable(xtabs(dcya$Weight ~ dcya$PeerVic_bin + dcya$Transgender_bin), chisq = TRUE)

# Bias-Based Harassment
#CrossTable(xtabs(dcya$Weight ~ dcya$BB_bin + dcya$Transgender_bin), chisq = TRUE)


#dcya_vis <- data.frame(matrix(ncol = 3, nrow = 0))
#colnames(dcya_vis) <- c("Outcome", "Cisgender", "Transgender")
#dcya_err <- data.frame(matrix(ncol = 7, nrow = 0))
#colnames(dcya_err) <- c("Outcome", "Cisgender.n", "Transgender.n", "Cisgender.lb", "Cisgender.ub", "Transgender.lb", "Transgender.ub")
#outcomes = c("SI_bin", "SA_bin", "NSSI_bin", "Anxious_bin", "Depression_bin", "PeerVic_bin", "BB_bin")

#for (o in outcomes) {
#  ret = CrossTable(xtabs(dcya$Weight ~ dcya[[o]] + dcya$Transgender_bin), chisq = TRUE)
#  dcya_vis[nrow(dcya_vis)+1,] <- c(o, as.vector(ret$prop.col[2,]) * 100)
#  dcya_err[nrow(dcya_err)+1,] <- c(o, as.vector(colSums(ret$t)), 0, 0, 0, 0)
#}

#dcya_vis$Transgender <- as.numeric(dcya_vis$Transgender)
#dcya_vis$Cisgender <- as.numeric(dcya_vis$Cisgender)
#dcya_err$Transgender.n <- as.numeric(dcya_err$Transgender.n)
#dcya_err$Cisgender.n <- as.numeric(dcya_err$Cisgender.n)

#phat <- dcya_vis$Cisgender / 100
#dcya_err$Cisgender.lb <- (phat - (qnorm(0.975) * sqrt(phat * (1 - phat) / dcya_err$Cisgender.n))) * 100
#dcya_err$Cisgender.ub <- (phat + (qnorm(0.975) * sqrt(phat * (1 - phat) / dcya_err$Cisgender.n))) * 100

#phat <- dcya_vis$Transgender / 100
#dcya_err$Transgender.lb <- (phat - (qnorm(0.975) * sqrt(phat * (1 - phat) / dcya_err$Transgender.n))) * 100
#dcya_err$Transgender.ub <- (phat + (qnorm(0.975) * sqrt(phat * (1 - phat) / dcya_err$Transgender.n))) * 100

#df <- melt(dcya_vis, id.vars = "Outcome", variable.name = "variable", value.name = "value")
#df$lb <- c(dcya_err$Cisgender.lb, dcya_err$Transgender.lb)
#df$ub <- c(dcya_err$Cisgender.ub, dcya_err$Transgender.ub)

#p1 <- ggplot(df, aes(x = Outcome, y = value, ymin = lb, ymax = ub)) + 
#  geom_bar(aes(fill = variable), stat = "identity", width = 0.75, position = "dodge") +
#  geom_errorbar(aes(fill = variable), position = position_dodge(width=0.75), colour = "black", width = 0.2)

#p1 + ggtitle("(a) Mental health and victimization by gender identity") + 
#  scale_x_discrete(name = "",
#                      limits = c("Anxious_bin", "Depression_bin", "NSSI_bin", 
#                       "SI_bin", "SA_bin", "BB_bin", "PeerVic_bin"),
#                      labels = c("Anxiety", "Depression", "Nonsuicidal\n Self-injury", "Suicidal\n Ideation", "Suicide\n Attempt", "Bias-based\n Harassment", "Peer\n Victimization")) +
#  scale_y_continuous(name = "Percent", limits=c(0, 80)) +
#  scale_fill_brewer(name = "Gender Identity",
#                      labels = c("Cisgender", "Transgender"),
#                      palette = "Paired") +
#  theme_minimal() +
#  theme(axis.text.x = element_text(margin = margin(t = -20)),
#        panel.grid.major.x = element_blank())

## Gender Conformity/Modality Levels 
# SI
## 0 = NO, 1 = YES
## apply weights
CrossTable(xtabs(dcya$Weight ~ dcya$SI_bin + dcya$GNCT), chisq = TRUE)

# SA
CrossTable(xtabs(dcya$Weight ~ dcya$SA_bin + dcya$GNCT), chisq = TRUE)

# NSSI
CrossTable(xtabs(dcya$Weight ~ dcya$NSSI_bin + dcya$GNCT), chisq = TRUE)

# Anxiety
CrossTable(xtabs(dcya$Weight ~ dcya$Anxious_bin + dcya$GNCT), chisq = TRUE)

# Depression
CrossTable(xtabs(dcya$Weight ~ dcya$Depression_bin + dcya$GNCT), chisq = TRUE)

# Peer Victimization
CrossTable(xtabs(dcya$Weight ~ dcya$PeerVic_bin + dcya$GNCT), chisq = TRUE)

# Bias-Based Harassment
CrossTable(xtabs(dcya$Weight ~ dcya$BB_bin + dcya$GNCT), chisq = TRUE)


## plot 2, by gender conformity level
# dcya_vis2 <- data.frame(matrix(ncol = 5, nrow = 0))
#colnames(dcya_vis2) <- c("Outcome", "GC", "Androgynous", "Moderately_GNC", "Highly_GNC")
#dcya_err2 <- data.frame(matrix(ncol = 13, nrow = 0))
#colnames(dcya_err2) <- c("Outcome", "GC.n", "Androgynous.n", "ModeratelyGNC.n", "HighlyGNC.n",
#                         "GC.lb", "GC.ub", "Androgynous.lb", "Androgynous.ub", "ModeratelyGNC.lb", 
#                         "ModeratelyGNC.ub", "HighlyGNC.lb", "HighlyGNC.ub")
#outcomes2 = c("SI_bin", "SA_bin", "NSSI_bin", "Anxious_bin", "Depression_bin", "PeerVic_bin", "BB_bin")

#for (o in outcomes2) {
#  ret = CrossTable(xtabs(dcya$Weight ~ dcya[[o]] + dcya$GNC), chisq = TRUE)
#  dcya_vis2[nrow(dcya_vis2)+1,] <- c(o, as.vector(ret$prop.col[2,]) * 100)
#  dcya_err2[nrow(dcya_err2)+1,] <- c(o, as.vector(colSums(ret$t)), 0, 0, 0, 0, 0, 0, 0, 0)
#}

#dcya_vis2$GC <- as.numeric(dcya_vis2$GC)
#dcya_vis2$Androgynous <- as.numeric(dcya_vis2$Androgynous)
#dcya_vis2$Moderately_GNC <- as.numeric(dcya_vis2$Moderately_GNC)
#dcya_vis2$Highly_GNC <- as.numeric(dcya_vis2$Highly_GNC)

#dcya_err2$GC.n <- as.numeric(dcya_err2$GC.n)
#dcya_err2$Androgynous.n <- as.numeric(dcya_err2$Androgynous.n)
#dcya_err2$ModeratelyGNC.n <- as.numeric(dcya_err2$ModeratelyGNC.n)
#dcya_err2$HighlyGNC.n <- as.numeric(dcya_err2$HighlyGNC.n)

#phat <- dcya_vis2$GC / 100
#dcya_err2$GC.lb <- (phat - (qnorm(0.975) * sqrt(phat * (1 - phat) / dcya_err2$GC.n))) * 100
#dcya_err2$GC.ub <- (phat + (qnorm(0.975) * sqrt(phat * (1 - phat) / dcya_err2$GC.n))) * 100

#phat <- dcya_vis2$Androgynous / 100
#dcya_err2$Androgynous.lb <- (phat - (qnorm(0.975) * sqrt(phat * (1 - phat) / dcya_err2$Androgynous.n))) * 100
#dcya_err2$Androgynous.ub <- (phat + (qnorm(0.975) * sqrt(phat * (1 - phat) / dcya_err2$Androgynous.n))) * 100

#phat <- dcya_vis2$Moderately_GNC / 100
#dcya_err2$Moderately_GNC.lb <- (phat - (qnorm(0.975) * sqrt(phat * (1 - phat) / dcya_err2$ModeratelyGNC.n))) * 100
#dcya_err2$Moderately_GNC.ub <- (phat + (qnorm(0.975) * sqrt(phat * (1 - phat) / dcya_err2$ModeratelyGNC.n))) * 100

#phat <- dcya_vis2$Highly_GNC / 100
#dcya_err2$Highly_GNC.lb <- (phat - (qnorm(0.975) * sqrt(phat * (1 - phat) / dcya_err2$HighlyGNC.n))) * 100
#dcya_err2$Highly_GNC.ub <- (phat + (qnorm(0.975) * sqrt(phat * (1 - phat) / dcya_err2$HighlyGNC.n))) * 100


#df2 <- melt(dcya_vis2, id.vars = "Outcome", variable.name = "variable", value.name = "value")
#df2$lb <- c(dcya_err2$GC.lb, dcya_err2$Androgynous.lb, dcya_err2$Moderately_GNC.lb, dcya_err2$Highly_GNC.lb)
#df2$ub <- c(dcya_err2$GC.ub, dcya_err2$Androgynous.ub, dcya_err2$Moderately_GNC.ub, dcya_err2$Highly_GNC.ub)

#p2 <- ggplot(df2, aes(fill = variable, x = Outcome, y = value, ymin = lb, ymax = ub)) + 
#  geom_bar(aes(fill = variable), stat = "identity", width = 0.75, position = "dodge") +
#  geom_errorbar(aes(fill = variable), position = position_dodge(width=0.75), colour = "black", width = 0.2)

#p2 + ggtitle("(b) Mental health and victimization by gender conformity level") + 
#  scale_x_discrete(name = "",
#                   limits = c("Anxious_bin", "Depression_bin", "NSSI_bin", 
#                              "SI_bin", "SA_bin", "BB_bin", "PeerVic_bin"),
#                   labels = c("Anxiety", "Depression", "Nonsuicidal\n Self-injury", "Suicidal\n Ideation", "Suicide\n Attempt", "Bias-based\n Harassment", "Peer\n Victimization")) +
#  scale_y_continuous(name = "Percent", limits=c(0, 80)) +
#  scale_fill_brewer(name = "Gender Conformity Level",
#                    labels = c("Gender Conforming", "Androgynous", "Moderately GNC", "Highly GNC"),
#                    palette = "Paired") +
#  theme_minimal() +
#  theme(axis.text.x = element_text(margin = margin(t = -20)),
#        panel.grid.major.x = element_blank())

### multivariable models ###
### Note: initial models estimated w. sexual orientation, based on feedback, sexual orientation removed from models

## GNCT relative to gender conforming youth, adjusted for covariates
# SI
m8 <- glm(SI_bin ~ GNCT + AgeRecode.c + RaceRecode.p + Sex + GLBQ.p, data = dcya, weights = Weight, family = quasibinomial)
summary(m8)

# cluster standard errors by type
cluster_se_SI_GNC <- cluster.vcov(m8, ~Building)
cluster_se_SI_GNC_extracted <- sqrt(diag(cluster_se_SI_GNC))
coeftest(m8, cluster_se_SI_GNC)

# Adjusted Odds Ratio (AOR) and 95% CI
est <- exp(m8$coef)
lower.ci <- est-qnorm(0.975)*cluster_se_SI_GNC_extracted
upper.ci <- est+qnorm(0.975)*cluster_se_SI_GNC_extracted
cbind(est,lower.ci, upper.ci)


# SA
m9 <- glm(SA_bin ~ GNCT + AgeRecode.c + RaceRecode.p + Sex + GLBQ.p, data = dcya, weights = Weight, family = quasibinomial)
summary(m9)

# cluster standard errors by type
cluster_se_SA_GNC <- cluster.vcov(m9, ~Building)
cluster_se_SA_GNC_extracted <- sqrt(diag(cluster_se_SA_GNC))
coeftest(m9, cluster_se_SA_GNC)

# Adjusted Odds Ratio (AOR) and 95% CI
est <- exp(m9$coef)
lower.ci <- est-qnorm(0.975)*cluster_se_SA_GNC_extracted 
upper.ci <- est+qnorm(0.975)*cluster_se_SA_GNC_extracted 
cbind(est,lower.ci, upper.ci)


# NSSI
m10 <- glm(NSSI_bin ~ GNCT + AgeRecode.c + RaceRecode.p + Sex + GLBQ.p, data = dcya, weights = Weight, family = quasibinomial)
summary(m10)

# cluster standard errors by type
cluster_se_NSSI_GNC <- cluster.vcov(m10, ~Building)
cluster_se_NSSI_GNC_extracted <- sqrt(diag(cluster_se_NSSI_GNC))
coeftest(m10, cluster_se_NSSI_GNC)

# Adjusted Odds Ratio (AOR) and 95% CI
est <- exp(m10$coef)
lower.ci <- est-qnorm(0.975)*cluster_se_NSSI_GNC_extracted
upper.ci <- est+qnorm(0.975)*cluster_se_NSSI_GNC_extracted
cbind(est,lower.ci, upper.ci)


# Anxiety 
m11 <- glm(Anxious_bin ~ GNCT + AgeRecode.c + RaceRecode.p + Sex + GLBQ.p, data = dcya, weights = Weight, family = quasibinomial)
summary(m11)

# cluster standard errors by type
cluster_se_Anxious_GNC <- cluster.vcov(m11, ~Building)
cluster_se_Anxious_GNC_extracted <- sqrt(diag(cluster_se_Anxious_GNC))
coeftest(m11, cluster_se_Anxious_GNC)

# Adjusted Odds Ratio (AOR) and 95% CI
est <- exp(m11$coef)
lower.ci <- est-qnorm(0.975)*cluster_se_Anxious_GNC_extracted
upper.ci <- est+qnorm(0.975)*cluster_se_Anxious_GNC_extracted
cbind(est,lower.ci, upper.ci)


# Depression
m12 <- glm(Depression_bin ~ GNCT + AgeRecode.c + RaceRecode.p + Sex + GLBQ.p, data = dcya, weights = Weight, family = quasibinomial)
summary(m12)

# cluster standard errors by type
cluster_se_depress_GNC <- cluster.vcov(m12, ~Building)
cluster_se_depress_GNC_extracted <- sqrt(diag(cluster_se_depress_GNC))
coeftest(m12, cluster_se_depress_GNC)

# Adjusted Odds Ratio (AOR) and 95% CI
est <- exp(m12$coef)
lower.ci <- est-qnorm(0.975)*cluster_se_depress_GNC_extracted
upper.ci <- est+qnorm(0.975)*cluster_se_depress_GNC_extracted
cbind(est,lower.ci, upper.ci)

# Peer Victimization
m13 <- glm(PeerVic_bin ~ GNCT + AgeRecode.c + RaceRecode.p + Sex + GLBQ.p, data = dcya, weights = Weight, family = quasibinomial)
summary(m13)

# cluster standard errors by type
cluster_se_PV_GNC <- cluster.vcov(m13, ~Building)
cluster_se_PV_GNC_extracted <- sqrt(diag(cluster_se_PV_GNC))
coeftest(m13, cluster_se_PV_GNC)

# Adjusted Odds Ratio (AOR) and 95% CI
est <- exp(m13$coef)
lower.ci <- est-qnorm(0.975)*cluster_se_PV_GNC_extracted
upper.ci <- est+qnorm(0.975)*cluster_se_PV_GNC_extracted
cbind(est,lower.ci, upper.ci)

# Bias-Based Harassment
m14 <- glm(BB_bin ~ GNCT + AgeRecode.c + RaceRecode.p + Sex + GLBQ.p, data = dcya, weights = Weight, family = quasibinomial)
summary(m14)

# cluster standard errors by type
cluster_se_BB_GNC <- cluster.vcov(m14, ~Building)
cluster_se_BB_GNC_extracted <- sqrt(diag(cluster_se_BB_GNC))
coeftest(m14, cluster_se_BB_GNC)

# Adjusted Odds Ratio (AOR) and 95% CI
est <- exp(m14$coef)
lower.ci <- est-qnorm(0.975)*cluster_se_BB_GNC_extracted
upper.ci <- est+qnorm(0.975)*cluster_se_BB_GNC_extracted
cbind(est,lower.ci, upper.ci)

########################################################################################
## Step 3: Stratified models to examine effect of victimization on mental health
########################################################################################
## Trans youth
dcya_transgender = dcya %>%
	filter(Transgender_bin == "2_Transgender")
## SI by PV
# unadjusted model
trans_glm1 <- glm(SI_bin ~ PeerVic_bin, weights = Weight, family = "quasibinomial", data = dcya_transgender)
summary(trans_glm1)
cluster_se_trans <- cluster.vcov(trans_glm1, ~Building)
cluster_se_trans_extracted <- sqrt(diag(cluster_se_trans))
coeftest(trans_glm1, cluster_se_trans)

# Odds Ratio and 95% CI
est <- exp(trans_glm1$coef)
lower.ci <- est-qnorm(0.975)*cluster_se_trans_extracted
upper.ci <- est+qnorm(0.975)*cluster_se_trans_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
trans_glm2 <- glm(SI_bin ~ PeerVic_bin + AgeRecode.c + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_transgender)
summary(trans_glm2)
cluster_se_trans_a <- cluster.vcov(trans_glm2, ~Building)
cluster_se_trans_extracted_a <- sqrt(diag(cluster_se_trans_a))
coeftest(trans_glm2, cluster_se_trans_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(trans_glm2$coef)
lower.ci <- est-qnorm(0.975)*cluster_se_trans_extracted_a
upper.ci <- est+qnorm(0.975)*cluster_se_trans_extracted_a
cbind(est,lower.ci, upper.ci)

## SI by BB
# unadjusted model
trans_glm3 <- glm(SI_bin ~ BB_bin, weights = Weight, family = "quasibinomial", data = dcya_transgender)
summary(trans_glm3)
cluster_se_trans_BB <- cluster.vcov(trans_glm3, ~Building)
cluster_se_trans_BB_extracted <- sqrt(diag(cluster_se_trans_BB))
coeftest(trans_glm3, cluster_se_trans_BB)

# Odds Ratio and 95% CI
est <- exp(trans_glm3$coef)
lower.ci <- est-qnorm(0.975)*cluster_se_trans_BB_extracted
upper.ci <- est+qnorm(0.975)*cluster_se_trans_BB_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
trans_glm4 <- glm(SI_bin ~ BB_bin + AgeRecode.c + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_transgender)
summary(trans_glm4)
cluster_se_trans_BB_a <- cluster.vcov(trans_glm4, ~Building)
cluster_se_trans_BB_extracted_a <- sqrt(diag(cluster_se_trans_BB_a))
coeftest(trans_glm4, cluster_se_trans_BB_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(trans_glm4$coef)
lower.ci <- est-qnorm(0.975)*cluster_se_trans_BB_extracted_a
upper.ci <- est+qnorm(0.975)*cluster_se_trans_BB_extracted_a
cbind(est,lower.ci, upper.ci)


## SA by PV
# unadjusted model
trans_glm5 <- glm(SA_bin ~ PeerVic_bin, weights = Weight, family = "quasibinomial", data = dcya_transgender)
summary(trans_glm5)
cluster_se_trans_SA_PV <- cluster.vcov(trans_glm5, ~Building)
cluster_se_trans_SA_PV_extracted <- sqrt(diag(cluster_se_trans_SA_PV))
coeftest(trans_glm5, cluster_se_trans_SA_PV)

# Odds Ratio and 95% CI
est <- exp(trans_glm5$coef)
lower.ci <- est-qnorm(0.975)*cluster_se_trans_SA_PV_extracted
upper.ci <- est+qnorm(0.975)*cluster_se_trans_SA_PV_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
trans_glm6 <- glm(SA_bin ~ PeerVic_bin + AgeRecode.c + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_transgender)
summary(trans_glm6)
cluster_se_trans_SA_PV_a <- cluster.vcov(trans_glm6, ~Building)
cluster_se_trans_SA_PV_extracted_a <- sqrt(diag(cluster_se_trans_SA_PV_a))
coeftest(trans_glm6, cluster_se_trans_SA_PV_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(trans_glm6$coef)
lower.ci <- est-qnorm(0.975)*cluster_se_trans_SA_PV_extracted_a
upper.ci <- est+qnorm(0.975)*cluster_se_trans_SA_PV_extracted_a
cbind(est,lower.ci, upper.ci)

## SA by BB
# unadjusted model
trans_glm7 <- glm(SA_bin ~ BB_bin, weights = Weight, family = "quasibinomial", data = dcya_transgender)
summary(trans_glm7)
cluster_se_trans_SA_BB <- cluster.vcov(trans_glm7, ~Building)
cluster_se_trans_SA_BB_extracted <- sqrt(diag(cluster_se_trans_SA_BB))
coeftest(trans_glm7, cluster_se_trans_SA_BB)

# Odds Ratio and 95% CI
est <- exp(trans_glm7$coef)
lower.ci <- est-qnorm(0.975)*cluster_se_trans_SA_BB_extracted
upper.ci <- est+qnorm(0.975)*cluster_se_trans_SA_BB_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
trans_glm8 <- glm(SA_bin ~ BB_bin + AgeRecode.c + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_transgender)
summary(trans_glm8)
cluster_se_trans_SA_BB_a <- cluster.vcov(trans_glm8, ~Building)
cluster_se_trans_SA_BB_extracted_a <- sqrt(diag(cluster_se_trans_SA_BB_a))
coeftest(trans_glm8, cluster_se_trans_SA_BB_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(trans_glm8$coef)
lower.ci <- est-qnorm(0.975)*cluster_se_trans_SA_BB_extracted_a
upper.ci <- est+qnorm(0.975)*cluster_se_trans_SA_BB_extracted_a
cbind(est,lower.ci, upper.ci)

## NSSI by PV
# unadjusted model
trans_glm9 <- glm(NSSI_bin ~ PeerVic_bin, weights = Weight, family = "quasibinomial", data = dcya_transgender)
summary(trans_glm9)
cluster_se_trans_NSSI_PV <- cluster.vcov(trans_glm9, ~Building)
cluster_se_trans_NSSI_PV_extracted <- sqrt(diag(cluster_se_trans_NSSI_PV))
coeftest(trans_glm9, cluster_se_trans_NSSI_PV)

# Odds Ratio and 95% CI
est <- exp(trans_glm9$coef)
lower.ci <- est-qnorm(0.975)*cluster_se_trans_NSSI_PV_extracted
upper.ci <- est+qnorm(0.975)*cluster_se_trans_NSSI_PV_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
trans_glm10 <- glm(NSSI_bin ~ PeerVic_bin + AgeRecode.c + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_transgender)
summary(trans_glm10)
cluster_se_trans_NSSI_PV_a <- cluster.vcov(trans_glm10, ~Building)
cluster_se_trans_NSSI_PV_extracted_a <- sqrt(diag(cluster_se_trans_NSSI_PV_a))
coeftest(trans_glm10, cluster_se_trans_NSSI_PV_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(trans_glm10$coef)
lower.ci <- est-qnorm(0.975)*cluster_se_trans_NSSI_PV_extracted_a 
upper.ci <- est+qnorm(0.975)*cluster_se_trans_NSSI_PV_extracted_a 
cbind(est,lower.ci, upper.ci)


## NSSI by BB
# unadjusted model
trans_glm11 <- glm(NSSI_bin ~ BB_bin, weights = Weight, family = "quasibinomial", data = dcya_transgender)
summary(trans_glm11)
cluster_se_trans_NSSI_BB <- cluster.vcov(trans_glm11, ~Building)
cluster_se_trans_NSSI_BB_extracted <- sqrt(diag(cluster_se_trans_NSSI_BB))
coeftest(trans_glm11, cluster_se_trans_NSSI_BB)

# Odds Ratio and 95% CI
est <- exp(trans_glm11$coef)
lower.ci <- est-qnorm(0.975)*cluster_se_trans_NSSI_BB_extracted
upper.ci <- est+qnorm(0.975)*cluster_se_trans_NSSI_BB_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
trans_glm12 <- glm(NSSI_bin ~ BB_bin + AgeRecode.c + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_transgender)
summary(trans_glm12)
cluster_se_trans_NSSI_BB_a <- cluster.vcov(trans_glm12, ~Building)
cluster_se_trans_NSSI_BB_extracted_a <- sqrt(diag(cluster_se_trans_NSSI_BB_a))
coeftest(trans_glm12, cluster_se_trans_NSSI_BB_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(trans_glm12$coef)
lower.ci <- est-qnorm(0.975)*cluster_se_trans_NSSI_BB_extracted_a 
upper.ci <- est+qnorm(0.975)*cluster_se_trans_NSSI_BB_extracted_a 
cbind(est,lower.ci, upper.ci)


## Anxiety by PV
# unadjusted model
trans_glm13 <- glm(Anxious_bin ~ PeerVic_bin, weights = Weight, family = "quasibinomial", data = dcya_transgender)
summary(trans_glm13)
cluster_se_trans_Anx_PV <- cluster.vcov(trans_glm13, ~Building)
cluster_se_trans_Anx_PV_extracted <- sqrt(diag(cluster_se_trans_Anx_PV))
coeftest(trans_glm13, cluster_se_trans_Anx_PV)

# Odds Ratio and 95% CI
est <- exp(trans_glm13$coef)
lower.ci <- est-qnorm(0.975)*cluster_se_trans_Anx_PV_extracted
upper.ci <- est+qnorm(0.975)*cluster_se_trans_Anx_PV_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
trans_glm14 <- glm(Anxious_bin ~ PeerVic_bin + AgeRecode.c + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_transgender)
summary(trans_glm14)
cluster_se_trans_Anx_PV_a <- cluster.vcov(trans_glm14, ~Building)
cluster_se_trans_Anx_PV_extracted_a <- sqrt(diag(cluster_se_trans_Anx_PV_a))
coeftest(trans_glm14, cluster_se_trans_Anx_PV_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(trans_glm14$coef)
lower.ci <- est-qnorm(0.975)*cluster_se_trans_Anx_PV_extracted_a
upper.ci <- est+qnorm(0.975)*cluster_se_trans_Anx_PV_extracted_a
cbind(est,lower.ci, upper.ci)

## Anxiety by BB
# unadjusted model
trans_glm15 <- glm(Anxious_bin ~ BB_bin, weights = Weight, family = "quasibinomial", data = dcya_transgender)
summary(trans_glm15)
cluster_se_trans_Anx_BB <- cluster.vcov(trans_glm15, ~Building)
cluster_se_trans_Anx_BB_extracted <- sqrt(diag(cluster_se_trans_Anx_BB))
coeftest(trans_glm15, cluster_se_trans_Anx_BB)

# Odds Ratio and 95% CI
est <- exp(trans_glm15$coef)
lower.ci <- est-1.96*cluster_se_trans_Anx_BB_extracted
upper.ci <- est+1.96*cluster_se_trans_Anx_BB_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
trans_glm16 <- glm(Anxious_bin ~ BB_bin + AgeRecode.c + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_transgender)
summary(trans_glm16)
cluster_se_trans_Anx_BB_a <- cluster.vcov(trans_glm16, ~Building)
cluster_se_trans_Anx_BB_extracted_a <- sqrt(diag(cluster_se_trans_Anx_BB_a))
coeftest(trans_glm16, cluster_se_trans_Anx_BB_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(trans_glm16$coef)
lower.ci <- est-qnorm(0.975)*cluster_se_trans_Anx_BB_extracted_a
upper.ci <- est+qnorm(0.975)*cluster_se_trans_Anx_BB_extracted_a
cbind(est,lower.ci, upper.ci)

## Depression by PV
# adjusted model
trans_glm17 <- glm(Depression_bin ~ PeerVic_bin + AgeRecode.c + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_transgender)
summary(trans_glm17)
cluster_se_a <- cluster.vcov(trans_glm17, ~Building)
cluster_se_extracted_a <- sqrt(diag(cluster_se_a))
coeftest(trans_glm17, cluster_se_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(trans_glm17$coef)
lower.ci <- est-qnorm(0.975)*cluster_se_extracted_a
upper.ci <- est+qnorm(0.975)*cluster_se_extracted_a
cbind(est,lower.ci, upper.ci)

## Depression by BB
# adjusted model
trans_glm18 <- glm(Depression_bin ~ BB_bin + AgeRecode.c + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_transgender)
summary(trans_glm18)
cluster_se_a <- cluster.vcov(trans_glm18, ~Building)
cluster_se_extracted_a <- sqrt(diag(cluster_se_a))
coeftest(trans_glm18,cluster_se_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(trans_glm18$coef)
lower.ci <- est-qnorm(0.975)*cluster_se_extracted_a
upper.ci <- est+qnorm(0.975)*cluster_se_extracted_a
cbind(est,lower.ci, upper.ci)

## GNC youth
dcya_GNC = dcya %>%
	filter(GNC == "2_Androgynous" | GNC == "3_ModeratelyGNC" | GNC == "4_HighlyGNC")
## SI by PV
# unadjusted model
GNC_glm1 <- glm(SI_bin ~ PeerVic_bin, weights = Weight, family = "quasibinomial", data = dcya_GNC)
summary(GNC_glm1)
cluster_se_GNC <- cluster.vcov(GNC_glm1, ~Building)
cluster_se_GNC_extracted <- sqrt(diag(cluster_se_GNC))
coeftest(GNC_glm1, cluster_se_GNC)

# Odds Ratio and 95% CI
est <- exp(GNC_glm1$coef)
lower.ci <- est-1.96*cluster_se_GNC_extracted
upper.ci <- est+1.96*cluster_se_GNC_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
GNC_glm2 <- glm(SI_bin ~ PeerVic_bin + AgeRecode + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_GNC)
summary(GNC_glm2)
cluster_se_GNC_a <- cluster.vcov(GNC_glm2, ~Building)
cluster_se_GNC_extracted_a <- sqrt(diag(cluster_se_GNC_a))
coeftest(GNC_glm2, cluster_se_GNC_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(GNC_glm2$coef)
lower.ci <- est-qnorm(0.975)*cluster_se_GNC_extracted_a
upper.ci <- est+qnorm(0.975)*cluster_se_GNC_extracted_a
cbind(est,lower.ci, upper.ci)


## SI by BB
# unadjusted model
GNC_glm3 <- glm(SI_bin ~ BB_bin, weights = Weight, family = "quasibinomial", data = dcya_GNC)
summary(GNC_glm3)
cluster_se_GNC_BB <- cluster.vcov(GNC_glm3, ~Building)
cluster_se_GNC_BB_extracted <- sqrt(diag(cluster_se_GNC_BB))
coeftest(GNC_glm3, cluster_se_GNC_BB)

# Odds Ratio and 95% CI
est <- exp(GNC_glm3$coef)
lower.ci <- est-1.96*cluster_se_GNC_BB_extracted
upper.ci <- est+1.96*cluster_se_GNC_BB_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
GNC_glm4 <- glm(SI_bin ~ BB_bin + AgeRecode + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_GNC)
summary(GNC_glm4)
cluster_se_GNC_BB_a <- cluster.vcov(GNC_glm4, ~Building)
cluster_se_GNC_BB_extracted_a <- sqrt(diag(cluster_se_GNC_BB_a))
coeftest(GNC_glm4, cluster_se_GNC_BB_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(GNC_glm4$coef)
lower.ci <- est-qnorm(0.975)*cluster_se_GNC_BB_extracted_a
upper.ci <- est+qnorm(0.975)*cluster_se_GNC_BB_extracted_a
cbind(est,lower.ci, upper.ci)


## SA by PV
# unadjusted model
GNC_glm5 <- glm(SA_bin ~ PeerVic_bin, weights = Weight, family = "quasibinomial", data = dcya_GNC)
summary(GNC_glm5)
cluster_se_GNC_SA_PV <- cluster.vcov(GNC_glm5, ~Building)
cluster_se_GNC_SA_PV_extracted <- sqrt(diag(cluster_se_GNC_SA_PV))
coeftest(GNC_glm5, cluster_se_GNC_SA_PV)

# Odds Ratio and 95% CI
est <- exp(GNC_glm5$coef)
lower.ci <- est-1.96*cluster_se_GNC_SA_PV_extracted
upper.ci <- est+1.96*cluster_se_GNC_SA_PV_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
GNC_glm6 <- glm(SA_bin ~ PeerVic_bin + AgeRecode + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_GNC)
summary(GNC_glm6)
cluster_se_GNC_SA_PV_a <- cluster.vcov(GNC_glm6, ~Building)
cluster_se_GNC_SA_PV_extracted_a <- sqrt(diag(cluster_se_GNC_SA_PV_a))
coeftest(GNC_glm6, cluster_se_GNC_SA_PV_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(GNC_glm6$coef)
lower.ci <- est-qnorm(0.975)*cluster_se_GNC_SA_PV_extracted_a
upper.ci <- est+qnorm(0.975)*cluster_se_GNC_SA_PV_extracted_a
cbind(est,lower.ci, upper.ci)

## SA by BB
# unadjusted model
GNC_glm7 <- glm(SA_bin ~ BB_bin, weights = Weight, family = "quasibinomial", data = dcya_GNC)
summary(GNC_glm7)
cluster_se_GNC_SA_BB <- cluster.vcov(GNC_glm7, ~Building)
cluster_se_GNC_SA_BB_extracted <- sqrt(diag(cluster_se_GNC_SA_BB))
coeftest(GNC_glm7, cluster_se_GNC_SA_BB)

# Odds Ratio and 95% CI
est <- exp(GNC_glm7$coef)
lower.ci <- est-1.96*cluster_se_GNC_SA_BB_extracted
upper.ci <- est+1.96*cluster_se_GNC_SA_BB_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
GNC_glm8 <- glm(SA_bin ~ BB_bin + AgeRecode + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_GNC)
summary(GNC_glm8)
cluster_se_GNC_SA_BB_a <- cluster.vcov(GNC_glm8, ~Building)
cluster_se_GNC_SA_BB_extracted_a <- sqrt(diag(cluster_se_GNC_SA_BB_a))
coeftest(GNC_glm8, cluster_se_GNC_SA_BB_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(GNC_glm8$coef)
lower.ci <- est-qnorm(0.975)*cluster_se_GNC_SA_BB_extracted_a
upper.ci <- est+qnorm(0.975)*cluster_se_GNC_SA_BB_extracted_a
cbind(est,lower.ci, upper.ci)

## NSSI by PV
# unadjusted model
GNC_glm9 <- glm(NSSI_bin ~ PeerVic_bin, weights = Weight, family = "quasibinomial", data = dcya_GNC)
summary(GNC_glm9)
cluster_se_GNC_NSSI_PV <- cluster.vcov(GNC_glm9, ~Building)
cluster_se_GNC_NSSI_PV_extracted <- sqrt(diag(cluster_se_GNC_NSSI_PV))
coeftest(GNC_glm9, cluster_se_GNC_NSSI_PV)

# Odds Ratio and 95% CI
est <- exp(GNC_glm9$coef)
lower.ci <- est-1.96*cluster_se_GNC_NSSI_PV_extracted
upper.ci <- est+1.96*cluster_se_GNC_NSSI_PV_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
GNC_glm10 <- glm(NSSI_bin ~ PeerVic_bin + AgeRecode + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_GNC)
summary(GNC_glm10)
cluster_se_GNC_NSSI_PV_a <- cluster.vcov(GNC_glm10, ~Building)
cluster_se_GNC_NSSI_PV_extracted_a <- sqrt(diag(cluster_se_GNC_NSSI_PV_a))
coeftest(GNC_glm10, cluster_se_GNC_NSSI_PV_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(GNC_glm10$coef)
lower.ci <- est-qnorm(0.975)*cluster_se_GNC_NSSI_PV_extracted_a 
upper.ci <- est+qnorm(0.975)*cluster_se_GNC_NSSI_PV_extracted_a 
cbind(est,lower.ci, upper.ci)


## NSSI by BB
# unadjusted model
GNC_glm11 <- glm(NSSI_bin ~ BB_bin, weights = Weight, family = "quasibinomial", data = dcya_GNC)
summary(GNC_glm11)
cluster_se_GNC_NSSI_BB <- cluster.vcov(GNC_glm11, ~Building)
cluster_se_GNC_NSSI_BB_extracted <- sqrt(diag(cluster_se_GNC_NSSI_BB))
coeftest(GNC_glm11, cluster_se_GNC_NSSI_BB)

# Odds Ratio and 95% CI
est <- exp(GNC_glm11$coef)
lower.ci <- est-1.96*cluster_se_GNC_NSSI_BB_extracted
upper.ci <- est+1.96*cluster_se_GNC_NSSI_BB_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
GNC_glm12 <- glm(NSSI_bin ~ BB_bin + AgeRecode + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_GNC)
summary(GNC_glm12)
cluster_se_GNC_NSSI_BB_a <- cluster.vcov(GNC_glm12, ~Building)
cluster_se_GNC_NSSI_BB_extracted_a <- sqrt(diag(cluster_se_GNC_NSSI_BB_a))
coeftest(GNC_glm12, cluster_se_GNC_NSSI_BB_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(GNC_glm12$coef)
lower.ci <- est-qnorm(0.975)*cluster_se_GNC_NSSI_BB_extracted_a 
upper.ci <- est+qnorm(0.975)*cluster_se_GNC_NSSI_BB_extracted_a 
cbind(est,lower.ci, upper.ci)


## Anxiety by PV
# unadjusted model
GNC_glm13 <- glm(Anxious_bin ~ PeerVic_bin, weights = Weight, family = "quasibinomial", data = dcya_GNC)
summary(GNC_glm13)
cluster_se_GNC_Anx_PV <- cluster.vcov(GNC_glm13, ~Building)
cluster_se_GNC_Anx_PV_extracted <- sqrt(diag(cluster_se_GNC_Anx_PV))
coeftest(GNC_glm13, cluster_se_GNC_Anx_PV)

# Odds Ratio and 95% CI
est <- exp(GNC_glm13$coef)
lower.ci <- est-1.96*cluster_se_GNC_Anx_PV_extracted
upper.ci <- est+1.96*cluster_se_GNC_Anx_PV_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
GNC_glm14 <- glm(Anxious_bin ~ PeerVic_bin + AgeRecode + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_GNC)
summary(GNC_glm14)
cluster_se_GNC_Anx_PV_a <- cluster.vcov(GNC_glm14, ~Building)
cluster_se_GNC_Anx_PV_extracted_a <- sqrt(diag(cluster_se_GNC_Anx_PV_a))
coeftest(GNC_glm14, cluster_se_GNC_Anx_PV_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(GNC_glm14$coef)
lower.ci <- est-qnorm(0.975)*cluster_se_GNC_Anx_PV_extracted_a
upper.ci <- est+qnorm(0.975)*cluster_se_GNC_Anx_PV_extracted_a
cbind(est,lower.ci, upper.ci)

## Anxiety by BB
# unadjusted model
GNC_glm15 <- glm(Anxious_bin ~ BB_bin, weights = Weight, family = "quasibinomial", data = dcya_GNC)
summary(GNC_glm15)
cluster_se_GNC_Anx_BB <- cluster.vcov(GNC_glm15, ~Building)
cluster_se_GNC_Anx_BB_extracted <- sqrt(diag(cluster_se_GNC_Anx_BB))
coeftest(GNC_glm15, cluster_se_GNC_Anx_BB)

# Odds Ratio and 95% CI
est <- exp(GNC_glm15$coef)
lower.ci <- est-1.96*cluster_se_GNC_Anx_BB_extracted
upper.ci <- est+1.96*cluster_se_GNC_Anx_BB_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
GNC_glm16 <- glm(Anxious_bin ~ BB_bin + AgeRecode + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_GNC)
summary(GNC_glm16)
cluster_se_GNC_Anx_BB_a <- cluster.vcov(GNC_glm16, ~Building)
cluster_se_GNC_Anx_BB_extracted_a <- sqrt(diag(cluster_se_GNC_Anx_BB_a))
coeftest(GNC_glm16, cluster_se_GNC_Anx_BB_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(GNC_glm16$coef)
lower.ci <- est-qnorm(0.975)*cluster_se_GNC_Anx_BB_extracted_a
upper.ci <- est+qnorm(0.975)*cluster_se_GNC_Anx_BB_extracted_a
cbind(est,lower.ci, upper.ci)


## Depression by PV
# adjusted model
GNC_glm17 <- glm(Depression_bin ~ PeerVic_bin + AgeRecode + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_GNC)
summary(GNC_glm17)
cluster_se_GNC <- cluster.vcov(GNC_glm17, ~Building)
cluster_se_GNC_extracted <- sqrt(diag(cluster_se_GNC))
coeftest(GNC_glm17, cluster_se_GNC)

# Adjusted Odds Ratio and 95% CI
est <- exp(GNC_glm17$coef)
lower.ci <- est-qnorm(0.975)*cluster_se_GNC_extracted
upper.ci <- est+qnorm(0.975)*cluster_se_GNC_extracted
cbind(est,lower.ci, upper.ci)

## Depression by BB
# adjusted model
GNC_glm18 <- glm(Depression_bin ~ BB_bin + AgeRecode + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_GNC)
summary(GNC_glm18)
cluster_se_GNC <- cluster.vcov(GNC_glm18, ~Building)
cluster_se_GNC_extracted <- sqrt(diag(cluster_se_GNC))
coeftest(GNC_glm18, cluster_se_GNC)

# Adjusted Odds Ratio and 95% CI
est <- exp(GNC_glm18$coef)
lower.ci <- est-qnorm(0.975)*cluster_se_GNC_extracted
upper.ci <- est+qnorm(0.975)*cluster_se_GNC_extracted
cbind(est,lower.ci, upper.ci)

#########################################################################
## Step 4: Moderation Analyses
#########################################################################
## Trans youth

## SI by PV, Moderated by School Connectedness and Family Support/Monitoring Scale
# unadjusted model
trans_glm1 <- glm(SI_bin ~ PeerVic_bin*SC_Scale_Mean.c + PeerVic_bin*FSM_Scale_Mean.c, weights = Weight, family = "quasibinomial", data = dcya_transgender)
summary(trans_glm1)
cluster_se_trans <- cluster.vcov(trans_glm1, ~Building)
cluster_se_trans_extracted <- sqrt(diag(cluster_se_trans))
coeftest(trans_glm1, cluster_se_trans)

# adjusted model
trans_glm2 <- glm(SI_bin ~ PeerVic_bin*SC_Scale_Mean.c + PeerVic_bin*FSM_Scale_Mean.c + AgeRecode + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_transgender)
summary(trans_glm2)
cluster_se_trans_a <- cluster.vcov(trans_glm2, ~Building)
cluster_se_trans_extracted_a <- sqrt(diag(cluster_se_trans_a))
coeftest(trans_glm2, cluster_se_trans_a)



## SI by BB, moderated by School Connectedness and Family Support/Monitoring Scale
# unadjusted model
trans_glm3 <- glm(SI_bin ~ BB_bin*SC_Scale_Mean + BB_bin*FSM_Scale_Mean, weights = Weight, family = "quasibinomial", data = dcya_transgender)
summary(trans_glm3)
cluster_se_trans_BB <- cluster.vcov(trans_glm3, ~Building)
cluster_se_trans_BB_extracted <- sqrt(diag(cluster_se_trans_BB))
coeftest(trans_glm3, cluster_se_trans_BB)

# adjusted model
trans_glm4 <- glm(SI_bin ~ BB_bin*SC_Scale_Mean.c + BB_bin*FSM_Scale_Mean.c + AgeRecode + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_transgender)
summary(trans_glm4)
cluster_se_trans_BB_a <- cluster.vcov(trans_glm4, ~Building)
cluster_se_trans_BB_extracted_a <- sqrt(diag(cluster_se_trans_BB_a))
coeftest(trans_glm4, cluster_se_trans_BB_a)


## SA by PV, moderated by School Connectedness and Family Support/Monitoring Scale
# unadjusted model
trans_glm5 <- glm(SA_bin ~ PeerVic_bin*SC_Scale_Mean + PeerVic_bin*FSM_Scale_Mean, weights = Weight, family = "quasibinomial", data = dcya_transgender)
summary(trans_glm5)
cluster_se_trans_SA_PV <- cluster.vcov(trans_glm5, ~Building)
cluster_se_trans_SA_PV_extracted <- sqrt(diag(cluster_se_trans_SA_PV))
coeftest(trans_glm5, cluster_se_trans_SA_PV)

# adjusted model
trans_glm6 <- glm(SA_bin ~ PeerVic_bin*SC_Scale_Mean.c + PeerVic_bin*FSM_Scale_Mean.c + AgeRecode + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_transgender)
summary(trans_glm6)
cluster_se_trans_SA_PV_a <- cluster.vcov(trans_glm6, ~Building)
cluster_se_trans_SA_PV_extracted_a <- sqrt(diag(cluster_se_trans_SA_PV_a))
coeftest(trans_glm6, cluster_se_trans_SA_PV_a)


## SA by BB, moderated by School Connectedness and Family Support/Monitoring
# unadjusted model
trans_glm7 <- glm(SA_bin ~ BB_bin*SC_Scale_Mean + BB_bin*FSM_Scale_Mean, weights = Weight, family = "quasibinomial", data = dcya_transgender)
summary(trans_glm7)
cluster_se_trans_SA_BB <- cluster.vcov(trans_glm7, ~Building)
cluster_se_trans_SA_BB_extracted <- sqrt(diag(cluster_se_trans_SA_BB))
coeftest(trans_glm7, cluster_se_trans_SA_BB)

# adjusted model
trans_glm8 <- glm(SA_bin ~ BB_bin*SC_Scale_Mean.c + BB_bin*FSM_Scale_Mean.c + AgeRecode + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_transgender)
summary(trans_glm8)
cluster_se_trans_SA_BB_a <- cluster.vcov(trans_glm8, ~Building)
cluster_se_trans_SA_BB_extracted_a <- sqrt(diag(cluster_se_trans_SA_BB_a))
coeftest(trans_glm8, cluster_se_trans_SA_BB_a)


## NSSI by PV, moderated by School Connectedness and Family Support/Monitoring
# unadjusted model
trans_glm9 <- glm(NSSI_bin ~ PeerVic_bin*SC_Scale_Mean + PeerVic_bin*FSM_Scale_Mean, weights = Weight, family = "quasibinomial", data = dcya_transgender)
summary(trans_glm9)
cluster_se_trans_NSSI_PV <- cluster.vcov(trans_glm9, ~Building)
cluster_se_trans_NSSI_PV_extracted <- sqrt(diag(cluster_se_trans_NSSI_PV))
coeftest(trans_glm9, cluster_se_trans_NSSI_PV)

# adjusted model
trans_glm10 <- glm(NSSI_bin ~ PeerVic_bin*SC_Scale_Mean.c + PeerVic_bin*FSM_Scale_Mean.c + AgeRecode + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_transgender)
summary(trans_glm10)
cluster_se_trans_NSSI_PV_a <- cluster.vcov(trans_glm10, ~Building)
cluster_se_trans_NSSI_PV_extracted_a <- sqrt(diag(cluster_se_trans_NSSI_PV_a))
coeftest(trans_glm10, cluster_se_trans_NSSI_PV_a)


## NSSI by BB, moderated by School Connectedness and Family Support/Monitoring
# unadjusted model
trans_glm11 <- glm(NSSI_bin ~ BB_bin*SC_Scale_Mean + BB_bin*FSM_Scale_Mean, weights = Weight, family = "quasibinomial", data = dcya_transgender)
summary(trans_glm11)
cluster_se_trans_NSSI_BB <- cluster.vcov(trans_glm11, ~Building)
cluster_se_trans_NSSI_BB_extracted <- sqrt(diag(cluster_se_trans_NSSI_BB))
coeftest(trans_glm11, cluster_se_trans_NSSI_BB)

# adjusted model
trans_glm12 <- glm(NSSI_bin ~ BB_bin*SC_Scale_Mean.c + BB_bin*FSM_Scale_Mean.c + AgeRecode + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_transgender)
summary(trans_glm12)
cluster_se_trans_NSSI_BB_a <- cluster.vcov(trans_glm12, ~Building)
cluster_se_trans_NSSI_BB_extracted_a <- sqrt(diag(cluster_se_trans_NSSI_BB_a))
coeftest(trans_glm12, cluster_se_trans_NSSI_BB_a)


## Anxiety by PV, moderated by School Connectedness and FSM
# unadjusted model
trans_glm13 <- glm(Anxious_bin ~ PeerVic_bin*SC_Scale_Mean.c + PeerVic_bin*FSM_Scale_Mean.c, weights = Weight, family = "quasibinomial", data = dcya_transgender)
summary(trans_glm13)
cluster_se_trans_Anx_PV <- cluster.vcov(trans_glm13, ~Building)
cluster_se_trans_Anx_PV_extracted <- sqrt(diag(cluster_se_trans_Anx_PV))
coeftest(trans_glm13, cluster_se_trans_Anx_PV)

# adjusted model
trans_glm14 <- glm(Anxious_bin ~ PeerVic_bin*SC_Scale_Mean.c + PeerVic_bin*FSM_Scale_Mean.c + AgeRecode + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_transgender)
summary(trans_glm14)
cluster_se_trans_Anx_PV_a <- cluster.vcov(trans_glm14, ~Building)
cluster_se_trans_Anx_PV_extracted_a <- sqrt(diag(cluster_se_trans_Anx_PV_a))
coeftest(trans_glm14, cluster_se_trans_Anx_PV_a)


## Anxiety by BB, moderated by School Connectedness and Family Support/Monitoring
# unadjusted model
trans_glm15 <- glm(Anxious_bin ~ BB_bin*SC_Scale_Mean.c + BB_bin*FSM_Scale_Mean.c, weights = Weight, family = "quasibinomial", data = dcya_transgender)
summary(trans_glm15)
cluster_se_trans_Anx_BB <- cluster.vcov(trans_glm15, ~Building)
cluster_se_trans_Anx_BB_extracted <- sqrt(diag(cluster_se_trans_Anx_BB))
coeftest(trans_glm15, cluster_se_trans_Anx_BB)

# adjusted model
trans_glm16 <- glm(Anxious_bin ~ BB_bin*SC_Scale_Mean.c + BB_bin*FSM_Scale_Mean.c + AgeRecode + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_transgender)
summary(trans_glm16)
cluster_se_trans_Anx_BB_a <- cluster.vcov(trans_glm16, ~Building)
cluster_se_trans_Anx_BB_extracted_a <- sqrt(diag(cluster_se_trans_Anx_BB_a))
coeftest(trans_glm16, cluster_se_trans_Anx_BB_a)


## Depression by PV, moderated by School Connectedness and Family Support/Monitoring
# unadjusted model
trans_glm17.u <- glm(Depression_bin ~ PeerVic_bin*SC_Scale_Mean.c + PeerVic_bin*FSM_Scale_Mean.c, weights = Weight, family = "quasibinomial", data = dcya_transgender)
summary(trans_glm17.u)
cluster_se <- cluster.vcov(trans_glm17.u, ~Building)
coeftest(trans_glm17.u, cluster_se)

# adjusted model
trans_glm17 <- glm(Depression_bin ~ PeerVic_bin*SC_Scale_Mean.c + PeerVic_bin*FSM_Scale_Mean.c + AgeRecode + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_transgender)
summary(trans_glm17)
cluster_se <- cluster.vcov(trans_glm17, ~Building)
cluster_se_extracted <- sqrt(diag(cluster_se))
coeftest(trans_glm17, cluster_se)

## Depression by BB, moderated by School Connectedness and Family Support/Monitoring
# unadjusted model
trans_glm18.u <- glm(Depression_bin ~ BB_bin*SC_Scale_Mean.c + BB_bin*FSM_Scale_Mean.c, weights = Weight, family = "quasibinomial", data = dcya_transgender)
summary(trans_glm18.u)
cluster_se <- cluster.vcov(trans_glm18.u, ~Building)
coeftest(trans_glm18.u, cluster_se)

# adjusted model
trans_glm18 <- glm(Depression_bin ~ BB_bin*SC_Scale_Mean.c + BB_bin*FSM_Scale_Mean.c + AgeRecode + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_transgender)
summary(trans_glm18)
cluster_se_a <- cluster.vcov(trans_glm18, ~Building)
cluster_se_extracted_a <- sqrt(diag(cluster_se_a))
coeftest(trans_glm18, cluster_se_a)


### GNC youth
## SI by PV, Moderated by School Connectedness and Family Support/Monitoring
# unadjusted model
GNC_glm1 <- glm(SI_bin ~ PeerVic_bin*SC_Scale_Mean.c + PeerVic_bin*FSM_Scale_Mean.c, weights = Weight, family = "quasibinomial", data = dcya_GNC)
summary(GNC_glm1)
cluster_se_GNC <- cluster.vcov(GNC_glm1, ~Building)
cluster_se_GNC_extracted <- sqrt(diag(cluster_se_GNC))
coeftest(GNC_glm1, cluster_se_GNC)

# adjusted model
GNC_glm2 <- glm(SI_bin ~ PeerVic_bin*SC_Scale_Mean.c + PeerVic_bin*FSM_Scale_Mean.c + AgeRecode + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_GNC)
summary(GNC_glm2)
cluster_se_GNC_a <- cluster.vcov(GNC_glm2, ~Building)
cluster_se_GNC_extracted_a <- sqrt(diag(cluster_se_GNC_a))
coeftest(GNC_glm2, cluster_se_GNC_a)


## SI by BB, moderated by School Connectedness and Family Support/Monitoring
# unadjusted model
GNC_glm3 <- glm(SI_bin ~ BB_bin*SC_Scale_Mean.c + BB_bin*FSM_Scale_Mean.c, weights = Weight, family = "quasibinomial", data = dcya_GNC)
summary(GNC_glm3)
cluster_se_GNC_BB <- cluster.vcov(GNC_glm3, ~Building)
cluster_se_GNC_BB_extracted <- sqrt(diag(cluster_se_GNC_BB))
coeftest(GNC_glm3, cluster_se_GNC_BB)

# adjusted model
GNC_glm4 <- glm(SI_bin ~ BB_bin*SC_Scale_Mean.c + BB_bin*FSM_Scale_Mean.c + AgeRecode + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_GNC)
summary(GNC_glm4)
cluster_se_GNC_BB_a <- cluster.vcov(GNC_glm4, ~Building)
cluster_se_GNC_BB_extracted_a <- sqrt(diag(cluster_se_GNC_BB_a))
coeftest(GNC_glm4, cluster_se_GNC_BB_a)


## SA by PV, moderated by School Connectedness and Family Support/Monitoring
# unadjusted model
GNC_glm5 <- glm(SA_bin ~ PeerVic_bin*SC_Scale_Mean.c + PeerVic_bin*FSM_Scale_Mean.c, weights = Weight, family = "quasibinomial", data = dcya_GNC)
summary(GNC_glm5)
cluster_se_GNC_SA_PV <- cluster.vcov(GNC_glm5, ~Building)
cluster_se_GNC_SA_PV_extracted <- sqrt(diag(cluster_se_GNC_SA_PV))
coeftest(GNC_glm5, cluster_se_GNC_SA_PV)

# adjusted model
GNC_glm6 <- glm(SA_bin ~ PeerVic_bin*SC_Scale_Mean.c + PeerVic_bin*FSM_Scale_Mean.c + AgeRecode + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_GNC)
summary(GNC_glm6)
cluster_se_GNC_SA_PV_a <- cluster.vcov(GNC_glm6, ~Building)
cluster_se_GNC_SA_PV_extracted_a <- sqrt(diag(cluster_se_GNC_SA_PV_a))
coeftest(GNC_glm6, cluster_se_GNC_SA_PV_a)


## SA by BB, moderated by School Connectedness and Family Support/Monitoring
# unadjusted model
GNC_glm7 <- glm(SA_bin ~ BB_bin*SC_Scale_Mean.c + BB_bin*FSM_Scale_Mean.c, weights = Weight, family = "quasibinomial", data = dcya_GNC)
summary(GNC_glm7)
cluster_se_GNC_SA_BB <- cluster.vcov(GNC_glm7, ~Building)
cluster_se_GNC_SA_BB_extracted <- sqrt(diag(cluster_se_GNC_SA_BB))
coeftest(GNC_glm7, cluster_se_GNC_SA_BB)

# adjusted model
GNC_glm8 <- glm(SA_bin ~ BB_bin*SC_Scale_Mean.c + BB_bin*FSM_Scale_Mean.c + AgeRecode + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_GNC)
summary(GNC_glm8)
cluster_se_GNC_SA_BB_a <- cluster.vcov(GNC_glm8, ~Building)
cluster_se_GNC_SA_BB_extracted_a <- sqrt(diag(cluster_se_GNC_SA_BB_a))
coeftest(GNC_glm8, cluster_se_GNC_SA_BB_a)

## NSSI by PV, moderated by School Connectedness and Family Support/Monitoring
# unadjusted model
GNC_glm9 <- glm(NSSI_bin ~ PeerVic_bin*SC_Scale_Mean.c + PeerVic_bin*FSM_Scale_Mean.c, weights = Weight, family = "quasibinomial", data = dcya_GNC)
summary(GNC_glm9)
cluster_se_GNC_NSSI_PV <- cluster.vcov(GNC_glm9, ~Building)
cluster_se_GNC_NSSI_PV_extracted <- sqrt(diag(cluster_se_GNC_NSSI_PV))
coeftest(GNC_glm9, cluster_se_GNC_NSSI_PV)

# adjusted model
GNC_glm10 <- glm(NSSI_bin ~ PeerVic_bin*SC_Scale_Mean.c + PeerVic_bin*FSM_Scale_Mean.c + AgeRecode + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_GNC)
summary(GNC_glm10)
cluster_se_GNC_NSSI_PV_a <- cluster.vcov(GNC_glm10, ~Building)
cluster_se_GNC_NSSI_PV_extracted_a <- sqrt(diag(cluster_se_GNC_NSSI_PV_a))
coeftest(GNC_glm10, cluster_se_GNC_NSSI_PV_a)


## NSSI by BB, moderated by School Connectedness and Family Support/Monitoring
# unadjusted model
GNC_glm11 <- glm(NSSI_bin ~ BB_bin*SC_Scale_Mean.c + BB_bin*FSM_Scale_Mean.c, weights = Weight, family = "quasibinomial", data = dcya_GNC)
summary(GNC_glm11)
cluster_se_GNC_NSSI_BB <- cluster.vcov(GNC_glm11, ~Building)
cluster_se_GNC_NSSI_BB_extracted <- sqrt(diag(cluster_se_GNC_NSSI_BB))
coeftest(GNC_glm11, cluster_se_GNC_NSSI_BB)

# adjusted model
GNC_glm12 <- glm(NSSI_bin ~ BB_bin*SC_Scale_Mean.c + BB_bin*FSM_Scale_Mean.c + AgeRecode + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_GNC)
summary(GNC_glm12)
cluster_se_GNC_NSSI_BB_a <- cluster.vcov(GNC_glm12, ~Building)
cluster_se_GNC_NSSI_BB_extracted_a <- sqrt(diag(cluster_se_GNC_NSSI_BB_a))
coeftest(GNC_glm12, cluster_se_GNC_NSSI_BB_a)


## Anxiety by PV, moderated by School Connectedness and Family Support/Monitoring
# unadjusted model
GNC_glm13 <- glm(Anxious_bin ~ PeerVic_bin*SC_Scale_Mean.c + PeerVic_bin*FSM_Scale_Mean.c, weights = Weight, family = "quasibinomial", data = dcya_GNC)
summary(GNC_glm13)
cluster_se_GNC_Anx_PV <- cluster.vcov(GNC_glm13, ~Building)
cluster_se_GNC_Anx_PV_extracted <- sqrt(diag(cluster_se_GNC_Anx_PV))
coeftest(GNC_glm13, cluster_se_GNC_Anx_PV)

# adjusted model
GNC_glm14 <- glm(Anxious_bin ~ PeerVic_bin*SC_Scale_Mean.c + PeerVic_bin*FSM_Scale_Mean.c + AgeRecode + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_GNC)
summary(GNC_glm14)
cluster_se_GNC_Anx_PV_a <- cluster.vcov(GNC_glm14, ~Building)
cluster_se_GNC_Anx_PV_extracted_a <- sqrt(diag(cluster_se_GNC_Anx_PV_a))
coeftest(GNC_glm14, cluster_se_GNC_Anx_PV_a)


## Anxiety by BB, moderated by School Connectedness and Family Support/Monitoring
# unadjusted model
GNC_glm15 <- glm(Anxious_bin ~ BB_bin*SC_Scale_Mean.c + BB_bin*FSM_Scale_Mean.c, weights = Weight, family = "quasibinomial", data = dcya_GNC)
summary(GNC_glm15)
cluster_se_GNC_Anx_BB <- cluster.vcov(GNC_glm15, ~Building)
cluster_se_GNC_Anx_BB_extracted <- sqrt(diag(cluster_se_GNC_Anx_BB))
coeftest(GNC_glm15, cluster_se_GNC_Anx_BB)

# adjusted model
GNC_glm16 <- glm(Anxious_bin ~ BB_bin*SC_Scale_Mean.c + BB_bin*FSM_Scale_Mean.c + AgeRecode + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_GNC)
summary(GNC_glm16)
cluster_se_GNC_Anx_BB_a <- cluster.vcov(GNC_glm16, ~Building)
cluster_se_GNC_Anx_BB_extracted_a <- sqrt(diag(cluster_se_GNC_Anx_BB_a))
coeftest(GNC_glm16, cluster_se_GNC_Anx_BB_a)


## Depression by PV, moderated by School Connectedness and Family Support/Monitoring
# unadjusted model
GNC_glm17.u <- glm(Depression_bin ~ PeerVic_bin*SC_Scale_Mean.c + PeerVic_bin*FSM_Scale_Mean.c, weights = Weight, family = "quasibinomial", data = dcya_GNC)
summary(GNC_glm17.u)
cluster_se <- cluster.vcov(GNC_glm17.u, ~Building)
coeftest(GNC_glm17.u, cluster_se)


# adjusted model
GNC_glm17 <- glm(Depression_bin ~ PeerVic_bin*SC_Scale_Mean.c + PeerVic_bin*FSM_Scale_Mean.c + AgeRecode + RaceRecode + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_GNC)
summary(GNC_glm17)
cluster_se <- cluster.vcov(GNC_glm17, ~Building)
cluster_se_extracted <- sqrt(diag(cluster_se))
coeftest(GNC_glm17, cluster_se)

## Depression by BB, moderated by School Connectedness and Family Support/Monitoring
# unadjusted model
GNC_glm18.u <- glm(Depression_bin ~ BB_bin*SC_Scale_Mean.c + BB_bin*FSM_Scale_Mean.c, weights = Weight, family = "quasibinomial", data = dcya_GNC)
summary(GNC_glm18.u)
cluster_se <- cluster.vcov(GNC_glm18.u, ~Building)
coeftest(GNC_glm18.u, cluster_se)

# adjusted model
GNC_glm18 <- glm(Depression_bin ~ BB_bin*SC_Scale_Mean.c + BB_bin*FSM_Scale_Mean.c + AgeRecode + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_GNC)
summary(GNC_glm18)
cluster_se_a <- cluster.vcov(GNC_glm18, ~Building)
cluster_se_extracted_a <- sqrt(diag(cluster_se_a))
coeftest(GNC_glm18, cluster_se_a)

### plot interaction 
#p <- interact_plot(GNC_glm18, pred = BB_bin, modx = SC_Scale_Mean.c, modx.values = NULL, legend.main = "School-connectedness", 
#                   cluster = dcya_GNC$Building, interval = TRUE, pred.labels = c("None", "Any"),
#                   colors = "Blues")

#p + theme_minimal() + labs(x = "Bias-based harassment", y = "Depression") + ggtitle("Predicted probabilities of depression among GNC youth")  +
#   scale_y_continuous(limits = c(0,.70)) +
#  theme(axis.text.x = element_text(margin = margin(t = -20)),
#        panel.grid.major.x = element_blank(),
#        panel.grid.minor.x = element_blank())


### check correlation between FSM and SC scale mean
# cor(dcya$FSM_Scale_Mean, dcya$SC_Scale_Mean, use = "complete.obs")


### moderation graphs
# FSM, bin into groups
#dcya_transgender_2 <- dcya_transgender[!is.na(dcya_transgender$FSM_Scale_Mean),]
#summary(dcya_transgender$FSM_Scale_Mean) # with NAs
#summary(dcya_transgender_2$FSM_Scale_Mean) # without NAs
#fsm_hist <- hist(dcya_transgender_2$FSM_Scale_Mean, ylim = c(0,60), xlim = c(0,4), col = "blue")
#text(fsm_hist$mids, fsm_hist$counts, labels = fsm_hist$counts, adj = c(0.5, -0.5))

#summary(dcya_transgender_2$FSM_Scale_Mean)
#fsm_hist <- hist(dcya_transgender_2$FSM_Scale_Mean, ylim = c(0,60), xlim = c(0,4), col = "blue", breaks = 0.25*c(1:16))
#text(fsm_hist$mids, fsm_hist$counts, labels = fsm_hist$counts, adj = c(0.5, -0.5))

#summary(dcya_transgender_2$FSM_Scale_Mean)
#fsm_hist <- hist(dcya_transgender_2$FSM_Scale_Mean, ylim = c(0,60), xlim = c(0,4), col = "blue", breaks = 0.10*c(1:40))
#text(fsm_hist$mids, fsm_hist$counts, labels = fsm_hist$counts, adj = c(0.5, -0.5))

## histogram with density plot, include mean
# dens_p <- ggplot(data = dcya_transgender_2, mapping = aes(x = FSM_Scale_Mean)) +
#  geom_histogram(aes(y=..density..), fill = "blue", color="white", alpha=0.4) +
#  geom_density() +
#  geom_rug() +
#  labs(x = 'Mean FSM score') 
 
# dens_p + geom_vline(aes(xintercept=mean(FSM_Scale_Mean)),
#                    color = "black", linetype = "dashed", size =1)


 ## binning
 # create 2 bins for FSM mean scale
# dcya_transgender_2$FSM_factor_2bin <- cut(x = dcya_transgender_2$FSM_Scale_Mean, breaks = c(0, 2.5, 4), labels = c("Low", "High"))
# head(dcya_transgender_2$FSM_factor_2bin)
# plot(dcya_transgender_2$FSM_factor_2bin)
# summary(dcya_transgender_2$FSM_factor_2bin)

# create 3 bins for FSM mean scale
# dcya_transgender_2$FSM_factor_3bin <- cut(x = dcya_transgender_2$FSM_Scale_Mean, breaks = c(0, 2.5, 3.25, 4), labels = c("Low", "Medium", "High"))
# head(dcya_transgender_2$FSM_factor_3bin)
# plot(dcya_transgender_2$FSM_factor_3bin)
# summary(dcya_transgender_2$FSM_factor_3bin)

#################################################################################################################################################3
### Predicted probabilities
# convert Peer Vic to factor
# dcya_transgender$PeerVic_bin_factor <- factor(dcya_transgender$PeerVic_bin,
#                                                levels = c(0,1),
#                                                labels = c("No Peer Victimization", "Peer Victimization"))
# levels(dcya_transgender$PeerVic_bin_factor)


# m1 <- glm(Depression_bin ~ PeerVic_bin_factor*SC_Scale_Mean + PeerVic_bin_factor*FSM_Scale_Mean + AgeRecode.c + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_transgender)
# summary(m1)
# cluster <- cluster.vcov(m1, ~Building)
# cluster_se_extract <- sqrt(diag(cluster_se))
# coeftest(m1, cluster)

# library(sjPlot)
## visualize model
# p <- plot_model(m1, type = "pred", terms = c("PeerVic_bin_factor", "FSM_Scale_Mean [1,  4]"), robust = TRUE, axis.title = "Depression",
#                 axis_title.x = "Peer Victimization", title = "(a) Predicted probabilities of depression",
#           legend.title = "School-connectedness\nMean Score")

# p + theme_minimal() + labs(x = "Peer Victimization")
 
 
# p2 <- plot_model(m1, type = "int", mdrt.values = "meansd", robust = TRUE, axis.title = "Depression",
#                 axis_title.x = "Peer Victimization", title = "(a) Predicted probabilities of depression",
#                 legend.title = "School-connectedness\nMean Score")
 
# p2
# p2 + theme_minimal() + labs(x = "Peer Victimization")
 

 
# m1 <- glm(Depression_bin ~ PeerVic_bin*SC_Scale_Mean.c + PeerVic_bin*FSM_Scale_Mean.c + AgeRecode.c + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_transgender)
# summary(m1)
# cluster <- cluster.vcov(m1, ~Building)
# coeftest(m1, cluster)

# p <- interact_plot(m1, pred = PeerVic_bin, modx = SC_Scale_Mean.c, legend.main = "School-connectedness", robust = TRUE, interval = TRUE)
 
# p + theme_minimal() + labs(x = "Peer Victimization", y = "Depression") + ggtitle("(a) Predicted probabilities of depression")  +
#   scale_y_continuous(limits = c(0,.80)) 

 
 
# p2 <- interact_plot(m1, pred = SC_Scale_Mean.c, modx = PeerVic_bin, robust = TRUE)
 
# p2 + theme_minimal() + labs(x = "Mean School-connectedness", y = "Depression") + title = "(a) Predicted probabilities of depression"
 
#######################################################################################################################################
 ### Plot interaction 
 ## Depression by BB, moderated by School Connectedness and Family Support/Monitoring
 # adjusted model
# GNC_glm18 <- glm(Depression_bin ~ BB_bin*SC_Scale_Mean.c + BB_bin*FSM_Scale_Mean + AgeRecode + RaceRecode.p + Sex + GLBQ.p, weights = Weight, family = "quasibinomial", data = dcya_GNC)
# summary(GNC_glm18)
# cluster_se_a <- cluster.vcov(GNC_glm18, ~Building)
# cluster_se_extracted_a <- sqrt(diag(cluster_se_a))
# coeftest(GNC_glm18, cluster_se_a)
 
# p <- interact_plot(GNC_glm18, pred = BB_bin, modx = SC_Scale_Mean.c, modx.values = NULL, legend.main = "School-connectedness", 
#                    cluster = dcya_GNC$Building, interval = TRUE, pred.labels = c("None", "Any"),
#                    colors = "Blues")
 
# p + theme_minimal() + labs(x = "Bias-based harassment", y = "Depression") + ggtitle("Predicted probabilities of depression among GNC youth")  +
#   scale_y_continuous(limits = c(0,.70)) 
 
######################################################################################### 
###Remove sexual orientation as a covariate and estimate models ###
#########################################################################################
############################################################################## #
 ## GNCT relative to gender conforming youth, adjusted for covariates
 # SI
 m8 <- glm(SI_bin ~ GNCT + AgeRecode.c + RaceRecode.p + Sex, data = dcya, weights = Weight, family = quasibinomial)
 summary(m8)
 
 # cluster standard errors by type
 cluster_se_SI_GNC <- cluster.vcov(m8, ~Building)
 cluster_se_SI_GNC_extracted <- sqrt(diag(cluster_se_SI_GNC))
 coeftest(m8, cluster_se_SI_GNC)
 
 # Adjusted Odds Ratio (AOR) and 95% CI
 est <- exp(m8$coef)
 lower.ci <- est-qnorm(0.975)*cluster_se_SI_GNC_extracted
 upper.ci <- est+qnorm(0.975)*cluster_se_SI_GNC_extracted
 cbind(est,lower.ci, upper.ci)
 
 
 # SA
 m9 <- glm(SA_bin ~ GNCT + AgeRecode.c + RaceRecode.p + Sex, data = dcya, weights = Weight, family = quasibinomial)
 summary(m9)
 
 # cluster standard errors by type
 cluster_se_SA_GNC <- cluster.vcov(m9, ~Building)
 cluster_se_SA_GNC_extracted <- sqrt(diag(cluster_se_SA_GNC))
 coeftest(m9, cluster_se_SA_GNC)
 
 # Adjusted Odds Ratio (AOR) and 95% CI
 est <- exp(m9$coef)
 lower.ci <- est-qnorm(0.975)*cluster_se_SA_GNC_extracted 
 upper.ci <- est+qnorm(0.975)*cluster_se_SA_GNC_extracted 
 cbind(est,lower.ci, upper.ci)
 
 
 # NSSI
 m10 <- glm(NSSI_bin ~ GNCT + AgeRecode.c + RaceRecode.p + Sex, data = dcya, weights = Weight, family = quasibinomial)
 summary(m10)
 
 # cluster standard errors by type
 cluster_se_NSSI_GNC <- cluster.vcov(m10, ~Building)
 cluster_se_NSSI_GNC_extracted <- sqrt(diag(cluster_se_NSSI_GNC))
 coeftest(m10, cluster_se_NSSI_GNC)
 
 # Adjusted Odds Ratio (AOR) and 95% CI
 est <- exp(m10$coef)
 lower.ci <- est-qnorm(0.975)*cluster_se_NSSI_GNC_extracted
 upper.ci <- est+qnorm(0.975)*cluster_se_NSSI_GNC_extracted
 cbind(est,lower.ci, upper.ci)
 
 
 # Anxiety 
 m11 <- glm(Anxious_bin ~ GNCT + AgeRecode.c + RaceRecode.p + Sex, data = dcya, weights = Weight, family = quasibinomial)
 summary(m11)
 
 # cluster standard errors by type
 cluster_se_Anxious_GNC <- cluster.vcov(m11, ~Building)
 cluster_se_Anxious_GNC_extracted <- sqrt(diag(cluster_se_Anxious_GNC))
 coeftest(m11, cluster_se_Anxious_GNC)
 
 # Adjusted Odds Ratio (AOR) and 95% CI
 est <- exp(m11$coef)
 lower.ci <- est-qnorm(0.975)*cluster_se_Anxious_GNC_extracted
 upper.ci <- est+qnorm(0.975)*cluster_se_Anxious_GNC_extracted
 cbind(est,lower.ci, upper.ci)
 
 
 # Depression
 m12 <- glm(Depression_bin ~ GNCT + AgeRecode.c + RaceRecode.p + Sex, data = dcya, weights = Weight, family = quasibinomial)
 summary(m12)
 
 # cluster standard errors by type
 cluster_se_depress_GNC <- cluster.vcov(m12, ~Building)
 cluster_se_depress_GNC_extracted <- sqrt(diag(cluster_se_depress_GNC))
 coeftest(m12, cluster_se_depress_GNC)
 
 # Adjusted Odds Ratio (AOR) and 95% CI
 est <- exp(m12$coef)
 lower.ci <- est-qnorm(0.975)*cluster_se_depress_GNC_extracted
 upper.ci <- est+qnorm(0.975)*cluster_se_depress_GNC_extracted
 cbind(est,lower.ci, upper.ci)
 
 # Peer Victimization
 m13 <- glm(PeerVic_bin ~ GNCT + AgeRecode.c + RaceRecode.p + Sex, data = dcya, weights = Weight, family = quasibinomial)
 summary(m13)
 
 # cluster standard errors by type
 cluster_se_PV_GNC <- cluster.vcov(m13, ~Building)
 cluster_se_PV_GNC_extracted <- sqrt(diag(cluster_se_PV_GNC))
 coeftest(m13, cluster_se_PV_GNC)
 
 # Adjusted Odds Ratio (AOR) and 95% CI
 est <- exp(m13$coef)
 lower.ci <- est-qnorm(0.975)*cluster_se_PV_GNC_extracted
 upper.ci <- est+qnorm(0.975)*cluster_se_PV_GNC_extracted
 cbind(est,lower.ci, upper.ci)
 
 # Bias-Based Harassment
 m14 <- glm(BB_bin ~ GNCT + AgeRecode.c + RaceRecode.p + Sex, data = dcya, weights = Weight, family = quasibinomial)
 summary(m14)
 
 # cluster standard errors by type
 cluster_se_BB_GNC <- cluster.vcov(m14, ~Building)
 cluster_se_BB_GNC_extracted <- sqrt(diag(cluster_se_BB_GNC))
 coeftest(m14, cluster_se_BB_GNC)
 
 # Adjusted Odds Ratio (AOR) and 95% CI
 est <- exp(m14$coef)
 lower.ci <- est-qnorm(0.975)*cluster_se_BB_GNC_extracted
 upper.ci <- est+qnorm(0.975)*cluster_se_BB_GNC_extracted
 cbind(est,lower.ci, upper.ci)
 
 
 ## Step 3: Stratified models to examine effect of victimization on mental health
 
 ## Trans youth
 dcya_transgender = dcya %>%
   filter(Transgender_bin == "2_Transgender")
 ## SI by PV
 # unadjusted model
 trans_glm1 <- glm(SI_bin ~ PeerVic_bin, weights = Weight, family = "quasibinomial", data = dcya_transgender)
 summary(trans_glm1)
 cluster_se_trans <- cluster.vcov(trans_glm1, ~Building)
 cluster_se_trans_extracted <- sqrt(diag(cluster_se_trans))
 coeftest(trans_glm1, cluster_se_trans)
 
 # Odds Ratio and 95% CI
 est <- exp(trans_glm1$coef)
 lower.ci <- est-qnorm(0.975)*cluster_se_trans_extracted
 upper.ci <- est+qnorm(0.975)*cluster_se_trans_extracted
 cbind(est,lower.ci, upper.ci)
 
 # adjusted model
 trans_glm2 <- glm(SI_bin ~ PeerVic_bin + AgeRecode.c + RaceRecode.p + Sex, weights = Weight, family = "quasibinomial", data = dcya_transgender)
 summary(trans_glm2)
 cluster_se_trans_a <- cluster.vcov(trans_glm2, ~Building)
 cluster_se_trans_extracted_a <- sqrt(diag(cluster_se_trans_a))
 coeftest(trans_glm2, cluster_se_trans_a)
 
 # Adjusted Odds Ratio and 95% CI
 est <- exp(trans_glm2$coef)
 lower.ci <- est-qnorm(0.975)*cluster_se_trans_extracted_a
 upper.ci <- est+qnorm(0.975)*cluster_se_trans_extracted_a
 cbind(est,lower.ci, upper.ci)
 
 ## SI by BB
 # unadjusted model
 trans_glm3 <- glm(SI_bin ~ BB_bin, weights = Weight, family = "quasibinomial", data = dcya_transgender)
 summary(trans_glm3)
 cluster_se_trans_BB <- cluster.vcov(trans_glm3, ~Building)
 cluster_se_trans_BB_extracted <- sqrt(diag(cluster_se_trans_BB))
 coeftest(trans_glm3, cluster_se_trans_BB)
 
 # Odds Ratio and 95% CI
 est <- exp(trans_glm3$coef)
 lower.ci <- est-qnorm(0.975)*cluster_se_trans_BB_extracted
 upper.ci <- est+qnorm(0.975)*cluster_se_trans_BB_extracted
 cbind(est,lower.ci, upper.ci)
 
 # adjusted model
 trans_glm4 <- glm(SI_bin ~ BB_bin + AgeRecode.c + RaceRecode.p + Sex, weights = Weight, family = "quasibinomial", data = dcya_transgender)
 summary(trans_glm4)
 cluster_se_trans_BB_a <- cluster.vcov(trans_glm4, ~Building)
 cluster_se_trans_BB_extracted_a <- sqrt(diag(cluster_se_trans_BB_a))
 coeftest(trans_glm4, cluster_se_trans_BB_a)
 
 # Adjusted Odds Ratio and 95% CI
 est <- exp(trans_glm4$coef)
 lower.ci <- est-qnorm(0.975)*cluster_se_trans_BB_extracted_a
 upper.ci <- est+qnorm(0.975)*cluster_se_trans_BB_extracted_a
 cbind(est,lower.ci, upper.ci)
 
 
 ## SA by PV
 # unadjusted model
 trans_glm5 <- glm(SA_bin ~ PeerVic_bin, weights = Weight, family = "quasibinomial", data = dcya_transgender)
 summary(trans_glm5)
 cluster_se_trans_SA_PV <- cluster.vcov(trans_glm5, ~Building)
 cluster_se_trans_SA_PV_extracted <- sqrt(diag(cluster_se_trans_SA_PV))
 coeftest(trans_glm5, cluster_se_trans_SA_PV)
 
 # Odds Ratio and 95% CI
 est <- exp(trans_glm5$coef)
 lower.ci <- est-qnorm(0.975)*cluster_se_trans_SA_PV_extracted
 upper.ci <- est+qnorm(0.975)*cluster_se_trans_SA_PV_extracted
 cbind(est,lower.ci, upper.ci)
 
 # adjusted model
 trans_glm6 <- glm(SA_bin ~ PeerVic_bin + AgeRecode.c + RaceRecode.p + Sex, weights = Weight, family = "quasibinomial", data = dcya_transgender)
 summary(trans_glm6)
 cluster_se_trans_SA_PV_a <- cluster.vcov(trans_glm6, ~Building)
 cluster_se_trans_SA_PV_extracted_a <- sqrt(diag(cluster_se_trans_SA_PV_a))
 coeftest(trans_glm6, cluster_se_trans_SA_PV_a)
 
 # Adjusted Odds Ratio and 95% CI
 est <- exp(trans_glm6$coef)
 lower.ci <- est-qnorm(0.975)*cluster_se_trans_SA_PV_extracted_a
 upper.ci <- est+qnorm(0.975)*cluster_se_trans_SA_PV_extracted_a
 cbind(est,lower.ci, upper.ci)
 
 ## SA by BB
 # unadjusted model
 trans_glm7 <- glm(SA_bin ~ BB_bin, weights = Weight, family = "quasibinomial", data = dcya_transgender)
 summary(trans_glm7)
 cluster_se_trans_SA_BB <- cluster.vcov(trans_glm7, ~Building)
 cluster_se_trans_SA_BB_extracted <- sqrt(diag(cluster_se_trans_SA_BB))
 coeftest(trans_glm7, cluster_se_trans_SA_BB)
 
 # Odds Ratio and 95% CI
 est <- exp(trans_glm7$coef)
 lower.ci <- est-qnorm(0.975)*cluster_se_trans_SA_BB_extracted
 upper.ci <- est+qnorm(0.975)*cluster_se_trans_SA_BB_extracted
 cbind(est,lower.ci, upper.ci)
 
 # adjusted model
 trans_glm8 <- glm(SA_bin ~ BB_bin + AgeRecode.c + RaceRecode.p + Sex, weights = Weight, family = "quasibinomial", data = dcya_transgender)
 summary(trans_glm8)
 cluster_se_trans_SA_BB_a <- cluster.vcov(trans_glm8, ~Building)
 cluster_se_trans_SA_BB_extracted_a <- sqrt(diag(cluster_se_trans_SA_BB_a))
 coeftest(trans_glm8, cluster_se_trans_SA_BB_a)
 
 # Adjusted Odds Ratio and 95% CI
 est <- exp(trans_glm8$coef)
 lower.ci <- est-qnorm(0.975)*cluster_se_trans_SA_BB_extracted_a
 upper.ci <- est+qnorm(0.975)*cluster_se_trans_SA_BB_extracted_a
 cbind(est,lower.ci, upper.ci)
 
 ## NSSI by PV
 # unadjusted model
 trans_glm9 <- glm(NSSI_bin ~ PeerVic_bin, weights = Weight, family = "quasibinomial", data = dcya_transgender)
 summary(trans_glm9)
 cluster_se_trans_NSSI_PV <- cluster.vcov(trans_glm9, ~Building)
 cluster_se_trans_NSSI_PV_extracted <- sqrt(diag(cluster_se_trans_NSSI_PV))
 coeftest(trans_glm9, cluster_se_trans_NSSI_PV)
 
 # Odds Ratio and 95% CI
 est <- exp(trans_glm9$coef)
 lower.ci <- est-qnorm(0.975)*cluster_se_trans_NSSI_PV_extracted
 upper.ci <- est+qnorm(0.975)*cluster_se_trans_NSSI_PV_extracted
 cbind(est,lower.ci, upper.ci)
 
 # adjusted model
 trans_glm10 <- glm(NSSI_bin ~ PeerVic_bin + AgeRecode.c + RaceRecode.p + Sex, weights = Weight, family = "quasibinomial", data = dcya_transgender)
 summary(trans_glm10)
 cluster_se_trans_NSSI_PV_a <- cluster.vcov(trans_glm10, ~Building)
 cluster_se_trans_NSSI_PV_extracted_a <- sqrt(diag(cluster_se_trans_NSSI_PV_a))
 coeftest(trans_glm10, cluster_se_trans_NSSI_PV_a)
 
 # Adjusted Odds Ratio and 95% CI
 est <- exp(trans_glm10$coef)
 lower.ci <- est-qnorm(0.975)*cluster_se_trans_NSSI_PV_extracted_a 
 upper.ci <- est+qnorm(0.975)*cluster_se_trans_NSSI_PV_extracted_a 
 cbind(est,lower.ci, upper.ci)
 
 
 ## NSSI by BB
 # unadjusted model
 trans_glm11 <- glm(NSSI_bin ~ BB_bin, weights = Weight, family = "quasibinomial", data = dcya_transgender)
 summary(trans_glm11)
 cluster_se_trans_NSSI_BB <- cluster.vcov(trans_glm11, ~Building)
 cluster_se_trans_NSSI_BB_extracted <- sqrt(diag(cluster_se_trans_NSSI_BB))
 coeftest(trans_glm11, cluster_se_trans_NSSI_BB)
 
 # Odds Ratio and 95% CI
 est <- exp(trans_glm11$coef)
 lower.ci <- est-qnorm(0.975)*cluster_se_trans_NSSI_BB_extracted
 upper.ci <- est+qnorm(0.975)*cluster_se_trans_NSSI_BB_extracted
 cbind(est,lower.ci, upper.ci)
 
 # adjusted model
 trans_glm12 <- glm(NSSI_bin ~ BB_bin + AgeRecode.c + RaceRecode.p + Sex, weights = Weight, family = "quasibinomial", data = dcya_transgender)
 summary(trans_glm12)
 cluster_se_trans_NSSI_BB_a <- cluster.vcov(trans_glm12, ~Building)
 cluster_se_trans_NSSI_BB_extracted_a <- sqrt(diag(cluster_se_trans_NSSI_BB_a))
 coeftest(trans_glm12, cluster_se_trans_NSSI_BB_a)
 
 # Adjusted Odds Ratio and 95% CI
 est <- exp(trans_glm12$coef)
 lower.ci <- est-qnorm(0.975)*cluster_se_trans_NSSI_BB_extracted_a 
 upper.ci <- est+qnorm(0.975)*cluster_se_trans_NSSI_BB_extracted_a 
 cbind(est,lower.ci, upper.ci)
 
 
 ## Anxiety by PV
 # unadjusted model
 trans_glm13 <- glm(Anxious_bin ~ PeerVic_bin, weights = Weight, family = "quasibinomial", data = dcya_transgender)
 summary(trans_glm13)
 cluster_se_trans_Anx_PV <- cluster.vcov(trans_glm13, ~Building)
 cluster_se_trans_Anx_PV_extracted <- sqrt(diag(cluster_se_trans_Anx_PV))
 coeftest(trans_glm13, cluster_se_trans_Anx_PV)
 
 # Odds Ratio and 95% CI
 est <- exp(trans_glm13$coef)
 lower.ci <- est-qnorm(0.975)*cluster_se_trans_Anx_PV_extracted
 upper.ci <- est+qnorm(0.975)*cluster_se_trans_Anx_PV_extracted
 cbind(est,lower.ci, upper.ci)
 
 # adjusted model
 trans_glm14 <- glm(Anxious_bin ~ PeerVic_bin + AgeRecode.c + RaceRecode.p + Sex, weights = Weight, family = "quasibinomial", data = dcya_transgender)
 summary(trans_glm14)
 cluster_se_trans_Anx_PV_a <- cluster.vcov(trans_glm14, ~Building)
 cluster_se_trans_Anx_PV_extracted_a <- sqrt(diag(cluster_se_trans_Anx_PV_a))
 coeftest(trans_glm14, cluster_se_trans_Anx_PV_a)
 
 # Adjusted Odds Ratio and 95% CI
 est <- exp(trans_glm14$coef)
 lower.ci <- est-qnorm(0.975)*cluster_se_trans_Anx_PV_extracted_a
 upper.ci <- est+qnorm(0.975)*cluster_se_trans_Anx_PV_extracted_a
 cbind(est,lower.ci, upper.ci)
 
 ## Anxiety by BB
 # unadjusted model
 trans_glm15 <- glm(Anxious_bin ~ BB_bin, weights = Weight, family = "quasibinomial", data = dcya_transgender)
 summary(trans_glm15)
 cluster_se_trans_Anx_BB <- cluster.vcov(trans_glm15, ~Building)
 cluster_se_trans_Anx_BB_extracted <- sqrt(diag(cluster_se_trans_Anx_BB))
 coeftest(trans_glm15, cluster_se_trans_Anx_BB)
 
 # Odds Ratio and 95% CI
 est <- exp(trans_glm15$coef)
 lower.ci <- est-1.96*cluster_se_trans_Anx_BB_extracted
 upper.ci <- est+1.96*cluster_se_trans_Anx_BB_extracted
 cbind(est,lower.ci, upper.ci)
 
 # adjusted model
 trans_glm16 <- glm(Anxious_bin ~ BB_bin + AgeRecode.c + RaceRecode.p + Sex, weights = Weight, family = "quasibinomial", data = dcya_transgender)
 summary(trans_glm16)
 cluster_se_trans_Anx_BB_a <- cluster.vcov(trans_glm16, ~Building)
 cluster_se_trans_Anx_BB_extracted_a <- sqrt(diag(cluster_se_trans_Anx_BB_a))
 coeftest(trans_glm16, cluster_se_trans_Anx_BB_a)
 
 # Adjusted Odds Ratio and 95% CI
 est <- exp(trans_glm16$coef)
 lower.ci <- est-qnorm(0.975)*cluster_se_trans_Anx_BB_extracted_a
 upper.ci <- est+qnorm(0.975)*cluster_se_trans_Anx_BB_extracted_a
 cbind(est,lower.ci, upper.ci)
 
 ## Depression by PV
 # adjusted model
 trans_glm17 <- glm(Depression_bin ~ PeerVic_bin + AgeRecode.c + RaceRecode.p + Sex, weights = Weight, family = "quasibinomial", data = dcya_transgender)
 summary(trans_glm17)
 cluster_se_a <- cluster.vcov(trans_glm17, ~Building)
 cluster_se_extracted_a <- sqrt(diag(cluster_se_a))
 coeftest(trans_glm17, cluster_se_a)
 
 # Adjusted Odds Ratio and 95% CI
 est <- exp(trans_glm17$coef)
 lower.ci <- est-qnorm(0.975)*cluster_se_extracted_a
 upper.ci <- est+qnorm(0.975)*cluster_se_extracted_a
 cbind(est,lower.ci, upper.ci)
 
 ## Depression by BB
 # adjusted model
 trans_glm18 <- glm(Depression_bin ~ BB_bin + AgeRecode.c + RaceRecode.p + Sex, weights = Weight, family = "quasibinomial", data = dcya_transgender)
 summary(trans_glm18)
 cluster_se_a <- cluster.vcov(trans_glm18, ~Building)
 cluster_se_extracted_a <- sqrt(diag(cluster_se_a))
 coeftest(trans_glm18,cluster_se_a)
 
 # Adjusted Odds Ratio and 95% CI
 est <- exp(trans_glm18$coef)
 lower.ci <- est-qnorm(0.975)*cluster_se_extracted_a
 upper.ci <- est+qnorm(0.975)*cluster_se_extracted_a
 cbind(est,lower.ci, upper.ci)
 
 ## GNC youth
 dcya_GNC = dcya %>%
   filter(GNC == "2_Androgynous" | GNC == "3_ModeratelyGNC" | GNC == "4_HighlyGNC")
 ## SI by PV
 # unadjusted model
 GNC_glm1 <- glm(SI_bin ~ PeerVic_bin, weights = Weight, family = "quasibinomial", data = dcya_GNC)
 summary(GNC_glm1)
 cluster_se_GNC <- cluster.vcov(GNC_glm1, ~Building)
 cluster_se_GNC_extracted <- sqrt(diag(cluster_se_GNC))
 coeftest(GNC_glm1, cluster_se_GNC)
 
 # Odds Ratio and 95% CI
 est <- exp(GNC_glm1$coef)
 lower.ci <- est-1.96*cluster_se_GNC_extracted
 upper.ci <- est+1.96*cluster_se_GNC_extracted
 cbind(est,lower.ci, upper.ci)
 
 # adjusted model
 GNC_glm2 <- glm(SI_bin ~ PeerVic_bin + AgeRecode + RaceRecode.p + Sex, weights = Weight, family = "quasibinomial", data = dcya_GNC)
 summary(GNC_glm2)
 cluster_se_GNC_a <- cluster.vcov(GNC_glm2, ~Building)
 cluster_se_GNC_extracted_a <- sqrt(diag(cluster_se_GNC_a))
 coeftest(GNC_glm2, cluster_se_GNC_a)
 
 # Adjusted Odds Ratio and 95% CI
 est <- exp(GNC_glm2$coef)
 lower.ci <- est-qnorm(0.975)*cluster_se_GNC_extracted_a
 upper.ci <- est+qnorm(0.975)*cluster_se_GNC_extracted_a
 cbind(est,lower.ci, upper.ci)
 
 
 ## SI by BB
 # unadjusted model
 GNC_glm3 <- glm(SI_bin ~ BB_bin, weights = Weight, family = "quasibinomial", data = dcya_GNC)
 summary(GNC_glm3)
 cluster_se_GNC_BB <- cluster.vcov(GNC_glm3, ~Building)
 cluster_se_GNC_BB_extracted <- sqrt(diag(cluster_se_GNC_BB))
 coeftest(GNC_glm3, cluster_se_GNC_BB)
 
 # Odds Ratio and 95% CI
 est <- exp(GNC_glm3$coef)
 lower.ci <- est-1.96*cluster_se_GNC_BB_extracted
 upper.ci <- est+1.96*cluster_se_GNC_BB_extracted
 cbind(est,lower.ci, upper.ci)
 
 # adjusted model
 GNC_glm4 <- glm(SI_bin ~ BB_bin + AgeRecode + RaceRecode.p + Sex, weights = Weight, family = "quasibinomial", data = dcya_GNC)
 summary(GNC_glm4)
 cluster_se_GNC_BB_a <- cluster.vcov(GNC_glm4, ~Building)
 cluster_se_GNC_BB_extracted_a <- sqrt(diag(cluster_se_GNC_BB_a))
 coeftest(GNC_glm4, cluster_se_GNC_BB_a)
 
 # Adjusted Odds Ratio and 95% CI
 est <- exp(GNC_glm4$coef)
 lower.ci <- est-qnorm(0.975)*cluster_se_GNC_BB_extracted_a
 upper.ci <- est+qnorm(0.975)*cluster_se_GNC_BB_extracted_a
 cbind(est,lower.ci, upper.ci)
 
 
 ## SA by PV
 # unadjusted model
 GNC_glm5 <- glm(SA_bin ~ PeerVic_bin, weights = Weight, family = "quasibinomial", data = dcya_GNC)
 summary(GNC_glm5)
 cluster_se_GNC_SA_PV <- cluster.vcov(GNC_glm5, ~Building)
 cluster_se_GNC_SA_PV_extracted <- sqrt(diag(cluster_se_GNC_SA_PV))
 coeftest(GNC_glm5, cluster_se_GNC_SA_PV)
 
 # Odds Ratio and 95% CI
 est <- exp(GNC_glm5$coef)
 lower.ci <- est-1.96*cluster_se_GNC_SA_PV_extracted
 upper.ci <- est+1.96*cluster_se_GNC_SA_PV_extracted
 cbind(est,lower.ci, upper.ci)
 
 # adjusted model
 GNC_glm6 <- glm(SA_bin ~ PeerVic_bin + AgeRecode + RaceRecode.p + Sex, weights = Weight, family = "quasibinomial", data = dcya_GNC)
 summary(GNC_glm6)
 cluster_se_GNC_SA_PV_a <- cluster.vcov(GNC_glm6, ~Building)
 cluster_se_GNC_SA_PV_extracted_a <- sqrt(diag(cluster_se_GNC_SA_PV_a))
 coeftest(GNC_glm6, cluster_se_GNC_SA_PV_a)
 
 # Adjusted Odds Ratio and 95% CI
 est <- exp(GNC_glm6$coef)
 lower.ci <- est-qnorm(0.975)*cluster_se_GNC_SA_PV_extracted_a
 upper.ci <- est+qnorm(0.975)*cluster_se_GNC_SA_PV_extracted_a
 cbind(est,lower.ci, upper.ci)
 
 ## SA by BB
 # unadjusted model
 GNC_glm7 <- glm(SA_bin ~ BB_bin, weights = Weight, family = "quasibinomial", data = dcya_GNC)
 summary(GNC_glm7)
 cluster_se_GNC_SA_BB <- cluster.vcov(GNC_glm7, ~Building)
 cluster_se_GNC_SA_BB_extracted <- sqrt(diag(cluster_se_GNC_SA_BB))
 coeftest(GNC_glm7, cluster_se_GNC_SA_BB)
 
 # Odds Ratio and 95% CI
 est <- exp(GNC_glm7$coef)
 lower.ci <- est-1.96*cluster_se_GNC_SA_BB_extracted
 upper.ci <- est+1.96*cluster_se_GNC_SA_BB_extracted
 cbind(est,lower.ci, upper.ci)
 
 # adjusted model
 GNC_glm8 <- glm(SA_bin ~ BB_bin + AgeRecode + RaceRecode.p + Sex, weights = Weight, family = "quasibinomial", data = dcya_GNC)
 summary(GNC_glm8)
 cluster_se_GNC_SA_BB_a <- cluster.vcov(GNC_glm8, ~Building)
 cluster_se_GNC_SA_BB_extracted_a <- sqrt(diag(cluster_se_GNC_SA_BB_a))
 coeftest(GNC_glm8, cluster_se_GNC_SA_BB_a)
 
 # Adjusted Odds Ratio and 95% CI
 est <- exp(GNC_glm8$coef)
 lower.ci <- est-qnorm(0.975)*cluster_se_GNC_SA_BB_extracted_a
 upper.ci <- est+qnorm(0.975)*cluster_se_GNC_SA_BB_extracted_a
 cbind(est,lower.ci, upper.ci)
 
 ## NSSI by PV
 # unadjusted model
 GNC_glm9 <- glm(NSSI_bin ~ PeerVic_bin, weights = Weight, family = "quasibinomial", data = dcya_GNC)
 summary(GNC_glm9)
 cluster_se_GNC_NSSI_PV <- cluster.vcov(GNC_glm9, ~Building)
 cluster_se_GNC_NSSI_PV_extracted <- sqrt(diag(cluster_se_GNC_NSSI_PV))
 coeftest(GNC_glm9, cluster_se_GNC_NSSI_PV)
 
 # Odds Ratio and 95% CI
 est <- exp(GNC_glm9$coef)
 lower.ci <- est-1.96*cluster_se_GNC_NSSI_PV_extracted
 upper.ci <- est+1.96*cluster_se_GNC_NSSI_PV_extracted
 cbind(est,lower.ci, upper.ci)
 
 # adjusted model
 GNC_glm10 <- glm(NSSI_bin ~ PeerVic_bin + AgeRecode + RaceRecode.p + Sex, weights = Weight, family = "quasibinomial", data = dcya_GNC)
 summary(GNC_glm10)
 cluster_se_GNC_NSSI_PV_a <- cluster.vcov(GNC_glm10, ~Building)
 cluster_se_GNC_NSSI_PV_extracted_a <- sqrt(diag(cluster_se_GNC_NSSI_PV_a))
 coeftest(GNC_glm10, cluster_se_GNC_NSSI_PV_a)
 
 # Adjusted Odds Ratio and 95% CI
 est <- exp(GNC_glm10$coef)
 lower.ci <- est-qnorm(0.975)*cluster_se_GNC_NSSI_PV_extracted_a 
 upper.ci <- est+qnorm(0.975)*cluster_se_GNC_NSSI_PV_extracted_a 
 cbind(est,lower.ci, upper.ci)
 
 
 ## NSSI by BB
 # unadjusted model
 GNC_glm11 <- glm(NSSI_bin ~ BB_bin, weights = Weight, family = "quasibinomial", data = dcya_GNC)
 summary(GNC_glm11)
 cluster_se_GNC_NSSI_BB <- cluster.vcov(GNC_glm11, ~Building)
 cluster_se_GNC_NSSI_BB_extracted <- sqrt(diag(cluster_se_GNC_NSSI_BB))
 coeftest(GNC_glm11, cluster_se_GNC_NSSI_BB)
 
 # Odds Ratio and 95% CI
 est <- exp(GNC_glm11$coef)
 lower.ci <- est-1.96*cluster_se_GNC_NSSI_BB_extracted
 upper.ci <- est+1.96*cluster_se_GNC_NSSI_BB_extracted
 cbind(est,lower.ci, upper.ci)
 
 # adjusted model
 GNC_glm12 <- glm(NSSI_bin ~ BB_bin + AgeRecode + RaceRecode.p + Sex, weights = Weight, family = "quasibinomial", data = dcya_GNC)
 summary(GNC_glm12)
 cluster_se_GNC_NSSI_BB_a <- cluster.vcov(GNC_glm12, ~Building)
 cluster_se_GNC_NSSI_BB_extracted_a <- sqrt(diag(cluster_se_GNC_NSSI_BB_a))
 coeftest(GNC_glm12, cluster_se_GNC_NSSI_BB_a)
 
 # Adjusted Odds Ratio and 95% CI
 est <- exp(GNC_glm12$coef)
 lower.ci <- est-qnorm(0.975)*cluster_se_GNC_NSSI_BB_extracted_a 
 upper.ci <- est+qnorm(0.975)*cluster_se_GNC_NSSI_BB_extracted_a 
 cbind(est,lower.ci, upper.ci)
 
 
 ## Anxiety by PV
 # unadjusted model
 GNC_glm13 <- glm(Anxious_bin ~ PeerVic_bin, weights = Weight, family = "quasibinomial", data = dcya_GNC)
 summary(GNC_glm13)
 cluster_se_GNC_Anx_PV <- cluster.vcov(GNC_glm13, ~Building)
 cluster_se_GNC_Anx_PV_extracted <- sqrt(diag(cluster_se_GNC_Anx_PV))
 coeftest(GNC_glm13, cluster_se_GNC_Anx_PV)
 
 # Odds Ratio and 95% CI
 est <- exp(GNC_glm13$coef)
 lower.ci <- est-1.96*cluster_se_GNC_Anx_PV_extracted
 upper.ci <- est+1.96*cluster_se_GNC_Anx_PV_extracted
 cbind(est,lower.ci, upper.ci)
 
 # adjusted model
 GNC_glm14 <- glm(Anxious_bin ~ PeerVic_bin + AgeRecode + RaceRecode.p + Sex, weights = Weight, family = "quasibinomial", data = dcya_GNC)
 summary(GNC_glm14)
 cluster_se_GNC_Anx_PV_a <- cluster.vcov(GNC_glm14, ~Building)
 cluster_se_GNC_Anx_PV_extracted_a <- sqrt(diag(cluster_se_GNC_Anx_PV_a))
 coeftest(GNC_glm14, cluster_se_GNC_Anx_PV_a)
 
 # Adjusted Odds Ratio and 95% CI
 est <- exp(GNC_glm14$coef)
 lower.ci <- est-qnorm(0.975)*cluster_se_GNC_Anx_PV_extracted_a
 upper.ci <- est+qnorm(0.975)*cluster_se_GNC_Anx_PV_extracted_a
 cbind(est,lower.ci, upper.ci)
 
 ## Anxiety by BB
 # unadjusted model
 GNC_glm15 <- glm(Anxious_bin ~ BB_bin, weights = Weight, family = "quasibinomial", data = dcya_GNC)
 summary(GNC_glm15)
 cluster_se_GNC_Anx_BB <- cluster.vcov(GNC_glm15, ~Building)
 cluster_se_GNC_Anx_BB_extracted <- sqrt(diag(cluster_se_GNC_Anx_BB))
 coeftest(GNC_glm15, cluster_se_GNC_Anx_BB)
 
 # Odds Ratio and 95% CI
 est <- exp(GNC_glm15$coef)
 lower.ci <- est-1.96*cluster_se_GNC_Anx_BB_extracted
 upper.ci <- est+1.96*cluster_se_GNC_Anx_BB_extracted
 cbind(est,lower.ci, upper.ci)
 
 # adjusted model
 GNC_glm16 <- glm(Anxious_bin ~ BB_bin + AgeRecode + RaceRecode.p + Sex, weights = Weight, family = "quasibinomial", data = dcya_GNC)
 summary(GNC_glm16)
 cluster_se_GNC_Anx_BB_a <- cluster.vcov(GNC_glm16, ~Building)
 cluster_se_GNC_Anx_BB_extracted_a <- sqrt(diag(cluster_se_GNC_Anx_BB_a))
 coeftest(GNC_glm16, cluster_se_GNC_Anx_BB_a)
 
 # Adjusted Odds Ratio and 95% CI
 est <- exp(GNC_glm16$coef)
 lower.ci <- est-qnorm(0.975)*cluster_se_GNC_Anx_BB_extracted_a
 upper.ci <- est+qnorm(0.975)*cluster_se_GNC_Anx_BB_extracted_a
 cbind(est,lower.ci, upper.ci)
 
 
 ## Depression by PV
 # adjusted model
 GNC_glm17 <- glm(Depression_bin ~ PeerVic_bin + AgeRecode + RaceRecode.p + Sex, weights = Weight, family = "quasibinomial", data = dcya_GNC)
 summary(GNC_glm17)
 cluster_se_GNC <- cluster.vcov(GNC_glm17, ~Building)
 cluster_se_GNC_extracted <- sqrt(diag(cluster_se_GNC))
 coeftest(GNC_glm17, cluster_se_GNC)
 
 # Adjusted Odds Ratio and 95% CI
 est <- exp(GNC_glm17$coef)
 lower.ci <- est-qnorm(0.975)*cluster_se_GNC_extracted
 upper.ci <- est+qnorm(0.975)*cluster_se_GNC_extracted
 cbind(est,lower.ci, upper.ci)
 
 ## Depression by BB
 # adjusted model
 GNC_glm18 <- glm(Depression_bin ~ BB_bin + AgeRecode + RaceRecode.p + Sex, weights = Weight, family = "quasibinomial", data = dcya_GNC)
 summary(GNC_glm18)
 cluster_se_GNC <- cluster.vcov(GNC_glm18, ~Building)
 cluster_se_GNC_extracted <- sqrt(diag(cluster_se_GNC))
 coeftest(GNC_glm18, cluster_se_GNC)
 
 # Adjusted Odds Ratio and 95% CI
 est <- exp(GNC_glm18$coef)
 lower.ci <- est-qnorm(0.975)*cluster_se_GNC_extracted
 upper.ci <- est+qnorm(0.975)*cluster_se_GNC_extracted
 cbind(est,lower.ci, upper.ci)
 
 ######################################################################################################################
 ## Step 4: Moderation Analyses
 ## Trans youth
 
 ## SI by PV, Moderated by School Connectedness and Family Support/Monitoring Scale
 # unadjusted model
 trans_glm1 <- glm(SI_bin ~ PeerVic_bin*SC_Scale_Mean.c + PeerVic_bin*FSM_Scale_Mean.c, weights = Weight, family = "quasibinomial", data = dcya_transgender)
 summary(trans_glm1)
 cluster_se_trans <- cluster.vcov(trans_glm1, ~Building)
 cluster_se_trans_extracted <- sqrt(diag(cluster_se_trans))
 coeftest(trans_glm1, cluster_se_trans)
 
 # adjusted model
 trans_glm2 <- glm(SI_bin ~ PeerVic_bin*SC_Scale_Mean.c + PeerVic_bin*FSM_Scale_Mean.c + AgeRecode + RaceRecode.p + Sex, weights = Weight, family = "quasibinomial", data = dcya_transgender)
 summary(trans_glm2)
 cluster_se_trans_a <- cluster.vcov(trans_glm2, ~Building)
 cluster_se_trans_extracted_a <- sqrt(diag(cluster_se_trans_a))
 coeftest(trans_glm2, cluster_se_trans_a)
 
 
 
 ## SI by BB, moderated by School Connectedness and Family Support/Monitoring Scale
 # unadjusted model
 trans_glm3 <- glm(SI_bin ~ BB_bin*SC_Scale_Mean + BB_bin*FSM_Scale_Mean, weights = Weight, family = "quasibinomial", data = dcya_transgender)
 summary(trans_glm3)
 cluster_se_trans_BB <- cluster.vcov(trans_glm3, ~Building)
 cluster_se_trans_BB_extracted <- sqrt(diag(cluster_se_trans_BB))
 coeftest(trans_glm3, cluster_se_trans_BB)
 
 # adjusted model
 trans_glm4 <- glm(SI_bin ~ BB_bin*SC_Scale_Mean.c + BB_bin*FSM_Scale_Mean.c + AgeRecode + RaceRecode.p + Sex, weights = Weight, family = "quasibinomial", data = dcya_transgender)
 summary(trans_glm4)
 cluster_se_trans_BB_a <- cluster.vcov(trans_glm4, ~Building)
 cluster_se_trans_BB_extracted_a <- sqrt(diag(cluster_se_trans_BB_a))
 coeftest(trans_glm4, cluster_se_trans_BB_a)
 
 
 ## SA by PV, moderated by School Connectedness and Family Support/Monitoring Scale
 # unadjusted model
 trans_glm5 <- glm(SA_bin ~ PeerVic_bin*SC_Scale_Mean + PeerVic_bin*FSM_Scale_Mean, weights = Weight, family = "quasibinomial", data = dcya_transgender)
 summary(trans_glm5)
 cluster_se_trans_SA_PV <- cluster.vcov(trans_glm5, ~Building)
 cluster_se_trans_SA_PV_extracted <- sqrt(diag(cluster_se_trans_SA_PV))
 coeftest(trans_glm5, cluster_se_trans_SA_PV)
 
 # adjusted model
 trans_glm6 <- glm(SA_bin ~ PeerVic_bin*SC_Scale_Mean.c + PeerVic_bin*FSM_Scale_Mean.c + AgeRecode + RaceRecode.p + Sex, weights = Weight, family = "quasibinomial", data = dcya_transgender)
 summary(trans_glm6)
 cluster_se_trans_SA_PV_a <- cluster.vcov(trans_glm6, ~Building)
 cluster_se_trans_SA_PV_extracted_a <- sqrt(diag(cluster_se_trans_SA_PV_a))
 coeftest(trans_glm6, cluster_se_trans_SA_PV_a)
 
 
 ## SA by BB, moderated by School Connectedness and Family Support/Monitoring
 # unadjusted model
 trans_glm7 <- glm(SA_bin ~ BB_bin*SC_Scale_Mean + BB_bin*FSM_Scale_Mean, weights = Weight, family = "quasibinomial", data = dcya_transgender)
 summary(trans_glm7)
 cluster_se_trans_SA_BB <- cluster.vcov(trans_glm7, ~Building)
 cluster_se_trans_SA_BB_extracted <- sqrt(diag(cluster_se_trans_SA_BB))
 coeftest(trans_glm7, cluster_se_trans_SA_BB)
 
 # adjusted model
 trans_glm8 <- glm(SA_bin ~ BB_bin*SC_Scale_Mean.c + BB_bin*FSM_Scale_Mean.c + AgeRecode + RaceRecode.p + Sex, weights = Weight, family = "quasibinomial", data = dcya_transgender)
 summary(trans_glm8)
 cluster_se_trans_SA_BB_a <- cluster.vcov(trans_glm8, ~Building)
 cluster_se_trans_SA_BB_extracted_a <- sqrt(diag(cluster_se_trans_SA_BB_a))
 coeftest(trans_glm8, cluster_se_trans_SA_BB_a)
 
 
 ## NSSI by PV, moderated by School Connectedness and Family Support/Monitoring
 # unadjusted model
 trans_glm9 <- glm(NSSI_bin ~ PeerVic_bin*SC_Scale_Mean + PeerVic_bin*FSM_Scale_Mean, weights = Weight, family = "quasibinomial", data = dcya_transgender)
 summary(trans_glm9)
 cluster_se_trans_NSSI_PV <- cluster.vcov(trans_glm9, ~Building)
 cluster_se_trans_NSSI_PV_extracted <- sqrt(diag(cluster_se_trans_NSSI_PV))
 coeftest(trans_glm9, cluster_se_trans_NSSI_PV)
 
 # adjusted model
 trans_glm10 <- glm(NSSI_bin ~ PeerVic_bin*SC_Scale_Mean.c + PeerVic_bin*FSM_Scale_Mean.c + AgeRecode + RaceRecode.p + Sex, weights = Weight, family = "quasibinomial", data = dcya_transgender)
 summary(trans_glm10)
 cluster_se_trans_NSSI_PV_a <- cluster.vcov(trans_glm10, ~Building)
 cluster_se_trans_NSSI_PV_extracted_a <- sqrt(diag(cluster_se_trans_NSSI_PV_a))
 coeftest(trans_glm10, cluster_se_trans_NSSI_PV_a)
 
 
 ## NSSI by BB, moderated by School Connectedness and Family Support/Monitoring
 # unadjusted model
 trans_glm11 <- glm(NSSI_bin ~ BB_bin*SC_Scale_Mean + BB_bin*FSM_Scale_Mean, weights = Weight, family = "quasibinomial", data = dcya_transgender)
 summary(trans_glm11)
 cluster_se_trans_NSSI_BB <- cluster.vcov(trans_glm11, ~Building)
 cluster_se_trans_NSSI_BB_extracted <- sqrt(diag(cluster_se_trans_NSSI_BB))
 coeftest(trans_glm11, cluster_se_trans_NSSI_BB)
 
 # adjusted model
 trans_glm12 <- glm(NSSI_bin ~ BB_bin*SC_Scale_Mean.c + BB_bin*FSM_Scale_Mean.c + AgeRecode + RaceRecode.p + Sex, weights = Weight, family = "quasibinomial", data = dcya_transgender)
 summary(trans_glm12)
 cluster_se_trans_NSSI_BB_a <- cluster.vcov(trans_glm12, ~Building)
 cluster_se_trans_NSSI_BB_extracted_a <- sqrt(diag(cluster_se_trans_NSSI_BB_a))
 coeftest(trans_glm12, cluster_se_trans_NSSI_BB_a)
 
 
 ## Anxiety by PV, moderated by School Connectedness and FSM
 # unadjusted model
 trans_glm13 <- glm(Anxious_bin ~ PeerVic_bin*SC_Scale_Mean.c + PeerVic_bin*FSM_Scale_Mean.c, weights = Weight, family = "quasibinomial", data = dcya_transgender)
 summary(trans_glm13)
 cluster_se_trans_Anx_PV <- cluster.vcov(trans_glm13, ~Building)
 cluster_se_trans_Anx_PV_extracted <- sqrt(diag(cluster_se_trans_Anx_PV))
 coeftest(trans_glm13, cluster_se_trans_Anx_PV)
 
 # adjusted model
 trans_glm14 <- glm(Anxious_bin ~ PeerVic_bin*SC_Scale_Mean.c + PeerVic_bin*FSM_Scale_Mean.c + AgeRecode + RaceRecode.p + Sex, weights = Weight, family = "quasibinomial", data = dcya_transgender)
 summary(trans_glm14)
 cluster_se_trans_Anx_PV_a <- cluster.vcov(trans_glm14, ~Building)
 cluster_se_trans_Anx_PV_extracted_a <- sqrt(diag(cluster_se_trans_Anx_PV_a))
 coeftest(trans_glm14, cluster_se_trans_Anx_PV_a)
 
 
 ## Anxiety by BB, moderated by School Connectedness and Family Support/Monitoring
 # unadjusted model
 trans_glm15 <- glm(Anxious_bin ~ BB_bin*SC_Scale_Mean.c + BB_bin*FSM_Scale_Mean.c, weights = Weight, family = "quasibinomial", data = dcya_transgender)
 summary(trans_glm15)
 cluster_se_trans_Anx_BB <- cluster.vcov(trans_glm15, ~Building)
 cluster_se_trans_Anx_BB_extracted <- sqrt(diag(cluster_se_trans_Anx_BB))
 coeftest(trans_glm15, cluster_se_trans_Anx_BB)
 
 # adjusted model
 trans_glm16 <- glm(Anxious_bin ~ BB_bin*SC_Scale_Mean.c + BB_bin*FSM_Scale_Mean.c + AgeRecode + RaceRecode.p + Sex, weights = Weight, family = "quasibinomial", data = dcya_transgender)
 summary(trans_glm16)
 cluster_se_trans_Anx_BB_a <- cluster.vcov(trans_glm16, ~Building)
 cluster_se_trans_Anx_BB_extracted_a <- sqrt(diag(cluster_se_trans_Anx_BB_a))
 coeftest(trans_glm16, cluster_se_trans_Anx_BB_a)
 
 
 ## Depression by PV, moderated by School Connectedness and Family Support/Monitoring
 # unadjusted model
 trans_glm17.u <- glm(Depression_bin ~ PeerVic_bin*SC_Scale_Mean.c + PeerVic_bin*FSM_Scale_Mean.c, weights = Weight, family = "quasibinomial", data = dcya_transgender)
 summary(trans_glm17.u)
 cluster_se <- cluster.vcov(trans_glm17.u, ~Building)
 coeftest(trans_glm17.u, cluster_se)
 
 # adjusted model
 trans_glm17 <- glm(Depression_bin ~ PeerVic_bin*SC_Scale_Mean.c + PeerVic_bin*FSM_Scale_Mean.c + AgeRecode + RaceRecode.p + Sex, weights = Weight, family = "quasibinomial", data = dcya_transgender)
 summary(trans_glm17)
 cluster_se <- cluster.vcov(trans_glm17, ~Building)
 cluster_se_extracted <- sqrt(diag(cluster_se))
 coeftest(trans_glm17, cluster_se)
 
 ## Depression by BB, moderated by School Connectedness and Family Support/Monitoring
 # unadjusted model
 trans_glm18.u <- glm(Depression_bin ~ BB_bin*SC_Scale_Mean.c + BB_bin*FSM_Scale_Mean.c, weights = Weight, family = "quasibinomial", data = dcya_transgender)
 summary(trans_glm18.u)
 cluster_se <- cluster.vcov(trans_glm18.u, ~Building)
 coeftest(trans_glm18.u, cluster_se)
 
 # adjusted model
 trans_glm18 <- glm(Depression_bin ~ BB_bin*SC_Scale_Mean.c + BB_bin*FSM_Scale_Mean.c + AgeRecode + RaceRecode.p + Sex, weights = Weight, family = "quasibinomial", data = dcya_transgender)
 summary(trans_glm18)
 cluster_se_a <- cluster.vcov(trans_glm18, ~Building)
 cluster_se_extracted_a <- sqrt(diag(cluster_se_a))
 coeftest(trans_glm18, cluster_se_a)
 
 
 ### GNC youth
 ## SI by PV, Moderated by School Connectedness and Family Support/Monitoring
 # unadjusted model
 GNC_glm1 <- glm(SI_bin ~ PeerVic_bin*SC_Scale_Mean.c + PeerVic_bin*FSM_Scale_Mean.c, weights = Weight, family = "quasibinomial", data = dcya_GNC)
 summary(GNC_glm1)
 cluster_se_GNC <- cluster.vcov(GNC_glm1, ~Building)
 cluster_se_GNC_extracted <- sqrt(diag(cluster_se_GNC))
 coeftest(GNC_glm1, cluster_se_GNC)
 
 # adjusted model
 GNC_glm2 <- glm(SI_bin ~ PeerVic_bin*SC_Scale_Mean.c + PeerVic_bin*FSM_Scale_Mean.c + AgeRecode + RaceRecode.p + Sex, weights = Weight, family = "quasibinomial", data = dcya_GNC)
 summary(GNC_glm2)
 cluster_se_GNC_a <- cluster.vcov(GNC_glm2, ~Building)
 cluster_se_GNC_extracted_a <- sqrt(diag(cluster_se_GNC_a))
 coeftest(GNC_glm2, cluster_se_GNC_a)
 
 
 ## SI by BB, moderated by School Connectedness and Family Support/Monitoring
 # unadjusted model
 GNC_glm3 <- glm(SI_bin ~ BB_bin*SC_Scale_Mean.c + BB_bin*FSM_Scale_Mean.c, weights = Weight, family = "quasibinomial", data = dcya_GNC)
 summary(GNC_glm3)
 cluster_se_GNC_BB <- cluster.vcov(GNC_glm3, ~Building)
 cluster_se_GNC_BB_extracted <- sqrt(diag(cluster_se_GNC_BB))
 coeftest(GNC_glm3, cluster_se_GNC_BB)
 
 # adjusted model
 GNC_glm4 <- glm(SI_bin ~ BB_bin*SC_Scale_Mean.c + BB_bin*FSM_Scale_Mean.c + AgeRecode + RaceRecode.p + Sex, weights = Weight, family = "quasibinomial", data = dcya_GNC)
 summary(GNC_glm4)
 cluster_se_GNC_BB_a <- cluster.vcov(GNC_glm4, ~Building)
 cluster_se_GNC_BB_extracted_a <- sqrt(diag(cluster_se_GNC_BB_a))
 coeftest(GNC_glm4, cluster_se_GNC_BB_a)
 
 
 ## SA by PV, moderated by School Connectedness and Family Support/Monitoring
 # unadjusted model
 GNC_glm5 <- glm(SA_bin ~ PeerVic_bin*SC_Scale_Mean.c + PeerVic_bin*FSM_Scale_Mean.c, weights = Weight, family = "quasibinomial", data = dcya_GNC)
 summary(GNC_glm5)
 cluster_se_GNC_SA_PV <- cluster.vcov(GNC_glm5, ~Building)
 cluster_se_GNC_SA_PV_extracted <- sqrt(diag(cluster_se_GNC_SA_PV))
 coeftest(GNC_glm5, cluster_se_GNC_SA_PV)
 
 # adjusted model
 GNC_glm6 <- glm(SA_bin ~ PeerVic_bin*SC_Scale_Mean.c + PeerVic_bin*FSM_Scale_Mean.c + AgeRecode + RaceRecode.p + Sex, weights = Weight, family = "quasibinomial", data = dcya_GNC)
 summary(GNC_glm6)
 cluster_se_GNC_SA_PV_a <- cluster.vcov(GNC_glm6, ~Building)
 cluster_se_GNC_SA_PV_extracted_a <- sqrt(diag(cluster_se_GNC_SA_PV_a))
 coeftest(GNC_glm6, cluster_se_GNC_SA_PV_a)
 
 
 ## SA by BB, moderated by School Connectedness and Family Support/Monitoring
 # unadjusted model
 GNC_glm7 <- glm(SA_bin ~ BB_bin*SC_Scale_Mean.c + BB_bin*FSM_Scale_Mean.c, weights = Weight, family = "quasibinomial", data = dcya_GNC)
 summary(GNC_glm7)
 cluster_se_GNC_SA_BB <- cluster.vcov(GNC_glm7, ~Building)
 cluster_se_GNC_SA_BB_extracted <- sqrt(diag(cluster_se_GNC_SA_BB))
 coeftest(GNC_glm7, cluster_se_GNC_SA_BB)
 
 # adjusted model
 GNC_glm8 <- glm(SA_bin ~ BB_bin*SC_Scale_Mean.c + BB_bin*FSM_Scale_Mean.c + AgeRecode + RaceRecode.p + Sex, weights = Weight, family = "quasibinomial", data = dcya_GNC)
 summary(GNC_glm8)
 cluster_se_GNC_SA_BB_a <- cluster.vcov(GNC_glm8, ~Building)
 cluster_se_GNC_SA_BB_extracted_a <- sqrt(diag(cluster_se_GNC_SA_BB_a))
 coeftest(GNC_glm8, cluster_se_GNC_SA_BB_a)
 
 ## NSSI by PV, moderated by School Connectedness and Family Support/Monitoring
 # unadjusted model
 GNC_glm9 <- glm(NSSI_bin ~ PeerVic_bin*SC_Scale_Mean.c + PeerVic_bin*FSM_Scale_Mean.c, weights = Weight, family = "quasibinomial", data = dcya_GNC)
 summary(GNC_glm9)
 cluster_se_GNC_NSSI_PV <- cluster.vcov(GNC_glm9, ~Building)
 cluster_se_GNC_NSSI_PV_extracted <- sqrt(diag(cluster_se_GNC_NSSI_PV))
 coeftest(GNC_glm9, cluster_se_GNC_NSSI_PV)
 
 # adjusted model
 GNC_glm10 <- glm(NSSI_bin ~ PeerVic_bin*SC_Scale_Mean.c + PeerVic_bin*FSM_Scale_Mean.c + AgeRecode + RaceRecode.p + Sex, weights = Weight, family = "quasibinomial", data = dcya_GNC)
 summary(GNC_glm10)
 cluster_se_GNC_NSSI_PV_a <- cluster.vcov(GNC_glm10, ~Building)
 cluster_se_GNC_NSSI_PV_extracted_a <- sqrt(diag(cluster_se_GNC_NSSI_PV_a))
 coeftest(GNC_glm10, cluster_se_GNC_NSSI_PV_a)
 
 
 ## NSSI by BB, moderated by School Connectedness and Family Support/Monitoring
 # unadjusted model
 GNC_glm11 <- glm(NSSI_bin ~ BB_bin*SC_Scale_Mean.c + BB_bin*FSM_Scale_Mean.c, weights = Weight, family = "quasibinomial", data = dcya_GNC)
 summary(GNC_glm11)
 cluster_se_GNC_NSSI_BB <- cluster.vcov(GNC_glm11, ~Building)
 cluster_se_GNC_NSSI_BB_extracted <- sqrt(diag(cluster_se_GNC_NSSI_BB))
 coeftest(GNC_glm11, cluster_se_GNC_NSSI_BB)
 
 # adjusted model
 GNC_glm12 <- glm(NSSI_bin ~ BB_bin*SC_Scale_Mean.c + BB_bin*FSM_Scale_Mean.c + AgeRecode + RaceRecode.p + Sex, weights = Weight, family = "quasibinomial", data = dcya_GNC)
 summary(GNC_glm12)
 cluster_se_GNC_NSSI_BB_a <- cluster.vcov(GNC_glm12, ~Building)
 cluster_se_GNC_NSSI_BB_extracted_a <- sqrt(diag(cluster_se_GNC_NSSI_BB_a))
 coeftest(GNC_glm12, cluster_se_GNC_NSSI_BB_a)
 
 
 ## Anxiety by PV, moderated by School Connectedness and Family Support/Monitoring
 # unadjusted model
 GNC_glm13 <- glm(Anxious_bin ~ PeerVic_bin*SC_Scale_Mean.c + PeerVic_bin*FSM_Scale_Mean.c, weights = Weight, family = "quasibinomial", data = dcya_GNC)
 summary(GNC_glm13)
 cluster_se_GNC_Anx_PV <- cluster.vcov(GNC_glm13, ~Building)
 cluster_se_GNC_Anx_PV_extracted <- sqrt(diag(cluster_se_GNC_Anx_PV))
 coeftest(GNC_glm13, cluster_se_GNC_Anx_PV)
 
 # adjusted model
 GNC_glm14 <- glm(Anxious_bin ~ PeerVic_bin*SC_Scale_Mean.c + PeerVic_bin*FSM_Scale_Mean.c + AgeRecode + RaceRecode.p + Sex, weights = Weight, family = "quasibinomial", data = dcya_GNC)
 summary(GNC_glm14)
 cluster_se_GNC_Anx_PV_a <- cluster.vcov(GNC_glm14, ~Building)
 cluster_se_GNC_Anx_PV_extracted_a <- sqrt(diag(cluster_se_GNC_Anx_PV_a))
 coeftest(GNC_glm14, cluster_se_GNC_Anx_PV_a)
 
 
 ## Anxiety by BB, moderated by School Connectedness and Family Support/Monitoring
 # unadjusted model
 GNC_glm15 <- glm(Anxious_bin ~ BB_bin*SC_Scale_Mean.c + BB_bin*FSM_Scale_Mean.c, weights = Weight, family = "quasibinomial", data = dcya_GNC)
 summary(GNC_glm15)
 cluster_se_GNC_Anx_BB <- cluster.vcov(GNC_glm15, ~Building)
 cluster_se_GNC_Anx_BB_extracted <- sqrt(diag(cluster_se_GNC_Anx_BB))
 coeftest(GNC_glm15, cluster_se_GNC_Anx_BB)
 
 # adjusted model
 GNC_glm16 <- glm(Anxious_bin ~ BB_bin*SC_Scale_Mean.c + BB_bin*FSM_Scale_Mean.c + AgeRecode + RaceRecode.p + Sex, weights = Weight, family = "quasibinomial", data = dcya_GNC)
 summary(GNC_glm16)
 cluster_se_GNC_Anx_BB_a <- cluster.vcov(GNC_glm16, ~Building)
 cluster_se_GNC_Anx_BB_extracted_a <- sqrt(diag(cluster_se_GNC_Anx_BB_a))
 coeftest(GNC_glm16, cluster_se_GNC_Anx_BB_a)
 
 
 ## Depression by PV, moderated by School Connectedness and Family Support/Monitoring
 # unadjusted model
 GNC_glm17.u <- glm(Depression_bin ~ PeerVic_bin*SC_Scale_Mean.c + PeerVic_bin*FSM_Scale_Mean.c, weights = Weight, family = "quasibinomial", data = dcya_GNC)
 summary(GNC_glm17.u)
 cluster_se <- cluster.vcov(GNC_glm17.u, ~Building)
 coeftest(GNC_glm17.u, cluster_se)
 
 
 # adjusted model
 GNC_glm17 <- glm(Depression_bin ~ PeerVic_bin*SC_Scale_Mean.c + PeerVic_bin*FSM_Scale_Mean.c + AgeRecode + RaceRecode + Sex, weights = Weight, family = "quasibinomial", data = dcya_GNC)
 summary(GNC_glm17)
 cluster_se <- cluster.vcov(GNC_glm17, ~Building)
 cluster_se_extracted <- sqrt(diag(cluster_se))
 coeftest(GNC_glm17, cluster_se)
 
 ## Depression by BB, moderated by School Connectedness and Family Support/Monitoring
 # unadjusted model
 GNC_glm18.u <- glm(Depression_bin ~ BB_bin*SC_Scale_Mean.c + BB_bin*FSM_Scale_Mean.c, weights = Weight, family = "quasibinomial", data = dcya_GNC)
 summary(GNC_glm18.u)
 cluster_se <- cluster.vcov(GNC_glm18.u, ~Building)
 coeftest(GNC_glm18.u, cluster_se)
 
 # adjusted model
 GNC_glm18 <- glm(Depression_bin ~ BB_bin*SC_Scale_Mean.c + BB_bin*FSM_Scale_Mean.c + AgeRecode + RaceRecode.p + Sex, weights = Weight, family = "quasibinomial", data = dcya_GNC)
 summary(GNC_glm18)
 cluster_se_a <- cluster.vcov(GNC_glm18, ~Building)
 cluster_se_extracted_a <- sqrt(diag(cluster_se_a))
 coeftest(GNC_glm18, cluster_se_a)
 
 #######################################################################################################################################
 ### Plot interactions 
 p <- interact_plot(trans_glm6, pred = PeerVic_bin, modx = FSM_Scale_Mean.c, modx.values = NULL, legend.main = "Family Support \n and Monitoring", 
                    cluster = dcya_transgender$Building, interval = TRUE, pred.labels = c("None", "Any"),
                    colors = "Blues")
 
 plot1 <- p + theme_classic() + labs(x = "Peer Victimization", y = "Suicide Attempt")  + ggtitle("A") +
   scale_y_continuous(limits = c(0,.70)) + theme(plot.title=element_text(face="bold"))
 
plot1
 
 
 p2 <- interact_plot(GNC_glm18, pred = BB_bin, modx = SC_Scale_Mean.c, modx.values = NULL, legend.main = "School-\nconnectedness", 
                     cluster = dcya_GNC$Building, interval = TRUE, pred.labels = c("None", "Any"),
                     colors = "Blues")
 
 plot2 <- p2 + theme_classic() + labs(x = "Bias-based Harassment", y = "Depression") + ggtitle("B")  +
   scale_y_continuous(limits = c(0,.70)) + theme(plot.title=element_text(face="bold"))
 
 plot2
 
 # top = "Figure 1. Predicted Probabilities of Mental Health Indicators Among Transgender Youth (Panel A) and GNC Youth (Panel B)"
 ### layout in panel
 grid.arrange(plot1, plot2, nrow = 2)
 
