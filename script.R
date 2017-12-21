## ============================= TGAP Demand Data Munging ============================

# The piece of code tidies and mould the Talent Gap Demand Questionaire data.
# The key columns are arranged to aid probing and analysis.
# The code takes in the raw dataset and outputs tidied data sets that could be analysed for certain questions

rm(list = ls()) #clear workspace
## checks for, installs and loads necessary packages
if ("dplyr" %in% row.names(installed.packages()) == FALSE){install.packages("dplyr")}
if ("tidyr" %in% row.names(installed.packages()) == FALSE){install.packages("tidyr")}
library(tidyr)
library(dplyr)

## set working directory: raw dataset should be in this directory

wrkDir <- "C:/Users/AROOGZ/Documents/My Works/WORK - CcHUB/Talent Gap Project/"
setwd(wrkDir)

## Note: the filepath variable is where I have the dataset in this case
filePath <- "Raw data/Copy of Talent Gap Analysis Research Project - Talent Demand Questionnaire (Responses) - Form Responses 1.csv"
tGapRaw <- read.csv(filePath, stringsAsFactors = F)
tGapRaw <- tGapRaw[!duplicated(tGapRaw$Organisation.Name),]


#deprecated
# cleanNames <- c("time.stamp", "org.name", "state", "core.biz",
#                 "solely.nig", "other.country", "no.employee", "no.non-nig.emp",
#                 "no.it.emp","mt.sal.exec", "mt.sal.sen-mgt", "mt.sal.mid-mgt",
#                 "mt.sal.non-exec","mt")
rawNames <- names(tGapRaw)

# *************** remember the effect of the gather operation ************
## csvs directory is created to hold the output csvs
if (!dir.exists("./csvs")){
  dir.create("./csvs")
}

#saves the raw data
write.csv(x= tGapRaw, "csvs/tGapRaw.csv", row.names = F)
# ============ Position and Salary =================
tGap.salary <- tGapRaw %>% 
  gather(key=position,value=month.salary, 10:15) %>%
  mutate(Position= factor(position,
                          labels= c("Executive", "Fresh Grad. with Bach.",
                                   "Fresh Grad. with Dipl.", "Mid. Mgt.",
                                   "Non- Executive", "Senior Mgt."))) %>%
  filter(!month.salary %in% c("", "N/A"))
write.csv(x = tGap.salary, "csvs/tGap.salary.csv", row.names = F)


# =========== Number of Professional ================
tGap.pro <- tGapRaw %>% gather(job.funct, number, 16:26) %>%
  mutate(job.function= factor(job.funct, 
                           label=c("Application Development", "Customer Support",
                                   "Data Management", "IT Architecture",
                                   "IT Business Analysis", "IT Policy and planning",
                                   "IT Project Mgt.", "IT Security",
                                   "IT vendor Mgt.", "Network and Telecomms.",
                                   "Systems Administration"))) %>%
  filter(!number %in% c("", "N/A")) %>% mutate(number = paste("'", number))
write.csv(x = tGap.pro, "csvs/tGap.pro.csv", row.names = F)

# ================ recruitment difficulty ====================
## aligns each "job function" next to the "difficulty level" to ease summarization and filter operations
tGap.recruitDiff <- gather(tGapRaw, job.funct, difficulty, 27:36 ) %>%
  mutate(job.function= factor(job.funct,
                              labels= c("Application Development", "Customer Support",
                                        "Data Management", "IT Architecture",
                                        "IT Business Analysis", "IT Policy and planning",
                                        "IT Project Mgt.", "IT Security",
                                        "Network and Telecomms.",
                                        "Systems Administration"))) %>%
  filter(!difficulty %in% c("", "N/A"))
write.csv(x = tGap.recruitDiff, "csvs/tGap.recruitDiff.csv", row.names = F)


## separating the specified column in the data frame to individual element

## the following two functions are helper functions that follow
max.cell.items <- function(data, col, sep){
  ## sub-helper function for the next function. It helps to retrieve the row with the most number of items separated by commas in a given column
  
  splits <- strsplit(data[,col], split = sep)
  lengths <- sapply(splits, length)
  max.length <- sort(lengths, decreasing = T)[1]
  return(max.length)
}
separate.col <- function(data, col, sep, name){
  ## separates a column with values seperated by commas into individual columns
 
  max.length <- max.cell.items(data, col, sep)
  var.names <- paste(rep(name,max.length), seq(max.length), sep = "")
  result <- separate_(data = data, col = get("col"), into = var.names, sep= sep)
  
  return(result)
}

## ======= Emerging Areas ======
## aligns each "emerging area" next to the "emerging term" to ease summarization and filter operations
tidy.emergin <- function(data, name){
  tGap.emerging <- gather(data, emerging.term, emerging.area, 37:40)
  max.length <- max.cell.items(tGap.emerging, "emerging.area", sep = ",")
  tGap.emerging <- separate.col(tGap.emerging, col = "emerging.area", sep = ", ", name = "")
  
  tGap.emerging <- gather_(tGap.emerging, key_col = "area.pos", value_col = "area",
                           gather_cols= paste(rep(name,max.length), seq(max.length), sep = ""))
  tGap.emerging <- subset(tGap.emerging, !(area == ""|area == "N/A"|area == "--")) %>%
    mutate(Impact.Term= factor(emerging.term, 
                               labels= c("12.months.short.term",
                                         "2-3.years.mid.term.impact",
                                         "2.3.years.months.mid.term.impact",
                                         "5.years.months..long.term.impact")))
  return(tGap.emerging)
}
tGap.emerging <- tidy.emergin(data = tGapRaw, name = "")
write.csv(x = tGap.emerging, "csvs/tGap.emerging.csv", row.names = F)


## ========== needs =====
## aligns each "need area" next to the "need term" to ease summarization and filter operations
tidy.needs <- function(data, name){
  tGap.needs <- gather(data, need.term, need.area, 41:57)
  max.length <- max.cell.items(tGap.needs, "need.area", sep = ",")
  tGap.needs <- separate.col(data = tGap.needs, col = "need.area", sep = ", ", name = name)
  tGap.needs <- gather_(tGap.needs, key_col = "need.pos", value_col = "need.area",
                        gather_cols = paste(rep(name, max.length),seq(max.length), sep = ""))
  tGap.needs <- subset(tGap.needs, !(need.area == "N/A"|need.area=="")) %>%
    mutate(Future.Need.Term= factor(need.term, 
                                    labels= c("2.3.years..in.Systems.Administration",
                                              "2.3.years..in.Systems.Administration",
                                              "3.5.years..in.Application.Development",
                                              "3.5.years..in.Data.Management",
                                              "3.5.years..in.IT.Architecture",
                                              "3.5.years..in.IT.Business.Analysis",
                                              "3.5.years..in.IT.Project.Management",
                                              "3.5.years..in.IT.Security",
                                              "3.5.years..in.Network.and.Telecommunications",
                                              "12.months..in.Application.Development",
                                              "12.months..in.Data.Management",
                                              "12.months..in.IT.Architecture",
                                              "12.months..in.IT.Business.Analysis",
                                              "12.months..in.IT.Project.Management",
                                              "12.months..in.IT.Security",
                                              "12.months..in.Network.and.Telecommunications",
                                              "12.months..in.Systems.Administration")))
  return(tGap.needs)
}
tGap.needs <- tidy.needs(tGapRaw, name = "")
write.csv(x = tGap.needs, "csvs/tGap.needs.csv", row.names = F)

## ======== attributes ======
tidy.attribute <- function(data, name){
  att.col <- names(data)[59]
  max.length <- max.cell.items(data, att.col, ",")
  tGap.att <- separate.col(data, col = att.col, sep = ", ", name = name)
  tGap.att <- gather_(tGap.att, key_col= "att.rank", value_col= "attribute",
                      gather_cols= paste(rep(name, max.length),seq(max.length), sep = ""))
  tGap.att <- subset(tGap.att, !(attribute == ""|attribute == "N/A"))
  return(tGap.att)
}
tGap.attribute <- tidy.attribute(tGapRaw, name = "")
write.csv(x = tGap.attribute, "csvs/tGap.attribute.csv", row.names = F)

## ========= attribute.difficulty ============
tidy.att.diff <- function(data){
  tGap.att.diff <- gather(data, applicant.type, difficulty, 60:63)
  tGap.att.diff <- subset(tGap.att.diff, !(difficulty == ""|difficulty == "N/A")) %>%
    mutate(Type.of.Applicant= factor(applicant.type,
                                     labels= c("Applicants.with.sufficient.and.relevant.non..technical.competencies",
                                               "Applicants.with.sufficient.and.relevant.technical.competencies",
                                               "Applicants.with.the.required.years.of.relevant.experience",
                                               "Entry.level.applicants.with.high.academic.and.co.curriculum.performance")))
}
tGap.att.diff <- tidy.att.diff(tGapRaw)
write.csv(x = tGap.att.diff, "csvs/tGap.att.diff.csv", row.names = F)

## ========= growth driver =========
tidy.growthDriver <- function(data, name){
  driver.col <- names(data)[67]
  max.length <- max.cell.items(data, driver.col, ",")
  tGap.growthDriver <- separate.col(data, driver.col, ", ", name= name)
  tGap.growthDriver <- gather_(tGap.growthDriver, key_col= "driver.rank",
                               value_col= "driver", 
                               gather_cols=paste(rep(name, max.length), seq(max.length), sep = ""))
  tGap.growthDriver <- subset(tGap.growthDriver, !(is.na(driver)|driver== ""|driver == "N/A"))
  return(tGap.growthDriver)
}
tGap.growthDriver <- tidy.growthDriver(tGapRaw, name = "")
write.csv(x = tGap.growthDriver, "csvs/tGap.growthDriver.csv", row.names = F)

## ========= company attraction =======
tidy.compAttraction <- function(data, name){
  compAttract.col <- names(data)[68]
  max.length <- max.cell.items(data, compAttract.col, ",")
  tGap.compAttract <- separate.col(data, compAttract.col, ", ",name)
  tGap.compAttract <- gather_(tGap.compAttract, key_col= "factor.pos",
                              value_col= "factor",
                              gather_cols= paste(rep(name, max.length), seq(max.length), sep = ""))
  tGap.compAttract <- subset(tGap.compAttract, !(factor == ""|factor == "N/A"))
  return(tGap.compAttract)
}
tGap.compAttraction <- tidy.compAttraction(tGapRaw, name = "")
write.csv(x = tGap.compAttraction, "csvs/tGap.compAttraction.csv", row.names = F)




