# Run necessary Libraries
# library(Rtools)
library(Hmisc)
library(rsconnect)
library(fontawesome)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(data.table)
library(DT)
library(magrittr)
library(tidyverse)
library(plotly)
library(survminer)
library(survival)
library(ggsurvfit)
library(flextable)
library(extrafont)

# Import Data
pooleddata <- read.csv2(paste0(wdset,"Datasets/pooled_data.csv"), header = TRUE,sep=",",check.names = F)

# Data wrangling
pooleddata$"Survival  (months)" <- as.numeric(pooleddata$"Survival  (months)")
pooleddata$"Overall Survival Censor" <- as.numeric(pooleddata$"Overall Survival Censor")
pooleddata$"Disease-free interval (months)" <- as.numeric(pooleddata$"Disease-free interval (months)")
pooleddata$"Disease Specific Survival Censor" <- as.numeric(pooleddata$"Disease Specific Survival Censor")
pooleddata$`Current Smoker` <- as.integer(pooleddata$`Current Smoker`)
pooleddata$`Smoking History` <- as.integer(pooleddata$`Smoking History`)
pooleddata$`Height (m)` <- as.numeric(pooleddata$`Height (m)`)
pooleddata$`BW Start tx (kg)` <- as.numeric(pooleddata$`BW Start tx (kg)`)
pooleddata$`BW stop treat (kg)` <- as.numeric(pooleddata$`BW stop treat (kg)`)
pooleddata <- pooleddata %>%
  mutate(`Overall Survival Censor` = ifelse(`Overall Survival Censor` == 0, 1, 2)) %>%
  mutate(`Disease Specific Survival Censor` = ifelse(`Disease Specific Survival Censor` == 0, 1, 2))
pooleddata$Age <- as.numeric(pooleddata$Age)
pooleddata$M <- as.character(pooleddata$M)
pooleddata$`Date of recurrence` <- as.numeric(pooleddata$`Date of recurrence`)

moveMe <- function(data, tomove, where = "last", ba = NULL) {
  temp <- setdiff(names(data), tomove)
  x <- switch(
    where,
    first = data[c(tomove, temp)],
    last = data[c(temp, tomove)],
    before = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      data[append(temp, values = tomove, after = (match(ba, temp)-1))]
    },
    after = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      data[append(temp, values = tomove, after = (match(ba, temp)))]
    })
  x
}

pooleddata <- moveMe(pooleddata, c('BMI start treat (kg/m2)','BMI stop treat (kg/m2)'), "after", "BW stop treat (kg)")



chartonum <- c('Date of Diagnosis','Last Contact Date','Follow up duration (year)','Follow up duration (month)','Date of Death','RT Total Dose (Gy)','Dose/Fraction (Gy/fx)','BMI start treat (kg/m2)','BMI stop treat (kg/m2)','Date Start RT','Date Stop RT','Time between pre and post image (months)','Time from preRT image to start RT (month)','Time from RT stop to follow up imaging (months)','Pre-RT L3 Skeletal Muscle Cross Sectional Area (cm2)','Post-RT L3 Skeletal Muscle Cross Sectional Area (cm2)','Pre-RT L3 Adipose Tissue Cross Sectional Area (cm2)','Post-RT L3 Adipose Tissue Cross Sectional Area (cm2)','Pre-RT L3 Skeletal Muscle Index (cm2/m2)','Post-RT L3 Skeletal Muscle Index (cm2/m2)','Pre-RT L3 Adiposity Index (cm2/m2)','Post-RT L3 Adiposity Index (cm2/m2)','Pre-RT CT-derived lean body mass (kg)','Post-RT CT-derived lean body mass (kg)','Pre-RT CT-derived fat body mass (kg)','Post-RT CT-derived fat body mass (kg)','Pre-RT Imaging Date','Pre-RT Imaging Modality','CT sim date','post-RT imaging date')
pooleddata[,chartonum] <-  lapply(pooleddata[,chartonum,drop=FALSE],as.numeric)

pooleddata <- pooleddata %>% 
  select(-`Date of Diagnosis`,-`Last Contact Date`,-`Pre-RT Imaging Date`,-`Pre-RT Imaging Modality`,-`CT sim date`,-`post-RT imaging date`,
         -`Date Start RT`,`Date Stop RT`,-`Date of Death`,-`Date of recurrence`)

datasel = pooleddata
datasel$`Age` <- as.numeric(datasel$`Age`)

# Create Filtering Variables
numvec <- colnames(pooleddata %>% select(where(~!all(is.na(.))) & where(is.numeric)))
charvec <- colnames(pooleddata %>% select(where(~!all(is.na(.))) & where(is.character)))
survec <- c("Survivalmonths","Diseasefreeintervalmonths")

cat(charvec, sep = "','")

filtereddata <- read.csv2(paste0(wdset,"python/new_filtered data.csv"), header = TRUE,sep=",",check.names = F)

library(reticulate)
source_python(paste0(wdset,"python/ml_models_for_r_platform.py"))
xcolumns <- c(colnames(filtereddata[, c(3:18)]))

survdata = pooleddata %>%
  rename_at(vars(everything()), ~str_replace_all(., "\\s+", "")) %>%
  rename_at(vars(everything()), ~str_replace_all(., "-", ""))
names(survdata) <- gsub(x = names(survdata), pattern = "\\(", replacement = "") 
names(survdata) <- gsub(x = names(survdata), pattern = "\\)", replacement = "") 
names(survdata) <- gsub(x = names(survdata), pattern = "\\/", replacement = "") 
numvecsurv <- colnames(survdata %>% select(where(~!all(is.na(.))) & where(is.numeric)))
charvecsurv <- colnames(survdata %>% select(where(~!all(is.na(.))) & where(is.character)))

# Run functions
source(paste0(wdset, "App/functions.R"))

# Run UIs and Servers
source(paste0(wdset, "modules/frontpage.R"))
source(paste0(wdset, "modules/dataviewer.R"))
source(paste0(wdset, "modules/demographics.R"))
source(paste0(wdset, "modules/patientprofile.R"))
source(paste0(wdset, "modules/survival.R"))
source(paste0(wdset, "modules/classifier.R"))