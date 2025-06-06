###############################################################################################
### Assessing Distinguishable Social Skills in Medical Admission -                          ###
### Does Construct-Driven Development Solve Validity Issues of Situational Judgment Tests?  ###
###############################################################################################

# This script contains the code to prepare the dataset we used for determining our main results.

# (c) 2021 Ina Mielke.


# This code is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
# 
# For a copy of the GNU General Public License, 
# see <http://www.gnu.org/licenses/>.

# load necessary packages
library(readxl)
library (dplyr)

###########################################################################################
##### Step 1: Data Upload                                                             ####
###########################################################################################

# Set working directory

# Import dataexport from database with all variables
dataexport <- read_excel("dataexport.xlsx")

###########################################################################################
##### Step 2a: Data Preparation Construct-Driven SJT Data                              ####
###########################################################################################

# "SJTkb00xxx_raw" are original answers, "SJTkb00xxx" are recoded to their construct level

# select participants with construct-driven SJT data
data <- subset(dataexport, dataexport$NRec > 0)

# delete unnecessary variables
data <- dplyr::select(data, -c(Ort,SJTkbWarmherzigkeit:SJTkbDurchsetzungsfaehigkeit))

# inspect items
cdSJT <- data %>% dplyr::select (SJTkb00218:SJTkb00247)
summary(cdSJT) # values look reasonable

###########################################################################################
##### Step 2b: Data Preparation Traditional SJT Data                                   ####
###########################################################################################

# "SJTKL00xxxx" are original answers, "SJTKLGesxx_xxx" are different final scores compared to expert ratings

# delete unnecessary variables
data <- dplyr:: select (data, -c(FID_1:Datum_3,SJTKLGesAR_mean:SJTKLGesSR_sum,SJTKLGesSZ_sum:HAMSJTSZ))

# inspect items
tSJT <- data %>% dplyr::select (SJTKL0005A1:SJTKL0125G4)
summary(tSJT) # values look reasonable

###########################################################################################
##### Step 2c: Data Preparation Online Survey                                          ####
###########################################################################################

# delete unnecessary variables and scales (will be computed again as means)
data <- dplyr:: select (data, -c(FID_5:Datum_7,BFIExtra:BFIKreaEin,IALJK:IALNO,
                                 ISKPS:ISKSE,NARQADM:NARQRIV,VerhaltenAg:ATICSiH244D))

# inspect items
survey <- data %>% dplyr::select (BFI01:VerhaltenCO4)
summary(survey) # values look reasonable

###########################################################################################
##### Step 2d: Data Preparation Sociodemographics                                      ####
###########################################################################################

# delete unnessary variables
data <- dplyr:: select (data, -c(FID_13:Datum_15,"05_01":SozioDemoCode))

# rename variables

data$sex <- data$"01_01" # 1=female, 2=male, 3=NA
data$age <- 2020-data$"02_01"
data$grade1 <- data$"03_01"
data$grade2 <- data$"04_01"

# inspect items
socio <- data %>% dplyr::select (sex:grade2)
summary(socio) # values look reasonable

###########################################################################################
##### Step 3: Save dataset for main analyses                                          ####
###########################################################################################

write.table(data, file = "data")


