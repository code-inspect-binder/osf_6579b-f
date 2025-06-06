###############################################################################################
### Assessing Distinguishable Social Skills in Medical Admission -                          ###
### Does Construct-Driven Development Solve Validity Issues of Situational Judgment Tests?  ###
###############################################################################################

# This script contains the code to compute our supplemental results.

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
library(psych)
library(plyr)
library(tidyverse)
library(MBESS)

###########################################################################################
##### Step 1: Data Upload                                                             ####
###########################################################################################

# Set working directory

# Import Test data
data <- read.csv("D:/Promotion/04_Research/01_Development_Validation_Constructbased_SJT/04_Analysis/data2", sep="")

# Import Pretest data
pretest_goe <- read_excel("Pretest_Goe.xlsx")
pretest_mue <- read_excel("Pretest_Mue.xlsx")

# Import Pilot data
pilotexport <- read.delim("Pilot.txt")

###########################################################################################
##### Step 2a: Correlations All Variables Main Data                                    ####
###########################################################################################

### BFI-2: compute variables and reliabilities

# Extraversion
data$BFI11_r <- mapvalues(data$BFI11,c(1,2,3,4,5),c(5,4,3,2,1))
data$BFI16_r <- mapvalues(data$BFI16,c(1,2,3,4,5),c(5,4,3,2,1))
data$BFI26_r <- mapvalues(data$BFI26,c(1,2,3,4,5),c(5,4,3,2,1))
data$BFI31_r <- mapvalues(data$BFI31,c(1,2,3,4,5),c(5,4,3,2,1))
# Domain
data$BFI_E <- apply(data[,c("BFI01","BFI06","BFI11_r","BFI16_r","BFI21","BFI26_r","BFI31_r","BFI36_r","BFI41","BFI46","BFI51_r","BFI56")], MARGIN = 1, FUN = mean)
# Facets: Sociability, Energy Level
data$BFI_E_S <- apply(data[,c("BFI01","BFI16_r","BFI31_r","BFI46")], MARGIN = 1, FUN = mean)
data$BFI_E_EL <- apply(data[,c("BFI11_r","BFI26_r","BFI41","BFI56")], MARGIN = 1, FUN = mean)

# Agreeableness
data$BFI12_r <- mapvalues(data$BFI12,c(1,2,3,4,5),c(5,4,3,2,1))
data$BFI22_r <- mapvalues(data$BFI22,c(1,2,3,4,5),c(5,4,3,2,1))
data$BFI37_r <- mapvalues(data$BFI37,c(1,2,3,4,5),c(5,4,3,2,1))
data$BFI42_r <- mapvalues(data$BFI42,c(1,2,3,4,5),c(5,4,3,2,1))
# Domain
data$BFI_A <- apply(data[,c("BFI02","BFI07","BFI12_r","BFI17_r","BFI22_r","BFI27","BFI32","BFI37_r","BFI42_r","BFI47_r","BFI52","BFI57")], MARGIN = 1, FUN = mean)
# Facets: Respectfulness, Trust
data$BFI_A_R <- apply(data[,c("BFI07","BFI22_r","BFI37_r","BFI52")], MARGIN = 1, FUN = mean)
data$BFI_A_T <- apply(data[,c("BFI12_r","BFI27","BFI42_r","BFI57")], MARGIN = 1, FUN = mean)

# Conscientiousness
data$BFI03_r <- mapvalues(data$BFI03,c(1,2,3,4,5),c(5,4,3,2,1))
data$BFI08_r <- mapvalues(data$BFI08,c(1,2,3,4,5),c(5,4,3,2,1))
data$BFI23_r <- mapvalues(data$BFI23,c(1,2,3,4,5),c(5,4,3,2,1))
data$BFI28_r <- mapvalues(data$BFI28,c(1,2,3,4,5),c(5,4,3,2,1))
data$BFI48_r <- mapvalues(data$BFI48,c(1,2,3,4,5),c(5,4,3,2,1))
data$BFI58_r <- mapvalues(data$BFI58,c(1,2,3,4,5),c(5,4,3,2,1))
# Domain
data$BFI_C <- apply(data[,c("BFI03_r","BFI08_r","BFI13","BFI18","BFI23_r","BFI28_r","BFI33","BFI38","BFI43","BFI48_r","BFI53","BFI58_r")], MARGIN = 1, FUN = mean)
# Facets: Organization, Productiveness, Responsibility
data$BFI_C_O <- apply(data[,c("BFI03_r","BFI18","BFI33","BFI48_r")], MARGIN = 1, FUN = mean)
data$BFI_C_P <- apply(data[,c("BFI08_r","BFI23_r","BFI38","BFI53")], MARGIN = 1, FUN = mean)
data$BFI_C_R <- apply(data[,c("BFI13","BFI28_r","BFI43","BFI58_r")], MARGIN = 1, FUN = mean)

# Negative Emotionality
data$BFI04_r <- mapvalues(data$BFI04,c(1,2,3,4,5),c(5,4,3,2,1))
data$BFI09_r <- mapvalues(data$BFI09,c(1,2,3,4,5),c(5,4,3,2,1))
data$BFI24_r <- mapvalues(data$BFI24,c(1,2,3,4,5),c(5,4,3,2,1))
data$BFI29_r <- mapvalues(data$BFI29,c(1,2,3,4,5),c(5,4,3,2,1))
data$BFI44_r <- mapvalues(data$BFI44,c(1,2,3,4,5),c(5,4,3,2,1))
data$BFI49_r <- mapvalues(data$BFI49,c(1,2,3,4,5),c(5,4,3,2,1))
# Domain
data$BFI_N <- apply(data[,c("BFI04_r","BFI09_r","BFI14","BFI19","BFI24_r","BFI29_r","BFI34","BFI39","BFI44_r","BFI49_r","BFI54","BFI59")], MARGIN = 1, FUN = mean)
# Facets: Anxiety, Depression, Emotional Volatility
data$BFI_N_A <- apply(data[,c("BFI04_r","BFI19","BFI34","BFI49_r")], MARGIN = 1, FUN = mean)
data$BFI_N_D <- apply(data[,c("BFI09_r","BFI24_r","BFI39","BFI54")], MARGIN = 1, FUN = mean)
data$BFI_N_EV <- apply(data[,c("BFI14","BFI29_r","BFI44_r","BFI59")], MARGIN = 1, FUN = mean)

# Openness
data$BFI05_r <- mapvalues(data$BFI05,c(1,2,3,4,5),c(5,4,3,2,1))
data$BFI25_r <- mapvalues(data$BFI25,c(1,2,3,4,5),c(5,4,3,2,1))
data$BFI30_r <- mapvalues(data$BFI30,c(1,2,3,4,5),c(5,4,3,2,1))
data$BFI45_r <- mapvalues(data$BFI45,c(1,2,3,4,5),c(5,4,3,2,1))
data$BFI50_r <- mapvalues(data$BFI50,c(1,2,3,4,5),c(5,4,3,2,1))
data$BFI55_r <- mapvalues(data$BFI55,c(1,2,3,4,5),c(5,4,3,2,1))
# Domain
data$BFI_O <- apply(data[,c("BFI05_r","BFI10","BFI15","BFI20","BFI25_r","BFI30_r","BFI35","BFI40","BFI45_r","BFI50_r","BFI55_r","BFI60")], MARGIN = 1, FUN = mean)
# Facets: Intellectual Curiosity, Aesthtic Sensitivity, Creative Imagination
data$BFI_O_IC <- apply(data[,c("BFI10","BFI25_r","BFI40","BFI55_r")], MARGIN = 1, FUN = mean)
data$BFI_O_AS <- apply(data[,c("BFI05_r","BFI20","BFI35","BFI50_r")], MARGIN = 1, FUN = mean)
data$BFI_O_CI <- apply(data[,c("BFI15","BFI30_r","BFI45_r","BFI60")], MARGIN = 1, FUN = mean)

# Extraversion
psych::alpha(data[c("BFI01","BFI06","BFI11_r","BFI16_r","BFI21","BFI26_r","BFI31_r","BFI36_r","BFI41","BFI46","BFI51_r","BFI56")]) # a = .84
# Facets: Sociability, Assertiveness, Energy Level
psych::alpha(data[c("BFI01","BFI16_r","BFI31_r","BFI46")]) # a = .81
psych::alpha(data[c("BFI06","BFI21","BFI36_r","BFI51_r")]) # a = .73
psych::alpha(data[c("BFI11_r","BFI26_r","BFI41","BFI56")]) # a = .69

# Agreeableness
psych::alpha(data[c("BFI02","BFI07","BFI12_r","BFI17_r","BFI22_r","BFI27","BFI32","BFI37_r","BFI42_r","BFI47_r","BFI52","BFI57")]) # a = .74
# Facets: Compassion, Respectfulness, Trust
psych::alpha(data[c("BFI02","BFI17_r","BFI32","BFI47_r")]) # a = .63
psych::alpha(data[c("BFI07","BFI22_r","BFI37_r","BFI52")]) # a = .61
psych::alpha(data[c("BFI12_r","BFI27","BFI42_r","BFI57")]) # a = .56

# Conscientiousness
psych::alpha(data[c("BFI03_r","BFI08_r","BFI13","BFI18","BFI23_r","BFI28_r","BFI33","BFI38","BFI43","BFI48_r","BFI53","BFI58_r")]) # a = .86
# Facets: Organization, Productiveness, Responsibility
psych::alpha(data[c("BFI03_r","BFI18","BFI33","BFI48_r")]) # a = .89
psych::alpha(data[c("BFI08_r","BFI23_r","BFI38","BFI53")]) # a = .72
psych::alpha(data[c("BFI13","BFI28_r","BFI43","BFI58_r")]) # a = .60

# Negative Emotionality
psych::alpha(data[c("BFI04_r","BFI09_r","BFI14","BFI19","BFI24_r","BFI29_r","BFI34","BFI39","BFI44_r","BFI49_r","BFI54","BFI59")]) # a = .84
# Facets: Anxiety, Depression, Emotional Volatility
psych::alpha(data[c("BFI04_r","BFI19","BFI34","BFI49_r")]) # a = .63
psych::alpha(data[c("BFI09_r","BFI24_r","BFI39","BFI54")]) # a = .75
psych::alpha(data[c("BFI14","BFI29_r","BFI44_r","BFI59")]) # a = .78

# Openness
psych::alpha(data[c("BFI05_r","BFI10","BFI15","BFI20","BFI25_r","BFI30_r","BFI35","BFI40","BFI45_r","BFI50_r","BFI55_r","BFI60")]) # a = .83
# Facets: Intellectual Curiosity, Aesthtic Sensitivity, Creative Imagination
psych::alpha(data[c("BFI10","BFI25_r","BFI40","BFI55_r")]) # a = .64
psych::alpha(data[c("BFI05_r","BFI20","BFI35","BFI50_r")]) # a = .86
psych::alpha(data[c("BFI15","BFI30_r","BFI45_r","BFI60")]) # a = .78

### IAL: compute variables and reliabilities

# Unassuming-Ingenuous
data$IAL_JK <- apply(data[,c("IALJK1","IALJK2","IALJK3","IALJK4","IALJK5","IALJK6")], MARGIN = 1, FUN = mean)
# Arrogant-Calculating
data$IAL_BC <- apply(data[,c("IALBC1","IALBC2","IALBC3","IALBC4")], MARGIN = 1, FUN = mean)
# Aloof-Introverted
data$IAL_FG <- apply(data[,c("IALFG1","IALFG2","IALFG3","IALFG4")], MARGIN = 1, FUN = mean)
# Gregarious-Extraverted
data$IAL_NO <- apply(data[,c("IALNO1","IALNO2","IALNO3","IALNO4")], MARGIN = 1, FUN = mean)

# Unassuming-Ingenuous
psych::alpha(data[c("IALJK1","IALJK2","IALJK3","IALJK4","IALJK5","IALJK6")]) # a = .63
# Assured-Dominant
psych::alpha(data[c("IALPA1","IALPA2","IALPA3","IALPA4")]) # a = .71
# Arrogant-Calculating
psych::alpha(data[c("IALBC1","IALBC2","IALBC3","IALBC4")]) # a = .70
# Cold-Hearted
psych::alpha(data[c("IALDE1","IALDE2","IALDE3","IALDE4")]) # a = .75
# Aloof-Introverted
psych::alpha(data[c("IALFG1","IALFG2","IALFG3","IALFG4")]) # a = .68
# Unassured-Submissive
psych::alpha(data[c("IALHI1","IALHI2","IALHI3","IALHI4")]) # a = .72
# Warm-Agreeable
psych::alpha(data[c("IALLM1","IALLM2","IALLM3","IALLM4")]) # a = .76
# Gregarious-Extraverted
psych::alpha(data[c("IALNO1","IALNO2","IALNO3","IALNO4")]) # a = .80

### ISK: compute variables and reliabilities

# Social Orientation
data$ISK023_r<- mapvalues(data$ISK023,c(1,2,3,4),c(4,3,2,1))
data$ISK097_r<- mapvalues(data$ISK097,c(1,2,3,4),c(4,3,2,1))
data$ISK011_r<- mapvalues(data$ISK011,c(1,2,3,4),c(4,3,2,1))
data$ISK025_r<- mapvalues(data$ISK025,c(1,2,3,4),c(4,3,2,1))
data$ISK038_r<- mapvalues(data$ISK038,c(1,2,3,4),c(4,3,2,1))
data$ISK055_r<- mapvalues(data$ISK055,c(1,2,3,4),c(4,3,2,1))
data$ISK093_r<- mapvalues(data$ISK093,c(1,2,3,4),c(4,3,2,1))
data$ISK003_r<- mapvalues(data$ISK003,c(1,2,3,4),c(4,3,2,1))
data$ISK029_r<- mapvalues(data$ISK029,c(1,2,3,4),c(4,3,2,1))
data$ISK034_r<- mapvalues(data$ISK034,c(1,2,3,4),c(4,3,2,1))
data$ISK059_r<- mapvalues(data$ISK059,c(1,2,3,4),c(4,3,2,1))
data$ISK077_r<- mapvalues(data$ISK077,c(1,2,3,4),c(4,3,2,1))
data$ISK101_r<- mapvalues(data$ISK101,c(1,2,3,4),c(4,3,2,1))
# Domain
data$ISK_SO <- apply(data[,c("ISK001","ISK016_r","ISK033","ISK051_r","ISK068_r","ISK085","ISK104_r",
                             "ISK006","ISK023_r","ISK043","ISK063","ISK081","ISK097_r",
                             "ISK011_r","ISK025_r","ISK047","ISK065","ISK076","ISK089","ISK106",
                             "ISK012","ISK020","ISK038_r","ISK055_r","ISK072","ISK093_r",
                             "ISK003_r","ISK029_r","ISK034_r","ISK059_r","ISK077_r","ISK101_r")], MARGIN = 1, FUN = sum)
# Facets: Perspetive Change, Value Pluralism, Compromise Willingness, Listening
data$ISK_SO_PC <- apply(data[,c("ISK006","ISK023_r","ISK043","ISK063","ISK081","ISK097_r")], MARGIN = 1, FUN = sum)
data$ISK_SO_VP <- apply(data[,c("ISK011_r","ISK025_r","ISK047","ISK065","ISK076","ISK089","ISK106")], MARGIN = 1, FUN = sum)
data$ISK_SO_CW <- apply(data[,c("ISK012","ISK020","ISK038_r","ISK055_r","ISK072","ISK093_r")], MARGIN = 1, FUN = sum)
data$ISK_SO_L <- apply(data[,c("ISK003_r","ISK029_r","ISK034_r","ISK059_r","ISK077_r","ISK101_r")], MARGIN = 1, FUN = sum)

# Agency
data$ISK048_r<- mapvalues(data$ISK048,c(1,2,3,4),c(4,3,2,1))
data$ISK086_r<- mapvalues(data$ISK086,c(1,2,3,4),c(4,3,2,1))
data$ISK060_r<- mapvalues(data$ISK060,c(1,2,3,4),c(4,3,2,1))
data$ISK090_r<- mapvalues(data$ISK090,c(1,2,3,4),c(4,3,2,1))
data$ISK035_r<- mapvalues(data$ISK035,c(1,2,3,4),c(4,3,2,1))
data$ISK052_r<- mapvalues(data$ISK052,c(1,2,3,4),c(4,3,2,1))
data$ISK098_r<- mapvalues(data$ISK098,c(1,2,3,4),c(4,3,2,1))
# Domain
data$ISK_A <- apply(data[,c("ISK017","ISK021","ISK039_r","ISK056","ISK078_r","ISK094","ISK108",
                            "ISK002","ISK026","ISK048_r","ISK064","ISK086_r",
                            "ISK007","ISK030","ISK044","ISK060_r","ISK073","ISK090_r",
                            "ISK013","ISK035_r","ISK052_r","ISK069","ISK082","ISK098_r")], MARGIN = 1, FUN = sum)
# Facets: Conflict Willingsness, Extraversion, Decisionmaking
data$ISK_A_CW <- apply(data[,c("ISK002","ISK026","ISK048_r","ISK064","ISK086_r")], MARGIN = 1, FUN = sum)
data$ISK_A_E <- apply(data[,c("ISK007","ISK030","ISK044","ISK060_r","ISK073","ISK090_r")], MARGIN = 1, FUN = sum)
data$ISK_A_D <- apply(data[,c("ISK013","ISK035_r","ISK052_r","ISK069","ISK082","ISK098_r")], MARGIN = 1, FUN = sum)

# Self Regulation
data$ISK004_r<- mapvalues(data$ISK004,c(1,2,3,4),c(4,3,2,1))
data$ISK102_r<- mapvalues(data$ISK102,c(1,2,3,4),c(4,3,2,1))
data$ISK008_r<- mapvalues(data$ISK008,c(1,2,3,4),c(4,3,2,1))
data$ISK022_r<- mapvalues(data$ISK022,c(1,2,3,4),c(4,3,2,1))
data$ISK040_r<- mapvalues(data$ISK040,c(1,2,3,4),c(4,3,2,1))
data$ISK014_r<- mapvalues(data$ISK014,c(1,2,3,4),c(4,3,2,1))
data$ISK079_r<- mapvalues(data$ISK079,c(1,2,3,4),c(4,3,2,1))
data$ISK009_r<- mapvalues(data$ISK009,c(1,2,3,4),c(4,3,2,1))
data$ISK018_r<- mapvalues(data$ISK018,c(1,2,3,4),c(4,3,2,1))
data$ISK036_r<- mapvalues(data$ISK036,c(1,2,3,4),c(4,3,2,1))
data$ISK041_r<- mapvalues(data$ISK041,c(1,2,3,4),c(4,3,2,1))
data$ISK066_r<- mapvalues(data$ISK066,c(1,2,3,4),c(4,3,2,1))
data$ISK083_r<- mapvalues(data$ISK083,c(1,2,3,4),c(4,3,2,1))
data$ISK099_r<- mapvalues(data$ISK099,c(1,2,3,4),c(4,3,2,1))
# Domain
data$ISK_SR <- apply(data[,c("ISK004_r","ISK031","ISK045","ISK070","ISK087","ISK102_r",
                             "ISK008_r","ISK022_r","ISK040_r","ISK057","ISK074","ISK091",
                             "ISK014_r","ISK027","ISK049","ISK061","ISK079_r","ISK095",
                             "ISK009_r","ISK018_r","ISK036_r","ISK041_r","ISK053","ISK066_r","ISK083_r","ISK099_r","ISK107")], MARGIN = 1, FUN = sum)
# Facets: Self Control, Emotional Stability, Behavioral Flexibility, Internatility
data$ISK_SR_SC <- apply(data[,c("ISK004_r","ISK031","ISK045","ISK070","ISK087","ISK102_r")], MARGIN = 1, FUN = sum)
data$ISK_SR_ES <- apply(data[,c("ISK008_r","ISK022_r","ISK040_r","ISK057","ISK074","ISK091")], MARGIN = 1, FUN = sum)
data$ISK_SR_BF <- apply(data[,c("ISK014_r","ISK027","ISK049","ISK061","ISK079_r","ISK095")], MARGIN = 1, FUN = sum)
data$ISK_SR_I <- apply(data[,c("ISK009_r","ISK018_r","ISK036_r","ISK041_r","ISK053","ISK066_r","ISK083_r","ISK099_r","ISK107")], MARGIN = 1, FUN = sum)

# Social Orientation
psych::alpha(data[c("ISK001","ISK016_r","ISK033","ISK051_r","ISK068_r","ISK085","ISK104_r",
                    "ISK006","ISK023_r","ISK043","ISK063","ISK081","ISK097_r",
                    "ISK011_r","ISK025_r","ISK047","ISK065","ISK076","ISK089","ISK106",
                    "ISK012","ISK020","ISK038_r","ISK055_r","ISK072","ISK093_r",
                    "ISK003_r","ISK029_r","ISK034_r","ISK059_r","ISK077_r","ISK101_r")]) # a = .89
# Facets: Prosocial, Perspetive Change, Value Pluralism, Compromise Willingness, Listening
psych::alpha(data[c("ISK001","ISK016_r","ISK033","ISK051_r","ISK068_r","ISK085","ISK104_r")]) # a = .66
psych::alpha(data[c("ISK006","ISK023_r","ISK043","ISK063","ISK081","ISK097_r")]) # a = .79
psych::alpha(data[c("ISK011_r","ISK025_r","ISK047","ISK065","ISK076","ISK089","ISK106")]) # a = .67
psych::alpha(data[c("ISK012","ISK020","ISK038_r","ISK055_r","ISK072","ISK093_r")]) # a = .76
psych::alpha(data[c("ISK003_r","ISK029_r","ISK034_r","ISK059_r","ISK077_r","ISK101_r")]) # a = .80

# Agency
psych::alpha(data[c("ISK017","ISK021","ISK039_r","ISK056","ISK078_r","ISK094","ISK108",
                    "ISK002","ISK026","ISK048_r","ISK064","ISK086_r",
                    "ISK007","ISK030","ISK044","ISK060_r","ISK073","ISK090_r",
                    "ISK013","ISK035_r","ISK052_r","ISK069","ISK082","ISK098_r")]) # a = .87
# Facets: Assertiveness, Conflict Willingsness, Extraversion, Decisionmaking
psych::alpha(data[c("ISK017","ISK021","ISK039_r","ISK056","ISK078_r","ISK094","ISK108")]) # a = .76
psych::alpha(data[c("ISK002","ISK026","ISK048_r","ISK064","ISK086_r")]) # a = .70
psych::alpha(data[c("ISK007","ISK030","ISK044","ISK060_r","ISK073","ISK090_r")]) # a = .86
psych::alpha(data[c("ISK013","ISK035_r","ISK052_r","ISK069","ISK082","ISK098_r")]) # a = .81

# Self Regulation
psych::alpha(data[c("ISK004_r","ISK031","ISK045","ISK070","ISK087","ISK102_r",
                    "ISK008_r","ISK022_r","ISK040_r","ISK057","ISK074","ISK091",
                    "ISK014_r","ISK027","ISK049","ISK061","ISK079_r","ISK095",
                    "ISK009_r","ISK018_r","ISK036_r","ISK041_r","ISK053","ISK066_r","ISK083_r","ISK099_r","ISK107")]) # a = .87
# Facets: Self Control, Emotional Stability, Behavioral Flexibility, Internatility
psych::alpha(data[c("ISK004_r","ISK031","ISK045","ISK070","ISK087","ISK102_r")]) # a = .73
psych::alpha(data[c("ISK008_r","ISK022_r","ISK040_r","ISK057","ISK074","ISK091")]) # a = .78
psych::alpha(data[c("ISK014_r","ISK027","ISK049","ISK061","ISK079_r","ISK095")]) # a = .67
psych::alpha(data[c("ISK009_r","ISK018_r","ISK036_r","ISK041_r","ISK053","ISK066_r","ISK083_r","ISK099_r","ISK107")]) # a = .80

### NARQ: compute variables and reliabilities

# Admiration
data$NARQ_Adm <- apply(data[,c("NARQ02","NARQ04","NARQ05")], MARGIN = 1, FUN = mean)
psych::alpha(data[c("NARQ02","NARQ04","NARQ05")]) # a = .71
# Rivalry
data$NARQ_Riv <- apply(data[,c("NARQ01","NARQ03","NARQ06")], MARGIN = 1, FUN = mean)
psych::alpha(data[c("NARQ01","NARQ03","NARQ06")]) # a = .62

# All variables are included, ie also constructs and their facets that were not assumed to relate to the SJT scores
c_d_validity_all_var <-      data [c("SJT_a","SJT_c",
                                     "BFI_E","BFI_E_S","BFI_E_A","BFI_E_EL",
                                     "BFI_A","BFI_A_C","BFI_A_R","BFI_A_T",
                                     "BFI_N","BFI_C","BFI_O",
                                     "IAL_PA","IAL_HI","IAL_LM","IAL_DE","IAL_JK","IAL_FG","IAL_BC","IAL_NO",
                                     "ISK_A","ISK_A_A","ISK_A_CW","ISK_A_E","ISK_A_D",
                                     "ISK_SO","ISK_SO_P","ISK_SO_PC","ISK_SO_VP","ISK_SO_CW","ISK_SO_L",
                                     "NARQ_Adm","NARQ_Riv")]


# correlation matrix
easysub <- function(x){
  sub("0.", ".", x)
}

# function to create correlation matrix of data frame
corcons <- function(data){
  
  #Compute correlation matrix
  require(Hmisc)
  data <- as.matrix(data)
  correlation_matrix <- rcorr(data, type="pearson")
  R <- correlation_matrix$r # Matrix of correlation coefficients
  
  # trunctuate the correlation matrix to two decimals and format nicely
  R <- format(round(cbind(rep(-1.11, ncol(data)), R), 2))[,-1]
  
  # add significance coding
  pvalues <- correlation_matrix$P
  
  for (i in 1:nrow(R)){
    for (j in 1:ncol(R)){
      if (i != j & pvalues[i,j] < 0.05){R[i,j] <- paste0(R[i,j],"*")}
    }
  }
  
  
  # define row names and column names
  rownames(R) <- colnames(data)
  colnames(R) <- colnames(data) # paste0(colnames(x), "")
  
  # remove upper triangle of correlation matrix
  R <- as.matrix(R)
  R[upper.tri(R, diag = T)] <- ""
  R <- R[-1,]
  R <- R[,-ncol(R)]
  
  R <- as.data.frame(R)
  R <- apply(R, 1:2, easysub)
  
  # print matrix
  R
}

#  correlation matrix
cortable_all <- corcons(c_d_validity_all_var)
write.table(cortable_all, file = "Correlations All Variables", sep = ";")


###########################################################################################
##### Step 3a: Data Preparation Pretest Data                                           ####
###########################################################################################

# merge both datasets into one dataset
pretest <- bind_rows(pretest_goe,pretest_mue)

# delete incomplete datasets
pretest <- subset(pretest, pretest$BFI49bis60_BFI60>0)

# delete unneccessary variables
pretest <- pretest %>% dplyr::select(-(id:startlanguage))

# rename BFI variables 
pretest <- pretest %>%
  rename_all(
    funs(str_replace_all(.,"BFI_",""))
  )

pretest <- pretest %>%
  rename_all(
    funs(str_replace_all(.,"BFI23bis48_",""))
  )

pretest <- pretest %>%
  rename_all(
    funs(str_replace_all(.,"BFI49bis60_",""))
  )

### SJT
# compute Agency, Communion, and Emotional Stability variable with items that were also used in the pilot
# SJT Items are already recoded according to construct level
pretest$SJT_a <- apply(pretest[,c("SJTkb00191","SJTkb00210","SJTkb00196","SJTkb00174","SJTkb00168",
                                  "SJTkb00163","SJTkb00207","SJTkb00165","SJTkb00159","SJTkb00161", 
                                  "SJTkb00208","SJTkb00212","SJTkb00204","SJTkb00203","SJTkb00031")],
                       MARGIN = 1, FUN = mean, na.rm=T)
pretest$SJT_c <- apply(pretest[,c("SJTkb00058","SJTkb00055","SJTkb00028","SJTkb00039","SJTkb00060", 
                                  "SJTkb00010","SJTkb00038","SJTkb00035","SJTkb00015","SJTkb00026", 
                                  "SJTkb00027","SJTkb00007","SJTkb00053","SJTkb00072","SJTkb00054")],
                              MARGIN = 1, FUN =mean)
pretest$SJT_es <- apply(pretest[,c("SJTkb00127","SJTkb00096","SJTkb00104","SJTkb00143","SJTkb00134",
                                   "SJTkb00094","SJTkb00108","SJTkb00136","SJTkb00117","SJTkb00116", 
                                    "SJTkb00197","SJTkb00103","SJTkb00175","SJTkb00202","SJTkb00113")],
                        MARGIN = 1, FUN = mean, na.rm=T)


### BFI-2: 
# recode items and compute variables

# Extraversion
pretest$BFI11_r <- mapvalues(pretest$BFI11,c(1,2,3,4,5),c(5,4,3,2,1))
pretest$BFI16_r <- mapvalues(pretest$BFI16,c(1,2,3,4,5),c(5,4,3,2,1))
pretest$BFI26_r <- mapvalues(pretest$BFI26,c(1,2,3,4,5),c(5,4,3,2,1))
pretest$BFI31_r <- mapvalues(pretest$BFI31,c(1,2,3,4,5),c(5,4,3,2,1))
pretest$BFI36_r <- mapvalues(pretest$BFI36,c(1,2,3,4,5),c(5,4,3,2,1))
pretest$BFI51_r <- mapvalues(pretest$BFI51,c(1,2,3,4,5),c(5,4,3,2,1))
# Domain
pretest$BFI_E <- apply(pretest[,c("BFI01","BFI06","BFI11_r","BFI16_r","BFI21","BFI26_r","BFI31_r","BFI36_r","BFI41","BFI46","BFI51_r","BFI56")], MARGIN = 1, FUN = mean)
# Facets: Sociability, Assertiveness, Energy Level
pretest$BFI_E_S <- apply(pretest[,c("BFI01","BFI16_r","BFI31_r","BFI46")], MARGIN = 1, FUN = mean)
pretest$BFI_E_A <- apply(pretest[,c("BFI06","BFI21","BFI36_r","BFI51_r")], MARGIN = 1, FUN = mean)
pretest$BFI_E_EL <- apply(pretest[,c("BFI11_r","BFI26_r","BFI41","BFI56")], MARGIN = 1, FUN = mean)

# Agreeableness
pretest$BFI12_r <- mapvalues(pretest$BFI12,c(1,2,3,4,5),c(5,4,3,2,1))
pretest$BFI17_r <- mapvalues(pretest$BFI17,c(1,2,3,4,5),c(5,4,3,2,1))
pretest$BFI22_r <- mapvalues(pretest$BFI22,c(1,2,3,4,5),c(5,4,3,2,1))
pretest$BFI37_r <- mapvalues(pretest$BFI37,c(1,2,3,4,5),c(5,4,3,2,1))
pretest$BFI42_r <- mapvalues(pretest$BFI42,c(1,2,3,4,5),c(5,4,3,2,1))
pretest$BFI47_r <- mapvalues(pretest$BFI47,c(1,2,3,4,5),c(5,4,3,2,1))
# Domain
pretest$BFI_A <- apply(pretest[,c("BFI02","BFI07","BFI12_r","BFI17_r","BFI22_r","BFI27","BFI32","BFI37_r","BFI42_r","BFI47_r","BFI52","BFI57")], MARGIN = 1, FUN = mean)
# Facets: Compassion, Respectfulness, Trust
pretest$BFI_A_C <- apply(pretest[,c("BFI02","BFI17_r","BFI32","BFI47_r")], MARGIN = 1, FUN = mean)
pretest$BFI_A_R <- apply(pretest[,c("BFI07","BFI22_r","BFI37_r","BFI52")], MARGIN = 1, FUN = mean)
pretest$BFI_A_T <- apply(pretest[,c("BFI12_r","BFI27","BFI42_r","BFI57")], MARGIN = 1, FUN = mean)

# Conscientiousness
pretest$BFI03_r <- mapvalues(pretest$BFI03,c(1,2,3,4,5),c(5,4,3,2,1))
pretest$BFI08_r <- mapvalues(pretest$BFI08,c(1,2,3,4,5),c(5,4,3,2,1))
pretest$BFI23_r <- mapvalues(pretest$BFI23,c(1,2,3,4,5),c(5,4,3,2,1))
pretest$BFI28_r <- mapvalues(pretest$BFI28,c(1,2,3,4,5),c(5,4,3,2,1))
pretest$BFI48_r <- mapvalues(pretest$BFI48,c(1,2,3,4,5),c(5,4,3,2,1))
pretest$BFI58_r <- mapvalues(pretest$BFI58,c(1,2,3,4,5),c(5,4,3,2,1))
# Domain
pretest$BFI_C <- apply(pretest[,c("BFI03_r","BFI08_r","BFI13","BFI18","BFI23_r","BFI28_r","BFI33","BFI38","BFI43","BFI48_r","BFI53","BFI58_r")], MARGIN = 1, FUN = mean)
# Facets: Organization, Productiveness, Responsibility
pretest$BFI_C_O <- apply(pretest[,c("BFI03_r","BFI18","BFI33","BFI48_r")], MARGIN = 1, FUN = mean)
pretest$BFI_C_P <- apply(pretest[,c("BFI08_r","BFI23_r","BFI38","BFI53")], MARGIN = 1, FUN = mean)
pretest$BFI_C_R <- apply(pretest[,c("BFI13","BFI28_r","BFI43","BFI58_r")], MARGIN = 1, FUN = mean)

# Negative Emotionality
pretest$BFI04_r <- mapvalues(pretest$BFI04,c(1,2,3,4,5),c(5,4,3,2,1))
pretest$BFI09_r <- mapvalues(pretest$BFI09,c(1,2,3,4,5),c(5,4,3,2,1))
pretest$BFI24_r <- mapvalues(pretest$BFI24,c(1,2,3,4,5),c(5,4,3,2,1))
pretest$BFI29_r <- mapvalues(pretest$BFI29,c(1,2,3,4,5),c(5,4,3,2,1))
pretest$BFI44_r <- mapvalues(pretest$BFI44,c(1,2,3,4,5),c(5,4,3,2,1))
pretest$BFI49_r <- mapvalues(pretest$BFI49,c(1,2,3,4,5),c(5,4,3,2,1))
# Domain
pretest$BFI_N <- apply(pretest[,c("BFI04_r","BFI09_r","BFI14","BFI19","BFI24_r","BFI29_r","BFI34","BFI39","BFI44_r","BFI49_r","BFI54","BFI59")], MARGIN = 1, FUN = mean)
# Facets: Anxiety, Depression, Emotional Volatility
pretest$BFI_N_A <- apply(pretest[,c("BFI04_r","BFI19","BFI34","BFI49_r")], MARGIN = 1, FUN = mean)
pretest$BFI_N_D <- apply(pretest[,c("BFI09_r","BFI24_r","BFI39","BFI54")], MARGIN = 1, FUN = mean)
pretest$BFI_N_EV <- apply(pretest[,c("BFI14","BFI29_r","BFI44_r","BFI59")], MARGIN = 1, FUN = mean)

# Openness
pretest$BFI05_r <- mapvalues(pretest$BFI05,c(1,2,3,4,5),c(5,4,3,2,1))
pretest$BFI25_r <- mapvalues(pretest$BFI25,c(1,2,3,4,5),c(5,4,3,2,1))
pretest$BFI30_r <- mapvalues(pretest$BFI30,c(1,2,3,4,5),c(5,4,3,2,1))
pretest$BFI45_r <- mapvalues(pretest$BFI45,c(1,2,3,4,5),c(5,4,3,2,1))
pretest$BFI50_r <- mapvalues(pretest$BFI50,c(1,2,3,4,5),c(5,4,3,2,1))
pretest$BFI55_r <- mapvalues(pretest$BFI55,c(1,2,3,4,5),c(5,4,3,2,1))
# Domain
pretest$BFI_O <- apply(pretest[,c("BFI05_r","BFI10","BFI15","BFI20","BFI25_r","BFI30_r","BFI35","BFI40","BFI45_r","BFI50_r","BFI55_r","BFI60")], MARGIN = 1, FUN = mean)
# Facets: Intellectual Curiosity, Aesthtic Sensitivity, Creative Imagination
pretest$BFI_O_IC <- apply(pretest[,c("BFI10","BFI25_r","BFI40","BFI55_r")], MARGIN = 1, FUN = mean)
pretest$BFI_O_AS <- apply(pretest[,c("BFI05_r","BFI20","BFI35","BFI50_r")], MARGIN = 1, FUN = mean)
pretest$BFI_O_CI <- apply(pretest[,c("BFI15","BFI30_r","BFI45_r","BFI60")], MARGIN = 1, FUN = mean)

###########################################################################################
##### Step 3b: Descriptive Statistics Pretest                                          ####
###########################################################################################

# sample
# N=55
psych::describe(pretest$age) #one outlier with age=99: This was a test account
pretest <- subset(pretest, pretest$age<99)
# N = 54
table(pretest$sex)
psych::describe(pretest$age) #18-48, M=23.02, SD=5.66
table(pretest$Standort) # Göttingen=29, Münster=26

c_d_validity_pretest <- pretest[c("SJT_a","SJT_c","SJT_es",
                                   "BFI_E","BFI_E_A",
                                   "BFI_A","BFI_A_C",
                                   "BFI_N","BFI_N_A",    
                                   "BFI_C","BFI_O")]

# Descriptive Statistics of all variables
descriptives_v_pre <- psych::describe(c_d_validity_pretest) 
descriptives_variables_pre <- print(descriptives_v_pre, digits = 2)
write.table(descriptives_variables_pre,file="Descriptives_Variables_Pretest",sep = ";")

# Reliability

### SJT
# Computating of Omega H per dimension
omega(pretest[c("SJTkb00191","SJTkb00210","SJTkb00196","SJTkb00174","SJTkb00168",
                "SJTkb00163","SJTkb00207","SJTkb00165","SJTkb00159","SJTkb00161", 
                "SJTkb00208","SJTkb00212","SJTkb00204","SJTkb00203","SJTkb00031")], poly = T) #A: a=.78, o_h=.39, o_t=.84
omega(pretest[c("SJTkb00058","SJTkb00055","SJTkb00028","SJTkb00039","SJTkb00060", 
                  "SJTkb00010","SJTkb00038","SJTkb00035","SJTkb00015","SJTkb00026", 
                  "SJTkb00027","SJTkb00007","SJTkb00053","SJTkb00072","SJTkb00054")], poly = T) #C: a=.82, o_h=.33, o_t=.87
omega(pretest[c("SJTkb00127","SJTkb00096","SJTkb00104","SJTkb00143","SJTkb00134",
                "SJTkb00094","SJTkb00108","SJTkb00136","SJTkb00117","SJTkb00116", 
                "SJTkb00197","SJTkb00103","SJTkb00175","SJTkb00202","SJTkb00113")], poly = T) #ES: a=.78, o_h=.39, o_t=.84

### BFI-2
# Extraversion
psych::alpha(pretest[c("BFI01","BFI06","BFI11_r","BFI16_r","BFI21","BFI26_r","BFI31_r","BFI36_r","BFI41","BFI46","BFI51_r","BFI56")]) # a = .88
# Facets: Sociability, Assertiveness, Energy Level
psych::alpha(pretest[c("BFI01","BFI16_r","BFI31_r","BFI46")]) # a = .81
psych::alpha(pretest[c("BFI06","BFI21","BFI36_r","BFI51_r")]) # a = .83
psych::alpha(pretest[c("BFI11_r","BFI26_r","BFI41","BFI56")]) # a = .80

# Agreeableness
psych::alpha(pretest[c("BFI02","BFI07","BFI12_r","BFI17_r","BFI22_r","BFI27","BFI32","BFI37_r","BFI42_r","BFI47_r","BFI52","BFI57")]) # a = .91
# Facets: Compassion, Respectfulness, Trust
psych::alpha(pretest[c("BFI02","BFI17_r","BFI32","BFI47_r")]) # a = .89
psych::alpha(pretest[c("BFI07","BFI22_r","BFI37_r","BFI52")]) # a = .83
psych::alpha(pretest[c("BFI12_r","BFI27","BFI42_r","BFI57")]) # a = .73

# Conscientiousness
psych::alpha(pretest[c("BFI03_r","BFI08_r","BFI13","BFI18","BFI23_r","BFI28_r","BFI33","BFI38","BFI43","BFI48_r","BFI53","BFI58_r")]) # a = .86
# Facets: Organization, Productiveness, Responsibility
psych::alpha(pretest[c("BFI03_r","BFI18","BFI33","BFI48_r")]) # a = .79
psych::alpha(pretest[c("BFI08_r","BFI23_r","BFI38","BFI53")]) # a = .80
psych::alpha(pretest[c("BFI13","BFI28_r","BFI43","BFI58_r")]) # a = .67

# Negative Emotionality
psych::alpha(pretest[c("BFI04_r","BFI09_r","BFI14","BFI19","BFI24_r","BFI29_r","BFI34","BFI39","BFI44_r","BFI49_r","BFI54","BFI59")]) # a = .93
# Facets: Anxiety, Depression, Emotional Volatility
psych::alpha(pretest[c("BFI04_r","BFI19","BFI34","BFI49_r")]) # a = .75
psych::alpha(pretest[c("BFI09_r","BFI24_r","BFI39","BFI54")]) # a = .88
psych::alpha(pretest[c("BFI14","BFI29_r","BFI44_r","BFI59")]) # a = .90

# Openness
psych::alpha(pretest[c("BFI05_r","BFI10","BFI15","BFI20","BFI25_r","BFI30_r","BFI35","BFI40","BFI45_r","BFI50_r","BFI55_r","BFI60")]) # a = .91
# Facets: Intellectual Curiosity, Aesthtic Sensitivity, Creative Imagination
psych::alpha(pretest[c("BFI10","BFI25_r","BFI40","BFI55_r")]) # a = .81
psych::alpha(pretest[c("BFI05_r","BFI20","BFI35","BFI50_r")]) # a = .90
psych::alpha(pretest[c("BFI15","BFI30_r","BFI45_r","BFI60")]) # a = .84


###########################################################################################
##### Step 3c: Convergent and Discriminant Validity  Pretest                           ####
###########################################################################################

# Uncorrected correlation matrix
cortable_pre <- corcons(c_d_validity_pretest)
write.table(cortable_pre, file = "Correlations Pretest", sep = ";")

###########################################################################################
##### Step 4a: Data Preparation Pilot Data                                             ####
###########################################################################################


# select participants with construct-driven SJT data
pilot <- subset(pilotexport, pilotexport$NRec3 > 0)

# delete unnecessary variables
pilot <- dplyr::select(pilot, -c(FID1:RegNr,FID2:SJTKLGesSR_sum,SJTKLGesSZ_sum:NRec3,
                                 FID4:Datum4,BFIExtra:BFIKreaEin,IALJK:IALNO,IALNC,ISKPS:NRec5))

# replace missings values (-9 and 99) with NA
pilot<- na_if(pilot,99)
pilot<- na_if(pilot,-9)

# missing values SJT
pilot_a <- pilot[,c("SJT_kb_00174","SJT_kb_00207","SJT_kb_00211",
                       "SJT_kb_00031","SJT_kb_00210","SJT_kb_00204",
                       "SJT_kb_00203","SJT_kb_00163","SJT_kb_00168",
                       "SJT_kb_00165","SJT_kb_00196","SJT_kb_00208",
                       "SJT_kb_00200","SJT_kb_00212","SJT_kb_00191")]
pilot$missings_a <- apply(pilot_a,1,function(x) sum(is.na(x)))
# subset of participants with less then 4 missings
pilot<- subset(pilot, pilot$missings_a < 4)

pilot_c <- pilot[,c("SJT_kb_00007","SJT_kb_00028","SJT_kb_00058", 
                    "SJT_kb_00054","SJT_kb_00072","SJT_kb_00026", 
                    "SJT_kb_00060","SJT_kb_00053","SJT_kb_00039", 
                    "SJT_kb_00027","SJT_kb_00015","SJT_kb_00038", 
                    "SJT_kb_00035","SJT_kb_00055","SJT_kb_00010")]
pilot$missings_c <- apply(pilot_w,1,function(x) sum(is.na(x)))
# subset of participants with less then 4 missings
pilot<- subset(pilot, pilot$missings_c < 4)

pilot_es <- pilot[,c("SJT_kb_00103","SJT_kb_00197","SJT_kb_00136",
                     "SJT_kb_00134","SJT_kb_00113","SJT_kb_00128",
                     "SJT_kb_00117","SJT_kb_00127","SJT_kb_00104",
                     "SJT_kb_00108","SJT_kb_00094","SJT_kb_00143",
                     "SJT_kb_00116","SJT_kb_00202","SJT_kb_00175")]
pilot$missings_es <- apply(pilot_es,1,function(x) sum(is.na(x)))
# subset of participants with less then 4 missings
pilot<- subset(pilot, pilot$missings_es < 4)

# compute variables with items that were also used in the pilot
# SJT Items are already recoded according to construct level
pilot$SJT_a <- apply(pilot[,c("SJT_kb_00174","SJT_kb_00207","SJT_kb_00211",
                              "SJT_kb_00031","SJT_kb_00210","SJT_kb_00204",
                              "SJT_kb_00203","SJT_kb_00163","SJT_kb_00168",
                              "SJT_kb_00165","SJT_kb_00196","SJT_kb_00208",
                              "SJT_kb_00200","SJT_kb_00212","SJT_kb_00191")],
                       MARGIN = 1, FUN = mean, na.rm=T)
pilot$SJT_c <- apply(pilot[,c("SJT_kb_00007","SJT_kb_00028","SJT_kb_00058", 
                              "SJT_kb_00054","SJT_kb_00072","SJT_kb_00026", 
                              "SJT_kb_00060","SJT_kb_00053","SJT_kb_00039", 
                              "SJT_kb_00027","SJT_kb_00015","SJT_kb_00038", 
                              "SJT_kb_00035","SJT_kb_00055","SJT_kb_00010")],
                       MARGIN = 1, FUN =mean, na.rm=T)
pilot$SJT_es <- apply(pilot[,c("SJT_kb_00103","SJT_kb_00197","SJT_kb_00136",
                                   "SJT_kb_00134","SJT_kb_00113","SJT_kb_00128",
                                   "SJT_kb_00117","SJT_kb_00127","SJT_kb_00104",
                                   "SJT_kb_00108","SJT_kb_00094","SJT_kb_00143",
                                   "SJT_kb_00116","SJT_kb_00202","SJT_kb_00175")],
                        MARGIN = 1, FUN = mean, na.rm=T)

### BFI-2: 

# Extraversion
pilot$BFI11_r <- mapvalues(pilot$BFI11,c(1,2,3,4,5),c(5,4,3,2,1))
pilot$BFI16_r <- mapvalues(pilot$BFI16,c(1,2,3,4,5),c(5,4,3,2,1))
pilot$BFI26_r <- mapvalues(pilot$BFI26,c(1,2,3,4,5),c(5,4,3,2,1))
pilot$BFI31_r <- mapvalues(pilot$BFI31,c(1,2,3,4,5),c(5,4,3,2,1))
pilot$BFI36_r <- mapvalues(pilot$BFI36,c(1,2,3,4,5),c(5,4,3,2,1))
pilot$BFI51_r <- mapvalues(pilot$BFI51,c(1,2,3,4,5),c(5,4,3,2,1))
# Domain
pilot$BFI_E <- apply(pilot[,c("BFI01","BFI06","BFI11_r","BFI16_r","BFI21","BFI26_r","BFI31_r","BFI36_r","BFI41","BFI46","BFI51_r","BFI56")], MARGIN = 1, FUN = mean, na.rm=T)
# Facets: Sociability, Assertiveness, Energy Level
pilot$BFI_E_S <- apply(pilot[,c("BFI01","BFI16_r","BFI31_r","BFI46")], MARGIN = 1, FUN = mean, na.rm=T)
pilot$BFI_E_A <- apply(pilot[,c("BFI06","BFI21","BFI36_r","BFI51_r")], MARGIN = 1, FUN = mean, na.rm=T)
pilot$BFI_E_EL <- apply(pilot[,c("BFI11_r","BFI26_r","BFI41","BFI56")], MARGIN = 1, FUN = mean, na.rm=T)

# Agreeableness
pilot$BFI12_r <- mapvalues(pilot$BFI12,c(1,2,3,4,5),c(5,4,3,2,1))
pilot$BFI17_r <- mapvalues(pilot$BFI17,c(1,2,3,4,5),c(5,4,3,2,1))
pilot$BFI22_r <- mapvalues(pilot$BFI22,c(1,2,3,4,5),c(5,4,3,2,1))
pilot$BFI37_r <- mapvalues(pilot$BFI37,c(1,2,3,4,5),c(5,4,3,2,1))
pilot$BFI42_r <- mapvalues(pilot$BFI42,c(1,2,3,4,5),c(5,4,3,2,1))
pilot$BFI47_r <- mapvalues(pilot$BFI47,c(1,2,3,4,5),c(5,4,3,2,1))
# Domain
pilot$BFI_A <- apply(pilot[,c("BFI02","BFI07","BFI12_r","BFI17_r","BFI22_r","BFI27","BFI32","BFI37_r","BFI42_r","BFI47_r","BFI52","BFI57")], MARGIN = 1, FUN = mean, na.rm=T)
# Facets: Compassion, Respectfulness, Trust
pilot$BFI_A_C <- apply(pilot[,c("BFI02","BFI17_r","BFI32","BFI47_r")], MARGIN = 1, FUN = mean, na.rm=T)
pilot$BFI_A_R <- apply(pilot[,c("BFI07","BFI22_r","BFI37_r","BFI52")], MARGIN = 1, FUN = mean, na.rm=T)
pilot$BFI_A_T <- apply(pilot[,c("BFI12_r","BFI27","BFI42_r","BFI57")], MARGIN = 1, FUN = mean, na.rm=T)

# Conscientiousness
pilot$BFI03_r <- mapvalues(pilot$BFI03,c(1,2,3,4,5),c(5,4,3,2,1))
pilot$BFI08_r <- mapvalues(pilot$BFI08,c(1,2,3,4,5),c(5,4,3,2,1))
pilot$BFI23_r <- mapvalues(pilot$BFI23,c(1,2,3,4,5),c(5,4,3,2,1))
pilot$BFI28_r <- mapvalues(pilot$BFI28,c(1,2,3,4,5),c(5,4,3,2,1))
pilot$BFI48_r <- mapvalues(pilot$BFI48,c(1,2,3,4,5),c(5,4,3,2,1))
pilot$BFI58_r <- mapvalues(pilot$BFI58,c(1,2,3,4,5),c(5,4,3,2,1))
# Domain
pilot$BFI_C <- apply(pilot[,c("BFI03_r","BFI08_r","BFI13","BFI18","BFI23_r","BFI28_r","BFI33","BFI38","BFI43","BFI48_r","BFI53","BFI58_r")], MARGIN = 1, FUN = mean, na.rm=T)
# Facets: Organization, Productiveness, Responsibility
pilot$BFI_C_O <- apply(pilot[,c("BFI03_r","BFI18","BFI33","BFI48_r")], MARGIN = 1, FUN = mean, na.rm=T)
pilot$BFI_C_P <- apply(pilot[,c("BFI08_r","BFI23_r","BFI38","BFI53")], MARGIN = 1, FUN = mean, na.rm=T)
pilot$BFI_C_R <- apply(pilot[,c("BFI13","BFI28_r","BFI43","BFI58_r")], MARGIN = 1, FUN = mean, na.rm=T)

# Negative Emotionality
pilot$BFI04_r <- mapvalues(pilot$BFI04,c(1,2,3,4,5),c(5,4,3,2,1))
pilot$BFI09_r <- mapvalues(pilot$BFI09,c(1,2,3,4,5),c(5,4,3,2,1))
pilot$BFI24_r <- mapvalues(pilot$BFI24,c(1,2,3,4,5),c(5,4,3,2,1))
pilot$BFI29_r <- mapvalues(pilot$BFI29,c(1,2,3,4,5),c(5,4,3,2,1))
pilot$BFI44_r <- mapvalues(pilot$BFI44,c(1,2,3,4,5),c(5,4,3,2,1))
pilot$BFI49_r <- mapvalues(pilot$BFI49,c(1,2,3,4,5),c(5,4,3,2,1))
# Domain
pilot$BFI_N <- apply(pilot[,c("BFI04_r","BFI09_r","BFI14","BFI19","BFI24_r","BFI29_r","BFI34","BFI39","BFI44_r","BFI49_r","BFI54","BFI59")], MARGIN = 1, FUN = mean, na.rm=T)
# Facets: Anxiety, Depression, Emotional Volatility
pilot$BFI_N_A <- apply(pilot[,c("BFI04_r","BFI19","BFI34","BFI49_r")], MARGIN = 1, FUN = mean, na.rm=T)
pilot$BFI_N_D <- apply(pilot[,c("BFI09_r","BFI24_r","BFI39","BFI54")], MARGIN = 1, FUN = mean, na.rm=T)
pilot$BFI_N_EV <- apply(pilot[,c("BFI14","BFI29_r","BFI44_r","BFI59")], MARGIN = 1, FUN = mean, na.rm=T)

# Openness
pilot$BFI05_r <- mapvalues(pilot$BFI05,c(1,2,3,4,5),c(5,4,3,2,1))
pilot$BFI25_r <- mapvalues(pilot$BFI25,c(1,2,3,4,5),c(5,4,3,2,1))
pilot$BFI30_r <- mapvalues(pilot$BFI30,c(1,2,3,4,5),c(5,4,3,2,1))
pilot$BFI45_r <- mapvalues(pilot$BFI45,c(1,2,3,4,5),c(5,4,3,2,1))
pilot$BFI50_r <- mapvalues(pilot$BFI50,c(1,2,3,4,5),c(5,4,3,2,1))
pilot$BFI55_r <- mapvalues(pilot$BFI55,c(1,2,3,4,5),c(5,4,3,2,1))
# Domain
pilot$BFI_O <- apply(pilot[,c("BFI05_r","BFI10","BFI15","BFI20","BFI25_r","BFI30_r","BFI35","BFI40","BFI45_r","BFI50_r","BFI55_r","BFI60")], MARGIN = 1, FUN = mean, na.rm=T)
# Facets: Intellectual Curiosity, Aesthtic Sensitivity, Creative Imagination
pilot$BFI_O_IC <- apply(pilot[,c("BFI10","BFI25_r","BFI40","BFI55_r")], MARGIN = 1, FUN = mean, na.rm=T)
pilot$BFI_O_AS <- apply(pilot[,c("BFI05_r","BFI20","BFI35","BFI50_r")], MARGIN = 1, FUN = mean, na.rm=T)
pilot$BFI_O_CI <- apply(pilot[,c("BFI15","BFI30_r","BFI45_r","BFI60")], MARGIN = 1, FUN = mean, na.rm=T)


### IAL:
# recode and compute variables

# Unassuming-Ingenuous
pilot$IAL_JK <- apply(pilot[,c("IALJK1","IALJK2","IALJK3","IALJK4","IALJK5","IALJK6")], MARGIN = 1, FUN = mean, na.rm=T)
# Assured-Dominant
pilot$IAL_PA <- apply(pilot[,c("IALPA1","IALPA2","IALPA3","IALPA4")], MARGIN = 1, FUN = mean, na.rm=T)
# Arrogant-Calculating
pilot$IAL_BC <- apply(pilot[,c("IALBC1","IALBC2","IALBC3","IALBC4")], MARGIN = 1, FUN = mean, na.rm=T)
# Cold-Hearted
pilot$IAL_DE <- apply(pilot[,c("IALDE1","IALDE2","IALDE3","IALDE4")], MARGIN = 1, FUN = mean, na.rm=T)
# Aloof-Introverted
pilot$IAL_FG <- apply(pilot[,c("IALFG1","IALFG2","IALFG3","IALFG4")], MARGIN = 1, FUN = mean, na.rm=T)
# Unassured-Submissive
pilot$IAL_HI <- apply(pilot[,c("IALHI1","IALHI2","IALHI3","IALHI4")], MARGIN = 1, FUN = mean, na.rm=T)
# Warm-Agreeable
pilot$IAL_LM <- apply(pilot[,c("IALLM1","IALLM2","IALLM3","IALLM4")], MARGIN = 1, FUN = mean, na.rm=T)
# Gregarious-Extraverted
pilot$IAL_NO <- apply(pilot[,c("IALNO1","IALNO2","IALNO3","IALNO4")], MARGIN = 1, FUN = mean, na.rm=T)
# Neuroticism
pilot$IALNC2 <- mapvalues(pilot$IALNC2r,c(1,2,3,4,5,6,7,8),c(8,7,6,5,4,3,2,1))
pilot$IALNC5 <- mapvalues(pilot$IALNC5r,c(1,2,3,4,5,6,7,8),c(8,7,6,5,4,3,2,1))
pilot$IAL_NC <- apply(pilot[,c("IALNC1","IALNC2","IALNC3","IALNC4","IALNC5")], MARGIN = 1, FUN = mean, na.rm=T)


### ISK:

# Social Orientation
# Domain
pilot$ISK_SO <- apply(pilot[,c("ISK001","ISK016","ISK033","ISK051","ISK068","ISK085","ISK104",
                                 "ISK006","ISK023","ISK043","ISK063","ISK081","ISK097",
                                 "ISK011","ISK025","ISK047","ISK065","ISK076","ISK089","ISK106",
                                 "ISK012","ISK020","ISK038","ISK055","ISK072","ISK093",
                                 "ISK003","ISK029","ISK034","ISK059","ISK077","ISK101")], MARGIN = 1, FUN = sum)
# Facets: Prosocial, Perspetive Change, Value Pluralism, Compromise Willingness, Listening
pilot$ISK_SO_P <- apply(pilot[,c("ISK001","ISK016","ISK033","ISK051","ISK068","ISK085","ISK104")], MARGIN = 1, FUN = sum)
pilot$ISK_SO_PC <- apply(pilot[,c("ISK006","ISK023","ISK043","ISK063","ISK081","ISK097")], MARGIN = 1, FUN = sum)
pilot$ISK_SO_VP <- apply(pilot[,c("ISK011","ISK025","ISK047","ISK065","ISK076","ISK089","ISK106")], MARGIN = 1, FUN = sum)
pilot$ISK_SO_CW <- apply(pilot[,c("ISK012","ISK020","ISK038","ISK055","ISK072","ISK093")], MARGIN = 1, FUN = sum)
pilot$ISK_SO_L <- apply(pilot[,c("ISK003","ISK029","ISK034","ISK059","ISK077","ISK101")], MARGIN = 1, FUN = sum)

# Agency
# Domain
pilot$ISK_A <- apply(pilot[,c("ISK017","ISK021","ISK039","ISK056","ISK078","ISK094","ISK108",
                                "ISK002","ISK026","ISK048","ISK064","ISK086",
                                "ISK007","ISK030","ISK044","ISK060","ISK073","ISK090",
                                "ISK013","ISK035","ISK052","ISK069","ISK082","ISK098")], MARGIN = 1, FUN = sum)
# Facets: Assertiveness, Conflict Willingsness, Extraversion, Decisionmaking
pilot$ISK_A_A <- apply(pilot[,c("ISK017","ISK021","ISK039","ISK056","ISK078","ISK094","ISK108")], MARGIN = 1, FUN = sum)
pilot$ISK_A_CW <- apply(pilot[,c("ISK002","ISK026","ISK048","ISK064","ISK086")], MARGIN = 1, FUN = sum)
pilot$ISK_A_E <- apply(pilot[,c("ISK007","ISK030","ISK044","ISK060","ISK073","ISK090")], MARGIN = 1, FUN = sum)
pilot$ISK_A_D <- apply(pilot[,c("ISK013","ISK035","ISK052","ISK069","ISK082","ISK098")], MARGIN = 1, FUN = sum)

# Self Regulation
# Domain
pilot$ISK_SR <- apply(pilot[,c("ISK004","ISK031","ISK045","ISK070","ISK087","ISK102",
                                 "ISK008","ISK022","ISK040","ISK057","ISK074","ISK091",
                                 "ISK014","ISK027","ISK049","ISK061","ISK079","ISK095",
                                 "ISK009","ISK018","ISK036","ISK041","ISK053","ISK066","ISK083","ISK099","ISK107")], MARGIN = 1, FUN = sum)
# Facets: Self Control, Emotional Stability, Behavioral Flexibility, Internatility
pilot$ISK_SR_SC <- apply(pilot[,c("ISK004","ISK031","ISK045","ISK070","ISK087","ISK102")], MARGIN = 1, FUN = sum)
pilot$ISK_SR_ES <- apply(pilot[,c("ISK008","ISK022","ISK040","ISK057","ISK074","ISK091")], MARGIN = 1, FUN = sum)
pilot$ISK_SR_BF <- apply(pilot[,c("ISK014","ISK027","ISK049","ISK061","ISK079","ISK095")], MARGIN = 1, FUN = sum)
pilot$ISK_SR_I <- apply(pilot[,c("ISK009","ISK018","ISK036","ISK041","ISK053","ISK066","ISK083","ISK099","ISK107")], MARGIN = 1, FUN = sum)

###########################################################################################
##### Step 4b: Descriptive Statistics Pilot                                            ####
###########################################################################################

# sample
# N=836 (SJT data)
pilot$sex <- pilot$X01_01 # 1=female, 2=male, -9/99=NA
table(pilot$sex) #511(61%) female, 254(30%) male, 71(8%) no answer
pilot$year <- pilot$X02_01
pilot$age <- 2020-pilot$year
describe(pilot$age) #769 answers (92%), M=21.27, SD=2.00, range: 18-36

# traditional SJT is coded such as a high value means higher deviation from the expert panel
# linear recoding such as high value means less deviation (i.e. a better performance)
# highest possible value is 4 and we subtract the momentary value from 4
summary(pilot$SJTKLGesSZ_mean)
pilot$tradSJT <- 4-pilot$SJTKLGesSZ_mean
summary(pilot$tradSJT)

c_d_validity_pilot <- pilot [c("SJT_a","SJT_c","SJT_es",
                               "tradSJT","NatPunkte",
                               "BFI_E","BFI_E_A",
                               "BFI_A","BFI_A_C",
                               "BFI_N","BFI_N_A",    
                               "IAL_PA","IAL_HI","IAL_LM","IAL_DE","IAL_NC",
                               "ISK_A","ISK_A_A","ISK_SO","ISK_SO_P","ISK_SR","ISK_SR_ES")]

# Descriptive Statistics of all variables
descriptives_v_pilot <- psych::describe(c_d_validity_pilot) 
descriptives_variables_pilot <- print(descriptives_v_pilot, digits = 2)
write.table(descriptives_variables_pilot,file="Descriptives_Variables_Pilot",sep = ";")

# Reliability

### SJT
# Computating of Alpha and Omega per dimension
omega(pilot[c("SJT_kb_00174","SJT_kb_00207","SJT_kb_00211",
              "SJT_kb_00031","SJT_kb_00210","SJT_kb_00204",
              "SJT_kb_00203","SJT_kb_00163","SJT_kb_00168",
              "SJT_kb_00165","SJT_kb_00196","SJT_kb_00208",
              "SJT_kb_00200","SJT_kb_00212","SJT_kb_00191")], poly = T) #A: a=.59, o_h=.33, o_t=.64

omega(pilot[c("SJT_kb_00007","SJT_kb_00028","SJT_kb_00058", 
              "SJT_kb_00054","SJT_kb_00072","SJT_kb_00026", 
              "SJT_kb_00060","SJT_kb_00053","SJT_kb_00039", 
              "SJT_kb_00027","SJT_kb_00015","SJT_kb_00038", 
              "SJT_kb_00035","SJT_kb_00055","SJT_kb_00010")], poly = T) #C: a=.74, o_h=.64, o_t=.77

omega(pilot[c("SJT_kb_00103","SJT_kb_00197","SJT_kb_00136",
              "SJT_kb_00134","SJT_kb_00113","SJT_kb_00128",
              "SJT_kb_00117","SJT_kb_00127","SJT_kb_00104",
              "SJT_kb_00108","SJT_kb_00094","SJT_kb_00143",
              "SJT_kb_00116","SJT_kb_00202","SJT_kb_00175")], poly = T) #ES: a=.50, o_h=.54, o_t=.58

### BFI-2
# Extraversion
psych::alpha(pilot[c("BFI01","BFI06","BFI11_r","BFI16_r","BFI21","BFI26_r","BFI31_r","BFI36_r","BFI41","BFI46","BFI51_r","BFI56")]) # a = .84
# Facet: Assertiveness
psych::alpha(pilot[c("BFI06","BFI21","BFI36_r","BFI51_r")]) # a = .78

# Agreeableness
psych::alpha(pilot[c("BFI02","BFI07","BFI12_r","BFI17_r","BFI22_r","BFI27","BFI32","BFI37_r","BFI42_r","BFI47_r","BFI52","BFI57")]) # a = .78
# Facets: Compassion, Respectfulness, Trust
psych::alpha(pilot[c("BFI02","BFI17_r","BFI32","BFI47_r")]) # a = .69

# Negative Emotionality
psych::alpha(pilot[c("BFI04_r","BFI09_r","BFI14","BFI19","BFI24_r","BFI29_r","BFI34","BFI39","BFI44_r","BFI49_r","BFI54","BFI59")]) # a = .86
# Facets: Anxiety, Depression, Emotional Volatility
psych::alpha(pilot[c("BFI04_r","BFI19","BFI34","BFI49_r")]) # a = .64

### IAL
# Assured-Dominant
psych::alpha(pilot[c("IALPA1","IALPA2","IALPA3","IALPA4")]) # a = .70
# Cold-Hearted
psych::alpha(pilot[c("IALDE1","IALDE2","IALDE3","IALDE4")]) # a = .72
# Unassured-Submissive
psych::alpha(pilot[c("IALHI1","IALHI2","IALHI3","IALHI4")]) # a = .75
# Warm-Agreeable
psych::alpha(pilot[c("IALLM1","IALLM2","IALLM3","IALLM4")]) # a = .69
# Neurotic
psych::alpha(pilot[c("IALNC1","IALNC2","IALNC3","IALNC4","IALNC5")]) # a = .58

### ISK
# Social Orientation
psych::alpha(pilot[c("ISK001","ISK016","ISK033","ISK051","ISK068","ISK085","ISK104",
                     "ISK006","ISK023","ISK043","ISK063","ISK081","ISK097",
                     "ISK011","ISK025","ISK047","ISK065","ISK076","ISK089","ISK106",
                     "ISK012","ISK020","ISK038","ISK055","ISK072","ISK093",
                     "ISK003","ISK029","ISK034","ISK059","ISK077","ISK101")]) # a = .88
# Facet: Prosocial
psych::alpha(pilot[c("ISK001","ISK016","ISK033","ISK051","ISK068","ISK085","ISK104")]) # a = .67

# Agency
psych::alpha(pilot[c("ISK017","ISK021","ISK039","ISK056","ISK078","ISK094","ISK108",
                         "ISK002","ISK026","ISK048","ISK064","ISK086",
                         "ISK007","ISK030","ISK044","ISK060","ISK073","ISK090",
                         "ISK013","ISK035","ISK052","ISK069","ISK082","ISK098")]) # a = .86
# Facet: Assertiveness
psych::alpha(pilot[c("ISK017","ISK021","ISK039","ISK056","ISK078","ISK094","ISK108")]) # a = .76

# Self Regulation
psych::alpha(pilot[c("ISK004","ISK031","ISK045","ISK070","ISK087","ISK102",
                         "ISK008","ISK022","ISK040","ISK057","ISK074","ISK091",
                         "ISK014","ISK027","ISK049","ISK061","ISK079","ISK095",
                         "ISK009","ISK018","ISK036","ISK041","ISK053","ISK066","ISK083","ISK099","ISK107")]) # a = .83
# Facet: Emotional Stability
psych::alpha(pilot[c("ISK008","ISK022","ISK040","ISK057","ISK074","ISK091")]) # a = .71


###########################################################################################
##### Step 4c: Convergent and Discriminant Validity  Pilot                             ####
###########################################################################################

# Uncorrected correlation matrix
cortable_pilot <- corcons(c_d_validity_pilot)
write.table(cortable_pilot, file = "Correlations Pilot", sep = ";")

write.table(pilot, file = "pilotdata")
