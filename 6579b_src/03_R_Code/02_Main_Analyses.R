###############################################################################################
### Assessing Distinguishable Social Skills in Medical Admission -                          ###
### Does Construct-Driven Development Solve Validity Issues of Situational Judgment Tests?  ###
###############################################################################################

# This script contains the code to compute our main results.

# (c) 2021 Ina Mielke.

# This code is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
# 
# For a copy of the GNU General Public License, 
# see <http://www.gnu.org/licenses/>.

# load necessary packages
library(psych)
library(dplyr)
library(plyr)
library(tidyverse)
library(lavaan)
library(MBESS)
library(QuantPsyc)
library(ggplot2)

###########################################################################################
##### Step 1: Data Upload                                                              ####
###########################################################################################

# Set working directory

# Import dataset with prepared data
data <- read.csv("data", sep="")

###########################################################################################
##### Step 2a: Preparation Dataset                                                     ####
###########################################################################################

# delete raw items
data <- data %>% dplyr::select(-(SJTkb00218raw:SJTkb00247raw))

items_a <- c("SJTkb00233","SJTkb00234","SJTkb00235","SJTkb00236","SJTkb00237","SJTkb00238", 
             "SJTkb00239","SJTkb00240","SJTkb00241","SJTkb00242","SJTkb00243","SJTkb00244",
             "SJTkb00245","SJTkb00246","SJTkb00247")
items_c <- c("SJTkb00218","SJTkb00219","SJTkb00220","SJTkb00221","SJTkb00222","SJTkb00223", 
             "SJTkb00224","SJTkb00225","SJTkb00226","SJTkb00227","SJTkb00228","SJTkb00229",
             "SJTkb00230","SJTkb00231","SJTkb00232")

# missings values per construct and person
# in the preregistration we defined the at least 80% (=12 items) need to be completed
data$missings_a <- apply(data[c(items_a)],1,function(x) sum(is.na(x)))
excl_a <-subset(data, data$missings_a>3)
# one participant (FXST6519) has more than 3 missings and need to be excluded from further assertiveness analyses
# therefore, all agency items are set to NA
is.na(data$SJTkb00234) <- data$FID == "FXST6519"
is.na(data$SJTkb00235) <- data$FID == "FXST6519"
is.na(data$SJTkb00236) <- data$FID == "FXST6519"
is.na(data$SJTkb00237) <- data$FID == "FXST6519"
is.na(data$SJTkb00238) <- data$FID == "FXST6519"
is.na(data$SJTkb00239) <- data$FID == "FXST6519"
is.na(data$SJTkb00242) <- data$FID == "FXST6519"
is.na(data$SJTkb00243) <- data$FID == "FXST6519"
is.na(data$SJTkb00245) <- data$FID == "FXST6519"

data$missings_c <- apply(data[c(items_c)],1,function(x) sum(is.na(x)))
excl_c <-subset(data, data$missings_c>3)
# no participant with less than 80% completed communion items

# descriptive summary of sample
table(data$sex) # 1011/1527= 66% female, 481/1527=31% male, 2% NA/3
describe(data$age)
# number of participants in online survey (BFI items were mandatory)
online_survey <- subset(data, data$BFI01 > 0)

###########################################################################################
##### Step 2b: Data Preparation Traditional SJT                                        ####
###########################################################################################

# traditional SJT is coded such as a high value means higher deviation from the expert panel
# linear recoding such as high value means less deviation (i.e. a better performance)
# highest possible value is 4 and we subtract the momentary value from 4

summary(data$SJTKLGesSZ_mean)
data$tradSJT <- 4-data$SJTKLGesSZ_mean
summary(data$tradSJT)

###########################################################################################
##### Step 2c: Data Preparation Survey Data                                            ####
###########################################################################################

### BFI-2: included scales are sums and will coded as means

# Extraversion: Facet Assertiveness
data$BFI36_r <- mapvalues(data$BFI36,c(1,2,3,4,5),c(5,4,3,2,1))
data$BFI51_r <- mapvalues(data$BFI51,c(1,2,3,4,5),c(5,4,3,2,1))
data$BFI_E_A <- apply(data[,c("BFI06","BFI21","BFI36_r","BFI51_r")], MARGIN = 1, FUN = mean)

# Agreeableness: Facet Compassion
data$BFI17_r <- mapvalues(data$BFI17,c(1,2,3,4,5),c(5,4,3,2,1))
data$BFI47_r <- mapvalues(data$BFI47,c(1,2,3,4,5),c(5,4,3,2,1))
data$BFI_A_C <- apply(data[,c("BFI02","BFI17_r","BFI32","BFI47_r")], MARGIN = 1, FUN = mean)

### IAL: compute variables

# Assured-Dominant
data$IAL_PA <- apply(data[,c("IALPA1","IALPA2","IALPA3","IALPA4")], MARGIN = 1, FUN = mean)
# Unassured-Submissive
data$IAL_HI <- apply(data[,c("IALHI1","IALHI2","IALHI3","IALHI4")], MARGIN = 1, FUN = mean)
# Warm-Agreeable
data$IAL_LM <- apply(data[,c("IALLM1","IALLM2","IALLM3","IALLM4")], MARGIN = 1, FUN = mean)
# Cold-Hearted
data$IAL_DE <- apply(data[,c("IALDE1","IALDE2","IALDE3","IALDE4")], MARGIN = 1, FUN = mean)

### ISK: recode and compute variables

# Social Orientation: Facet Prosociality
data$ISK016_r<- mapvalues(data$ISK016,c(1,2,3,4),c(4,3,2,1))
data$ISK051_r<- mapvalues(data$ISK051,c(1,2,3,4),c(4,3,2,1))
data$ISK068_r<- mapvalues(data$ISK068,c(1,2,3,4),c(4,3,2,1))
data$ISK104_r<- mapvalues(data$ISK104,c(1,2,3,4),c(4,3,2,1))
data$ISK_SO_P <- apply(data[,c("ISK001","ISK016_r","ISK033","ISK051_r","ISK068_r","ISK085","ISK104_r")], MARGIN = 1, FUN = sum)

# Agency: Facet Assertiveness
data$ISK039_r<- mapvalues(data$ISK039,c(1,2,3,4),c(4,3,2,1))
data$ISK078_r<- mapvalues(data$ISK078,c(1,2,3,4),c(4,3,2,1))
data$ISK_A_A <- apply(data[,c("ISK017","ISK021","ISK039_r","ISK056","ISK078_r","ISK094","ISK108")], MARGIN = 1, FUN = sum)

### Personality Aggregates

# Agentic Personality
data$zBFI_E_A <- scale(data$BFI_E_A)
data$zIAL_PA <- scale(data$IAL_PA)
data$zIAL_HI <- scale(data$IAL_HI)
data$zIAL_HIr <- data$zIAL_HI*-1
data$zISK_A_A <- scale(data$ISK_A_A)
data$Pers_Ag <- apply(data[,c("zBFI_E_A", "zIAL_PA","zIAL_HIr","zISK_A_A")],
                      MARGIN = 1, FUN = mean)

# Communal Personality
data$zBFI_A_C <- scale(data$BFI_A_C)
data$zIAL_LM <- scale(data$IAL_LM)
data$zIAL_DE <- scale(data$IAL_DE)
data$zIAL_DEr <- data$zIAL_DE*-1
data$zISK_SO_P <- scale(data$ISK_SO_P)
data$Pers_Co <- apply(data[,c("zBFI_A_C", "zIAL_LM","zIAL_DEr","zISK_SO_P")],
                      MARGIN = 1, FUN = mean)
# Past Behavior: compute variables

### Agentic Behavior
data$Beha_Ag <- apply(data[,c("VerhaltenAG1","VerhaltenAG2","VerhaltenAG3","VerhaltenAG4")], MARGIN = 1, FUN = mean)
# Communal Behavior
data$Beha_Co <- apply(data[,c("VerhaltenCO1","VerhaltenCO2","VerhaltenCO3","VerhaltenCO4")], MARGIN = 1, FUN = mean)

###########################################################################################
##### Step 3: Reliabilities (without construct-driven SJT)                             ####
###########################################################################################

### traditional SJT 
# reliability was computed per day and version with the z-standardized, squared deviations
# which are not included in the present dataset but can be requested


# Day 1, Version A
# alpha: .59, omega_h: .17, omega_t: .62

# Day 1, Version B
# alpha: .56, omega_h: .58, omega_t: .59

# Day 1, Version C
# alpha: .63, omega_h: .22, omega_t: .66

# Day 1, Version D
# alpha: .72, omega_h: .62, omega_t: .76

# Day 2, Version A
# alpha: .89, omega_h: .85, omega_t: .90

# Day 2, Version B
# alpha: .58, omega_h: .23, omega_t: .61

# Day 2, Version C
# alpha: .79, omega_h: .32, omega_t: .81

# Day 2, Version D
# alpha: .59, omega_h: .56, omega_t: .62


trad_SJT_rel_SQD_Z <- c(.59,.56,.63,.72,.89,.58,.79,.59)
mean(trad_SJT_rel_SQD_Z) # alpha: .67

### Personality Aggregates

# Agentic Personality
psych::alpha(data[c("zBFI_E_A", "zIAL_PA","zIAL_HIr","zISK_A_A")]) # a = .84

# Communal Personality
psych::alpha(data[c("zBFI_A_C", "zIAL_LM","zIAL_DEr","zISK_SO_P")]) # a = .82

### Behavioral Indicators

# Agentic Behavior
psych::alpha(data[c("VerhaltenAG1","VerhaltenAG2","VerhaltenAG3","VerhaltenAG4")]) # a = .60
# Communal Behavior
psych::alpha(data[c("VerhaltenCO1","VerhaltenCO2","VerhaltenCO3","VerhaltenCO4")]) # a = .75

## Science Test
# reliability was computed per day with the raw answers which are not
# included in the present dataset but can be requested

# day1: alpha: .91 (with Kuder-Richardson-Formula 20)

# day2: alpha: .88 (with KR-20)
mean(c(.91,.88)) #.90
 
###########################################################################################
##### Step 4: Item Information                                                         ####
###########################################################################################

# descriptive item information
descriptives <- describe(data[c(items_a, items_c)]) 
descriptives_items <- print(descriptives, digits = 2) #items_c have higher mean
write.table(descriptives_items,file="Descriptives_Items",sep = ";")


###########################################################################################
##### Step 5: Confirmatory Factor Analysis                                             ####
###########################################################################################

### Data parceling
# For a parceling approach, 3 parcels per construct are built with 5 items each. Items are sorted by their factor loadings
# from high to low and are then averaged to the parcels as 1-2-3-3-2-1-1-2-3-3-2-1-1-2-3.

# Factor loadings of items are determined with a one factor model per construct
# Agency
model_a <- 'f_a =~ SJTkb00233+SJTkb00234+SJTkb00235+SJTkb00236+SJTkb00237+SJTkb00238+ 
             SJTkb00239+SJTkb00240+SJTkb00241+SJTkb00242+SJTkb00243+SJTkb00244+
             SJTkb00245+SJTkb00246+SJTkb00247' 
fit.model_a <- cfa(model_a, data = data, 
                   ordered = c("SJTkb00233","SJTkb00234","SJTkb00235","SJTkb00236","SJTkb00237","SJTkb00238", 
                               "SJTkb00239","SJTkb00240","SJTkb00241","SJTkb00242","SJTkb00243","SJTkb00244",
                               "SJTkb00245","SJTkb00246","SJTkb00247"), #marks ordinal data
                   std.lv=T)
summary(fit.model_a)
fl_a<-inspect(fit.model_a,what="std")$lambda #factor loadings
# Agency: SJTkb00243,SJTkb00239,SJTkb00241,SJTkb00238,SJTkb00233,SJTkb00237,SJTkb00236,SJTkb00235,SJTkb00247,SJTkb00242,
# SJTkb00234,SJTkb00240,SJTkb00246,SJTkb00245,SJTkb00244
data$parcel_a_1 <- apply(data[,c("SJTkb00243","SJTkb00237","SJTkb00236","SJTkb00240","SJTkb00246")], MARGIN = 1, FUN = mean)
data$parcel_a_2 <- apply(data[,c("SJTkb00239","SJTkb00233","SJTkb00235","SJTkb00234","SJTkb00245")], MARGIN = 1, FUN = mean)
data$parcel_a_3 <- apply(data[,c("SJTkb00241","SJTkb00238","SJTkb00247","SJTkb00242","SJTkb00244")], MARGIN = 1, FUN = mean)

# Communion
model_c <- 'f_c =~ SJTkb00218+SJTkb00219+SJTkb00220+SJTkb00221+SJTkb00222+SJTkb00223+ 
             SJTkb00224+SJTkb00225+SJTkb00226+SJTkb00227+SJTkb00228+SJTkb00229+
             SJTkb00230+SJTkb00231+SJTkb00232'
fit.model_c <- cfa(model_c, data = data, 
                   ordered = c("SJTkb00218","SJTkb00219","SJTkb00220","SJTkb00221","SJTkb00222","SJTkb00223", 
                               "SJTkb00224","SJTkb00225","SJTkb00226","SJTkb00227","SJTkb00228","SJTkb00229",
                               "SJTkb00230","SJTkb00231","SJTkb00232"), #marks ordinal data
                   std.lv=T)
summary(fit.model_c)
fl_c<-inspect(fit.model_c,what="std")$lambda #factor loadings
# Communion: SJTkb00228,SJTkb00231,SJTkb00226,SJTkb00229,SJTkb00232,SJTkb00220,SJTkb00219,SJTkb00222,SJTkb00230,SJTkb00221,
# SJTkb00227,SJTkb00225,SJTkb00218,SJTkb00224,SJTkb00223
data$parcel_c_1 <- apply(data[,c("SJTkb00228","SJTkb00220","SJTkb00219","SJTkb00225","SJTkb00218")], MARGIN = 1, FUN = mean)
data$parcel_c_2 <- apply(data[,c("SJTkb00231","SJTkb00232","SJTkb00222","SJTkb00227","SJTkb00224")], MARGIN = 1, FUN = mean)
data$parcel_c_3 <- apply(data[,c("SJTkb00226","SJTkb00229","SJTkb00230","SJTkb00221","SJTkb00223")], MARGIN = 1, FUN = mean)

### Data Modeling

# 1 Factor Model

model_1f <- 'f_g =~parcel_a_1+parcel_a_2+parcel_a_3+parcel_c_1+parcel_c_2+parcel_c_3'
fit.model_1f <- cfa(model_1f, data=data, std.lv=T)
summary(fit.model_1f, fit.measures=TRUE, standardized = TRUE) #Model Fit: CFI = .68, RMSEA = .14

# 2 Factor Model, uncorrelated factors

model_2f_uc <- 'f_a =~ parcel_a_1+parcel_a_2+parcel_a_3 # Agency
             f_c =~parcel_c_1+parcel_c_2+parcel_c_3 # Communion
             f_c~~0*f_a  # restricts the covariance of the factors to zero
            '
fit.model_2f_uc <- cfa(model_2f_uc, data=data, std.lv=T)
summary(fit.model_2f_uc, fit.measures=TRUE, standardized = TRUE) # Robust model: CFI = .94, RMSEA = .06

# 2 Factor Model, correlated factors 

model_2f <- 'f_a =~ parcel_a_1+parcel_a_2+parcel_a_3 # Agency
             f_c =~ parcel_c_1+parcel_c_2+parcel_c_3 # Communion
             '
fit.model_2f <- cfa(model_2f, data=data, std.lv=T)
summary(fit.model_2f, fit.measures=TRUE, standardized = TRUE) # Robust model: CFI = .99, RMSEA = .02

# Model comparisons
anova(fit.model_2f,fit.model_1f) # 2 correlated factors fit the data significantly better than 1 factor
anova(fit.model_2f,fit.model_2f_uc) # 2 correlated factors fit the data significantly better than 2 uncorrelated factors


###########################################################################################
##### Step 6: Reliability Analysis                                                     ####
###########################################################################################

# Computating of reliability parameters per dimension
omega(data[c(items_a)], poly = T) # alpha: .63, omega_h: .46, omega_t: .68 
omega(data[c(items_c)], poly = T) # alpha: .70, omega_h: .58, omega_t: .74 

###########################################################################################
##### Step 7: Convergent and Discriminant Validity                                     ####
###########################################################################################

# compute mean across SJT items
data$SJT_a <- apply(data[c(items_a)], MARGIN = 1, FUN = mean, na.rm = T)
data$SJT_c <- apply(data[c(items_c)], MARGIN = 1, FUN = mean, na.rm = T)

c_d_validity <- data [c("SJT_a","SJT_c",
                        "tradSJT","NATFaehigkeit",
                        "Pers_Ag","Pers_Co",
                        "Beha_Ag","Beha_Co")]

# Descriptive Statistics of all variables
descriptives_v <- describe(c_d_validity) 
descriptives_variables <- base::print(descriptives_v, digits = 2)
write.table(descriptives_variables,file="Descriptives_Variables",sep = ";")

#  correlation matrix 

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

cortable <- corcons(c_d_validity)
write.table(cortable, file = "Correlations", sep = ";")

# Relation between SJT Agency and Communion
data$zsjt_a <- scale(data$SJT_a)
data$zsjt_c <- scale(data$SJT_c)
# New variable: standardized Agency and COmmunion scores above mean
data$Combination <- ifelse (data$zsjt_a > 0 & data$zsjt_c > 0, "above average","not above average")
table(data$Combination)

ggplot(data, aes(x=zsjt_a, y=zsjt_c))+
  geom_count(aes(colour=Combination))+
  stat_smooth(method=lm, color="burlywood4")+
  scale_color_manual(values=c("coral","lightcyan4"))+
  ylim(-3,2)+xlim(-3,3)+
  xlab("SJT Agency (z-Score)")+ylab("SJT Communion (z-Score)")+
  theme_grey(base_size = 20)

SJT_a_c <- lm(zsjt_c ~ zsjt_a,data=data)
summary(SJT_a_c)
confint(SJT_a_c, level = 0.95)

### we hypothesized correlations of r = .20 for relations to personality tests and past behavior
## thus we can inspect this assumption by looking at confidence intervals: if the confidence interval included .20, there is no significant difference

# for agency
cor.test(data$SJT_a, data$tradSJT, use="complete.obs")
cor.test(data$SJT_a, data$NATFaehigkeit, use="complete.obs") 
cor.test(data$SJT_a, data$Pers_Ag, use="complete.obs") # difference
cor.test(data$SJT_a, data$Pers_Co, use="complete.obs") # no difference
cor.test(data$SJT_a, data$Beha_Ag, use="complete.obs") # difference
cor.test(data$SJT_a, data$Beha_Co, use="complete.obs") # difference

# for communion
cor.test(data$SJT_c, data$tradSJT, use="complete.obs")
cor.test(data$SJT_c, data$NATFaehigkeit, use="complete.obs") 
cor.test(data$SJT_c, data$Pers_Co, use="complete.obs") # no difference
cor.test(data$SJT_c, data$IAL_LM, use="complete.obs") # difference
cor.test(data$SJT_c, data$Beha_Co, use="complete.obs") # no difference


# relation to (self-reported) high school GPA or equivalent
data$gpa <- ifelse(!is.na(data$X03_01), data$X03_01, data$X04_01)
cor.test(data$SJT_a,data$gpa)
cor.test(data$SJT_c,data$gpa)

# relations to reasoning tests
cor.test(data$SJT_a,data$APFaehigkeit)
cor.test(data$SJT_a,data$RSFaehigkeit)
cor.test(data$SJT_c,data$APFaehigkeit)
cor.test(data$SJT_c,data$RSFaehigkeit)

###########################################################################################
##### Step 8: First Indication of Criterion Validity                                   ####
###########################################################################################

# Can SJT Agency predict agentic behavior when controlling for SJT Communion?
Beha_Ag <- lm(Beha_Ag ~ SJT_a+SJT_c,data=data)
summary(Beha_Ag)
confint(Beha_Ag, c('SJT_a','SJT_c'), level = 0.95)
lm.beta(Beha_Ag)

# Can SJT Communion predict communal behavior when controlling for SJT Agency?
Beha_Co <- lm(Beha_Co ~ SJT_a+SJT_c,data=data)
summary(Beha_Co)
confint(Beha_Co, c('SJT_a','SJT_c'), level = 0.95)
lm.beta(Beha_Co)

write.table(data, file = "data2")



