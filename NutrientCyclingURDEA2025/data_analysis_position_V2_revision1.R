library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(patchwork)

#Importing my data from excel spreadsheet with some cleaning up!
# ---------------------------------
excel_sheet <- read_excel("TheEntireDataSet.xlsx", sheet = "labeled")
rawResults<- excel_sheet

rawResults$Outlier <- as.integer(rawResults$Outlier)
rawResults$Outlier[is.na(rawResults$Outlier)] <- 0
rawResults$Position <- as.integer(rawResults$Position)
cleanedResults <- rawResults %>% drop_na(Bin_Number, Position)
head(cleanedResults)

# evil date formatting
#--------------------
str(cleanedResults$DateTime)
cleanedResults$DateTime <- as.numeric(cleanedResults$DateTime)
cleanedResults$DateTime <- as.POSIXct(
  cleanedResults$DateTime * 86400,
  origin = "1899-12-30",
  tz = "UTC"
)
cleanedResults$Date <- as.Date(cleanedResults$DateTime)
table(cleanedResults$Date)
cleanedResults <- cleanedResults %>%
  mutate(Date_Group = case_when(
    Date %in% as.Date(c("2006-01-30", "2006-01-31")) ~ "Week_1",
    Date %in% as.Date(c("2006-02-07", "2006-02-08")) ~ "Week_2", 
    Date %in% as.Date(c("2006-02-27")) ~ "Week_5",
    Date %in% as.Date(c("2006-3-20")) ~ "Week_8",
    Date %in% as.Date(c("2006-04-03")) ~ "Week_10"
  ))

#-----------
# end of evil date formatting

# remove unnecessary columns
 cleanedResults <- cleanedResults %>% select(-"File #",
      -"Operator", -"Name", -"ID", -"Field1", -"Field2",
      -"test", -"Application", -"Method", -"ElapsedTime", -"Alloy 1",
      -"Match Qual 1", -"Alloy 2", -"Match Qual 2", -"Alloy 3", -"Match Qual 3")
 
#-------------------------------------------------------
#End of clean up


# filtering 
#-----------------
# alternate filtering, including date_group 
filteredResults <- cleanedResults %>%
  select(Date_Group, Bin_Number, Position, P, Al, Fe, S, Ca, K)

# changing n/a & <LOD to 0 across measurements
    # changing columns to numeric
filteredResults <- filteredResults %>% 
  mutate(across(-c(Bin_Number, Position, Date_Group),
                ~ as.numeric(as.character(.))))
    # replacing N/A to the number 0
filteredResults <- filteredResults %>%
  mutate(across(P:Ca, ~replace_na(.x, 0)))

head(filteredResults)
#-------- 
# filtering ^

# full disclosure: below is using generative AI


# averaging results, chatGPT generated, needed help on this one
averagedResults <- filteredResults %>%
  group_by(Date_Group, Bin_Number, Position) %>%
  summarise(
    P_mean  = mean(P,  na.rm = TRUE),
    Al_mean = mean(Al, na.rm = TRUE),
    Fe_mean = mean(Fe, na.rm = TRUE),
    S_mean  = mean(S, na.rm  = TRUE),
    Ca_mean = mean(Ca, na.rm = TRUE),
    K_mean  = mean(K,  na.rm = TRUE),
    n_reps  = n(),   # tells you how many reps contributed
    .groups = "drop"
  )

head(averagedResults)

# var: longAverages, inclusion of columns of treatment/element etc
#--------

# creating a column of each element
longAverages <- averagedResults %>%
  pivot_longer(
    cols = ends_with("_mean"),
    names_to = "Element",
    values_to = "Value"
  )
#head(longAverages)

# making a column to identify treatment types
longAverages <- longAverages %>%
  mutate(Treatment_Number = as.numeric(str_extract(Bin_Number, "(?<=T)\\d+"))) %>%
  filter(!is.na(Treatment_Number))

# checking for any measurements outside of date ranges set
#cleanedResults %>%
#  filter(is.na(Date_Group)) %>%
#  select(DateTime, Date)

# excluding out of range dates for measurements
longAverages <- longAverages %>%
  filter(!is.na(Date_Group))

# making a column for positions types
longAverages <- longAverages %>%
  mutate(
    Surface = case_when(
      Position %in% c(1,2,3,4) ~ "Trough",
      Position %in% c(5,6,7,8) ~ "Mound",
      Position == 9 ~ "Center"
      # calling it center b/c of 9
    ),
    
    Inoculation = case_when(
      Position %in% c(1,2,5,6) ~ "Uncolonized",
      Position %in% c(3,4,7,8) ~ "Inoculated",
      Position == 9 ~ "Bridge" 
      # calling it bridge/center b/c of the transfer of nutrients
    )
  ) 
 
#-----------------------------------------------------------------------------------------------------------
###### Creating Datasets 
#-----------------------------------------------------------------------------------------------------------

# separating date_groups into different datasets

MiddleFinalResults <- longAverages %>% 
  filter(Date_Group %in% c("Week_5", "Week_10"))

# create a subset for only positions
# //////////// BARE GROUP DATA FILTERED
all_bare_group_data <- MiddleFinalResults %>%
  filter(Position %in% c(1, 2, 5, 6))
summary(all_bare_group_data)
# ////////// TREATMENT 
t4_bare_group_data <- MiddleFinalResults %>%
  filter(Position %in% c(1, 2, 5, 6),
         Treatment_Number == "4")
t1_bare_group_data <- MiddleFinalResults %>%
  filter(Position %in% c(1, 2, 5, 6),
         Treatment_Number == "1")

# //////////// CRUST GROUP DATA FILTERED
all_crust_group_data <- MiddleFinalResults %>%
  filter(Position %in% c(3, 4, 7, 8))
summary(all_bare_group_data)
t4_crust_group_data <- MiddleFinalResults %>%
  filter(Position %in% c(3, 4, 7, 8),
         Treatment_Number == "4")
t1_crust_group_data <- MiddleFinalResults %>%
  filter(Position %in% c(3, 4, 7, 8),
         Treatment_Number == "1")

# /////////// BARE GROUP modeling for PHOSPHORUS 
#------------------------------------------------------------------------------------------------------------

# ALL BARE GROUP MODEL
all_bare_group_model <- aov(Value ~ Surface * Date_Group,
             data = all_bare_group_data)
summary(all_bare_group_model)
#                      Df Sum Sq Mean Sq F value Pr(>F)
# Surface               1      2   1.915   0.199  0.656
# Date_Group            1     19  19.217   1.998  0.158
# Surface:Date_Group    1      2   1.975   0.205  0.651
# Residuals          1004   9659   9.620 
# ///////////////
# INTERPRETATION:
# no influence of surface towards element value across ALL groups

# ALL TREATMENTS 4 Phosphorus BARE /////////////////////////////////////////////
p_bare_group_data <- all_bare_group_data %>%
  filter(Element == "P_mean")
summary(p_bare_group_data)

all_p_bare_group_model <- aov(Value ~ Surface * Date_Group,
                            data = p_bare_group_data)
summary(all_p_bare_group_model)

# include interaction with inoculum 

#                     Df  Sum Sq  Mean Sq F value Pr(>F)   
# Surface              1 0.00135 0.001351   2.840 0.0939 . 
# Date_Group           1 0.00374 0.003735   7.849 0.0057 **
# Surface:Date_Group   1 0.00020 0.000200   0.419 0.5182   
# Residuals          164 0.07804 0.000476                  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# INTERPRETATION: # Surface  p = 0.0939 . 
# slight influence of surface towards phosphorus value across all treatments
TukeyHSD(all_p_bare_group_model, "Surface")
# $Surface
#                     diff           lwr        upr     p adj
# Runoff-Mound 0.005672222 -0.0009742285 0.01231867 0.0938693
### INTERPRETATION: RUNOFF > MOUNDS across ALL TREATMENTS

# all_p_bare_group_model <- aov(Value ~ Surface * Date_Group * Treatment_Number,
#                               data = p_bare_group_data)
# summary(all_p_bare_group_model)
# TukeyHSD(all_p_bare_group_model, "Treatment_Number") ERROR



# TREATMENT 4 Phosphorus BARE /////////////////////////////////////////////////
t4_p_bare_group_data <- t4_bare_group_data %>%
  filter(Element == "P_mean",
         Treatment_Number =="4")
summary(t4_p_bare_group_data)

t4_p_bare_group_model <- aov(Value ~ Surface * Date_Group,
                              data = t4_p_bare_group_data)
summary(t4_p_bare_group_model)
#                    Df   Sum Sq  Mean Sq F value   Pr(>F)    
# Surface             1 0.000005 0.000005   0.023 0.880322    
# Date_Group          1 0.003727 0.003727  17.037 0.000522 ***
# Surface:Date_Group  1 0.000033 0.000033   0.150 0.702429    
# Residuals          20 0.004375 0.000219                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## INTERPRETATION: surface does not show effect on P in TREATMENT 4


# adjust for treatment 6
# TREATMENT 1 Phosphorus BARE /////////////////////////////////////////////////
t1_p_bare_group_data <- t1_bare_group_data %>%
  filter(Element == "P_mean")
summary(t1_p_bare_group_data)

t1_p_bare_group_model <- aov(Value ~ Surface * Date_Group,
                             data = t1_p_bare_group_data)
summary(t1_p_bare_group_model)
#                    Df   Sum Sq   Mean Sq F value Pr(>F)
# Surface             1 0.000011 0.0000105   0.029  0.867
# Date_Group          1 0.001073 0.0010733   2.949  0.101
# Surface:Date_Group  1 0.000002 0.0000019   0.005  0.944
# Residuals          20 0.007280 0.0003640     
## INTERPRETATIONS:Surface has no have an effect on phosphorus levels in Control
TukeyHSD(t1_p_bare_group_model, "Surface")

## BOX PLOT comparisons ///////////////////////////////////////////////////////
boxplotPhosphorusT1 <- ggplot(t1_p_bare_group_data,
       aes(x = Surface, y = Value, fill = Date_Group)) +
  geom_boxplot(position = position_dodge()) +
  theme_minimal() +
  labs(
    title = "Treatment 1 Phosphorus Concentration",
    subtitle = "Week 5 vs Week 10",
    x = "Surface Type", 
    y = "P Concentration (PPM)" 
  )
# boxplotPhosphorusT1
boxplotPhosphorusT4 <- ggplot(t4_p_bare_group_data,
                              aes(x = Surface, y = Value, fill = Date_Group)) +
  geom_boxplot(position = position_dodge()) +
  theme_minimal() +
  labs(
    title = "Treatment 4 Phosphorus Concentration in BARE SOILS",
    subtitle = "Week 5 vs Week 10",
    x = "Surface Type", 
    y = "P Concentration (PPM)" 
  )
# boxplotPhosphorusT4
boxplotPhosphorusT1+boxplotPhosphorusT4
# ^^^^^ Looking at comparison

# comparing the differences between bare across treatment 1 &&& 4
anova(t1_p_bare_group_model, t4_p_bare_group_model)
# Model 1: Value ~ Surface * Date_Group
# Model 2: Value ~ Surface * Date_Group
#   Res.Df       RSS Df Sum of Sq F Pr(>F)
# 1     20 0.0072796                      
# 2     20 0.0043754  0 0.0029042   
# Potential interpretations::::::: ?????????
#         




# /////////// crust GROUP modeling for PHOSPHORUS 
#------------------------------------------------------------------------------------------------------------
# ALL CRUST GROUP MODEL
all_crust_group_model <- aov(Value ~ Surface * Date_Group,
                            data = all_crust_group_data)
summary(all_crust_group_model)
#                      Df Sum Sq Mean Sq F value Pr(>F)  
# Surface               1      7   7.375   0.929 0.3353  
# Date_Group            1     26  26.117   3.290 0.0700 .
# Surface:Date_Group    1     22  22.116   2.786 0.0954 .
# Residuals          1004   7969   7.937                 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# ///////////////
# INTERPRETATION: interaction between Surface/Date_Group might have an effect
# across all elements across all groups


# ALL TREATMENTS 4 Phosphorus CRUST!!//////////////////////////////////////////
p_crust_group_data <- all_crust_group_data %>%
  filter(Element == "P_mean")
#summary(p_crust_group_data)

all_p_crust_group_model <- aov(Value ~ Surface * Date_Group,
                              data = p_crust_group_data)
summary(all_p_crust_group_model)
#                     Df  Sum Sq  Mean Sq F value  Pr(>F)   
# Surface              1 0.00143 0.001434   1.581 0.21034   
# Date_Group           1 0.00691 0.006909   7.622 0.00642 **
# Surface:Date_Group   1 0.00016 0.000162   0.178 0.67349   
# Residuals          164 0.14867 0.000907                   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# INTERPRETATION: across all treatments=> Time has sigfig effect on phosphorus 

# what about surface is affecting phosphorus?
TukeyHSD(all_p_crust_group_model, "Surface")
#                    diff          lwr        upr     p adj
# Trough-Mound 0.00584246 -0.003331069 0.01501599 0.2103436
### INTERPRETATION: RUNOFF > MOUNDS across ALL TREATMENTS


# TREATMENT 4 Phosphorus CRUST /////////////////////////////////////////////////
t4_p_crust_group_data <- t4_crust_group_data %>%
  filter(Element == "P_mean")
summary(t4_p_crust_group_data)

t4_p_crust_group_model <- aov(Value ~ Surface * Date_Group,
                             data = t4_p_crust_group_data)
summary(t4_p_crust_group_model)
#                    Df   Sum Sq  Mean Sq F value Pr(>F)  
# Surface             1 0.000063 0.000063   0.041 0.8422  
# Date_Group          1 0.012447 0.012447   8.055 0.0102 *
# Surface:Date_Group  1 0.000111 0.000111   0.072 0.7913  
# Residuals          20 0.030906 0.001545                 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## INTERPRETATION: surface does not show effect on P in TREATMENT 4

# TREATMENT 1 Phosphorus CRUST /////////////////////////////////////////////////
t1_p_crust_group_data <- t1_crust_group_data %>%
  filter(Element == "P_mean")
summary(t1_p_crust_group_data)

t1_p_crust_group_model <- aov(Value ~ Surface * Date_Group,
                             data = t1_p_crust_group_data)
summary(t1_p_crust_group_model)
#                    Df    Sum Sq   Mean Sq F value Pr(>F)  
# Surface             1 0.0004960 0.0004960   4.699 0.0424 *
# Date_Group          1 0.0000029 0.0000029   0.027 0.8707  
# Surface:Date_Group  1 0.0001293 0.0001293   1.225 0.2816  
# Residuals          20 0.0021109 0.0001055                 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## INTERPRETATIONS:Surface might have an effect on phosphorus levels in Control
TukeyHSD(t1_p_crust_group_model, "Surface")
#                     diff          lwr        upr     p adj
# Trough-Mound 0.009091667 0.0003429112 0.01784042 0.0424213

## BOX PLOT comparisons ///////////////////////////////////////////////////////

boxplotPhosphorusT1CRUST <- ggplot(t1_p_crust_group_data,
                              aes(x = Surface, y = Value, fill = Date_Group)) +
  geom_boxplot(position = position_dodge()) +
  theme_minimal() +
  labs(
    title = "T1 Phosphorus Concentration over CRUST positions",
    subtitle = "Week 5 vs Week 10",
    x = "Surface Type", 
    y = "P Concentration (PPM)" 
  )
# boxplotPhosphorusT1
boxplotPhosphorusT4CRUST <- ggplot(t4_p_crust_group_data,
                              aes(x = Surface, y = Value, fill = Date_Group)) +
  geom_boxplot(position = position_dodge()) +
  theme_minimal() +
  labs(
    title = "Treatment 4 Phosphorus Concentration over CRUST positions",
    subtitle = "Week 5 vs Week 10",
    x = "Surface Type", 
    y = "P Concentration (PPM)" 
  )
# boxplotPhosphorusT4
boxplotPhosphorusT1CRUST+boxplotPhosphorusT4CRUST
# ^^^^^ Looking at comparison

# comparing the differences between bare across treatment 1 &&& 4
anova(t1_p_crust_group_model, t4_p_crust_group_model)
# Model 1: Value ~ Surface * Date_Group
# Model 2: Value ~ Surface * Date_Group
#   Res.Df       RSS Df Sum of Sq F Pr(>F)
# 1     20 0.0021109                      
# 2     20 0.0309056  0 -0.028795  
# Potential interpretations:::::::
# in terms of phosphorus contents in uncolonized soils
#   there seemed to have a bigger effect on phosphorus 
#   due to surface in treatment control


#comparing directly bare vs. crust t1 PHOSPHORUS
anova(t1_p_bare_group_model, t1_p_crust_group_model)
# Model 1: Value ~ Surface * Date_Group
# Model 2: Value ~ Surface * Date_Group
#   Res.Df       RSS Df Sum of Sq F Pr(>F)
# 1     20 0.0072796                      
# 2     20 0.0021109  0 0.0051687 


#comparing directly bare vs. crust t4 PHOSPHORUS
anova(t4_p_bare_group_model, t4_p_crust_group_model)
# Model 1: Value ~ Surface * Date_Group
# Model 2: Value ~ Surface * Date_Group
#   Res.Df       RSS Df Sum of Sq F Pr(>F)
# 1     20 0.0043754                      
# 2     20 0.0309056  0  -0.02653 

anova(all_bare_group_model, all_crust_group_model)
# Model 1: Value ~ Surface * Date_Group
# Model 2: Value ~ Surface * Date_Group
#   Res.Df    RSS Df Sum of Sq F Pr(>F)
# 1   1004 9658.9                      
# 2   1004 7969.2  0    1689.7  