library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(patchwork)

#Importing my data from excel spreadsheet with some cleaning up!
# Importing data ---------------------------------
excel_sheet <- read_excel("TheEntireDataSet.xlsx", sheet = "labeled")
rawResults<- excel_sheet

rawResults$Outlier <- as.integer(rawResults$Outlier)
rawResults$Outlier[is.na(rawResults$Outlier)] <- 0
rawResults$Position <- as.integer(rawResults$Position)
cleanedResults <- rawResults %>% drop_na(Bin_Number, Position)
head(cleanedResults)

# evil date formatting
# Date Formatting for dataset --------------------
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

# Removing unnessary columns 
 cleanedResults <- cleanedResults %>% select(-"File #",
      -"Operator", -"Name", -"ID", -"Field1", -"Field2",
      -"test", -"Application", -"Method", -"ElapsedTime", -"Alloy 1",
      -"Match Qual 1", -"Alloy 2", -"Match Qual 2", -"Alloy 3", -"Match Qual 3")
 
# Filtering -----------------
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
# Averaging Results -------- 
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
# creating a column of each element
longAverages <- averagedResults %>%
  pivot_longer(
    cols = ends_with("_mean"),
    names_to = "Element",
    values_to = "Value"
  )

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


longAverages <- longAverages %>%
  mutate(
    InoculationTreatmentType = case_when(
        Treatment_Number %in% c(2, 3, 4, 5)  ~ "Biocrust_inoculated",
        Treatment_Number %in% c(1, 6, 7) ~ "Uncolonized_soil"
    ),
    
    SurfaceTreatmentType = case_when(
      Treatment_Number %in% c(1, 2, 3, 7) ~ "Flat_Surfaces",
      Treatment_Number %in% c(4, 5, 6) ~ "Textured_Surfaces"
      )
  )
 
#-----------------------------------------------------------------------------------------------------------
###### Creating Datasets 
#-----------------------------------------------------------------------------------------------------------

# separating date_groups into different datasets

MiddleFinalResults <- 
  longAverages %>% 
  filter(Date_Group %in% c("Week_5", "Week_10"))

# create a subset for only positions
# //////////// BARE GROUP DATA FILTERED
all_bridge_data <- MiddleFinalResults %>%
  filter(Position == 9)
summary(all_bridge_data)
# ////////// TREATMENT 
all_bridge_model <- aov(Value ~ InoculationTreatmentType * SurfaceTreatmentType * Date_Group, 
                        data = all_bridge_data)
summary(all_bridge_model)
#                                                           Df Sum Sq Mean Sq F value Pr(>F)  
# InoculationTreatmentType                                   1   35.5   35.46   3.131 0.0781 .
# SurfaceTreatmentType                                       1    4.2    4.22   0.373 0.5421  
# Date_Group                                                 1    4.6    4.63   0.409 0.5233  
# InoculationTreatmentType:SurfaceTreatmentType              1    4.6    4.61   0.407 0.5240  
# InoculationTreatmentType:Date_Group                        1    5.3    5.28   0.466 0.4954  
# SurfaceTreatmentType:Date_Group                            1    0.3    0.27   0.024 0.8767  
# InoculationTreatmentType:SurfaceTreatmentType:Date_Group   1    2.3    2.27   0.201 0.6546  
# Residuals                                                238 2695.7   11.33                 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
TukeyHSD(all_bridge_model, "InoculationTreatmentType")
# Uncolonized_soil-Biocrust_inoculated 0.7706567 -0.08209457 1.623408 0.0763011
# POTENTIAL INTERPRETATION:
## uncolonized treatments showed more values of all elements


# ........................................... setting more data subsets
p_bridge_data <- all_bridge_data %>%
  filter(Element == "P_mean")

t2_bridge_data <- all_bridge_data %>%
  filter(Position == 9,
         Treatment_Number == 2)

t4_bridge_data <- all_bridge_data %>%
  filter(Position == 9,
         Treatment_Number == 4)

t5_bridge_data <- all_bridge_data %>%
  filter(Position == 9,
         Treatment_Number == 5)

t6_bridge_data <- all_bridge_data %>%
  filter(Position == 9,
         Treatment_Number == 6)


# .............................................................................
#### (almost) ALL ELEMENTS TREATMENT 2 ####
t2_bridge_model <- aov(Value ~ Date_Group,
                       data = t2_bridge_data)
summary(t2_bridge_model)

#/......................... Phosphorus t2
p_t2_bridge_data <- t2_bridge_data %>%
  filter(Element == "P_mean")
p_t2_bridge_model <- aov(Value ~ Date_Group, data = p_t2_bridge_data)
summary(p_t2_bridge_model)

#.......................... Aluminum t2
al_t2_bridge_data <- t2_bridge_data %>%
  filter(Element == "Al_mean")
al_t2_bridge_model <- aov(Value ~ Date_Group, data = al_t2_bridge_data)
summary(al_t2_bridge_model)


#........................... Potassium t2
k_t2_bridge_data <- t2_bridge_data %>%
  filter(Element == "K_mean")
k_t2_bridge_model <- aov(Value ~ Date_Group, data = k_t2_bridge_data)
summary(k_t2_bridge_model)

#............................ Sulfur t2
s_t2_bridge_data <- t2_bridge_data %>%
  filter(Element == "S_mean")
s_t2_bridge_model <- aov(Value ~ Date_Group, data = s_t2_bridge_data)
summary(s_t2_bridge_model)
#### end of t2 ####

# //////////////////////////////////////////////////////////
#### Treatment 5 ELEMENTS ####
p_t5_bridge_data <- t5_bridge_data %>%
  filter(Element == "P_mean")
p_t5_bridge_model <- aov(Value ~ Date_Group, data = p_t5_bridge_data)
summary(p_t5_bridge_model)


al_t5_bridge_data <- t5_bridge_data %>%
  filter(Element == "Al_mean")
al_t5_bridge_model <- aov(Value ~ Date_Group, data = al_t5_bridge_data)
summary(al_t5_bridge_model)


k_t5_bridge_data <- t5_bridge_data %>%
  filter(Element == "K_mean")
k_t5_bridge_model <- aov(Value ~ Date_Group, data = k_t5_bridge_data)
summary(k_t5_bridge_model)


fe_t5_bridge_data <- t5_bridge_data %>%
  filter(Element == "Fe_mean")
fe_t5_bridge_model <- aov(Value ~ Date_Group, data = fe_t5_bridge_data)
summary(fe_t5_bridge_model)



# Basic grouped boxplot
# Example: Two separate datasets

boxplot(p_t2_bridge_data, p_t5_bridge_data, names = c("Treatment 2", "Treatment 5"))



pBridgeBoxplot <- ggplot(p_bridge_data, 
                           aes(x = InoculationTreatmentType, y = mean(Value,)-Value, fill = Date_Group)) +
  geom_boxplot(position = position_dodge()) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # Zero reference line
  theme_minimal() +
  labs(
    title = "Phosphorus Concentration",
    subtitle = "Week 5 vs Week 10",
    x = "Uncolonized Soil Positions",
    y = "P Concentration (PPM)"
  )
pBridgeBoxplot
