library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(patchwork)

# Importing data ------------------------------------------
# Importing my data from excel spreadsheet with some cleaning up!

excel_sheet <- read_excel("TheEntireDataSet.xlsx", sheet = "labeled")
rawResults<- excel_sheet

rawResults$Outlier <- as.integer(rawResults$Outlier)
rawResults$Outlier[is.na(rawResults$Outlier)] <- 0
rawResults$Position <- as.integer(rawResults$Position)
cleanedResults <- rawResults %>% drop_na(Bin_Number, Position)
head(cleanedResults)

# Date Formatting --------------------
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

# remove unnecessary columns
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
 

# Creating Datasets -------------------------------------------------------

# separating date_groups into different datasets

MiddleFinalResults <- longAverages %>% 
  filter(Date_Group %in% c("Week_5", "Week_10"))

# create a subset for only positions
# //////////// BARE GROUP DATA FILTERED
# all_bare_group_data <- MiddleFinalResults %>%
#   filter(Position %in% c(1, 2, 5, 6))
# summary(all_bare_group_data)
# ////////// TREATMENT 
t4_bare_group_data <- MiddleFinalResults %>%
  filter(Position %in% c(1, 2, 5, 6),
         Treatment_Number == "4")
t5_bare_group_data <- MiddleFinalResults %>%
  filter(Position %in% c(1, 2, 5, 6),
         Treatment_Number == "5")
t6_bare_group_data <- MiddleFinalResults %>%
  filter(Position %in% c(1, 2, 5, 6),
         Treatment_Number == "6")

# //////////// CRUST GROUP DATA FILTERED
# all_crust_group_data <- MiddleFinalResults %>%
#   filter(Position %in% c(3, 4, 7, 8))
# summary(all_bare_group_data)
t4_crust_group_data <- MiddleFinalResults %>%
  filter(Position %in% c(3, 4, 7, 8),
         Treatment_Number == "4")
t5_crust_group_data <- MiddleFinalResults %>%
  filter(Position %in% c(3, 4, 7, 8),
         Treatment_Number == "5")
t6_crust_group_data <- MiddleFinalResults %>%
  filter(Position %in% c(3, 4, 7, 8),
         Treatment_Number == "6")
# ALUMINUM ================================================================
# t5_bare_group_data <- MiddleFinalResults %>%
#   filter(Position %in% c(1, 2, 5, 6),
#          Treatment_Number == "5")




# PHOSPHORUS BARE GROUP modeling ---------------------------------------
### TREATMENT 4 Phosphorus BARE ----------------------------------------
p_t4_bare_group_data <- t4_bare_group_data %>%
  filter(Element == "P_mean",
         Treatment_Number =="4")
summary(p_t4_bare_group_data)
# ... ANOVA
p_t4_bare_group_model <- aov(Value ~ Surface * Date_Group,
                              data = p_t4_bare_group_data)
summary(p_t4_bare_group_model)
# #                    Df   Sum Sq  Mean Sq F value   Pr(>F)    
# # Surface             1 0.000005 0.000005   0.023 0.880322    
# # Date_Group          1 0.003727 0.003727  17.037 0.000522 ***
# # Surface:Date_Group  1 0.000033 0.000033   0.150 0.702429    
# # Residuals          20 0.004375 0.000219                     
# # ---
# # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# ## INTERPRETATION: surface does not show effect on P in TREATMENT 4

## TREATMENT 5 Phosphorus BARE ------------------------------------
# T5: INOCULUM & DEPRESSIONS

p_t5_bare_group_data <- t5_bare_group_data %>%
  filter(Element == "P_mean",
         Treatment_Number =="5")
summary(p_t5_bare_group_data)
# ... ANOVA
p_t5_bare_group_model <- aov(Value ~ Surface * Date_Group,
                             data = p_t5_bare_group_data)
summary(p_t5_bare_group_model)
#                    Df    Sum Sq   Mean Sq F value Pr(>F)
# Surface             1 0.0000025 2.480e-06   0.037  0.850
# Date_Group          1 0.0001054 1.053e-04   1.554  0.227
# Surface:Date_Group  1 0.0000118 1.178e-05   0.174  0.681
# Residuals          20 0.0013555 6.778e-05


## TREATMENT 6 Phosphorus BARE ----------------------------------------
# Treatment 6: Depressions 
p_t6_bare_group_data <- t6_bare_group_data %>%
  filter(Element == "P_mean")

p_t6_bare_group_model <- aov(Value ~ Surface * Date_Group,
                             data = p_t6_bare_group_data)
summary(p_t6_bare_group_model)
#                    Df    Sum Sq   Mean Sq F value   Pr(>F)    
# Surface             1 0.0000350 0.0000350   0.246 0.625125    
# Date_Group          1 0.0022737 0.0022737  15.979 0.000708 ***
# Surface:Date_Group  1 0.0000763 0.0000763   0.536 0.472419    
# Residuals          20 0.0028458 0.0001423                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1     
# ## INTERPRETATIONS:Surface has no have an effect on phosphorus levels in Control
# 

### BOX PLOT comparisons Bare  T6 & T5 -------------------------------------------------
boxplotPhosphorusT6BARE <- ggplot(p_t6_bare_group_data,
       aes(x = Surface, y = Value, fill = Date_Group)) +
  geom_boxplot(position = position_dodge()) +
  theme_minimal() +
  labs(
    title = "Treatment 6 Phosphorus Concentration",
    subtitle = "Week 5 vs Week 10",
    x = "Uncolonized Soil Positions",
    y = "P Concentration (PPM)"
  )
boxplotPhosphorusT5BARE <- ggplot(p_t5_bare_group_data,
                              aes(x = Surface, y = Value, fill = Date_Group)) +
  geom_boxplot(position = position_dodge()) +
  theme_minimal() +
  labs(
    title = "Treatment 5 Phosphorus Concentration in BARE SOILS",
    subtitle = "Week 5 vs Week 10",
    x = "Uncolonized Soil Positions",
    y = "P Concentration (PPM)"
  )
boxplotPhosphorusT6BARE+boxplotPhosphorusT5BARE
# # ^^^^^ Looking at comparison
# 
# # comparing the differences between bare across treatment 1 &&& 4
anova(p_t5_bare_group_model, p_t6_bare_group_model)
# Model 1: Value ~ Surface * Date_Group
# Model 2: Value ~ Surface * Date_Group
#   Res.Df       RSS Df  Sum of Sq F Pr(>F)
# 1     20 0.0013555                       
# 2     20 0.0028458  0 -0.0014903 

# PHOSPHORUS CRUST GROUP modeling ---------------------------------------

## TREATMENT 5 Phosphorus CRUST-------------------------------------
p_t5_crust_group_data <- t5_crust_group_data %>%
  filter(Element == "P_mean")
summary(p_t5_crust_group_data)

p_t5_crust_group_model <- aov(Value ~ Surface * Date_Group,
                             data = p_t5_crust_group_data)
summary(p_t5_crust_group_model)
#                    Df   Sum Sq   Mean Sq F value Pr(>F)  
# Surface             1 0.000349 0.0003494   1.302 0.2673  
# Date_Group          1 0.000833 0.0008331   3.106 0.0933 .
# Surface:Date_Group  1 0.000096 0.0000961   0.358 0.5561  
# Residuals          20 0.005365 0.0002682                 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# ## INTERPRETATION: surface does not show effect on P in TREATMENT 5
# 
#
## TREATMENT 6 Phosphorus CRUST ---------------------------------------------
p_t6_crust_group_data <- t6_crust_group_data %>%
  filter(Element == "P_mean")
summary(p_t6_crust_group_data)
# 
p_t6_crust_group_model <- aov(Value ~ Surface * Date_Group,
                             data = p_t6_crust_group_data)
summary(p_t6_crust_group_model)
#                    Df   Sum Sq   Mean Sq F value Pr(>F)  
# Surface             1 0.000005 0.0000047   0.022 0.8838  
# Date_Group          1 0.001051 0.0010507   4.919 0.0383 *
# Surface:Date_Group  1 0.000269 0.0002687   1.258 0.2754  
# Residuals          20 0.004272 0.0002136                 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
TukeyHSD(p_t6_crust_group_model, "Date_Group")
#                       diff         lwr           upr     p adj
# Week_5-Week_10 -0.01323333 -0.02567969 -0.0007869725 0.0383068
### ////////////////// Potential interpretation
# phosphorus content @ wk 10 > wk 1




# TREATMENT 5 Phosphorus CRUST ----------------------------
p_t5_crust_group_data <- t5_crust_group_data %>%
  filter(Element == "P_mean")
summary(p_t5_crust_group_data)

p_t5_crust_group_model <- aov(Value ~ Surface * Date_Group,
                              data = p_t5_crust_group_data)
summary(p_t5_crust_group_model)
#                    Df   Sum Sq   Mean Sq F value Pr(>F)  
# Surface             1 0.000349 0.0003494   1.302 0.2673  
# Date_Group          1 0.000833 0.0008331   3.106 0.0933 .
# Surface:Date_Group  1 0.000096 0.0000961   0.358 0.5561  
# Residuals          20 0.005365 0.0002682                 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# ## INTERPRETATION: surface does not show effect on P in TREATMENT 5
# 
#
#### TREATMENT 6 Phosphorus CRUST ------------------------------------
p_t6_crust_group_data <- t6_crust_group_data %>%
  filter(Element == "P_mean")
summary(p_t6_crust_group_data)
# 
p_t6_crust_group_model <- aov(Value ~ Surface * Date_Group,
                              data = p_t6_crust_group_data)
summary(p_t6_crust_group_model)
#                    Df   Sum Sq   Mean Sq F value Pr(>F)  
# Surface             1 0.000005 0.0000047   0.022 0.8838  
# Date_Group          1 0.001051 0.0010507   4.919 0.0383 *
# Surface:Date_Group  1 0.000269 0.0002687   1.258 0.2754  
# Residuals          20 0.004272 0.0002136                 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
TukeyHSD(p_t6_crust_group_model, "Date_Group")
#                       diff         lwr           upr     p adj
# Week_5-Week_10 -0.01323333 -0.02567969 -0.0007869725 0.0383068
### ////////////////// Potential interpretation
# phosphorus content @ wk 10 > wk 1



## phosphorus bare  t5vt6 BOX PLOT comparisons----------------------------------------------------
boxplotPhosphorusT6BARE <- ggplot(p_t6_bare_group_data,
                                  aes(x = Surface, y = Value, fill = Date_Group)) +
  geom_boxplot(position = position_dodge()) +
  theme_minimal() +
  labs(
    title = "Treatment 6 Phosphorus Concentration",
    subtitle = "Week 5 vs Week 10",
    x = "Uncolonized Soil Positions",
    y = "P Concentration (PPM)"
  )
boxplotPhosphorusT5BARE <- ggplot(p_t5_bare_group_data,
                                  aes(x = Surface, y = Value, fill = Date_Group)) +
  geom_boxplot(position = position_dodge()) +
  theme_minimal() +
  labs(
    title = "Treatment 5 Phosphorus Concentration in BARE SOILS",
    subtitle = "Week 5 vs Week 10",
    x = "Uncolonized Soil Positions",
    y = "P Concentration (PPM)"
  )
boxplotPhosphorusT6BARE+boxplotPhosphorusT5BARE
# # ^^^^^ Looking at comparison
# 
# # comparing the differences between bare across treatment 1 &&& 4
anova(p_t5_bare_group_model, p_t6_bare_group_model)
# Model 1: Value ~ Surface * Date_Group
# Model 2: Value ~ Surface * Date_Group
#   Res.Df       RSS Df  Sum of Sq F Pr(>F)
# 1     20 0.0013555                       
# 2     20 0.0028458  0 -0.0014903 
#          
#
# 
# # /////////// crust GROUP modeling for PHOSPHORUS 




# Phos. BOX PLOT comparisons--------------------------------------------------------

boxplotPhosphorusT5CRUST <- ggplot(p_t5_crust_group_data,
                                   aes(x = Surface, y = Value, fill = Date_Group)) +
  geom_boxplot(position = position_dodge()) +
  theme_minimal() +
  labs(
    title = "T5 Phosphorus Concentration over CRUST positions",
    subtitle = "Week 5 vs Week 10",
    x = "Inoculated Positions",
    y = "P Concentration (PPM)"
  )
boxplotPhosphorusT5CRUST
boxplotPhosphorusT6CRUST <- ggplot(p_t6_crust_group_data,
                                   aes(x = Surface, y = Value, fill = Date_Group)) +
  geom_boxplot(position = position_dodge()) +
  theme_minimal() +
  labs(
    title = "Treatment 6 Phosphorus Concentration over CRUST positions",
    subtitle = "Week 5 vs Week 10",
    x = "Inoculated Positions",
    y = "P Concentration (PPM)"
  )
boxplotPhosphorusT6CRUST
boxplotPhosphorusT5CRUST+boxplotPhosphorusT6CRUST

# PhosT5 MAKING A DIFF BARE V CRUST BOXPLOT -------------------------------
p_t5_combined <- bind_rows(
  p_t5_bare_group_data %>% mutate(Group = "Bare"),
  p_t5_crust_group_data %>% mutate(Group = "Crust")
)


ggplot(p_t5_combined,
       aes(x = Group, y = Value, fill = Date_Group)) +
  geom_boxplot(position = position_dodge()) +
  coord_cartesian(ylim = c(0, NA)) +   # forces baseline at 0
  theme_minimal() +
  labs(
    title = "Treatment 5: Phosphorus (Crust vs Bare)",
    subtitle = "Week 5 vs Week 10",
    x = "Soil Type",
    y = "P Concentration (PPM)"
  )

p_t5_diff <- p_t5_combined %>%
  pivot_wider(names_from = Group, values_from = Value) %>%
  mutate(Difference = Crust - Bare)

ggplot(p_t5_diff,
       aes(x = Date_Group, y = Difference)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Treatment 5: Difference in Phosphorus (Crust - Bare)",
    y = "Difference in P (PPM)"
  )


p_t5_full <- bind_rows(
  p_t5_bare_group_data %>% mutate(Group = "Bare"),
  p_t5_crust_group_data %>% mutate(Group = "Crust")
)

p_t5_model <- aov(Value ~ Group * Date_Group, data = p_t5_full)
summary(p_t5_model)


ggplot(p_t5_full,
       aes(x = Group, y = Value, fill = Date_Group)) +
  geom_boxplot(position = position_dodge()) +
  expand_limits(y = 0) +
  theme_minimal()
# ggplot(p_t5_full,
#        aes(x = Group, y = Value, fill = Date_Group)) +
#   geom_boxplot(position = position_dodge()) +
#   expand_limits(y = 0) +   # safer than coord_cartesian here
#   theme_minimal()
# summary(p_t5_full$Value)
# range(p_t5_full$Value)

# p_t5_summary <- p_t5_full %>%
#   group_by(Date_Group, Group) %>%
#   summarise(mean_P = mean(Value, na.rm = TRUE), .groups = "drop") %>%
#   pivot_wider(names_from = Group, values_from = mean_P) %>%
#   mutate(Difference = Crust - Bare)
# 
# ggplot(p_t5_summary,
#        aes(x = Date_Group, y = Difference)) +
#   geom_col() +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   theme_minimal()
# 
p_t5_combined %>% count(Group, Date_Group)
summary(p_t5_full$Value)
sum(is.na(p_t5_full$Value))
sum(is.infinite(p_t5_full$Value))


# //// plotting differences
p_t5_summary <- p_t5_combined %>%
  group_by(Date_Group, Group) %>%
  summarise(mean_P = mean(Value, na.rm = TRUE), .groups = "drop")

p_t5_diff <- p_t5_summary %>%
  pivot_wider(names_from = Group, values_from = mean_P) %>%
  mutate(Difference = Crust - Bare)

ggplot(p_t5_diff,
       aes(x = Date_Group, y = Difference)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Treatment 5: Mean Difference in Phosphorus",
    subtitle = "Crust - Bare",
    y = "Difference in P (PPM)"
  )




#### Start of BOX PLOT comparisons -----------------------------------------------------
# 
boxplotPhosphorusT5CRUST <- ggplot(p_t5_crust_group_data,
                              aes(x = Surface, y = Value, fill = Date_Group)) +
  geom_boxplot(position = position_dodge()) +
  theme_minimal() +
  labs(
    title = "T5 Phosphorus Concentration over CRUST positions",
    subtitle = "Week 5 vs Week 10",
    x = "Inoculated Positions",
    y = "P Concentration (PPM)"
  )
boxplotPhosphorusT5CRUST
boxplotPhosphorusT6CRUST <- ggplot(p_t6_crust_group_data,
                              aes(x = Surface, y = Value, fill = Date_Group)) +
  geom_boxplot(position = position_dodge()) +
  theme_minimal() +
  labs(
    title = "Treatment 6 Phosphorus Concentration over CRUST positions",
    subtitle = "Week 5 vs Week 10",
    x = "Inoculated Positions",
    y = "P Concentration (PPM)"
  )
boxplotPhosphorusT6CRUST
boxplotPhosphorusT5CRUST+boxplotPhosphorusT6CRUST




#### Phos. MAKING A DIFF BARE V CRUST BOXPLOT FOR TREATMENT 5  --------------------------
p_t5_combined <- bind_rows(
  p_t5_bare_group_data %>% mutate(Group = "Bare"),
  p_t5_crust_group_data %>% mutate(Group = "Crust")
)
# p_t5_combined %>%
#   mutate(
#   Mound = ifelse(Surface == "Mound", 1, 0),
#   Trough = ifelse(Surface == "Trough", 1, 0)
#   )


# p_t5_combined <- p_t5_combined %>%
#   mutate(
#     Mound = case_when(
#       Surface %in% c("Mound") ~ "Mound"
#     )
#     Trough = case_when(
#       Surface %in% c("Trough") ~ "Trough"
#     )
#   )

# summary_df <- p_t5_combined %>%
#   group_by(Group) %>%
#   summarise(
#     mean_mound = mean(Surface, na.rm = TRUE),
#     mean_trough = mean(Trough, na.rm = TRUE)
#   ) %>%
#   mutate(diff = mean_mound - mean_trough) # Calculate the difference

# ///////////////////// printed and saved do not remove
ggplot(p_t5_combined,
       aes(x = Group, y = Value, fill = Date_Group)) +
  geom_boxplot(position = position_dodge()) +
  coord_cartesian(ylim = c(0, NA)) +   # forces baseline at 0
  #expand_limits(y = 0) +
  theme_minimal() +
  labs(
    title = "Treatment 5: Phosphorus (Crust vs Bare)",
    subtitle = "Week 5 vs Week 10",
    x = "Inoculation Type",
    y = "P Concentration (PPM)"
  )
# ////////////////////////////////////////////////////////////////////////

ggplot(p_t5_combined,
       aes(x = Group, y = Value, fill = Date_Group)) +
  geom_boxplot(position = position_dodge()) +
  coord_cartesian(ylim = c(0, NA)) +   # forces baseline at 0
  #expand_limits(y = 0) +
  theme_minimal() +
  labs(
    title = "Treatment 5: Phosphorus (Crust vs Bare)",
    subtitle = "Week 5 vs Week 10",
    x = "Inoculation Type",
    y = "P Concentration (PPM)"
  )

# p_t5_diff <- p_t5_combined %>%
#   # pivot_wider(names_from = Surface, values_from = Value) %>%
#   # mutate(Difference =   0 - Mound - Trough)
#   group

# ggplot(p_t5_diff,
#        aes(x = Group, y = 0 - Crust, fill = Date_Group)) +
#   geom_boxplot(position = position_dodge()) +
#   #expand_limits(y = 0) +
#   theme_minimal() +
#   labs(
#     title = "Treatment 5: Phosphorus (Crust vs Bare)",
#     subtitle = "Week 5 vs Week 10",
#     x = "Inoculation Type",
#     y = "P Concentration (PPM)"
#   )


# ggplot(p_t5_combined,
#        aes(x = Group, y = Difference, fill = Date_Group)) +
#   geom_boxplot() +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # Zero reference line
#   theme_minimal() +
#   labs(
#     title = "Treatment 5: Difference in Phosphorus (Crust - Bare)",
#     y = "Difference in P (PPM)"
#   )
# )

p_t5_model <- aov(Value ~ Group * Date_Group, data = p_t5_full)
summary(p_t5_model)




ggplot(p_t5_combined,
       aes(x = Group, y = Difference, fill = Surface)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Treatment 5: Mean Difference in Phosphorus",
    subtitle = "Crust - Bare",
    y = "Difference in P (PPM)"
  )


# ggplot(p_t5_full,
#        aes(x = Group, y = Value, fill = Date_Group)) +
#   geom_boxplot(position = position_dodge()) +
#   expand_limits(y = 0) +   # safer than coord_cartesian here
#   theme_minimal()
# summary(p_t5_full$Value)
# range(p_t5_full$Value)

# p_t5_summary <- p_t5_full %>%
#   group_by(Date_Group, Group) %>%
#   summarise(mean_P = mean(Value, na.rm = TRUE), .groups = "drop") %>%
#   pivot_wider(names_from = Group, values_from = mean_P) %>%
#   mutate(Difference = Crust - Bare)
# 
# ggplot(p_t5_summary,
#        aes(x = Date_Group, y = Difference)) +
#   geom_col() +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   theme_minimal()
# 
# p_t5_full %>% count(Group, Date_Group)
# summary(p_t5_full$Value)
# sum(is.na(p_t5_full$Value))
# sum(is.infinite(p_t5_full$Value))


# //// plotting differences
p_t5_summary <- p_t5_full %>%
  group_by(Date_Group, Group) %>%
  summarise(mean_P = mean(Value, na.rm = TRUE), .groups = "drop")

p_t5_diff <- p_t5_summary %>%
  pivot_wider(names_from = Group, values_from = mean_P) %>%
  mutate(Difference = Crust - Bare)

ggplot(p_t5_diff,
       aes(x = Date_Group, y = Difference)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Treatment 5: Mean Difference in Phosphorus",
    subtitle = "Crust - Bare",
    y = "Difference in P (PPM)"
  )

# ////////////  adjusted bar graphs
p_t5_full <- bind_rows(
  p_t5_bare_group_data %>% mutate(Group = "Bare"),
  p_t5_crust_group_data %>% mutate(Group = "Crust")
)

p_t5_summary <- p_t5_full %>%
  group_by(Group, Surface, Date_Group) %>%
  summarise(mean_P = mean(Value, na.rm = TRUE), .groups = "drop")

ggplot(p_t5_summary,
       aes(x = Surface, y = mean_P, fill = Date_Group)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~ Group) +
  expand_limits(y = 0) +
  theme_minimal() +
  labs(
    title = "Treatment 5: Phosphorus by Inoculation Type and Surface Position",
    x = "Surface Position Type",
    y = "Average P (PPM)"
  )

# WOAHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH
# WOAHHHHHHHHHHHHHHHHHHHHHHHH
# ////////// closest

# 4/16 excel-like plotting attempt ----------------------------------------------

p_t5_excel_avg_comparisons <- p_t5_combined %>%
  group_by(Bin_Number, Inoculation, Surface,Date_Group) %>%
  summarise(
            mean_P = mean(Value, na.rm = TRUE),
            sd_P = sd(Value, na.rm = TRUE),
            n = n(),
            .groups = "drop")
p_t5_excel_avg_comparisons %>% count(Bin_Number, Inoculation, Surface)
# gives us
# mean phosphorus per group
# standard deviation (for error bars later)
# sample size


p_diff_raw <- p_t5_excel_avg_comparisons %>%
  select(Bin_Number, Inoculation, Surface, mean_P) %>%
  pivot_wider(names_from = Surface, values_from = mean_P) %>%
  mutate(diff_M_T = Mound - Trough)


p_diff_summary <- p_diff_raw %>%
  group_by(Bin_Number, Inoculation) %>%
  summarise(
    mean_diff = mean(diff_M_T, na.rm = TRUE),
    sd_diff   = sd(diff_M_T, na.rm = TRUE),
    n         = sum(!is.na(diff_M_T)),
    se_diff   = sd_diff / sqrt(n),
    .groups = "drop"
  )

ggplot(p_diff_summary,
       aes(x = Inoculation, y = mean_diff, fill = Inoculation)) +
  geom_col(position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(ymin = mean_diff - se_diff,
                    ymax = mean_diff + se_diff),
                width = 0.2,
                position = position_dodge(width = 0.7)) +
  facet_wrap(~ Bin_Number) +
  geom_errorbar(aes(ymin = mean_diff - se_diff,
                    ymax = mean_diff + se_diff))+
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Mound − Trough (P difference)") +
  theme_minimal()

#### FIGURE: pt5 error bars ---------------
ggplot(p_t5_excel_avg_comparisons,
       aes(x = Surface, y = mean_P, fill = Date_Group)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_P - sd_P,
                    ymax = mean_P + sd_P),
                position = position_dodge(0.9),
                width = 0.2) +
  facet_wrap(~ Inoculation) +
  labs(title = "Treatment 5 Phosphorus Concentration",
       y = "Phosphorus Concentration",
       x = "Surface Texture Type") +
  theme_minimal()


#collapsed version across Bin_Number
p_t5_overall <- p_t5_combined %>%
  group_by(Inoculation, Surface) %>%
  summarise(
    mean_P = mean(Value, na.rm = TRUE),
    sd_P   = sd(Value, na.rm = TRUE),
    n      = n(),
    se_P   = sd_P / sqrt(n),
    .groups = "drop"
  )

ggplot(p_t5_overall,
       aes(x = Surface, y = mean_P, fill = Inoculation)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_P - se_P,
                    ymax = mean_P + se_P),
                position = position_dodge(0.9),
                width = 0.2) +
  theme_minimal()


# y-axis representing Mound − Trough
p_diff_raw <- p_t5_excel_avg_comparisons %>%
  select(Bin_Number, Inoculation, Surface, mean_P) %>%
  pivot_wider(names_from = Surface, values_from = mean_P) %>%
  mutate(diff_M_T = Mound - Trough)

p_diff_summary <- p_diff_raw %>%
  group_by(Inoculation) %>%
  summarise(
    mean_diff = mean(diff_M_T, na.rm = TRUE),
    sd_diff   = sd(diff_M_T, na.rm = TRUE),
    n         = sum(!is.na(diff_M_T)),
    se_diff   = sd_diff / sqrt(n),
    .groups = "drop"
  )
ggplot(p_diff_summary,
       aes(x = Inoculation, y = mean_diff, fill = Inoculation)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_diff - se_diff,
                    ymax = mean_diff + se_diff),
                width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(0, NA) +
  labs(y = "Mound − Trough (P difference)") +
  theme_minimal()


# summary_diff <- diff_df %>%
#   group_by(Inoculation) %>%
#   summarise(
#     mean_diff = mean(diff_M_T, na.rm = TRUE),
#     sd_diff   = sd(diff_M_T, na.rm = TRUE),
#     n         = n(),
#     se_diff   = sd_diff / sqrt(n)
#   )
# 
# ggplot(summary_diff, aes(x = Inoculation, y = mean_diff)) +
#   geom_point(size = 3) +
#   geom_errorbar(aes(ymin = mean_diff - se_diff,
#                     ymax = mean_diff + se_diff),
#                 width = 0.2) +
#   theme_minimal() +
#   labs(
#     y = "Mound − Trough mean P difference",
#     x = "Treatment"
#   )

ggplot(summary_diff,
       aes(x = Surface, y = mean_diff, fill = Inoculation)) +
  geom_col(width=0.5, position = position_dodge()) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # Zero reference line
  expand_limits(y = 0) +
  theme_minimal() +
  labs(
    title = "Treatment 5: Phosphorus by Inoculation Type and Surface Position",
    x = "Surface Position Type",
    y = "Difference between Avg P levels and group"
  )
# Positive value → Mound > Trough
# Negative value → Trough > Mound
# This removes absolute treatment effects and isolates microtopography effect






##### NO-Figure: P_T5 inoculation/surface Comparison --------------------------------
ggplot(p_t5_combined,
       aes(x = Surface, y = mean(Value,) - Value , fill = Date_Group)) +
  geom_col(position = position_dodge()) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # Zero reference line
  facet_wrap(~ Group) +
  expand_limits(y = 0) +
  theme_minimal() +
  labs(
    title = "Treatment 5: Phosphorus by Inoculation Type and Surface Position",
    x = "Surface Position Type",
    y = "Difference between Avg P levels and group"
  )




# ///////// CLOSER BUT NOT YET
ggplot(p_t5_combined,
       aes(x = Surface, y = mean(Value,) - Value , fill = Date_Group)) +
  geom_col(position = position_dodge()) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # Zero reference line
  facet_wrap(~ Group) +
  expand_limits(y = 0) +
  theme_minimal() +
  labs(
    title = "Treatment 5: Phosphorus by Inoculation Type and Surface Position",
    x = "Surface Position Type",
  )

##### Compute differences plot --------------
# ai generated, helping me out </3

diff_data <- p_t5_combined %>%
  group_by(Group, Date_Group, Surface) %>%
  summarise(mean_P = mean(Value, na.rm = TRUE), .groups = "drop")


diff_data <- diff_data %>%
  pivot_wider(names_from = Surface, values_from = mean_P) %>%
  mutate(diff = Mound - Trough)   # or Trough - Mound


##### PRINTED Figure: Actual difference Mounds - Troughs Phosphorus ----------------
ggplot(diff_data,
       aes(x = Date_Group, y = diff, fill = Date_Group)) +
  geom_col() +
  facet_wrap(~ Group) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(
    title = "Treatment 5: Bare vs Crust Phosphorus Difference",
    x = "Date Group",
    y = "Mean P Difference (Mound - Trough)"
  )

# Positive values (> 0): Mound has higher phosphorus than Trough
# Negative values (< 0): Trough has higher phosphorus than Mound
# 0: no difference


# Figure: Trough - Mound differences of phosphorus
diff_data <- diff_data %>%
  pivot_wider(names_from = Surface, values_from = mean_P) %>%
  mutate(diff = Trough - Mound )

ggplot(diff_data,
       aes(x = Date_Group, y = diff, fill = Date_Group)) +
  geom_col() +
  facet_wrap(~ Group) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(
    title = "Treatment 5: Mound vs Trough Phosphorus Difference",
    x = "Date Group",
    y = "Mean P Difference (Mound - Trough)"
  )



# continuing to modify above, aes

# when mound - trough
diff_data <- p_t5_combined %>%
  group_by(Group, Date_Group, Surface) %>%
  summarise(mean_P = mean(Value, na.rm = TRUE), .groups = "drop")
diff_data <- diff_data %>%
  pivot_wider(names_from = Surface, values_from = mean_P) %>%
  mutate(diff = Mound - Trough)   # or Trough - Mound

ggplot(diff_data,
       aes(x = Date_Group, y = diff)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~ Group) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(
    title = "Treatment 5: Mound vs Trough Phosphorus Difference",
    x = "Date Group",
    y = "Mean P Difference (Mound - Trough)"
  )
# " interpretation from ai  "
# Positive values (> 0): Mound has higher phosphorus than Trough
# Negative values (< 0): Trough has higher phosphorus than Mound
# 0: no difference

##### FIGURE: t5 avgP difference Mound - Trough ------------------------------------
# when trough - mound
diff_data <- p_t5_combined %>%
  group_by(Group, Date_Group, Surface) %>%
  summarise(mean_P = mean(Value, na.rm = TRUE), .groups = "drop")
# find avg of each tray here ^
diff_data <- diff_data %>%
  pivot_wider(names_from = Surface, values_from = mean_P) %>%
  mutate(diff =  Trough - Mound)   # or Trough - Mound

ggplot(diff_data,
       aes(x = Date_Group, y = diff)) +
  geom_col() + 
  facet_wrap(~ Group) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(
    title = "Treatment 5: Trough vs Mound Phosphorus Difference",
    x = "Date Group",
    y = "Mean P Difference (Trough vs Mound)"
  )
# Positive values (> 0): Trough has higher phosphorus than Mound
# Negative values (< 0): Mound has higher phosphorus than Trough
# 0: no difference

##### FIGURE: t5 avgP difference Trough - Mound ------------------------------------
# when trough - mound
diff_data <- p_t5_combined %>%
  group_by(Group, Date_Group, Surface) %>%
  summarise(mean_P = mean(Value, na.rm = TRUE), .groups = "drop")
diff_data <- diff_data %>%
  pivot_wider(names_from = Surface, values_from = mean_P) %>%
  mutate(diff = Trough - Mound) 


# # ^^^^^ Looking at comparison
# 
# # comparing the differences between bare across treatment 1 &&& 4
# anova(t1_p_crust_group_model, t4_p_crust_group_model)
# # Model 1: Value ~ Surface * Date_Group
# # Model 2: Value ~ Surface * Date_Group
# #   Res.Df       RSS Df Sum of Sq F Pr(>F)
# # 1     20 0.0021109                      
# # 2     20 0.0309056  0 -0.028795  
# # Potential interpretations:::::::
# # in terms of phosphorus contents in uncolonized soils
# #   there seemed to have a bigger effect on phosphorus 
# #   due to surface in treatment control
# 
# 
# #comparing directly bare vs. crust t1 PHOSPHORUS
# anova(t1_p_bare_group_model, t1_p_crust_group_model)
# # Model 1: Value ~ Surface * Date_Group
# # Model 2: Value ~ Surface * Date_Group
# #   Res.Df       RSS Df Sum of Sq F Pr(>F)
# # 1     20 0.0072796                      
# # 2     20 0.0021109  0 0.0051687 
# 
# 
# #comparing directly bare vs. crust t4 PHOSPHORUS
# anova(t4_p_bare_group_model, t4_p_crust_group_model)
# # Model 1: Value ~ Surface * Date_Group
# # Model 2: Value ~ Surface * Date_Group
# #   Res.Df       RSS Df Sum of Sq F Pr(>F)
# # 1     20 0.0043754                      
# # 2     20 0.0309056  0  -0.02653 
# 
# anova(all_bare_group_model, all_crust_group_model)
# # Model 1: Value ~ Surface * Date_Group
# # Model 2: Value ~ Surface * Date_Group
# #   Res.Df    RSS Df Sum of Sq F Pr(>F)
# # 1   1004 9658.9                      
# # 2   1004 7969.2  0    1689.7  




# ALUMINUM bare group modeling ---------------------------------------------

# looking at effects on aluminum across ALL treatments
al_all_data <- MiddleFinalResults %>%
  filter(Element == "Al_mean")
al_all_model <- aov(Value ~ Surface * Inoculation * Date_Group,
                    data = al_all_data)
summary(al_all_model)
#                                 Df Sum Sq Mean Sq F value   Pr(>F)    
# Surface                          2   46.1  23.069   7.453 0.000672 ***
# Inoculation                      1    0.8   0.800   0.258 0.611467    
# Date_Group                       1    3.3   3.331   1.076 0.300237    
# Surface:Inoculation              1    4.0   3.981   1.286 0.257525    
# Surface:Date_Group               2    0.2   0.102   0.033 0.967664    
# Inoculation:Date_Group           1    0.0   0.033   0.011 0.918391    
# Surface:Inoculation:Date_Group   1    0.8   0.847   0.274 0.601115    
# Residuals                      367 1136.0   3.095                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## INTERPRETATION, all treatments surface shows big sigfig on value of alumimum found


# not relevant 
# Looking at ALL Treatments with DEPRESSIONS
# al_allSurface_data <- MiddleFinalResults %>%
#   filter(Element == "Al_mean",
#          Position %in% c(4, 5, 6))
# al_allSurface_model <- aov(Value ~ Inoculation * Date_Group,
#                     data = al_allSurface_data)
# summary(al_allSurface_model)

# al_allInoculation_data <- MiddleFinalResults %>%
#   filter(Element == "Al_mean",
#          Position %in% c(2, 3, 5))
# al_allInoculation_model <- aov(Value ~ Surface * Date_Group,
#                            data = al_allInoculation_data)
# summary(al_allInoculation_model)
#                     Df Sum Sq Mean Sq F value  Pr(>F)   
# Surface              1   18.4  18.424   6.981 0.00932 **
# Date_Group           1    0.1   0.095   0.036 0.84971   
# Surface:Date_Group   1    0.1   0.050   0.019 0.89038   
# Residuals          122  322.0   2.639                   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##### not relevant 


# ALUMINUM TREATMENT 4 BARE -----------------------------------------------
al_t4_bare_group_data <- t4_bare_group_data %>%
  filter(Element == "Al_mean",
         Treatment_Number =="4")
summary(al_t4_bare_group_data)
# ... ANOVA
al_t4_bare_group_model <- aov(Value ~ Surface * Date_Group,
                             data = al_t4_bare_group_data)
summary(al_t4_bare_group_model)
#                    Df Sum Sq Mean Sq F value Pr(>F)  
# Surface             1  33.95   33.95   6.868 0.0164 *
# Date_Group          1  13.07   13.07   2.643 0.1196  
# Surface:Date_Group  1   3.03    3.03   0.612 0.4432  
# Residuals          20  98.87    4.94                 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
TukeyHSD(al_t4_bare_group_model, "Surface")
# $Surface
#                   diff       lwr        upr     p adj
# Trough-Mound -2.378663 -4.272052 -0.4852727 0.0163799
### INTERPRETATION: in t4, there was more aluminum MOUNDS > TROUGHS


# //////////// 
# T5: INOCULUM & DEPRESSIONS
## ALUMINUM TREATMENT 5 BARE -------------------------------------
al_t5_bare_group_data <- t5_bare_group_data %>%
  filter(Element == "Al_mean",
         Treatment_Number =="5")
summary(al_t5_bare_group_data)
# ... ANOVA
al_t5_bare_group_model <- aov(Value ~ Surface * Date_Group,
                             data = al_t5_bare_group_data)
summary(al_t5_bare_group_model)
# Df Sum Sq Mean Sq F value Pr(>F)
# Surface             1   0.69   0.691   0.149  0.703
# Date_Group          1   9.64   9.636   2.086  0.164
# Surface:Date_Group  1   2.65   2.653   0.574  0.457
# Residuals          20  92.40   4.620  
# INTERPRETATION: no significant change

# 
# T6 - DEPRESSIONS ONLY // 
# TREATMENT 6 Aluminum BARE -------------------------------------
al_t6_bare_group_data <- t6_bare_group_data %>%
  filter(Element == "Al_mean")
summary(al_t6_bare_group_data)

al_t6_bare_group_model <- aov(Value ~ Surface * Date_Group,
                             data = al_t6_bare_group_data)
summary(al_t6_bare_group_model)
#                    Df Sum Sq Mean Sq F value Pr(>F)
# Surface             1   0.80   0.805   0.163  0.691
# Date_Group          1   1.80   1.801   0.364  0.553
# Surface:Date_Group  1   0.14   0.139   0.028  0.868
# Residuals          20  99.00   4.950                    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1     
# ## INTERPRETATIONS:Surface has no have an effect on aluminum levels in Control
# 


############## csv sheets of each element of Treatment 5=========================
# P, Al, K, Ca, S

##### MODEL OF REPEAT ------
# p_t5_combined <- bind_rows(
#   p_t5_bare_group_data %>% mutate(Group = "Bare"),
#   p_t5_crust_group_data %>% mutate(Group = "Crust")
# )
# 
# p_t5_bare_group_data <- t5_bare_group_data %>%
#   filter(Element == "P_mean",
#          Treatment_Number =="5")


# CSV aluminum ===================================================================
al_t5_bare_group_data <- t5_bare_group_data %>%
  filter(Element == "Al_mean",
         Treatment_Number =="5")
al_t5_crust_group_data <- t5_crust_group_data %>%
  filter(Element == "Al_mean",
         Treatment_Number =="5")

al_t5_combined <- bind_rows(
  al_t5_bare_group_data %>% mutate(Group = "Bare"),
  al_t5_crust_group_data %>% mutate(Group = "Crust")
)

write.csv(al_t5_combined, "al_t5_combined.csv")



# CSV Potassium     ===================================================================
k_t5_bare_group_data <- t5_bare_group_data %>%
  filter(Element == "K_mean",
         Treatment_Number =="5")
al_t5_crust_group_data <- t5_crust_group_data %>%
  filter(Element == "K_mean",
         Treatment_Number =="5")

al_t5_combined <- bind_rows(
  al_t5_bare_group_data %>% mutate(Group = "Bare"),
  al_t5_crust_group_data %>% mutate(Group = "Crust")
)

#write.csv(al_t5_combined, "al_t5_combined.csv")

# CSV Iron ===================================================================
fe_t5_bare_group_data <- t5_bare_group_data %>%
  filter(Element == "Fe_mean",
         Treatment_Number =="5")
fe_t5_crust_group_data <- t5_crust_group_data %>%
  filter(Element == "Fe_mean",
         Treatment_Number =="5")

fe_t5_combined <- bind_rows(
  fe_t5_bare_group_data %>% mutate(Group = "Bare"),
  fe_t5_crust_group_data %>% mutate(Group = "Crust")
)

#write.csv(fe_t5_combined, "fe_t5_combined.csv")

# CSV Calcium
ca_t5_bare_group_data <- t5_bare_group_data %>%
  filter(Element == "Ca_mean",
         Treatment_Number =="5")
al_t5_crust_group_data <- t5_crust_group_data %>%
  filter(Element == "Al_mean",
         Treatment_Number =="5")

al_t5_combined <- bind_rows(
  al_t5_bare_group_data %>% mutate(Group = "Bare"),
  al_t5_crust_group_data %>% mutate(Group = "Crust")
)

write.csv(al_t5_combined, "al_t5_combined.csv")