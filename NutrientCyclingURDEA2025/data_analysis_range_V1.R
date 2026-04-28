library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(patchwork)

# Importing data from excel -------------------
excel_sheet <- read_excel("TheEntireDataSet.xlsx",
                          sheet = "labeled")
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

# Filtering -------------
# remove unnecessary columns
 cleanedResults <- cleanedResults %>% select(-"File #",
      -"Operator", -"Name", -"ID", -"Field1", -"Field2",
      -"test", -"Application", -"Method", -"ElapsedTime", -"Alloy 1",
      -"Match Qual 1", -"Alloy 2", -"Match Qual 2", -"Alloy 3", -"Match Qual 3")
 
# alternate filtering, including date_group 
filteredResults <- cleanedResults %>%
  select(Date_Group, Bin_Number, Position, P, Al, Fe, S, Ca)

# changing n/a & <LOD to 0 across measurements
    # changing columns to numeric
filteredResults <- filteredResults %>% 
  mutate(across(-c(Bin_Number, Position, Date_Group),
                ~ as.numeric(as.character(.))))
    # replacing N/A to the number 0
filteredResults <- filteredResults %>%
  mutate(across(P:Ca, ~replace_na(.x, 0)))

head(filteredResults)

# averaging results, chatGPT generated, needed help on this one
averagedResults <- filteredResults %>%
  group_by(Date_Group, Bin_Number, Position) %>%
  summarise(
    P_mean  = mean(P,  na.rm = TRUE),
    Al_mean = mean(Al, na.rm = TRUE),
    Fe_mean = mean(Fe, na.rm = TRUE),
    S_mean  = mean(S, na.rm  = TRUE),
    Ca_mean = mean(Ca, na.rm = TRUE),
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
      Position %in% c(1,2,5,6) ~ "Non_inoculated",
      Position %in% c(3,4,7,8) ~ "Inoculated",
      Position == 9 ~ "Bridge" 
      # calling it bridge/center b/c of the transfer of nutrients
      
    ),
    
    InoculationTreatmentType = case_when(
      Treatment_Number %in% c(2, 3, 4, 5)  ~ "Biocrust_inoculated",
      Treatment_Number %in% c(1, 6, 7) ~ "Uncolonized_soil"
    ),
    
    SurfaceTreatmentType = case_when(
      Treatment_Number %in% c(1, 2, 3, 7) ~ "Flat_Surfaces",
      Treatment_Number %in% c(4, 5, 6) ~ "Textured_Surfaces"
    )
  ) 



# graphing begins !!
# Creating Sub Datasets -------------------------------------------------------
# separating date_groups into different datasets

RangeResults <- longAverages %>%
  filter(Date_Group %in% c("Week_1", "Week_10"))

# Range = Week10 - Week1
  # all treatments of phosphorus
p_all_anova_data <- RangeResults %>%
  filter(Element == "P_mean")
  # T1:control - phosphorus value 
p_t1_anova_data <- RangeResults %>%
  filter(Element == "P_mean",
         Treatment_Number =="1")
  # T4:3_factors - phosphorus value 
p_t4_anova_data <- RangeResults %>%
  filter(Element == "P_mean",
         Treatment_Number =="4")
p_t5_anova_data <- RangeResults %>%
  filter(Element == "P_mean",
         Treatment_Number =="5")

# all treatments of phosphorus
al_all_anova_data <- RangeResults %>%
  filter(Element == "Al_mean")
  # T1:control - Aluminum value 
al_t1_anova_data <- RangeResults %>%
  filter(Element == "P_mean",
         Treatment_Number =="1")
  # T4:3_factors - Aluminum value 
al_t4_anova_data <- RangeResults %>%
  filter(Element == "Al_mean",
         Treatment_Number =="4")

# CALCIUM DATA SETS 
ca_all_anova_data <- RangeResults %>%
  filter(Element == "Ca_mean")
# treatment 4
ca_t4_anova_data <- RangeResults %>%
  filter(Element == "Ca_mean",
         Treatment_Number == "4")





#testing out models
# Phosphorus Modeling ------------------------------------------------------------
## Phosphorus All Treatments Modeling -----------------------------------------
# phosphorus additive model! OF ALL TREATMENTS
p_all_full_model <- aov(Value ~ Date_Group + Inoculation + Surface, 
                        data = p_all_anova_data)
summary(p_all_full_model)
#              Df  Sum Sq Mean Sq F value   Pr(>F)    
# Date_Group    1 0.10494 0.10494 255.560  < 2e-16 ***
# Inoculation   2 0.00722 0.00361   8.789 0.000186 ***
# Surface       1 0.00011 0.00011   0.275 0.600606    
# Residuals   372 0.15275 0.00041                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# Let's look at interactions and see if above sigfig is still relevant?
p_all_interactive_model <- aov(Value ~ Date_Group * Inoculation * Surface, 
                               data = p_all_anova_data)
summary(p_all_interactive_model)
#                                 Df  Sum Sq Mean Sq F value   Pr(>F)    
# Date_Group                       1 0.10494 0.10494 255.270  < 2e-16 ***
# Inoculation                      2 0.00722 0.00361   8.779 0.000189 ***
# Surface                          1 0.00011 0.00011   0.274 0.600817    
# Date_Group:Inoculation           2 0.00128 0.00064   1.555 0.212571    
# Date_Group:Surface               1 0.00054 0.00054   1.320 0.251284    
# Inoculation:Surface              1 0.00004 0.00004   0.097 0.755662    
# Date_Group:Inoculation:Surface   1 0.00002 0.00002   0.050 0.822623    
# Residuals                      367 0.15087 0.00041                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

### OKay still date/inoculum still relevant! but their interaction is not?
# Weird! lets get into inoculum then
# creating reduced
reduced_interact_p_model <- aov(Value ~ Date_Group * Inoculation, 
                                data = p_all_anova_data)
summary(reduced_interact_p_model)
#                         Df  Sum Sq Mean Sq F value   Pr(>F)    
# Date_Group               1 0.10494 0.10494 256.837  < 2e-16 ***
# Inoculation              2 0.00722 0.00361   8.832 0.000179 ***
# Date_Group:Inoculation   2 0.00128 0.00064   1.567 0.209988    
# Residuals              371 0.15159 0.00041                     


# interaction plot
interaction.plot(p_all_anova_data$Inoculation,
                p_all_anova_data$Date_Group,
                 p_all_anova_data$Value)
# ggplot interaction plot
ggplot(p_all_anova_data, aes(x = Inoculation, y = Value, 
                             color = Date_Group, group = Date_Group)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun = mean, geom = "point") +
labs(title = "Phosphorus & Inoculation Types Across All Treatments", 
     subtitle = "Interaction Plot",
     x = "Inoculation Type",
     y = "P Concentration (PPM)")

# this graph shows that there is no interaction between the groups
# therefore an additive model will be most relevant to add
# can include this as

#still relevant

TukeyHSD(reduced_interact_p_model, "Inoculation")
#   Tukey multiple comparisons of means
#     95% family-wise confidence level
# 
# Fit: aov(formula = Value ~ Date_Group * Inoculation, data = p_all_anova_data)
# 
# $Inoculation
#                                   diff          lwr          upr     p adj
# Inoculated-Bridge          0.005249603 -0.002956232  0.013455438 0.2895292
# Non_inoculated-Bridge     -0.004024348 -0.012235096  0.004186399 0.4819435
# Non_inoculated-Inoculated -0.009273951 -0.014471541 -0.004076362 0.0000996

AIC(p_all_full_model, reduced_interact_p_model)

          # make tukey for date:group

# OKay I dont think I understadn this but maybe This seems pretty vague?
# Maybe if we compare one treatment to another?
# Instead of all treatments?

## Phosphorus Treatment 1 -----------------------------------------------------
# Look at Treatment 1:Control creating a full additive model for phosphorus
p_t1_full_model <- aov(Value ~ Date_Group + Inoculation + Surface, data = p_t1_anova_data)
summary(p_t1_full_model)
#             Df   Sum Sq  Mean Sq F value   Pr(>F)    
# Date_Group   1 0.007872 0.007872  82.361 4.55e-12 ***
# Inoculation  2 0.000589 0.000295   3.082   0.0549 .  
# Surface      1 0.000012 0.000012   0.126   0.7246    
# Residuals   49 0.004684 0.000096    



## Phosphorus Treatment 4 ----------------------------------------------------
# Look at Treatment 4:3_Factors
p_t4_full_model <- aov(Value ~ Date_Group + Inoculation + Surface, data = p_t4_anova_data)
summary(p_t4_full_model)
#             Df  Sum Sq Mean Sq F value   Pr(>F)    
# Date_Group   1 0.04303 0.04303  68.466 7.29e-11 ***
# Inoculation  2 0.00843 0.00421   6.704  0.00267 ** 
# Surface      1 0.00002 0.00002   0.027  0.86988    
# Residuals   49 0.03079 0.00063                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#INTERACTION MODEL 
p_t4_interact_model <- aov(Value ~ Date_Group * Inoculation * Surface, data = p_t4_anova_data)
summary(p_t4_interact_model)
#                                Df  Sum Sq Mean Sq F value   Pr(>F)    
# Date_Group                      1 0.04303 0.04303  71.643 8.78e-11 ***
# Inoculation                     2 0.00843 0.00421   7.015  0.00227 ** 
# Surface                         1 0.00002 0.00002   0.028  0.86701    
# Date_Group:Inoculation          2 0.00433 0.00217   3.606  0.03545 *  
# Date_Group:Surface              1 0.00002 0.00002   0.037  0.84755    
# Inoculation:Surface             1 0.00001 0.00001   0.023  0.88023    
# Date_Group:Inoculation:Surface  1 0.00000 0.00000   0.001  0.98084    
# Residuals                      44 0.02643 0.00060                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Reduced model!Of treatment 4 of phosphorus
reduced_interact_p_t4_model <- aov(Value ~ Date_Group * Inoculation, data = p_t4_anova_data)
summary(reduced_interact_p_t4_model)
#                        Df  Sum Sq Mean Sq F value   Pr(>F)    
# Date_Group              1 0.04303 0.04303  77.998 1.26e-11 ***
# Inoculation             2 0.00843 0.00421   7.637  0.00132 ** 
# Date_Group:Inoculation  2 0.00433 0.00217   3.926  0.02635 *  
# Residuals              48 0.02648 0.00055

# looking at tukey of the dategroup:inoculation
TukeyHSD(reduced_interact_p_t4_model, "Date_Group:Inoculation")
#   Tukey multiple comparisons of means
#     95% family-wise confidence level
# 
# Fit: aov(formula = Value ~ Date_Group * Inoculation, data = p_t4_anova_data)
# 
# $`Date_Group:Inoculation`
#                                                      diff          lwr           upr     p adj
# Week_10:Bridge-Week_1:Bridge                  0.041650000 -0.015266712  0.0985667122 0.2697325
# Week_1:Inoculated-Week_1:Bridge              -0.003111111 -0.048107723  0.0418855008 0.9999464
# Week_10:Inoculated-Week_1:Bridge              0.073366667  0.028370055  0.1183632785 0.0001934
# Week_1:Non_inoculated-Week_1:Bridge          -0.011429167 -0.056425779  0.0335674452 0.9737841
# Week_10:Non_inoculated-Week_1:Bridge          0.028706944 -0.016289667  0.0737035563 0.4185413
# Week_1:Inoculated-Week_10:Bridge             -0.044761111 -0.089757723  0.0002355008 0.0519408
# Week_10:Inoculated-Week_10:Bridge             0.031716667 -0.013279945  0.0767132785 0.3086935
# Week_1:Non_inoculated-Week_10:Bridge         -0.053079167 -0.098075779 -0.0080825548 0.0122412
# Week_10:Non_inoculated-Week_10:Bridge        -0.012943056 -0.057939667  0.0320535563 0.9554699
# Week_10:Inoculated-Week_1:Inoculated          0.076477778  0.048019422  0.1049361339 0.0000000
# Week_1:Non_inoculated-Week_1:Inoculated      -0.008318056 -0.036776412  0.0201403005 0.9524041
# Week_10:Non_inoculated-Week_1:Inoculated      0.031818056  0.003359699  0.0602764117 0.0202410
# Week_1:Non_inoculated-Week_10:Inoculated     -0.084795833 -0.113254189 -0.0563374772 0.0000000
# Week_10:Non_inoculated-Week_10:Inoculated    -0.044659722 -0.073118078 -0.0162013661 0.0003521
# Week_10:Non_inoculated-Week_1:Non_inoculated  0.040136111  0.011677755  0.0685944672 0.0015960

### [] potential interpretations
# Week_1:Non_inoculated-Week_10:Inoculated     -0.084795833 -0.113254189 -0.0563374772 0.0000000
# signif adj p = 0.0, week 10 inoculated had a bigger effect than week 1 uninoculated
  # as expected and would make sense

# Week_10:Inoculated-Week_1:Inoculated          0.076477778  0.048019422  0.1049361339 0.0000000
# signif adj p = 0.0 ,  both INOCULATED: week10 > week 1, 
  # 

#phosphorus t4 boxplot
ggplot(p_t4_anova_data,
       aes(x = Inoculation, y = Value, fill = Date_Group)) +
  geom_boxplot(position = position_dodge()) +
  theme_minimal() +
  labs(
    title = "Treatment 4 Phosphorus Concentration",
    subtitle = "Week 1 vs Week 10",
    x = "Inoculum Type", 
    y = "P Concentration (PPM)" 
  )

# p_t4_surface_boxpot <- ggplot(p_t4_data,
#        aes(x = Surface, y = Value, fill = Date_Group)) +
#   geom_boxplot() +
#   facet_wrap(~ Element, scales = "free") +
#   labs(title = "Treatment 4: Phosphrous Observations")+
#   theme_minimal()


# Look at Treatment 4:3_Factors
p_t4_full_model <- aov(Value ~ Date_Group + Inoculation + Surface, data = p_t4_anova_data)
summary(p_t4_full_model)
#             Df  Sum Sq Mean Sq F value   Pr(>F)    
# Date_Group   1 0.04303 0.04303  68.466 7.29e-11 ***
# Inoculation  2 0.00843 0.00421   6.704  0.00267 ** 
# Surface      1 0.00002 0.00002   0.027  0.86988    
# Residuals   49 0.03079 0.00063                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#INTERACTION MODEL 
p_t4_interact_model <- aov(Value ~ Date_Group * Inoculation * Surface, data = p_t4_anova_data)
summary(p_t4_interact_model)
#                                Df  Sum Sq Mean Sq F value   Pr(>F)    
# Date_Group                      1 0.04303 0.04303  71.643 8.78e-11 ***
# Inoculation                     2 0.00843 0.00421   7.015  0.00227 ** 
# Surface                         1 0.00002 0.00002   0.028  0.86701    
# Date_Group:Inoculation          2 0.00433 0.00217   3.606  0.03545 *  
# Date_Group:Surface              1 0.00002 0.00002   0.037  0.84755    
# Inoculation:Surface             1 0.00001 0.00001   0.023  0.88023    
# Date_Group:Inoculation:Surface  1 0.00000 0.00000   0.001  0.98084    
# Residuals                      44 0.02643 0.00060                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Reduced model!Of treatment 4 of phosphorus
reduced_interact_p_t4_model <- aov(Value ~ Date_Group * Inoculation, data = p_t4_anova_data)
summary(reduced_interact_p_t4_model)
#                        Df  Sum Sq Mean Sq F value   Pr(>F)    
# Date_Group              1 0.04303 0.04303  77.998 1.26e-11 ***
# Inoculation             2 0.00843 0.00421   7.637  0.00132 ** 
# Date_Group:Inoculation  2 0.00433 0.00217   3.926  0.02635 *  
# Residuals              48 0.02648 0.00055

# looking at tukey of the dategroup:inoculation
TukeyHSD(reduced_interact_p_t4_model, "Date_Group:Inoculation")
#   Tukey multiple comparisons of means
#     95% family-wise confidence level
# 
# Fit: aov(formula = Value ~ Date_Group * Inoculation, data = p_t4_anova_data)
# 
# $`Date_Group:Inoculation`
#                                                      diff          lwr           upr     p adj
# Week_10:Bridge-Week_1:Bridge                  0.041650000 -0.015266712  0.0985667122 0.2697325
# Week_1:Inoculated-Week_1:Bridge              -0.003111111 -0.048107723  0.0418855008 0.9999464
# Week_10:Inoculated-Week_1:Bridge              0.073366667  0.028370055  0.1183632785 0.0001934
# Week_1:Non_inoculated-Week_1:Bridge          -0.011429167 -0.056425779  0.0335674452 0.9737841
# Week_10:Non_inoculated-Week_1:Bridge          0.028706944 -0.016289667  0.0737035563 0.4185413
# Week_1:Inoculated-Week_10:Bridge             -0.044761111 -0.089757723  0.0002355008 0.0519408
# Week_10:Inoculated-Week_10:Bridge             0.031716667 -0.013279945  0.0767132785 0.3086935
# Week_1:Non_inoculated-Week_10:Bridge         -0.053079167 -0.098075779 -0.0080825548 0.0122412
# Week_10:Non_inoculated-Week_10:Bridge        -0.012943056 -0.057939667  0.0320535563 0.9554699
# Week_10:Inoculated-Week_1:Inoculated          0.076477778  0.048019422  0.1049361339 0.0000000
# Week_1:Non_inoculated-Week_1:Inoculated      -0.008318056 -0.036776412  0.0201403005 0.9524041
# Week_10:Non_inoculated-Week_1:Inoculated      0.031818056  0.003359699  0.0602764117 0.0202410
# Week_1:Non_inoculated-Week_10:Inoculated     -0.084795833 -0.113254189 -0.0563374772 0.0000000
# Week_10:Non_inoculated-Week_10:Inoculated    -0.044659722 -0.073118078 -0.0162013661 0.0003521
# Week_10:Non_inoculated-Week_1:Non_inoculated  0.040136111  0.011677755  0.0685944672 0.0015960

### [] potential interpretations
# Week_1:Non_inoculated-Week_10:Inoculated     -0.084795833 -0.113254189 -0.0563374772 0.0000000
# signif adj p = 0.0, week 10 inoculated had a bigger effect than week 1 uninoculated
  # as expected and would make sense

# Week_10:Inoculated-Week_1:Inoculated          0.076477778  0.048019422  0.1049361339 0.0000000
# signif adj p = 0.0 ,  both INOCULATED: week10 > week 1, 
  # 

#phosphorus t4 boxplot
ggplot(p_t4_anova_data,
       aes(x = Inoculation, y = Value, fill = Date_Group)) +
  geom_boxplot(position = position_dodge()) +
  theme_minimal() +
  labs(
    title = "Treatment 4 Phosphorus Concentration",
    subtitle = "Week 1 vs Week 10",
    x = "Inoculum Type", 
    y = "P Concentration (PPM)" 
  )

# p_t4_surface_boxpot <- ggplot(p_t4_data,
#        aes(x = Surface, y = Value, fill = Date_Group)) +
#   geom_boxplot() +
#   facet_wrap(~ Element, scales = "free") +
#   labs(title = "Treatment 4: Phosphrous Observations")+
#   theme_minimal()



## Phosphorus Treatment 5 ---------------------------------------------------
# Look at Treatment 5: without fertilizer 
p_t5_full_model <- aov(Value ~ Date_Group + Inoculation + Surface, data = p_t5_anova_data)
summary(p_t5_full_model)
#             Df   Sum Sq  Mean Sq F value   Pr(>F)    
# Date_Group   1 0.004992 0.004992  43.005 3.21e-08 ***
# Inoculation  2 0.002097 0.001048   9.032 0.000458 ***
# Surface      1 0.000000 0.000000   0.000 0.992024    
# Residuals   49 0.005688 0.000116           

#INTERACTION MODEL 
p_t5_interact_model <- aov(Value ~ Date_Group * Inoculation * Surface, data = p_t5_anova_data)
summary(p_t5_interact_model)
#                                Df   Sum Sq  Mean Sq F value   Pr(>F)    
# Date_Group                      1 0.004992 0.004992  40.661 9.42e-08 ***
# Inoculation                     2 0.002097 0.001048   8.540 0.000735 ***
# Surface                         1 0.000000 0.000000   0.000 0.992249    
# Date_Group:Inoculation          2 0.000142 0.000071   0.578 0.565250    
# Date_Group:Surface              1 0.000094 0.000094   0.769 0.385299    
# Inoculation:Surface             1 0.000038 0.000038   0.310 0.580720    
# Date_Group:Inoculation:Surface  1 0.000012 0.000012   0.095 0.758819    
# Residuals                      44 0.005402 0.000123                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

reduced_interact_p_t5_model <- aov(Value ~ Date_Group * Inoculation, data = p_t5_anova_data)
summary(reduced_interact_p_t5_model)
#                        Df   Sum Sq  Mean Sq F value   Pr(>F)    
# Date_Group              1 0.004992 0.004992  43.205 3.32e-08 ***
# Inoculation             2 0.002097 0.001048   9.074 0.000454 ***
# Date_Group:Inoculation  2 0.000142 0.000071   0.614 0.545324    
# Residuals              48 0.005546 0.000116                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

TukeyHSD(reduced_interact_p_t5_model, "Inoculation")
#                                   diff         lwr          upr     p adj
# Inoculated-Bridge          0.005009722 -0.00685602  0.016875465 0.5673333
# Non_inoculated-Bridge     -0.008167361 -0.02003310  0.003698381 0.2291884
# Non_inoculated-Inoculated -0.013177083 -0.02068164 -0.005672529 0.0002875

#phosphorus t5 boxplot
ggplot(p_t5_anova_data,
       aes(x = Inoculation, y = Value, fill = Date_Group)) +
  geom_boxplot(position = position_dodge()) +
  theme_minimal() +
  labs(
    title = "Treatment 5 Phosphorus Concentration",
    subtitle = "Week 1 vs Week 10",
    x = "Inoculum Type", 
    y = "P Concentration (PPM)" 
  )



# Aluminum  Modeling -------------------------------------------------------

# aluminum all 
al_all_full_model <- aov(Value ~ Date_Group + Inoculation + Surface, 
                        data = al_all_anova_data)
summary(al_all_full_model)

al_all_dummy_model <- aov(Value ~ Date_Group * Inoculation * Surface * Bin_Number, 
                         data = al_all_anova_data)
summary(al_all_dummy_model)
TukeyHSD(al_all_dummy_model, "Bin_Number")


al_all_interactive_model <- aov(Value ~ Date_Group * Inoculation * Surface, 
                               data = al_all_anova_data)
summary(al_all_interactive_model)
#                                 Df Sum Sq Mean Sq F value   Pr(>F)    
# Date_Group                       1   72.8   72.79  28.611 1.56e-07 ***
# Inoculation                      2    6.0    3.00   1.179 0.308774    
# Surface                          1   34.9   34.91  13.723 0.000244 ***
# Date_Group:Inoculation           2    9.3    4.65   1.827 0.162383    
# Date_Group:Surface               1    2.5    2.54   0.998 0.318543    
# Inoculation:Surface              1    8.9    8.88   3.492 0.062480 .  
# Date_Group:Inoculation:Surface   1    3.7    3.66   1.439 0.231097    
# Residuals                      367  933.7    2.54                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

TukeyHSD(al_all_interactive_model, "Surface")
#Runoff-Mound  -0.6456455 -1.0557938 -0.2354972 0.0007136

TukeyHSD(al_all_interactive_model, "Date_Group:Surface")
# Week_10:Runoff-Week_1:Mound   -1.5272030 -2.2344015 -0.820004428 0.0000000
# Week_10:Mound-Week_1:Mound    -1.0551062 -1.7623048 -0.347907710 0.0003505
TukeyHSD(al_all_interactive_model, "Inoculation:Surface")
# Non_inoculated:Runoff-Non_inoculated:Mound  -0.97229202 -1.74257954 -0.20200449 0.0031235


reduced_interact_al_t4_model <- aov(Value ~ Date_Group * Surface, data = al_t4_anova_data)
summary(reduced_interact_al_t4_model)
#                    Df Sum Sq Mean Sq F value Pr(>F)  
# Date_Group          1  13.62  13.623   6.581 0.0135 *
# Surface             2  15.66   7.829   3.782 0.0298 *
# Date_Group:Surface  2   1.33   0.667   0.322 0.7260  
# Residuals          48  99.36   2.070                 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

TukeyHSD(reduced_interact_al_t4_model, "Surface")

# aluminum additive model! OF ALL TREATMENTS
al_all_full_model <- aov(Value ~ Date_Group + Inoculation + Surface, 
                        data = al_all_anova_data)
summary(al_all_full_model)
#              Df Sum Sq Mean Sq F value   Pr(>F)    
# Date_Group    1   72.8   72.79  28.262 1.83e-07 ***
# Inoculation   2    6.0    3.00   1.165 0.313190    
# Surface       1   34.9   34.91  13.556 0.000266 ***
# Residuals   372  958.0    2.58                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Interactive Model for Aluminum
al_all_interactive_model <- aov(Value ~ Date_Group * Inoculation * Surface, 
                               data = al_all_anova_data)
summary(al_all_interactive_model)
#                                 Df Sum Sq Mean Sq F value   Pr(>F)    
# Date_Group                       1   72.8   72.79  28.611 1.56e-07 ***
# Inoculation                      2    6.0    3.00   1.179 0.308774    
# Surface                          1   34.9   34.91  13.723 0.000244 ***
# Date_Group:Inoculation           2    9.3    4.65   1.827 0.162383    
# Date_Group:Surface               1    2.5    2.54   0.998 0.318543    
# Inoculation:Surface              1    8.9    8.88   3.492 0.062480 .  
# Date_Group:Inoculation:Surface   1    3.7    3.66   1.439 0.231097    
# Residuals                      367  933.7    2.54                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# interaction model
reduced_interact_al_model <- aov(Value ~ Date_Group * Surface, 
                                data = al_all_anova_data)
summary(reduced_interact_al_model)
#                     Df Sum Sq Mean Sq F value   Pr(>F)    
# Date_Group           1   72.8   72.79  28.512 1.63e-07 ***
# Surface              2   40.9   20.43   8.004 0.000395 ***
# Date_Group:Surface   2   11.0    5.49   2.151 0.117801    
# Residuals          371  947.1    2.55                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# interaction plot
interaction.plot(al_all_anova_data$Surface,
                 al_all_anova_data$Date_Group,
                 al_all_anova_data$Value)

# ggplot interaction plot
ggplot(al_all_anova_data, aes(x = Surface, y = Value, 
                             color = Date_Group, group = Date_Group)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun = mean, geom = "point") +
  labs(title = "Aluminum & Surface Types Across All Treatments", 
       subtitle = "Interaction Plot",
       x = "SUrface Type",
       y = "A Concentration (PPM)")

TukeyHSD(reduced_interact_al_model, "Surface")
#  Tukey multiple comparisons of means
#     95% family-wise confidence level
# 
# Fit: aov(formula = Value ~ Date_Group * Surface, data = al_all_anova_data)
# 
# $Surface
#                      diff         lwr        upr     p adj
# Mound-Center   0.72301334  0.07400332  1.3720234 0.0246707
# Runoff-Center  0.07729435 -0.57132739  0.7259161 0.9575845
# Runoff-Mound  -0.64571900 -1.05655705 -0.2348809 0.0007288

#[] interpretation: Mound > Runoff


AIC(al_all_full_model, reduced_interact_al_model)

# Aluminum Treatment 1 - potential Modeling #####################################
al_t1_full_model <- aov(Value ~ Date_Group + Inoculation + Surface, 
                        data = al_t1_anova_data)
summary(al_t1_full_model)
#             Df   Sum Sq  Mean Sq F value   Pr(>F)    
# Date_Group   1 0.007872 0.007872  82.361 4.55e-12 ***
# Inoculation  2 0.000589 0.000295   3.082   0.0549 .  
# Surface      1 0.000012 0.000012   0.126   0.7246    
# Residuals   49 0.004684 0.000096                     
# ---

al_t1_interaction_model <- 
  aov(Value ~ Date_Group * Inoculation * Surface, data = al_t1_anova_data)
summary(al_t1_interaction_model)
#                                Df   Sum Sq  Mean Sq F value   Pr(>F)    
# Date_Group                      1 0.007872 0.007872 100.289 6.39e-13 ***
# Inoculation                     2 0.000589 0.000295   3.753  0.03127 *  
# Surface                         1 0.000012 0.000012   0.153  0.69769    
# Date_Group:Inoculation          2 0.001177 0.000589   7.499  0.00158 ** 
# Date_Group:Surface              1 0.000009 0.000009   0.108  0.74365    
# Inoculation:Surface             1 0.000011 0.000011   0.136  0.71450    
# Date_Group:Inoculation:Surface  1 0.000033 0.000033   0.425  0.51801    
# Residuals                      44 0.003454 0.000078                     

al_t1_reduced_model <- aov(Value ~ Date_Group * Inoculation, data = al_t1_anova_data)
summary(al_t1_reduced_model)
#                        Df   Sum Sq  Mean Sq F value   Pr(>F)    
# Date_Group              1 0.007872 0.007872 107.402 7.77e-14 ***
# Inoculation             2 0.000589 0.000295   4.019 0.024331 *  
# Date_Group:Inoculation  2 0.001177 0.000589   8.031 0.000981 ***
# Residuals              48 0.003518 0.000073 

TukeyHSD(al_t1_reduced_model, "Date_Group:Inoculation")
# Week_10:Non_inoculated-Week_1:Bridge          0.036583333  0.020181659  0.052985007 0.0000004
# Week_10:Non_inoculated-Week_1:Inoculated      0.030066667  0.019693337  0.040439996 0.0000000
# Week_10:Non_inoculated-Week_1:Non_inoculated  0.032425000  0.022051670  0.042798330 0.0000000


## ALUMINUM TREATMENT 4 -------------------------------------------------------
#aluminum - full additive model
al_t4_full_model <- aov(Value ~ Date_Group + Inoculation + Surface, data = al_t4_anova_data)
summary(al_t4_full_model)
#             Df Sum Sq Mean Sq F value  Pr(>F)   
# Date_Group   1  13.62  13.623   6.755 0.01231 * 
# Inoculation  2   1.94   0.972   0.482 0.62034   
# Surface      1  15.59  15.594   7.733 0.00767 **
# Residuals   49  98.81   2.017                   

  
# aluminum interaction model  
al_t4_interact_model <- aov(Value ~ Date_Group * Inoculation * Surface, data = al_t4_anova_data)
summary(al_t4_interact_model)
#                                 Df Sum Sq Mean Sq F value  Pr(>F) 
# Date_Group                      1  13.62  13.623   6.402 0.01505 * 
# Inoculation                     2   1.94   0.972   0.457 0.63619   
# Surface                         1  15.59  15.594   7.328 0.00963 **
# Date_Group:Inoculation          2   1.53   0.763   0.359 0.70069   
# Date_Group:Surface              1   0.06   0.060   0.028 0.86781   
# Inoculation:Surface             1   0.08   0.083   0.039 0.84449   
# Date_Group:Inoculation:Surface  1   3.52   3.515   1.652 0.20540   
# Residuals                      44  93.63   2.128                   

reduced_interact_al_t4_model <- 
  aov(Value ~ Date_Group * Surface, data = al_t4_anova_data)

summary(reduced_interact_al_t4_model)
#                    Df Sum Sq Mean Sq F value Pr(>F)  
# Date_Group          1  13.62  13.623   6.581 0.0135 *
# Surface             2  15.66   7.829   3.782 0.0298 *
# Date_Group:Surface  2   1.33   0.667   0.322 0.7260  
# Residuals          48  99.36   2.070                 

TukeyHSD(reduced_interact_al_t4_model, "Surface")
# $Surface
#                     diff       lwr        upr     p adj
# Mound-Center   0.4595312 -1.128669  2.0477317 0.7647688
# Runoff-Center -0.6804125 -2.268613  0.9077879 0.5580067
# Runoff-Mound  -1.1399437 -2.144410 -0.1354776 0.0227103
# nothing significant
TukeyHSD(reduced_interact_al_t4_model, "Date_Group")
# $Date_Group
#                     diff       lwr        upr     p adj
# Week_10-Week_1 -1.004549 -1.791863 -0.2172341 0.0134852

## Aluminum Treatment 5 Modeling -------------------------------------------
al_t5_anova_data <- al_all_anova_data %>%
  filter(Treatment_Number == 5)

al_t5_full_model <- aov(Value ~ Date_Group + Inoculation + Surface, data = al_t5_anova_data)
summary(al_t5_full_model)
#             Df Sum Sq Mean Sq F value Pr(>F)
# Date_Group   1   4.94   4.939   1.341  0.252
# Inoculation  2  15.82   7.909   2.148  0.128
# Surface      1   4.20   4.199   1.140  0.291
# Residuals   49 180.42   3.682                 


# aluminum interaction model  
al_t5_interact_model <- aov(Value ~ Date_Group * Inoculation * Surface, data = al_t5_anova_data)
summary(al_t5_interact_model)
#                                Df Sum Sq Mean Sq F value Pr(>F)  
# Date_Group                      1   4.94   4.939   1.646 0.2062  
# Inoculation                     2  15.82   7.909   2.637 0.0829 .
# Surface                         1   4.20   4.199   1.400 0.2431  
# Date_Group:Inoculation          2  15.29   7.646   2.549 0.0897 .
# Date_Group:Surface              1  20.93  20.926   6.976 0.0114 *
# Inoculation:Surface             1   3.80   3.799   1.266 0.2666  
# Date_Group:Inoculation:Surface  1   8.42   8.425   2.809 0.1009  
# Residuals                      44 131.98   3.000                 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1                 

al_t5_reduced_interact_model <- 
  aov(Value ~ Date_Group * Surface, data = al_t5_anova_data)
summary(al_t5_reduced_interact_model)
#                    Df Sum Sq Mean Sq F value Pr(>F)  
# Date_Group          1   4.94   4.939   1.460 0.2328  
# Surface             2  13.96   6.979   2.064 0.1381  
# Date_Group:Surface  2  24.14  12.071   3.569 0.0359 *
# Residuals          48 162.34   3.382                 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1          


TukeyHSD(al_t5_reduced_interact_model, "Date_Group:Surface")
# Week_10:Trough-Week_1:Trough   2.0979708 -0.1302835 4.3262252 0.0757054


# Aluminum PLOTS ########################################################
#aluminum t4 boxplot
ggplot(al_t4_anova_data,
       aes(x = Surface, y = Value, fill = Date_Group)) +
  geom_boxplot(position = position_dodge()) +
  theme_minimal() +
  labs(
    title = "Treatment 4 Aluminum Concentration",
    subtitle = "Week 1 vs Week 10",
    x = "Surface Type", 
    y = "P Concentration (PPM)" 
  )

# interaction plot
interaction.plot(al_t4_anova_data$Surface,
                 al_t4_anova_data$Date_Group,
                 al_t4_anova_data$Value,
                 cex.key = 0.7)
# ggplot interaction plot
ggplot(al_t4_anova_data, aes(x = Surface, y = Value, 
                             color = Date_Group, group = Date_Group)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun = mean, geom = "point") +
  labs(title = "Aluminum & Surface Types Across Treatment 4", 
       subtitle = "Interaction Plot",
       x = "Inoculation Type",
       y = "Al Concentration (PPM)")

#aluminum t5 boxplot
ggplot(al_t5_anova_data,
       aes(x = Surface, y = Value, fill = Date_Group)) +
  geom_boxplot(position = position_dodge()) +
  theme_minimal() +
  labs(
    title = "Treatment 5 Aluminum Concentration",
    subtitle = "Week 1 vs Week 10",
    x = "Surface Type", 
    y = "P Concentration (PPM)" 
  )




#### IRON ELEMENT MODELING #### -----------------------------------------

# IRON DATA SETS ##################
fe_all_anova_data <- RangeResults %>%
  filter(Element == "Fe_mean")
fe_t4_anova_data <- RangeResults %>%
  filter(Element == "Fe_mean",
         Treatment_Number == 4)
fe_t1_anova_data <- RangeResults %>%
  filter(Element == "Fe_mean",
         Treatment_Number == 1)

# IRON ALL treatments modeling ##########################
# fe ALL treatments additive full model 
fe_all_full_model <- aov(Value~Date_Group + Inoculation + Surface, data = fe_all_anova_data)
summary(fe_all_full_model)
#              Df Sum Sq Mean Sq F value Pr(>F)    
# Date_Group    1   1.19   1.186   1.929 0.1657    
# Inoculation   2  53.62  26.812  43.609 <2e-16 ***
# Surface       1   3.27   3.268   5.315 0.0217 *  
# Residuals   372 228.72   0.615                   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# interactive model
fe_all_interactive_model <- aov(Value~Date_Group * Inoculation * Surface, data = fe_all_anova_data)
summary(fe_all_interactive_model)
#                                 Df Sum Sq Mean Sq F value Pr(>F)    
# Date_Group                       1   1.19   1.186   1.909 0.1680    
# Inoculation                      2  53.62  26.812  43.146 <2e-16 ***
# Surface                          1   3.27   3.268   5.259 0.0224 *  
# Date_Group:Inoculation           2   0.17   0.085   0.137 0.8722    
# Date_Group:Surface               1   0.30   0.298   0.480 0.4890    
# Inoculation:Surface              1   0.03   0.026   0.043 0.8366    
# Date_Group:Inoculation:Surface   1   0.16   0.157   0.252 0.6160    
# Residuals                      367 228.07   0.621                   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


fe_all_reductive_model <- aov(Value~Inoculation * Surface, data = fe_all_anova_data)
summary(fe_all_reductive_model)
#                      Df Sum Sq Mean Sq F value Pr(>F)    
# Inoculation           2  53.58  26.791  43.349 <2e-16 ***
# Surface               1   3.28   3.279   5.306 0.0218 *  
# Inoculation:Surface   1   0.03   0.026   0.042 0.8380    
# Residuals           372 229.91   0.618                   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

TukeyHSD(fe_all_reductive_model, "Inoculation")
#   Tukey multiple comparisons of means
#     95% family-wise confidence level
# 
# Fit: aov(formula = Value ~ Inoculation * Surface, data = fe_all_anova_data)
# 
# $Inoculation
#                                 diff        lwr        upr     p adj
# Inoculated-Bridge          0.6118958  0.2927540  0.9310376 0.0000257
# Non_inoculated-Bridge     -0.1743918 -0.4937246  0.1449411 0.4044785
# Non_inoculated-Inoculated -0.7862876 -0.9884325 -0.5841427 0.0000000
TukeyHSD(fe_all_reductive_model, "Surface")
# $Surface
#                      diff        lwr         upr     p adj
# Mound-Center   0.09923239 -0.2201005 0.418565237 0.7451034
# Runoff-Center -0.09864172 -0.4177835 0.220500084 0.7474454
# Runoff-Mound  -0.19787411 -0.4000190 0.004270825 0.0565501

# Iron Treatment 4 modeling #####  ----------------------------------
### LOOKING AT INOCULATED AND SURFACE TYPES ###
fe_t4_full_model <- aov(Value ~ Date_Group + Inoculation + Surface, data = fe_t4_anova_data)
summary(fe_t4_full_model)
#             Df Sum Sq Mean Sq F value   Pr(>F)    
# Date_Group   1   1.17   1.168   1.793  0.18676    
# Inoculation  2  29.25  14.624  22.441 1.21e-07 ***
# Surface      1   5.89   5.889   9.038  0.00416 ** 
# Residuals   49  31.93   0.652                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

fe_t4_interactive_model <- aov(Value ~ Date_Group * Inoculation * Surface, data = fe_t4_anova_data)
summary(fe_t4_interactive_model)
#                                Df Sum Sq Mean Sq F value   Pr(>F)    
# Date_Group                      1  1.168   1.168   1.776  0.18956    
# Inoculation                     2 29.247  14.624  22.225 2.13e-07 ***
# Surface                         1  5.889   5.889   8.950  0.00453 ** 
# Date_Group:Inoculation          2  0.974   0.487   0.740  0.48289    
# Date_Group:Surface              1  0.362   0.362   0.550  0.46225    
# Inoculation:Surface             1  1.597   1.597   2.426  0.12647    
# Date_Group:Inoculation:Surface  1  0.046   0.046   0.071  0.79183    
# Residuals                      44 28.951   0.658                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
fe_t4_reduced_model <- aov(Value ~ Inoculation + Surface, data = fe_t4_anova_data)
summary(fe_t4_reduced_model)
#             Df Sum Sq Mean Sq F value   Pr(>F)    
# Inoculation  2  29.25  14.624  22.091 1.33e-07 ***
# Surface      1   5.89   5.889   8.897  0.00441 ** 
# Residuals   50  33.10   0.662
fe_t4_reduced_int_model <- aov(Value ~ Inoculation * Surface, data = fe_t4_anova_data)
summary(fe_t4_reduced_int_model)
#                     Df Sum Sq Mean Sq F value   Pr(>F)    
# Inoculation          2 29.247  14.624  22.746 1.03e-07 ***
# Surface              1  5.889   5.889   9.161  0.00393 ** 
# Inoculation:Surface  1  1.597   1.597   2.483  0.12149    
# Residuals           49 31.502   0.643                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
TukeyHSD(fe_t4_reduced_int_model, "Inoculation")
# $Inoculation
#                                 diff        lwr        upr     p adj
# Inoculated-Bridge          1.0880785  0.2035477  1.9726093 0.0124594
# Non_inoculated-Bridge     -0.4589465 -1.3434773  0.4255843 0.4277139
# Non_inoculated-Inoculated -1.5470250 -2.1064514 -0.9875986 0.0000001
TukeyHSD(fe_t4_reduced_int_model, "Surface")
# $Surface
#                     diff        lwr        upr     p adj
# Mound-Center   0.3502764 -0.5342544  1.2348072 0.6071698
# Runoff-Center -0.3502764 -1.2348072  0.5342544 0.6071698
# Runoff-Mound  -0.7005528 -1.2599792 -0.1411264 0.0107902



# Iron Treatment 5 modeling #####
fe_t5_anova_data <- fe_all_anova_data %>% 
  filter(Treatment_Number == 5)


fe_t5_full_model <- aov(Value ~ Date_Group + Inoculation + Surface, data = fe_t5_anova_data)
summary(fe_t4_full_model)
#             Df Sum Sq Mean Sq F value   Pr(>F)    
# Date_Group   1   1.17   1.168   1.793  0.18676    
# Inoculation  2  29.25  14.624  22.441 1.21e-07 ***
# Surface      1   5.89   5.889   9.038  0.00416 ** 
# Residuals   49  31.93   0.652                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

fe_t5_interactive_model <- aov(Value ~ Date_Group * Inoculation * Surface, data = fe_t5_anova_data)
summary(fe_t5_interactive_model)
#                               Df Sum Sq Mean Sq F value   Pr(>F)    
# Date_Group                      1  1.476   1.476   3.200   0.0805 .  
# Inoculation                     2 13.252   6.626  14.360 1.58e-05 ***
# Surface                         1  0.605   0.605   1.311   0.2585    
# Date_Group:Inoculation          2  0.453   0.227   0.491   0.6153    
# Date_Group:Surface              1  1.084   1.084   2.349   0.1325    
# Inoculation:Surface             1  0.050   0.050   0.107   0.7447    
# Date_Group:Inoculation:Surface  1  0.009   0.009   0.020   0.8890    
# Residuals                      44 20.302   0.461                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


fe_t5_reduced_model <- aov(Value ~ Inoculation + Date_Group, data = fe_t5_anova_data)
summary(fe_t5_reduced_model)
#             Df Sum Sq Mean Sq F value   Pr(>F)    
# Inoculation  2 13.252   6.626   14.72 9.39e-06 ***
# Date_Group   1  1.476   1.476    3.28   0.0761 .  
# Residuals   50 22.502   0.450                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

fe_t5_reduced_int_model <- aov(Value ~ Inoculation * Date_Group, data = fe_t5_anova_data)
summary(fe_t5_reduced_int_model)
#                        Df Sum Sq Mean Sq F value   Pr(>F)    
# Inoculation             2 13.252   6.626  14.424 1.24e-05 ***
# Date_Group              1  1.476   1.476   3.214   0.0793 .  
# Inoculation:Date_Group  2  0.453   0.227   0.493   0.6137    
# Residuals              48 22.049   0.459                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

TukeyHSD(fe_t5_reduced_int_model, "Inoculation")
# $Inoculation
#                                 diff        lwr        upr     p adj
# Inoculated-Bridge          0.8552715  0.1071039  1.6034392 0.0215875
# Non_inoculated-Bridge     -0.1704937 -0.9186614  0.5776739 0.8463934
# Non_inoculated-Inoculated -1.0257653 -1.4989481 -0.5525825 0.0000104

TukeyHSD(fe_t5_reduced_int_model, "Date_Group")
# $Date_Group
#                     diff        lwr       upr     p adj
# Week_10-Week_1 0.3306938 -0.0401935 0.7015812 0.0793174



# Iron Control Treatment ---------------------------------------------------
# Iron control treatment - potential Modeling
fe_t1_full_model <- 
  aov(Value ~ Date_Group + Inoculation + Surface, data = fe_t1_anova_data)
summary(al_t1_full_model)
#             Df   Sum Sq  Mean Sq F value   Pr(>F)    
# Date_Group   1 0.007872 0.007872  82.361 4.55e-12 ***
# Inoculation  2 0.000589 0.000295   3.082   0.0549 .  
# Surface      1 0.000012 0.000012   0.126   0.7246    
# Residuals   49 0.004684 0.000096                     
# ---
TukeyHSD(fe_t1_full_model, "Inoculation")


## IRON PLOTS ######################################################
# iron t4 boxplot
# surface 
ggplot(fe_t4_anova_data,
       aes(x = Surface, y = Value, fill = Date_Group)) +
  geom_boxplot(position = position_dodge()) +
  theme_minimal() +
  labs(
    title = "Treatment 4 Iron Concentration",
    subtitle = "Week 1 vs Week 10",
    x = "Surface Type", 
    y = "Iron Concentration (PPM)" 
  )


# interaction plot
# ggplot interaction plot
ggplot(fe_t4_anova_data, aes(x = Inoculation, y = Value, 
                             color = Date_Group, group = Date_Group)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun = mean, geom = "point") +
  labs(title = "Iron & Inoculation Types Across All Treatments", 
       subtitle = "Interaction Plot",
       x = "Inoculation Type",
       y = "Fe Concentration (PPM)")

ggplot(fe_t4_anova_data, aes(x = Surface, y = Value, 
                             color = Date_Group, group = Date_Group)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun = mean, geom = "point") +
  labs(title = "Iron & Surface Types Across All Treatments", 
       subtitle = "Interaction Plot",
       x = "Surface Type",
       y = "Fe Concentration (PPM)")













###############################################
#######################################################
##########################################################
###################################################################
###########################################################################
#### old modeling attempts BELOW
# p_full_model <- aov(Value ~ Date_Group + Inoculation + Surface, data = p_t4_anova_data)
# summary(p_full_model)
# interaction_p_full_model <- aov(Value ~ Date_Group * Inoculation * Surface, data = p_anova_data)
# summary(interaction_p_full_model)
# #                                Df  Sum Sq Mean Sq F value   Pr(>F)    
# # Date_Group                      1 0.04303 0.04303  71.643 8.78e-11 ***
# # Inoculation                     2 0.00843 0.00421   7.015  0.00227 ** 
# # Surface                         1 0.00002 0.00002   0.028  0.86701    
# # Date_Group:Inoculation          2 0.00433 0.00217   3.606  0.03545 *  
# # Date_Group:Surface              1 0.00002 0.00002   0.037  0.84755    
# # Inoculation:Surface             1 0.00001 0.00001   0.023  0.88023    
# # Date_Group:Inoculation:Surface  1 0.00000 0.00000   0.001  0.98084    
# # Residuals                      44 0.02643 0.00060                     
# # ---
# # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# # formula...
# # Value ∼ Date_Group∗ Inoculation ∗ Surface
# # This expands to...
# #   Value∼Date_Group+Inoculation+Surface
# # + Date_Group:Inoculation+Date_Group:Surface
# # + Inoculation:Surface
# # + Date_Group:Inoculation:Surface
# 
# reduced_interact_p_model <- aov(Value ~ Date_Group * Inoculation, data = p_anova_data)
# summary(reduced_interact_p_model)
# #                        Df  Sum Sq Mean Sq F value   Pr(>F)    
# # Date_Group              1 0.04303 0.04303  77.998 1.26e-11 ***
# # Inoculation             2 0.00843 0.00421   7.637  0.00132 ** 
# # Date_Group:Inoculation  2 0.00433 0.00217   3.926  0.02635 *  
# # Residuals              48 0.02648 0.00055                     
# # ---
# # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# plot(lm(reduced_interact_p_model))
# 
# 
# # which combinations differ?
# # Pair-wise comparison between two specific groups
# TukeyHSD(reduced_interact_p_model, "Date_Group:Inoculation")
# #   Tukey multiple comparisons of means
# #     95% family-wise confidence level
# # 
# # Fit: aov(formula = Value ~ Date_Group * Inoculation, data = p_anova_data)
# # 
# # $`Date_Group:Inoculation`
# #                                                      diff          lwr           upr     p adj
# # Week_10:Bridge-Week_1:Bridge                  0.041650000 -0.015266712  0.0985667122 0.2697325
# # Week_1:Inoculated-Week_1:Bridge              -0.003111111 -0.048107723  0.0418855008 0.9999464
# # Week_10:Inoculated-Week_1:Bridge              0.073366667  0.028370055  0.1183632785 0.0001934
# # Week_1:Non_inoculated-Week_1:Bridge          -0.011429167 -0.056425779  0.0335674452 0.9737841
# # Week_10:Non_inoculated-Week_1:Bridge          0.028706944 -0.016289667  0.0737035563 0.4185413
# # Week_1:Inoculated-Week_10:Bridge             -0.044761111 -0.089757723  0.0002355008 0.0519408
# # Week_10:Inoculated-Week_10:Bridge             0.031716667 -0.013279945  0.0767132785 0.3086935
# # Week_1:Non_inoculated-Week_10:Bridge         -0.053079167 -0.098075779 -0.0080825548 0.0122412
# # Week_10:Non_inoculated-Week_10:Bridge        -0.012943056 -0.057939667  0.0320535563 0.9554699
# # Week_10:Inoculated-Week_1:Inoculated          0.076477778  0.048019422  0.1049361339 0.0000000
# # Week_1:Non_inoculated-Week_1:Inoculated      -0.008318056 -0.036776412  0.0201403005 0.9524041
# # Week_10:Non_inoculated-Week_1:Inoculated      0.031818056  0.003359699  0.0602764117 0.0202410
# # Week_1:Non_inoculated-Week_10:Inoculated     -0.084795833 -0.113254189 -0.0563374772 0.0000000
# # Week_10:Non_inoculated-Week_10:Inoculated    -0.044659722 -0.073118078 -0.0162013661 0.0003521
# # Week_10:Non_inoculated-Week_1:Non_inoculated  0.040136111  0.011677755  0.0685944672 0.0015960
# 
# ####[POTENTIAL INTERPRETATION]
# # changes over time within treatments
# #   Inoculated... LARGER INCREASE OVER TIME
# # Week_10:Inoculated-Week_1:Inoculated / diff:  +0.076477778 | p < 0.0000000
# #   non-inoculated...           
# # Week_10:Non_inoculated-Week_1:Non_inoculated / +0.040136111 | p = 0.0015960
# #   The inoculated shows the largest increase between baseline
# #      and final results. while non-inoculated shows a smaller
# #        but still significant increase,
# #        bridge shows no significant change.
# 
# 
# # interaction plot
# interaction.plot(p_anova_data$Date_Group,
#                  p_anova_data$Inoculation,
#                  p_anova_data$Value)
# #  interaction plot
# ggplot(p_anova_data, aes(x = Date_Group, y = Value, 
#                          color = Inoculation, group = Inoculation)) +
#   stat_summary(fun = mean, geom = "line") +
#   stat_summary(fun = mean, geom = "point")
# 
# 
# 
# 
# TukeyHSD(reduced_interact_p_model, "Inoculation")
# #   Tukey multiple comparisons of means
# #     95% family-wise confidence level
# # 
# # Fit: aov(formula = Value ~ Date_Group * Inoculation, data = p_anova_data)
# # 
# # $Inoculation
# #                                  diff         lwr         upr
# # Inoculated-Bridge          0.01430278 -0.01162469  0.04023025
# # Non_inoculated-Bridge     -0.01218611 -0.03811358  0.01374136
# # Non_inoculated-Inoculated -0.02648889 -0.04288686 -0.01009092
# #                               p adj
# # Inoculated-Bridge         0.3835295
# # Non_inoculated-Bridge     0.4965218
# # Non_inoculated-Inoculated 0.0008404
# 
# 
# # TukeyHSD results
# #tukey_results <- TukeyHSD(reduced_interact_p_model, "Inoculation")
# 
# # Plotting the results
# # plot(tukey_results, las = 1, col = "blue")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# al_full_model <- aov(Value ~ Date_Group + Inoculation + Surface, data = al_anova_data)
# summary(al_full_model)
# 
# 
# reduced_model <- aov(Value ~ Date_Group + Inoculation * Surface, 
#                      data = anova_data)
# summary(reduced_model)
# #                      Df  Sum Sq Mean Sq F value  Pr(>F)    
# # Date_Group            1 0.10494 0.10494 254.936 < 2e-16 ***
# # Inoculation           2 0.00722 0.00361   8.767 0.00019 ***
# # Surface               1 0.00011 0.00011   0.274 0.60105    
# # Inoculation:Surface   1 0.00004 0.00004   0.092 0.76241    
# # Residuals           371 0.15272 0.00041                    
# # ---
# # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# # suggested to use step() to iterate through potential models
# #step_model <- step(full_model)
# #summary(step_model)
# 
# anova(full_model, reduced_model)
# 
# best_model <- aov(Value ~ Date_Group + Inoculation, data = anova_data)
# summary(best_model)
# 
# best_model <- aov(Value ~ Date_Group + Inoculation, data = anova_data)
# summary(best_model)
# # --------- 
# # ^^miscellaneous
# # week 1 v 2, comparisons of 3 elements across all treatments
# ggplot(RangeResults,
#        aes(x = factor(Treatment_Number), y = Value, fill = Date_Group)) +
#          geom_boxplot(position = position_dodge()) +
#          facet_wrap(~Element, scales = "free_y") + 
#          theme_minimal() +
#          labs(
#            title = "Week 1-Week 10 Comparison Across Treatments",
#            x = "Treatment", 
#            y = "Concentration" 
#          )



################# EXTRA ##################

#statistical testing
#gen_model <- aov(Value ~ Date_Group * Treatment_Num * Position,
#             data = longAverages)
#summary(gen_model)




# results
#                                     Df Sum Sq Mean Sq F value Pr(>F)  
# Date_Group                           1     19  19.002   3.429 0.0643 .
# Treatment_Num                        1      0   0.053   0.010 0.9218  
# Position                             1     24  24.061   4.342 0.0374 *
#   Date_Group:Treatment_Num             1      8   7.617   1.375 0.2413  
# Date_Group:Position                  1      1   0.953   0.172 0.6784  
# Treatment_Num:Position               1      1   1.168   0.211 0.6462  
# Date_Group:Treatment_Num:Position    1      0   0.158   0.028 0.8660  
# Residuals                         1114   6173   5.541                 
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# interpretations: 
  #
#plot(model)
# interpretations:
# position turned out to be slightly significant

# 
# # focusing on one group (Al)
# subset_Al_data <- longAverages %>%
#   filter(Element == "Al_mean")
# #focus on Phosphorus subset
# subset_p_data <- longAverages %>% 
#   filter(Element == "P_mean")
# #focus on Iron subset
# subset_fe_data <- longAverages %>% 
#   filter(Element == "Fe_mean")
# #focus on Sulfur subset
# subset_s_data <- longAverages %>% 
#   filter(Element == "S_mean")
# subset_ca_data <- longAverages %>%
#   filter(Element == "Ca_mean")
# 
# 
# #histogram Al > normally distributed
# ggplot(subset_Al_data,
#        aes(x = Value, fill = Date_Group)) +
#   geom_histogram(alpha = 0.5, bins = 15, position = "identity") +
#   theme_minimal() +
#   labs(
#     title = "Distribution of Al",
#     x = "Al",
#     y = "Count"
#   )
# #histogram P -> turned out to be slightly skewed right
# ggplot(subset_p_data,
#        aes(x = Value, fill = Date_Group)) +
#   geom_histogram(alpha = 0.5, bins = 15, position = "identity") +
#   theme_minimal() +
#   labs(
#     title = "Distribution of P",
#     x = "P",
#     y = "Count"
#   )
# #histogram Fe -> turned out to be skewed right
# ggplot(subset_fe_data,
#        aes(x = Value, fill = Date_Group)) +
#   geom_histogram(alpha = 0.5, bins = 15, position = "identity") +
#   theme_minimal() +
#   labs(
#     title = "Distribution of Fe",
#     x = "Fe",
#     y = "Count"
#   )
# #histogram Sulfur -> turned out to be skewed right
# ggplot(subset_s_data,
#        aes(x = Value, fill = Date_Group)) +
#   geom_histogram(alpha = 0.5, bins = 15, position = "identity") +
#   theme_minimal() +
#   labs(
#     title = "Distribution of Sulfur",
#     x = "Sulfur",
#     y = "Count"
#   )
# #histogram calcium
# ggplot(subset_ca_data,
#        aes(x = Value, fill = Date_Group)) +
#   geom_histogram(alpha = 0.5, bins = 15, position = "identity") +
#   theme_minimal() +
#   labs(
#     title = "Distribution of Ca",
#     x = "Ca",
#     y = "Count"
#   )
# 
# 
# 
# 
# #looking at measurement OUTLIERS
# # ------------------
# #phosphorus outliers 
# p_outliers <- longAverages %>%
#   filter(Element == "P_mean") %>%
#   group_by(Element) %>%
#   mutate(
#     Q1 = quantile(Value, 0.25, na.rm = TRUE),
#     Q3 = quantile(Value, 0.75, na.rm = TRUE),
#     IQR = Q3 - Q1,
#     upper_bound = Q3 + 1.5 * IQR
#   ) %>%
#   filter(Value > upper_bound) %>%
#   select(Date_Group, Bin_Number, Position, Treatment_Num, Value)
# #iron outliers
# fe_outliers <- longAverages %>%
#   filter(Element == "Fe_mean") %>%
#   group_by(Element) %>%
#   mutate(
#     Q1 = quantile(Value, 0.25, na.rm = TRUE),
#     Q3 = quantile(Value, 0.75, na.rm = TRUE),
#     IQR = Q3 - Q1,
#     upper_bound = Q3 + 1.5 * IQR
#   ) %>%
#   filter(Value > upper_bound) %>%
#   select(Date_Group, Bin_Number, Position, Treatment_Num, Value)
# # which positions cause these outliers?
# table(p_outliers$Position)
# table(fe_outliers$Position)
# # what about treatments?
# table(p_outliers$Treatment_Num)
# table(fe_outliers$Treatment_Num)
# #focus on Phosphorus subset
# subset_p_data <- longAverages %>% 
#   filter(Element == "P_mean")
# # does date group have an effect?
# table(p_outliers$Date_Group)
# table(fe_outliers$Date_Group)
# #visual confirmation
# #box plot by position
# ggplot(filter(longAverages, Element == "P_mean"),
#        aes(x = factor(Position), y = Value)) +
#   geom_boxplot() +
#   theme_minimal() +
#   labs(title = "P by Position")
# # sort and inspect largest values directly
# longAverages %>%
#   filter(Element == "P_mean") %>%
#   arrange(desc(Value)) %>%
#   select(Date_Group, Bin_Number, Position, Treatment_Num, Value) %>%
#   head(15)
# #------------
# # playing with histogram results ^^
# 
# # log transformation
# # if the skew is strong 
# #longAverages <- longAverages %>%
# #  mutate(log_Value = log(Value + 1))
# 
# ggplot(longAverages,
#        aes(x = Value)) +
#   geom_histogram(bins = 30) +
#   facet_wrap(~ Element, scales = "free")
# 
# #box plot
# ggplot(subset_Al_data,
#        aes(x = factor(Treatment_Num), y = Value, fill = Date_Group)) +
#   geom_boxplot(position = position_dodge()) +
#   labs(
#     title = "Al at Position 4: Jan vs Feb",
#     x = "Treatment",
#     y = "Al Concentration"
#   )
# 
# # 
# #box plot P @ pos4
# ggplot(subset_p_data,
#        aes(x = factor(Treatment_Num), y = Value, fill = Date_Group)) +
#   geom_boxplot(position = position_dodge()) +
#   labs(
#     title = "Phosphorus at Position 4: Jan vs Feb",
#     x = "Treatment",
#     y = "P Concentration (PPM)"
#   )
# 
# #box plot sulfur
# ggplot(subset_s_data,
#        aes(x = factor(Treatment_Num), y = Value, fill = Date_Group)) +
#   geom_boxplot(position = position_dodge()) +
#   labs(
#     title = "Sulfur at Position 4: Jan vs Feb",
#     x = "Treatment",
#     y = "S Concentration"
#   )
# 
# # boxplot calcium
# ggplot(subset_ca_data,
#        aes(x = factor(Treatment_Num), y = Value, fill = Date_Group)) +
#   geom_boxplot(position = position_dodge()) +
#   labs(
#     title = "Ca at Position 4: Jan vs Feb",
#     x = "Treatment",
#     y = "Ca Concentration"
#   )
# 
# # FIRST STATISTICAL significant METHOD MODEL 
# gen_p_model <- aov(Value ~ Date_Group * Treatment_Num * Position,
#                  data = subset_p_data)
# summary(gen_p_model)
# #                                     Df  Sum Sq   Mean Sq F value   Pr(>F)    
# # Date_Group                          1 0.00238 0.0023750  22.068 3.73e-06 ***
# # Treatment_Num                       1 0.00000 0.0000036   0.033  0.85596    
# # Position                            1 0.00082 0.0008232   7.649  0.00597 ** 
# # Date_Group:Treatment_Num            1 0.00077 0.0007668   7.125  0.00794 ** 
# # Date_Group:Position                 1 0.00000 0.0000009   0.008  0.92895    
# # Treatment_Num:Position              1 0.00006 0.0000624   0.580  0.44696    
# # Date_Group:Treatment_Num:Position   1 0.00002 0.0000231   0.214  0.64364    
# # Residuals                         366 0.03939 0.0001076       
# #interaction.plot(subset_p_data$Date_Group,
# #                 subset_p_data$Treatment_Num,
# #                 subset_p_data$Value, col = 2:6)
# 
# 
# 
# 
# 
# 
# 
# #  creating derived factors
# 
# #filter to treatment 4, inoculum/fertilizer/depressions
# t4_data <- longAverages %>%
#   filter(Treatment_Num == 4)
# al_t4_data <- subset_Al_data %>%
#   filter(Treatment_Num == 4)
#         # checking
# table(al_t4_data$Surface, al_t4_data$Inoculation)
# 
# p_t4_data <- subset_p_data %>%
#   filter(Treatment_Num == 4)
# fe_t4_data <- subset_fe_data %>%
#   filter(Treatment_Num == 4)
# 
# s_t4_data <- subset_s_data %>%
#   filter(Treatment_Num == 4)
# 
# ca_t4_data <- subset_ca_data %>%
#   filter(Treatment_Num == 4)
# 
# # #histogram Fe -> turned out to be skewed right
# #---------
# # ggplot(fe_t4_data,
# #        aes(x = Value, fill = Date_Group)) +
# #   geom_histogram(alpha = 0.5, bins = 15, position = "identity") +
# #   theme_minimal() +
# #   labs(
# #     title = "Distribution of p in treatment Fe",
# #     x = "Fe",
# #     y = "Count"
# #   )
# #------------
# ggplot(s_t4_data,
#        aes(x = Value, fill = Date_Group)) +
#   geom_histogram(alpha = 0.5, bins = 15, position = "identity") +
#   theme_minimal() +
#   labs(
#     title = "Distribution of Sulfur in treatment 4",
#     x = "Sulfur",
#     y = "Count"
#   )
# 
# ggplot(ca_t4_data,
#        aes(x = Value, fill = Date_Group)) +
#   geom_histogram(alpha = 0.5, bins = 15, position = "identity") +
#   theme_minimal() +
#   labs(
#     title = "Distribution of Calcium in treatment 4",
#     x = "Calcium",
#     y = "Count"
#   )
# 
# #### TREATMENT 4 AOV INTERACTION MODELS
# # <----------------------------------------------
# ### TREATMENT 4 AOV INTERACTION MODEL
#     # Phosphorus
# p_t4_model <- aov(Value ~ Date_Group * Surface * Inoculation,
#                    data = p_t4_data)
# summary(p_t4_model)
# #                                 Df   Sum Sq   Mean Sq F value  Pr(>F)   
# # Date_Group                      1 0.000443 0.0004435   3.636 0.06309 . 
# # Surface                         2 0.000672 0.0003362   2.757 0.07448 . 
# # Inoculation                     1 0.001361 0.0013605  11.154 0.00172 **
# #   Date_Group:Surface              2 0.000083 0.0000416   0.341 0.71269   
# # Date_Group:Inoculation          1 0.000065 0.0000651   0.534 0.46879   
# # Surface:Inoculation             1 0.000070 0.0000699   0.573 0.45313   
# # Date_Group:Surface:Inoculation  1 0.000027 0.0000274   0.225 0.63771   
# # Residuals                      44 0.005367 0.0001220                   
# # ---
# #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# 
# 
# 
#     # Aluminum
# al_t4_model <- aov(Value ~ Date_Group * Surface * Inoculation,
#                    data = al_t4_data)
# summary(al_t4_model)
# #                                 Df Sum Sq Mean Sq F value Pr(>F)
# # Date_Group                      1  10.12  10.124   1.851  0.181
# # Surface                         2  20.71  10.354   1.893  0.163
# # Inoculation                     1   0.81   0.805   0.147  0.703
# # Date_Group:Surface              2   1.42   0.710   0.130  0.879
# # Date_Group:Inoculation          1   3.12   3.122   0.571  0.454
# # Surface:Inoculation             1   0.87   0.867   0.159  0.692
# # Date_Group:Surface:Inoculation  1   1.52   1.517   0.277  0.601
# # Residuals                      44 240.65   5.469       
# 
#     # Iron
# fe_t4_model <- aov(Value ~ Date_Group * Surface * Inoculation,
#                    data = fe_t4_data)
# summary(fe_t4_model)
# plot(fe_t4_model)
# #                                 Df Sum Sq Mean Sq F value   Pr(>F)    
# # Date_Group                      1  0.288   0.288   0.721  0.40054    
# # Surface                         2  6.036   3.018   7.555  0.00151 ** 
# # Inoculation                     1 23.878  23.878  59.771 9.84e-10 ***
# # Date_Group:Surface              2  0.513   0.256   0.642  0.53114    
# # Date_Group:Inoculation          1  0.079   0.079   0.199  0.65783    
# # Surface:Inoculation             1  1.264   1.264   3.164  0.08220 .  
# # Date_Group:Surface:Inoculation  1  0.006   0.006   0.014  0.90473    
# # Residuals                      44 17.578   0.399                     
# # ---
# #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
#     # Sulfur
# s_t4_model <- aov(Value ~ Date_Group * Surface * Inoculation,
#                    data = s_t4_data)
# summary(s_t4_model)
# #                                Df Sum Sq Mean Sq F value   Pr(>F)    
# # Date_Group                      1  164.7  164.68  19.190 7.22e-05 ***
# # Surface                         2   17.7    8.86   1.032    0.365    
# # Inoculation                     1    2.2    2.22   0.259    0.613    
# # Date_Group:Surface              2    6.5    3.23   0.376    0.689    
# # Date_Group:Inoculation          1    6.5    6.52   0.759    0.388    
# # Surface:Inoculation             1    4.1    4.14   0.482    0.491    
# # Date_Group:Surface:Inoculation  1    2.8    2.77   0.322    0.573    
# # Residuals                      44  377.6    8.58                     
# # ---
# #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# 
# ca_t4_model <- aov(Value ~ Date_Group * Surface * Inoculation,
#                   data = ca_t4_data)
# summary(ca_t4_model)
# 
# 
# # <----------------------------------------------------------
# 
# # looking at interaction plots
# #plot(p_t4_model)
# 
# 
# # box plot treatment 4: run off vs mound
# ggplot(t4_data,
#        aes(x = Surface, y = Value, fill = Date_Group)) +
#   geom_boxplot(position = position_dodge()) +
#   facet_wrap(~ Element, scales = "free") +
#   theme_minimal() +
#   labs(title = "Treatment 4: Runoff vs Mound")
# 
# 
# # phosphorus
# ggplot(p_t4_data,
#        aes(x = Surface, y = Value, fill = Date_Group)) +
#   geom_boxplot(position = position_dodge()) +
#   facet_wrap(~ Element, scales = "free") +
#   theme_minimal() +
#   labs(
#     title = "Phosphorus (Treatment 4): Runoff vs Mound",
#     x = "Surface Type",
#     y = "Value (ppm)"     
#     )
# 
# 
# # comparing inoculated vs non inoculated
# ggplot(t4_data,
#        aes(x = Inoculation, y = Value, fill = Date_Group)) +
#   geom_boxplot(position = position_dodge()) +
#   facet_wrap(~ Element, scales = "free") +
#   theme_minimal() +
#   labs(title = "Treatment 4: Inoculation Effect")
# 
# # phosphorus 
# p_t4_inoculation_boxpot <- ggplot(p_t4_data,
#        aes(x = Inoculation, y = Value, fill = Date_Group)) +
#   geom_boxplot(position = position_dodge()) +
#   facet_wrap(~ Element, scales = "free") +
#   theme_minimal() +
#   labs(title = "Phosphorus (Treatment 4): Inoculation Effect",
#        y = "Phosphorus (PPM)")
# 
# 
# p_t4_inoculation_boxpot
# 
# 
# # position comparisons
# ggplot(t4_data,
#        aes(x = factor(Position), y = Value, fill = Date_Group)) +
#   geom_boxplot(position = position_dodge()) +
#   facet_wrap(~ Element, scales = "free") +
#   theme_minimal() +
#   labs(title = "Treatment 4: Position Comparison")
# 
# 
# #anova test for aluminum for treatments 4
# # type of model: additive (independent factors)
# model_t4 <- aov(Value ~ Date_Group + Surface + Inoculation,
#                 data = t4_data)
# summary(model_t4)
# #results of model_t4:
# #         Df Sum Sq Mean Sq F value Pr(>F)
# # Date_Group   1  10.12  10.124   2.004  0.163
# # Surface      2  20.71  10.354   2.049  0.140
# # Inoculation  1   0.81   0.805   0.159  0.691
# # Residuals   49 247.57   5.053 
#       # interpretation:
# # Pr(>F): 0.163;
#   # There is no stat evid that aluminum differs between
#     # Jan & Feb for Treatment 4
# # Pr(>F): 0.140 (WEAK TREND)
#   # There is no stat evid that surface area may influence 
#     # aluminum but not enough evidence to conclude.
# # Pr(>F): 0.691
#   # very not sigfig. No evidence that inoculation 
#     # affects al levels.
# 
# #visualize this: box plot surface of t4 & al
# ggplot(t4_data,
#        aes(x = Surface, y = Value, fill = Date_Group)) +
#   geom_boxplot() +
#   facet_wrap(~ Element, scales = "free") +
#   labs(title = "Treatment 4: Aluminum Observations")+
#   theme_minimal()
# 
# # playing with different potential interactions
# interaction_model_t4 <- aov(Value ~ Date_Group * Inoculation + Surface,
#                 data = t4_data)
# summary(interaction_model_t4)
# 
# 
# # PHOSPHORUS
# #visualize this: box plot surface of t4 & al
# p_t4_surface_boxpot <- ggplot(p_t4_data,
#        aes(x = Surface, y = Value, fill = Date_Group)) +
#   geom_boxplot() +
#   facet_wrap(~ Element, scales = "free") +
#   labs(title = "Treatment 4: Phosphrous Observations")+
#   theme_minimal()
# 
# #----- working with one on ones ------------------
# 
# inoculation_p_t4_model <- aov(Value ~ Inoculation,
#                             data = p_t4_data)
# summary(surfaces_p_t4_model )
# 
# 
# TukeyHSD(inoculation_p_t4_model)
# 
# 
# surfaces_p_t4_model <- aov(Value ~ Surface, 
#                            data = p_t4_data)
# 
# TukeyHSD(surfaces_p_t4_model)
# 
# 
# ino_suf_p_t4_model <-aov(Value ~ Inoculation * Surface,
#                          data = p_t4_data)
# summary(ino_suf_p_t4_model)
# TukeyHSD(ino_suf_p_t4_model)
# 
# 
# table(p_t4_data$Inoculation, p_t4_data$Surface)
# 
# # create subset that only looks to compare run off and mound
# subset_data <- subset(p_t4_data, Inoculation != "Bridge", Surface != "Center")
# 
# model <- aov(Value ~ Surface + Inoculation, data = subset_data)
# TukeyHSD(model)
# 
# aov(Value ~ Inoculation + Surface, data = p_t4_data)

### histogram ###
#--------
# position specific comparisons
# ggplot(longAverages,
#        aes(x = Date_Group, y = Value, fill = Date_Group)) +
#   geom_boxplot() +
#   facet_grid(Element ~ Position, scales = "free_y") +
#   theme_minimal()
# suggested statistical test
# still need to interpret this myself
#If p-value < 0.05:
# Significant difference between Jan and Feb
# Possible instrument drift
# Or sample changes
# null hyp: mean Jan = mean Feb
# alt hyp: means =/= 
# t.test(Value ~ Date_Group,
#        data = filter(longAverages, Element == "P_mean"))
# 
# t.test(Value ~ Date_Group,
#        data = filter(longAverages, Element == "Al_mean"))
# 
# t.test(Value ~ Date_Group,
#        data = filter(longAverages, Element == "Fe_mean"))
#comparing bins, positions, and months
# ggplot(longAverages,
# aes(x = Bin_Number, y = Value, fill = Date_Group)) +
#   geom_boxplot(position = position_dodge()) +
#   facet_wrap(~ Element, scales = "free") +
#   theme_minimal() +
#   labs(title = "Element Comparison by Bin and Month")


# histogram, comparing element distribution across all treatments
#ggplot(longAverages, aes(x = Value)) +
#  geom_histogram(bins = 30) + 
#  facet_wrap(~Element, scales = "free") + 
#  theme_minimal()
#-----
#cleanup ^^^



# ----------more in depth with variables------- 


