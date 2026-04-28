library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(patchwork)

#Importing my data from excel spreadsheet with some cleaning up!
# ---------------------------------
excel_sheet <- read_excel("ROUND_ONE_MEASUREMENTS_copy.xlsx", 
                          sheet = "labeled_feb8_results")
rawResults<- excel_sheet

rawResults$OUTLIER <- as.integer(rawResults$OUTLIER)
rawResults$OUTLIER[is.na(rawResults$OUTLIER)] <- 0
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
    Date %in% as.Date(c("2006-02-07", "2006-02-08")) ~ "Week_2" #,TRUE ~ NA_character_
  ))
table(cleanedResults$Date_Group)
#-----------
# end of evil date formatting



# remove unnecessary columns
 cleanedResults <- cleanedResults %>% select(-"File #", -"OUTLIER", -"File #",
      -"Operator", -"Name", -"ID", -"Field1", -"Field2",
      -"test", -"Application", -"Method", -"ElapsedTime", -"Alloy 1",
      -"Match Qual 1", -"Alloy 2", -"Match Qual 2", -"Alloy 3", -"Match Qual 3")
 
#-------------------------------------------------------
#End of clean up


# filtering 
#-----------------
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
#-------- 
# filtering ^


# miscellaneous
#---- 
# separating date_groups into different datasets
#JanuaryMeasurements <- filter(filteredResults,
                              #grepl("Week1", Date_Group))
#Jan30_31
#FebruaryMeasurements <- filter(filteredResults,
                             # grepl("Week2", Date_Group))
#Feb7_8
# dropping Date_Group from each new data set
#JanuaryMeasurements <- select(JanuaryMeasurements, -c(Date_Group))
#FebruaryMeasurements <- select(FebruaryMeasurements, -c(Date_Group))
# --------- 
# ^^miscellaneous


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
  mutate(Treatment_Num = as.numeric(str_extract(Bin_Number, "(?<=T)\\d+"))) %>%
  filter(!is.na(Treatment_Num))

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
      Position %in% c(1,2,3,4) ~ "Runoff",
      Position %in% c(5,6,7,8) ~ "Mound",
      Position == 9 ~ "Center"
      # calling it center b/c of 9
    ),
    
    Inoculation = case_when(
      Position %in% c(1,2,5,6) ~ "Non_inoculated",
      Position %in% c(3,4,7,8) ~ "Inoculated",
      Position == 9 ~ "Bridge" 
      # calling it bridge/center b/c of the transfer of nutrients
    )
  ) 



# graphing begins !!
# ---------------------------
# graphing comparisons


# cleanup
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



# week 1 v 2, comparisons of 3 elements across all treatments
ggplot(longAverages,
       aes(x = factor(Treatment_Num), y = Value, fill = Date_Group)) +
         geom_boxplot(position = position_dodge()) +
         facet_wrap(~Element, scales = "free_y") + 
         theme_minimal() +
         labs(
           title = "Week 1 vs Week 2 Comparison Across Treatments",
           x = "Treatment", 
           y = "Concentration" 
         )


#statistical testing
gen_model <- aov(Value ~ Date_Group * Treatment_Num * Position,
             data = longAverages)
summary(gen_model)
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


# focusing on one group (Al)
subset_Al_data <- longAverages %>%
  filter(Element == "Al_mean")
#focus on Phosphorus subset
subset_p_data <- longAverages %>% 
  filter(Element == "P_mean")
#focus on Iron subset
subset_fe_data <- longAverages %>% 
  filter(Element == "Fe_mean")
#focus on Sulfur subset
subset_s_data <- longAverages %>% 
  filter(Element == "S_mean")
subset_ca_data <- longAverages %>%
  filter(Element == "Ca_mean")


#histogram Al > normally distributed
ggplot(subset_Al_data,
       aes(x = Value, fill = Date_Group)) +
  geom_histogram(alpha = 0.5, bins = 15, position = "identity") +
  theme_minimal() +
  labs(
    title = "Distribution of Al",
    x = "Al",
    y = "Count"
  )
#histogram P -> turned out to be slightly skewed right
ggplot(subset_p_data,
       aes(x = Value, fill = Date_Group)) +
  geom_histogram(alpha = 0.5, bins = 15, position = "identity") +
  theme_minimal() +
  labs(
    title = "Distribution of P",
    x = "P",
    y = "Count"
  )
#histogram Fe -> turned out to be skewed right
ggplot(subset_fe_data,
       aes(x = Value, fill = Date_Group)) +
  geom_histogram(alpha = 0.5, bins = 15, position = "identity") +
  theme_minimal() +
  labs(
    title = "Distribution of Fe",
    x = "Fe",
    y = "Count"
  )
#histogram Sulfur -> turned out to be skewed right
ggplot(subset_s_data,
       aes(x = Value, fill = Date_Group)) +
  geom_histogram(alpha = 0.5, bins = 15, position = "identity") +
  theme_minimal() +
  labs(
    title = "Distribution of Sulfur",
    x = "Sulfur",
    y = "Count"
  )
#histogram calcium
ggplot(subset_ca_data,
       aes(x = Value, fill = Date_Group)) +
  geom_histogram(alpha = 0.5, bins = 15, position = "identity") +
  theme_minimal() +
  labs(
    title = "Distribution of Ca",
    x = "Ca",
    y = "Count"
  )




#looking at measurement OUTLIERS
# ------------------
#phosphorus outliers 
p_outliers <- longAverages %>%
  filter(Element == "P_mean") %>%
  group_by(Element) %>%
  mutate(
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    upper_bound = Q3 + 1.5 * IQR
  ) %>%
  filter(Value > upper_bound) %>%
  select(Date_Group, Bin_Number, Position, Treatment_Num, Value)
#iron outliers
fe_outliers <- longAverages %>%
  filter(Element == "Fe_mean") %>%
  group_by(Element) %>%
  mutate(
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    upper_bound = Q3 + 1.5 * IQR
  ) %>%
  filter(Value > upper_bound) %>%
  select(Date_Group, Bin_Number, Position, Treatment_Num, Value)
# which positions cause these outliers?
table(p_outliers$Position)
table(fe_outliers$Position)
# what about treatments?
table(p_outliers$Treatment_Num)
table(fe_outliers$Treatment_Num)
#focus on Phosphorus subset
subset_p_data <- longAverages %>% 
  filter(Element == "P_mean")
# does date group have an effect?
table(p_outliers$Date_Group)
table(fe_outliers$Date_Group)
#visual confirmation
#box plot by position
ggplot(filter(longAverages, Element == "P_mean"),
       aes(x = factor(Position), y = Value)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "P by Position")
# sort and inspect largest values directly
longAverages %>%
  filter(Element == "P_mean") %>%
  arrange(desc(Value)) %>%
  select(Date_Group, Bin_Number, Position, Treatment_Num, Value) %>%
  head(15)
#------------
# playing with histogram results ^^

# log transformation
# if the skew is strong 
#longAverages <- longAverages %>%
#  mutate(log_Value = log(Value + 1))

ggplot(longAverages,
       aes(x = Value)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ Element, scales = "free")

#box plot
ggplot(subset_Al_data,
       aes(x = factor(Treatment_Num), y = Value, fill = Date_Group)) +
  geom_boxplot(position = position_dodge()) +
  labs(
    title = "Al at Position 4: Jan vs Feb",
    x = "Treatment",
    y = "Al Concentration"
  )

# 
#box plot P @ pos4
ggplot(subset_p_data,
       aes(x = factor(Treatment_Num), y = Value, fill = Date_Group)) +
  geom_boxplot(position = position_dodge()) +
  labs(
    title = "Phosphorus at Position 4: Jan vs Feb",
    x = "Treatment",
    y = "P Concentration (PPM)"
  )

#box plot sulfur
ggplot(subset_s_data,
       aes(x = factor(Treatment_Num), y = Value, fill = Date_Group)) +
  geom_boxplot(position = position_dodge()) +
  labs(
    title = "Sulfur at Position 4: Jan vs Feb",
    x = "Treatment",
    y = "S Concentration"
  )

# boxplot calcium
ggplot(subset_ca_data,
       aes(x = factor(Treatment_Num), y = Value, fill = Date_Group)) +
  geom_boxplot(position = position_dodge()) +
  labs(
    title = "Ca at Position 4: Jan vs Feb",
    x = "Treatment",
    y = "Ca Concentration"
  )

# FIRST STATISTICAL significant METHOD MODEL 
gen_p_model <- aov(Value ~ Date_Group * Treatment_Num * Position,
                 data = subset_p_data)
summary(gen_p_model)
#                                     Df  Sum Sq   Mean Sq F value   Pr(>F)    
# Date_Group                          1 0.00238 0.0023750  22.068 3.73e-06 ***
# Treatment_Num                       1 0.00000 0.0000036   0.033  0.85596    
# Position                            1 0.00082 0.0008232   7.649  0.00597 ** 
# Date_Group:Treatment_Num            1 0.00077 0.0007668   7.125  0.00794 ** 
# Date_Group:Position                 1 0.00000 0.0000009   0.008  0.92895    
# Treatment_Num:Position              1 0.00006 0.0000624   0.580  0.44696    
# Date_Group:Treatment_Num:Position   1 0.00002 0.0000231   0.214  0.64364    
# Residuals                         366 0.03939 0.0001076       
#interaction.plot(subset_p_data$Date_Group,
#                 subset_p_data$Treatment_Num,
#                 subset_p_data$Value, col = 2:6)







#  creating derived factors

#filter to treatment 4, inoculum/fertilizer/depressions
t4_data <- longAverages %>%
  filter(Treatment_Num == 4)
al_t4_data <- subset_Al_data %>%
  filter(Treatment_Num == 4)
        # checking
table(al_t4_data$Surface, al_t4_data$Inoculation)

p_t4_data <- subset_p_data %>%
  filter(Treatment_Num == 4)
fe_t4_data <- subset_fe_data %>%
  filter(Treatment_Num == 4)

s_t4_data <- subset_s_data %>%
  filter(Treatment_Num == 4)

ca_t4_data <- subset_ca_data %>%
  filter(Treatment_Num == 4)

# #histogram Fe -> turned out to be skewed right
#---------
# ggplot(fe_t4_data,
#        aes(x = Value, fill = Date_Group)) +
#   geom_histogram(alpha = 0.5, bins = 15, position = "identity") +
#   theme_minimal() +
#   labs(
#     title = "Distribution of p in treatment Fe",
#     x = "Fe",
#     y = "Count"
#   )
#------------
ggplot(s_t4_data,
       aes(x = Value, fill = Date_Group)) +
  geom_histogram(alpha = 0.5, bins = 15, position = "identity") +
  theme_minimal() +
  labs(
    title = "Distribution of Sulfur in treatment 4",
    x = "Sulfur",
    y = "Count"
  )

ggplot(ca_t4_data,
       aes(x = Value, fill = Date_Group)) +
  geom_histogram(alpha = 0.5, bins = 15, position = "identity") +
  theme_minimal() +
  labs(
    title = "Distribution of Calcium in treatment 4",
    x = "Calcium",
    y = "Count"
  )

#### TREATMENT 4 AOV INTERACTION MODELS
# <----------------------------------------------
### TREATMENT 4 AOV INTERACTION MODEL
    # Phosphorus
p_t4_model <- aov(Value ~ Date_Group * Surface * Inoculation,
                   data = p_t4_data)
summary(p_t4_model)
#                                 Df   Sum Sq   Mean Sq F value  Pr(>F)   
# Date_Group                      1 0.000443 0.0004435   3.636 0.06309 . 
# Surface                         2 0.000672 0.0003362   2.757 0.07448 . 
# Inoculation                     1 0.001361 0.0013605  11.154 0.00172 **
#   Date_Group:Surface              2 0.000083 0.0000416   0.341 0.71269   
# Date_Group:Inoculation          1 0.000065 0.0000651   0.534 0.46879   
# Surface:Inoculation             1 0.000070 0.0000699   0.573 0.45313   
# Date_Group:Surface:Inoculation  1 0.000027 0.0000274   0.225 0.63771   
# Residuals                      44 0.005367 0.0001220                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1




    # Aluminum
al_t4_model <- aov(Value ~ Date_Group * Surface * Inoculation,
                   data = al_t4_data)
summary(al_t4_model)
#                                 Df Sum Sq Mean Sq F value Pr(>F)
# Date_Group                      1  10.12  10.124   1.851  0.181
# Surface                         2  20.71  10.354   1.893  0.163
# Inoculation                     1   0.81   0.805   0.147  0.703
# Date_Group:Surface              2   1.42   0.710   0.130  0.879
# Date_Group:Inoculation          1   3.12   3.122   0.571  0.454
# Surface:Inoculation             1   0.87   0.867   0.159  0.692
# Date_Group:Surface:Inoculation  1   1.52   1.517   0.277  0.601
# Residuals                      44 240.65   5.469       

    # Iron
fe_t4_model <- aov(Value ~ Date_Group * Surface * Inoculation,
                   data = fe_t4_data)
summary(fe_t4_model)
plot(fe_t4_model)
#                                 Df Sum Sq Mean Sq F value   Pr(>F)    
# Date_Group                      1  0.288   0.288   0.721  0.40054    
# Surface                         2  6.036   3.018   7.555  0.00151 ** 
# Inoculation                     1 23.878  23.878  59.771 9.84e-10 ***
# Date_Group:Surface              2  0.513   0.256   0.642  0.53114    
# Date_Group:Inoculation          1  0.079   0.079   0.199  0.65783    
# Surface:Inoculation             1  1.264   1.264   3.164  0.08220 .  
# Date_Group:Surface:Inoculation  1  0.006   0.006   0.014  0.90473    
# Residuals                      44 17.578   0.399                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

    # Sulfur
s_t4_model <- aov(Value ~ Date_Group * Surface * Inoculation,
                   data = s_t4_data)
summary(s_t4_model)
#                                Df Sum Sq Mean Sq F value   Pr(>F)    
# Date_Group                      1  164.7  164.68  19.190 7.22e-05 ***
# Surface                         2   17.7    8.86   1.032    0.365    
# Inoculation                     1    2.2    2.22   0.259    0.613    
# Date_Group:Surface              2    6.5    3.23   0.376    0.689    
# Date_Group:Inoculation          1    6.5    6.52   0.759    0.388    
# Surface:Inoculation             1    4.1    4.14   0.482    0.491    
# Date_Group:Surface:Inoculation  1    2.8    2.77   0.322    0.573    
# Residuals                      44  377.6    8.58                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


ca_t4_model <- aov(Value ~ Date_Group * Surface * Inoculation,
                  data = ca_t4_data)
summary(ca_t4_model)


# <----------------------------------------------------------

# looking at interaction plots
#plot(p_t4_model)


# box plot treatment 4: run off vs mound
ggplot(t4_data,
       aes(x = Surface, y = Value, fill = Date_Group)) +
  geom_boxplot(position = position_dodge()) +
  facet_wrap(~ Element, scales = "free") +
  theme_minimal() +
  labs(title = "Treatment 4: Runoff vs Mound")


# phosphorus
ggplot(p_t4_data,
       aes(x = Surface, y = Value, fill = Date_Group)) +
  geom_boxplot(position = position_dodge()) +
  facet_wrap(~ Element, scales = "free") +
  theme_minimal() +
  labs(
    title = "Phosphorus (Treatment 4): Runoff vs Mound",
    x = "Surface Type",
    y = "Value (ppm)"     
    )


# comparing inoculated vs non inoculated
ggplot(t4_data,
       aes(x = Inoculation, y = Value, fill = Date_Group)) +
  geom_boxplot(position = position_dodge()) +
  facet_wrap(~ Element, scales = "free") +
  theme_minimal() +
  labs(title = "Treatment 4: Inoculation Effect")

# phosphorus 
p_t4_inoculation_boxpot <- ggplot(p_t4_data,
       aes(x = Inoculation, y = Value, fill = Date_Group)) +
  geom_boxplot(position = position_dodge()) +
  facet_wrap(~ Element, scales = "free") +
  theme_minimal() +
  labs(title = "Phosphorus (Treatment 4): Inoculation Effect",
       y = "Phosphorus (PPM)")


p_t4_inoculation_boxpot


# position comparisons
ggplot(t4_data,
       aes(x = factor(Position), y = Value, fill = Date_Group)) +
  geom_boxplot(position = position_dodge()) +
  facet_wrap(~ Element, scales = "free") +
  theme_minimal() +
  labs(title = "Treatment 4: Position Comparison")


#anova test for aluminum for treatments 4
# type of model: additive (independent factors)
model_t4 <- aov(Value ~ Date_Group + Surface + Inoculation,
                data = t4_data)
summary(model_t4)
#results of model_t4:
#         Df Sum Sq Mean Sq F value Pr(>F)
# Date_Group   1  10.12  10.124   2.004  0.163
# Surface      2  20.71  10.354   2.049  0.140
# Inoculation  1   0.81   0.805   0.159  0.691
# Residuals   49 247.57   5.053 
      # interpretation:
# Pr(>F): 0.163;
  # There is no stat evid that aluminum differs between
    # Jan & Feb for Treatment 4
# Pr(>F): 0.140 (WEAK TREND)
  # There is no stat evid that surface area may influence 
    # aluminum but not enough evidence to conclude.
# Pr(>F): 0.691
  # very not sigfig. No evidence that inoculation 
    # affects al levels.

#visualize this: box plot surface of t4 & al
ggplot(t4_data,
       aes(x = Surface, y = Value, fill = Date_Group)) +
  geom_boxplot() +
  facet_wrap(~ Element, scales = "free") +
  labs(title = "Treatment 4: Aluminum Observations")+
  theme_minimal()

# playing with different potential interactions
interaction_model_t4 <- aov(Value ~ Date_Group * Inoculation + Surface,
                data = t4_data)
summary(interaction_model_t4)


# PHOSPHORUS
#visualize this: box plot surface of t4 & al
p_t4_surface_boxpot <- ggplot(p_t4_data,
       aes(x = Surface, y = Value, fill = Date_Group)) +
  geom_boxplot() +
  facet_wrap(~ Element, scales = "free") +
  labs(title = "Treatment 4: Phosphrous Observations")+
  theme_minimal()

#----- working with one on ones ------------------

inoculation_p_t4_model <- aov(Value ~ Inoculation,
                            data = p_t4_data)
summary(surfaces_p_t4_model )


TukeyHSD(inoculation_p_t4_model)


surfaces_p_t4_model <- aov(Value ~ Surface, 
                           data = p_t4_data)

TukeyHSD(surfaces_p_t4_model)


ino_suf_p_t4_model <-aov(Value ~ Inoculation * Surface,
                         data = p_t4_data)
summary(ino_suf_p_t4_model)
TukeyHSD(ino_suf_p_t4_model)


table(p_t4_data$Inoculation, p_t4_data$Surface)

# create subset that only looks to compare run off and mound
subset_data <- subset(p_t4_data, Inoculation != "Bridge", Surface != "Center")

model <- aov(Value ~ Surface + Inoculation, data = subset_data)
TukeyHSD(model)

aov(Value ~ Inoculation + Surface, data = p_t4_data)


# ----------more in depth with variables------- 


