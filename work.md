ST558_Project3
================
Hui Fang/Joy Zhou
2023-11-02

# Introduction

Diabetes is a seriously pervasive chronic disease that disrupts the
body’s ability to regulate blood glucose levels, leading to a diminished
quality of life and reduced life expectancy. It stands as one of the
most prevalent chronic illnesses in the United States, impacting
millions of Americans annually and imposing a significant economic
burden on the nation.

In this project, we will use the `diabetes binary health indicators`
dataset obtained from
[Kaggle](https://www.kaggle.com/datasets/alexteboul/diabetes-health-indicators-dataset/)
to conduct comprehensive exploratory data analysis (EDA) and develop
predictive models. This dataset comprises 253,680 survey responses to
the CDC’s BRFSS (Behavioral Risk Factor Surveillance System) from year
2015. The primary target variable, `Diabetes_binary`, offers binary
classification, distinguishing between 0 for no diabetes, and 1 for
prediabetes or diabetes. This dataset encompasses 21 feature variables
and is not balanced. Detailed information of variable can be found
[here](https://www.kaggle.com/datasets/alexteboul/diabetes-health-indicators-dataset/?select=diabetes_binary_health_indicators_BRFSS2015.csv).

Our analysis will primarily focus on a subset of key variables,
including High blood pressure (HighBP), High cholesterol (HighChol),
cholesterol check (CholCheck), Body Mass Index (BMI), Smoker, Fruits,
Veggies, and Age.  
In our EDA phase, we will start by summarizing basic statistics
visualizing variable frequencies. This will be followed by the
exploration of correlations between variables and the creation of
contingency tables to better understand the interplay of these factors.

Based on the results from EDA, we will split the dataset into training
(70%) and test (30%) subsets for each educational level. Subsequently,
we will employ the training data to fit six distinct models, including a
logistic regression, a LASSO logistic regression, a classification tree
model, a random forest model, a xx model, as well as a xxx model. The
performance of these models will be rigorously evaluated using the test
dataset, and we will determine the most effective model for predicting
diabetes outcomes.

Description of variables in the data set: + Diabetes_binary: 0 = no
diabetes 1 = prediabetes or diabetes  
+ HighBP: High blood pressure  
+ HighChol: High cholesterol  
+ CholCheck: 0 = no cholesterol check in 5 years 1 = yes cholesterol
check in 5 years  
+ BMI: Body Mass Index  
+ Smoker: Have you smoked at least 100 cigarettes in your entire life? 0
= no 1 = yes  
+ Stroke: 0 = no 1 = yes  
+ HeartDiseaseorAttack: coronary heart disease (CHD) or myocardial
infarction (MI) 0 = no 1 = yes  
+ PhysActivity: physical activity in past 30 days - not including job 0
= no 1 = yes  
+ Fruits: Consume Fruit 1 or more times per day 0 = no 1 = yes  
+ Veggies: Consume Vegetables 1 or more times per day 0 = no 1 = yes  
+ HvyAlcoholConsump: (adult men \>=14 drinks per week and adult
women\>=7 drinks per week) 0 = no 1 = yes  
+ AnyHealthcare: Health care coverage 0 = no 1 = yes  
+ NoDocbcCost: Was there a time in the past 12 months when you needed to
see a doctor but could not because of cost? 0 = no 1 = yes  
+ GenHlth: in general your health is: scale 1-5 1 = excellent 2 = very
good 3 = good 4 = fair 5 = poor  
+ MentHlth: days of poor mental health scale 1-30 days  
+ PhysHlth: physical illness or injury days in past 30 days scale 1-30  
+ DiffWalk: Do you have serious difficulty walking or climbing stairs? 0
= no 1 = yes  
+ Sex: 0 = female 1 = male  
+ Age: 13-level age category 1 = 18-24 2 = 25-29 3 = 30-34 4 = 35-39 5 =
40-44 6 = 45-49 7 = 50-54 8 = 55-59 9 = 60-64 10 = 65-69 11 = 70-74 12 =
75-79 13 = 80 or older  
+ Education: scale 1-6 1 = Never attended school or only kindergarten 2
= Grades 1 through 8 (Elementary) 3 = Grades 9 through 11 (Some high
school) 4 = Grade 12 or GED (High school graduate) 5 = College 1 year to
3 years (Some college or technical school) 6 = College 4 years or more
(College graduate)  
+ Income: scale 1-8 1 = less than \$10,000 5 = less than \$35,000 8 =
\$75,000 or more

# Data

## Read in data

``` r
library(dplyr)
library(readr)
diabetes <- as_tibble(read.csv("diabetes_binary_health_indicators_BRFSS2015.csv", header = TRUE))
head(diabetes)
```

    ## # A tibble: 6 × 22
    ##   Diabetes_binary HighBP HighChol CholCheck   BMI Smoker Stroke
    ##             <dbl>  <dbl>    <dbl>     <dbl> <dbl>  <dbl>  <dbl>
    ## 1               0      1        1         1    40      1      0
    ## 2               0      0        0         0    25      1      0
    ## 3               0      1        1         1    28      0      0
    ## 4               0      1        0         1    27      0      0
    ## 5               0      1        1         1    24      0      0
    ## 6               0      1        1         1    25      1      0
    ## # ℹ 15 more variables: HeartDiseaseorAttack <dbl>, PhysActivity <dbl>,
    ## #   Fruits <dbl>, Veggies <dbl>, HvyAlcoholConsump <dbl>, AnyHealthcare <dbl>,
    ## #   NoDocbcCost <dbl>, GenHlth <dbl>, MentHlth <dbl>, PhysHlth <dbl>,
    ## #   DiffWalk <dbl>, Sex <dbl>, Age <dbl>, Education <dbl>, Income <dbl>

# grouping Education levels

``` r
diabetes$Education <- ifelse(diabetes$Education %in% c(1, 2), "Non_Elementary",
                         ifelse(diabetes$Education == 3, "SomeHighSchool", 
                              ifelse(diabetes$Education == 4, "HighSchool",
                                   ifelse(diabetes$Education == 5, "SomeCollege",
                                        ifelse(diabetes$Education == 6, "College", NA)))))

# Convert some variables to factor
diabetes$Education <- as.factor(diabetes$Education)
#diabetes$Diabetes_binary <- as.factor(diabetes$Diabetes_binary)
#diabetes$Income <- as.factor(diabetes$Income)
diabetes$HighBP <- as.factor(diabetes$HighBP)
diabetes$HighChol <- as.factor(diabetes$HighChol)
diabetes$Sex <- as.factor(diabetes$Sex)
diabetes$Fruits <- as.factor(diabetes$Fruits)
params$Edu
```

    ## [1] "Non_Elementary"

## Subsetting the dataset based on education level

``` r
EducationData <- filter(diabetes, (Education == params$Edu))
```

## EDA

``` r
# Checking missing values of columns in diabetes2
missing_values <- colSums(is.na(EducationData))
```

``` r
# Describing 
library(knitr)
kable(summary(EducationData))
```

|     | Diabetes_binary | HighBP | HighChol | CholCheck      | BMI           | Smoker         | Stroke          | HeartDiseaseorAttack | PhysActivity   | Fruits | Veggies        | HvyAlcoholConsump | AnyHealthcare  | NoDocbcCost    | GenHlth       | MentHlth       | PhysHlth       | DiffWalk       | Sex    | Age            | Education           | Income        |
|:----|:----------------|:-------|:---------|:---------------|:--------------|:---------------|:----------------|:---------------------|:---------------|:-------|:---------------|:------------------|:---------------|:---------------|:--------------|:---------------|:---------------|:---------------|:-------|:---------------|:--------------------|:--------------|
|     | Min. :0.0000    | 0:1740 | 0:1965   | Min. :0.0000   | Min. :12.00   | Min. :0.0000   | Min. :0.00000   | Min. :0.0000         | Min. :0.0000   | 0:1785 | Min. :0.0000   | Min. :0.00000     | Min. :0.0000   | Min. :0.0000   | Min. :1.000   | Min. : 0.000   | Min. : 0.000   | Min. :0.0000   | 0:2283 | Min. : 1.000   | College : 0         | Min. :1.000   |
|     | 1st Qu.:0.0000  | 1:2477 | 1:2252   | 1st Qu.:1.0000 | 1st Qu.:25.00 | 1st Qu.:0.0000 | 1st Qu.:0.00000 | 1st Qu.:0.0000       | 1st Qu.:0.0000 | 1:2432 | 1st Qu.:0.0000 | 1st Qu.:0.00000   | 1st Qu.:1.0000 | 1st Qu.:0.0000 | 1st Qu.:3.000 | 1st Qu.: 0.000 | 1st Qu.: 0.000 | 1st Qu.:0.0000 | 1:1934 | 1st Qu.: 7.000 | HighSchool : 0      | 1st Qu.:2.000 |
|     | Median :0.0000  | NA     | NA       | Median :1.0000 | Median :28.00 | Median :0.0000 | Median :0.00000 | Median :0.0000       | Median :1.0000 | NA     | Median :1.0000 | Median :0.00000   | Median :1.0000 | Median :0.0000 | Median :4.000 | Median : 0.000 | Median : 1.000 | Median :0.0000 | NA     | Median :10.000 | Non_Elementary:4217 | Median :3.000 |
|     | Mean :0.2917    | NA     | NA       | Mean :0.9718   | Mean :29.46   | Mean :0.4804   | Mean :0.08537   | Mean :0.1914         | Mean :0.5682   | NA     | Mean :0.6929   | Mean :0.02656     | Mean :0.8406   | Mean :0.1788   | Mean :3.471   | Mean : 5.219   | Mean : 8.368   | Mean :0.3801   | NA     | Mean : 9.111   | SomeCollege : 0     | Mean :3.313   |
|     | 3rd Qu.:1.0000  | NA     | NA       | 3rd Qu.:1.0000 | 3rd Qu.:33.00 | 3rd Qu.:1.0000 | 3rd Qu.:0.00000 | 3rd Qu.:0.0000       | 3rd Qu.:1.0000 | NA     | 3rd Qu.:1.0000 | 3rd Qu.:0.00000   | 3rd Qu.:1.0000 | 3rd Qu.:0.0000 | 3rd Qu.:4.000 | 3rd Qu.: 5.000 | 3rd Qu.:15.000 | 3rd Qu.:1.0000 | NA     | 3rd Qu.:12.000 | SomeHighSchool: 0   | 3rd Qu.:5.000 |
|     | Max. :1.0000    | NA     | NA       | Max. :1.0000   | Max. :84.00   | Max. :1.0000   | Max. :1.00000   | Max. :1.0000         | Max. :1.0000   | NA     | Max. :1.0000   | Max. :1.00000     | Max. :1.0000   | Max. :1.0000   | Max. :5.000   | Max. :30.000   | Max. :30.000   | Max. :1.0000   | NA     | Max. :13.000   | NA                  | Max. :8.000   |

``` r
# Create a correlation matrix between variables
Cor_Matrix <- EducationData %>% 
         select(Diabetes_binary, BMI, MentHlth, PhysHlth, Age, Income) %>%
         cor()

# Round the correlation matrix to two decimal places
rounded_Cor_Matrix <- round(Cor_Matrix, digits = 2)
# Print the rounded correlation matrix
kable(rounded_Cor_Matrix)
```

|                 | Diabetes_binary |   BMI | MentHlth | PhysHlth |   Age | Income |
|:----------------|----------------:|------:|---------:|---------:|------:|-------:|
| Diabetes_binary |            1.00 |  0.17 |     0.10 |     0.16 |  0.16 |  -0.15 |
| BMI             |            0.17 |  1.00 |     0.07 |     0.08 | -0.12 |  -0.05 |
| MentHlth        |            0.10 |  0.07 |     1.00 |     0.40 | -0.08 |  -0.15 |
| PhysHlth        |            0.16 |  0.08 |     0.40 |     1.00 |  0.07 |  -0.19 |
| Age             |            0.16 | -0.12 |    -0.08 |     0.07 |  1.00 |  -0.12 |
| Income          |           -0.15 | -0.05 |    -0.15 |    -0.19 | -0.12 |   1.00 |
