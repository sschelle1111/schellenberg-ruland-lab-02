#not sure how to get intro part here to work

#title: "9041 Assignment 2"
#author: Sam Schellenberg-Ruland
#date: today
#format: 
 # html:
  # toc: true
   # code-fold: true
   # code-summary: "Show code"
    #df-print: paged
#execute:
 # echo: false
  #warning: false


#```{r, load_libraries}```
#| #output: FALSE

#library(tidyverse)
#library(readxl)
#library(psych)
#library(gt)
#library(janitor)




data <- read_excel("./Attachment_Anxiety_Data.xlsx")



data.clean <- data |>
  mutate(Gender = case_when(Gender == 1 ~ "male", Gender == 2 ~ "female", TRUE ~ NA)) |>
  mutate(Relationship = case_when(Relationship == 1 ~ "married", 
                                  Relationship == 2 ~ "in a relationship",
                                  Relationship == 4 ~ "divorced",
                                  Relationship == 5 ~ "in a relationship",
                                  Relationship == 6 ~ "never been in a relationship",
                                  Relationship == 7 ~ "prefer not to say")) |>
  mutate(Ethnicity = case_when(Ethnicity == 1 ~ "white/Caucasian",
                               Ethnicity == 2 ~ "Asian/Asian British",
                               Ethnicity == 3 ~ "Other ethnic group",
                               Ethnicity == 4 ~ "prefer not to say")) |>
  select(Gender, Relationship,Ethnicity, SA_1:SEst_10) |>
    mutate(across(SA_1:SEst_10, as.integer))
  
#Social Interaction Anxiety Scale 
#Social_Anxiety
key_SA <- list(Social_Anxiety = c("SA_1","SA_2","SA_3","SA_4","-SA_5","SA_6","SA_7","SA_8","-SA_9","SA_10","-SA_11","SA_12","SA_13","SA_14","SA_15","SA_16","SA_17","SA_18","SA_19","SA_20"))
scores_SA<-scoreItems(key_SA, data.clean, totals = F, min=1, max=4)
scores_SA.df<-as.data.frame(scores_SA$scores)

#Experiences in Close Relationships Anxiety 
#Attachment_Related_Anxiety

key_AA <- list(Attachment_Related_Anxiety = c("AA_1","AA_2","AA_3","AA_4","AA_5","AA_6","AA_7","AA_8","AA_9"))
scores_AA<-scoreItems(key_AA, data.clean, totals = F, min=1, max=7)
scores_AA.df<-as.data.frame(scores_AA$scores)

#Rosenberg Self Esteem 
#Self_Esteem
key_SE <- list(Self_Esteem = c("SEst_1","-SEst_2","SEst_3","SEst_4","-SEst_5","-SEst_6","SEst_7","-SEst_8","-SEst_9","SEst_10"))
scores_SE<-scoreItems(key_SE, data.clean, totals = F, min=1, max=4)
scores_SE.df<-as.data.frame(scores_SE$scores)

#bind scores data frames together
allscores.df <- cbind(scores_SA.df, scores_AA.df, scores_SE.df)

#combine scores dataframe with clean dataframe
data.clean<-cbind(data.clean, allscores.df)

data.final <- data.clean |>
  select("Gender", "Relationship", "Ethnicity", "Social_Anxiety", "Attachment_Related_Anxiety", "Self_Esteem") |>
  mutate_if(is.numeric, round, 2)

demographic.table <- data.final |>
  tabyl(Gender, Relationship)

demographic.gt.table <- demographic.table |>
  gt() |>
  tab_header(
    title = md("*Distribution of Males and Females by Relationship")
  )
  
#making my summary

summary <- describe(select(data.final, Social_Anxiety:Self_Esteem))



n <- (describe(data.final))[1,2]


Scales <- c("Social_Anxiety", "Attachment_Related_Anxiety", "Self_Esteem")
summary$Scales<- Scales

alphas <- round(c(scores_SA$alpha,
                  scores_AA$alpha,
                  scores_SE$alpha),2)
  
summary <- summary |>
  select(Scales, mean, median, sd, range) |>
  mutate_if(is.numeric, round, 2)

final.summary <- cbind(summary, alphas)

# Create a reproducible table
summary.gt.table <- gt(final.summary) |>
  tab_header(
    title = md("*Descriptive Statistics for Social Anxiety, Attachment-Related Anxiety, and Self-Esteem*")
  )
  
summary.gt.table

#Filter data

data.filtered <- data.clean |>
  filter(Relationship == "in a relationship")
         
#FILTERED table

alphas_filtered <- t(round(c(scores_SA$alpha,
                             scores_AA$alpha,
                             scores_SE$alpha),2))


summary_filtered <- describe(select(data.filtered, Social_Anxiety:Self_Esteem))
summary_filtered

Scales <- c("Social_Anxiety", "Attachment_Related_Anxiety", "Self_Esteem")

summary_filtered$Scales<- Scales

summary_filtered <- summary_filtered |>
  select(Scales, mean, median, sd, range) |>
  mutate_if(is.numeric, round, 2)



filtered.table <- cbind(summary_filtered, alphas_filtered)

filtered.gt.table <- gt(filtered.table) |>
  tab_header(
    title = md("*Descriptive Statistics for Social Anxiety, Attachment-Related Anxiety, and Self-Esteem for People in Relationships*")
  )

## Intro

## Methods

### Participants

### Measures

### Data Analysis Strategy

## Results

## Discussion


