#Required Packages
packages <- c("shiny","shinydashboard","shinybusy","tidyverse", "ggplot2","DT","readr","knitr","dplyr","gbm","randomForest","caret","tree", "plotly","class")
lapply(packages, library, character.only = TRUE)

#Read in the dataset and categorize the compressive strength
data  <-  read_csv("Concrete_Dataset.csv")
data  <-  data %>% mutate(data,Concrete_Comp_strength =
                    ifelse(`Concrete compressive strength(MPa, megapascals)` <45, "Low Compressive Strength",
                    ifelse(`Concrete compressive strength(MPa, megapascals)` >45, "High Compressive Strength", "")))
data$Concrete_Comp_strength <- factor(data$Concrete_Comp_strength, levels=c("Low Compressive Strength", "High Compressive Strength"))


#Doing a 70:30 split for training/testing
set.seed(365)
train  <-  sample(1:nrow(data), size = 0.7*nrow(data))
test  <- setdiff(1:nrow(data), train)
data_Train <- data[train, ]
data_Test <- data[test, ]

shinyServer(function(input, output, session) {
})


