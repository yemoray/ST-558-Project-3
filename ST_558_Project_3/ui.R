#Required Packages
packages <- c("shiny","shinydashboard","shinybusy","tidyverse", "ggplot2","DT","readr","knitr","dplyr","gbm","randomForest","caret","tree", "plotly","class","fontawesome")
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

varNames = names(data %>% select(-Concrete_Comp_strength))

modelChoices = c("Multiple Linear Regression", "Bagged Tree", "Random Forest")


#Create the UI dashboard
dashboardPage(skin = "green",
    
    #Title
    dashboardHeader(title=span("Compressive Strength of Concrete",style = "font-size: 17px"),titleWidth = "300px"),
    
    #Sidebar pages
    dashboardSidebar(width = "300px",
        sidebarMenu(
            menuItem("About", tabName = "about", icon = icon("home")),
            menuItem("Data", tabName = "data", icon = icon("table")), 
            menuItem("Data Exploration", tabName = "exploration", icon = icon("chart-bar")),
            menuItem("Modeling", tabName = "model", icon = icon("th"))
        )#End Sidebar menu
    ),#End Sidebar pages 
    
    
    #Side bar page contents
    dashboardBody(
        tabItems(
            
            #About Page content
            tabItem(tabName = "about",
                    ),#End About Page content
            
            
            #Data Page Content
            tabItem(tabName = "data",
                    ),#End Data Page content 
            
            
            #Data Exploration Page Content
            tabItem(tabName = "exploration",
                    ),#End Data Exploration Page Content
            
            
            #Modeling Page Content
            tabItem(tabName = "model",
                tabsetPanel(
                        
                    #Modeling Information tab
                    tabPanel("Modeling Info",
                        ),
                        
                    #Model Fitting tab
                    tabPanel("Model Fitting", 
                        ),
                        
                    #Prediction tab
                    tabPanel("Prediction",
                            )
                )#End tab set panel
                                 )#End Modeling Page content
                        )#End tab items
                    )#End Dashboard Body
)#End Dashboard Page