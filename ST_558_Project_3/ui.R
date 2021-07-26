#Required Packages
packages <- c("shiny","shinydashboard","shinybusy","tidyverse", "ggplot2","DT","readr","knitr","dplyr","gbm","randomForest","caret","tree", "plotly","class","png")
lapply(packages, library, character.only = TRUE)

#Read in the dataset and categorize the compressive strength
data  <-  read_csv("Concrete_Dataset.csv")
data  <-  data %>% mutate(data,Concrete_Comp_strength_cat =
                    ifelse(Concrete_compressive_strength <45, "Low Compressive Strength",
                    ifelse(Concrete_compressive_strength >45, "High Compressive Strength", "")))
data$Concrete_Comp_strength_cat <- factor(data$Concrete_Comp_strength_cat, levels=c("Low Compressive Strength", "High Compressive Strength"))


#Doing a 70:30 split for training/testing
set.seed(365)
train  <-  sample(1:nrow(data), size = 0.7*nrow(data))
test  <- setdiff(1:nrow(data), train)
data_Train <- data[train, ]
data_Test <- data[test, ]

var_names = names(data %>% select(-Concrete_Comp_strength_cat))

modelChoices = c("Multiple Linear Regression", "Bagged Tree", "Random Forest")


#Create the UI dashboard
dashboardPage(skin = "green",
    
    #Title
    dashboardHeader(title=span("Compressive Strength of Concrete",style = "font-size: 17px"),titleWidth = "350px"),
    
    #Sidebar pages
    dashboardSidebar(width = "350px",
        sidebarMenu(
            tags$img(src="navbar.png", width = '100%'),
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
                h3(strong("Purpose of this app")),
                box(background = "blue", width=12, 
                    h4("This app enables the user to run exploratory data analyses, fit supervised learning models and run predictions on the target variable from the",a("Concrete Compressive Strength Data Set",style="color:#ffa500",href="https://archive.ics.uci.edu/ml/datasets/Concrete+Compressive+Strength"), "donated by Prof. I-Cheng Yeh (Chung-Hua University, Taiwan) to the UCI Machine Learning Repository."),
                    br(),
                    h4("The dataset contains the following variables:"),
                    br(),
                    h4("Concrete Compressive Strength, MPa: The target variable"), 
                    h4("Cement content, Kg/m3 of mixture"),
                    h4("Blast Furnace Slag content, Kg/m3 of mixture"),
                    h4("Fly ash content, Kg/m3 of mixture"),
                    h4("Superplasticizer(HICON) content, Kg/m3 of mixture"),
                    h4("Coarse Aggregate content, Kg/m3 of mixture"),
                    h4("Fine Aggregate content, Kg/m3 of mixture"),
                    h4("Age of cured concrete when tested, days"),
                    br(),
                    h4("Concrete is a highly complex material, being able to predict its compressive strength, and understanding with variables have the biggest impact on this property is critical to the service life and safety of engineered materials like bridges."),
                    tags$img(src="Compressive-Strength-Of_Concrete-Kobe.png", style="width: 500px"),
                    br(),
                    br(),
                    h4(strong("What does each page do?")),
                    
                        HTML("<h4>The <b>Data</b> page contains the dataset to be used for analyses. The data can be sorted, scrolled through, and saved as a csv file.<h4>"),
                        br(),
                        HTML("<h4>The <b>Data Exploration</b> page contains two tabs for  creating numerical graphs and summaries. You can change the plot and summary types, and also change the variables and data subset used in the plot and summaries <h4>"),
                        br(),
                        HTML("<h4>The <b>Modeling</b> page contains 3 tabs: <em>Modeling Info</em> contains information on three supervised learning models, <em>Model Fitting</em> contains a choice of model settings, model comparisons, and appropriate summaries, the <em>Prediction tab</em> provides the user a way to use one of the models for prediction.<h4>"),
                    ),
                h5(strong("Data Citation")),"I.-C Yeh, 'Modeling of Strength of High-Performance Concrete Using Artificial Neural Networks', Cement and Concrete Research (1998): pp. 1797-1808, Elsevier Science Ltd.",
                    ),#End About Page content
            
            
            #Data Page Content
            tabItem(tabName = "data", strong(em(h2("Data Table"))),
                    
                fluidRow(box(title="What's in this page?", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, width = 10, background="blue",
                    h4("The data table is shown below. You can subset the table to show the levels of the different variables that contribute to high compressive strength or low compressive strength."),
                    h4("These subsets can also be downloaded as .csv files by clicking the download button.")),
                             
                    selectizeInput("comp_strength_cat", "Compressive Strength level", choices = levels(as.factor(data$Concrete_Comp_strength_cat))),
                    downloadButton("Download_Data_Table", "Download as .csv file"),
                    hr(), 
                    dataTableOutput("Data_Tables"))),#End Data Page content 
            
            
            #Data Exploration Page Content
            tabItem(tabName = "exploration",strong(em(h2("Graphical and Numeric Summaries"))),
                    
                    fluidRow(box(title="What's in this page?", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, width = 10, background="blue",
                                 h4("The graphical and numeric summaries are shown below. There is a choice of plot type and variables for the different plots."),
                                 h4("These plots can also be interacted with and downloaded as .png files by hovering over the top right of the plots and making a selection from the plotly options.")),
                             
                             ),
                             
                tabsetPanel(
                        
                    #Tab for graphical summaries
                    tabPanel("Graphical Summaries",
                        sidebarLayout(
                                     
                            #Sidebar for selecting type of plot
                            sidebarPanel(
                                selectInput("plot_type", 
                                    "Select the plot type",
                                    choices = c("Histogram", "Scatter", "Box_plot"), selected = "Histogram"),
                                conditionalPanel(condition="input.plot_type == 'Histogram'",
                                    selectInput("hist_var", "Select a Variable for the Histogram",
                                                choices = var_names,
                                                selected = "Cement"),
                                    checkboxInput("hist_color_code", "Color code this Histogram by compressive strength?"),
                                    checkboxInput("hist_density", "Overlay a density to this Histogram?"),
                                    sliderInput("nBins", "Select the number of bins for this Histogram",
                                                min=10, max=75, step=1, value=30),
                                    conditionalPanel(condition="input.hist_density == 1",
                                        sliderInput("alpha_value", "Select Opacity",
                                                    min=0.1, max=1, step=0.05, value=0.7),
                                        sliderInput("adjust_value", "Select an Adjustment Value",
                                                    min=0.1, max=1, step=0.05, value=0.5))),
                                conditionalPanel(condition="input.plot_type == 'Scatter'",
                                    selectInput("xScatter", "Choose an X axis",
                                                choices = var_names,
                                                selected = "Cement"),
                                    selectInput("yScatter", "Choose a Y axis",
                                                choices = var_names,
                                                selected = "Cement"),
                                                checkboxInput("scatter_color_code", "Color code this Scatter Plot by compressive strength?"),
                                                checkboxInput("scatter_trend", "Add a trendline?")),
                                conditionalPanel(condition="input.plot_type == 'Box_plot'",
                                    selectInput("box_var", "Select a Variable for this Boxplot",
                                                choices = var_names,
                                                selected="Cement"),
                                                checkboxInput("group_box", "Group the Boxplot by compressive strength?"))
                            ), #End sidebarPanel
                                     
                                mainPanel(
                                    box(width = 12, 
                                        plotlyOutput("summary_plot")
                                    )    
                                ) #End mainPanel
                            ) #End sidebar layout
                )#End Tab Panel
    
            )#End tabsetPanel
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