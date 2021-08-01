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

not_sel <- "Not Selected"

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
                    
                fluidRow(box(title="What's in this page?", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 10, background="blue",
                    h4("The data table is shown below. You can subset the table to show the levels of the different variables that contribute to high compressive strength or low compressive strength."),
                    h4("These subsets can also be downloaded as .csv files by clicking the download button.")),
                             
                    selectizeInput("comp_strength_cat", "Compressive Strength level", choices = levels(as.factor(data$Concrete_Comp_strength_cat))),
                    downloadButton("Download_Data_Table", "Download as .csv file"),
                    hr(), 
                    dataTableOutput("Data_Tables"))),#End Data Page content 
            
            
            #Data Exploration Page Content
            tabItem(tabName = "exploration",strong(em(h2("Graphical and Numeric Summaries"))),
                    
                    fluidRow(box(title="What's in this page?", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 10, background="blue",
                                 h4("The graphical and numeric summaries are shown below. There is a choice of plot type and variables for the different plots."),
                                 h4("These plots can also be interacted with and downloaded as .png files by hovering over the top right of the plots and making a selection from the plotly options."),
                                 h4("The numerical summaries include the minimum, maximum, IQR, mean and standard deviation. You have achoice of seeing the summaries grouped by the compressive strength.")),
                             
                             ),
                             
                tabsetPanel(
                        
                    #Tab for graphical summaries
                    tabPanel("Graphical Summaries",
                        sidebarLayout(
                                     
                            #Sidebar for selecting type of plot
                            sidebarPanel(
                                selectizeInput("plot_type", 
                                    "Select the plot type",
                                    choices = c(not_sel,"Histogram", "Scatter", "Box_plot"), selected = not_sel),
                                conditionalPanel(condition="input.plot_type == 'Histogram'",
                                    selectizeInput("hist_var", "Select a Variable for the Histogram",
                                                choices = c(not_sel,var_names),
                                                selected = "Cement"),
                                    checkboxInput("hist_color_code", "Color code by compressive strength"),
                                    checkboxInput("hist_density", "Overlay a density"),
                                    sliderInput("nBins", "Select the number of bins for this Histogram",
                                                min=10, max=100, step=1, value=20),
                                    conditionalPanel(condition="input.hist_density == 1",
                                        sliderInput("alpha_value", "Select Opacity",
                                                    min=0.1, max=1, step=0.05, value=0.5),
                                        sliderInput("adjust_value", "Select an Adjustment Value",
                                                    min=0.1, max=1, step=0.05, value=0.5))),
                                conditionalPanel(condition="input.plot_type == 'Scatter'",
                                    selectizeInput("xScatter", "Choose an X axis",
                                                   choices = var_names,
                                                selected = "Cement"),
                                    selectizeInput("yScatter", "Choose a Y axis",
                                                   choices = var_names,
                                                selected = "Concrete_compressive_strength"),
                                                checkboxInput("scatter_color_code", "Color code  by compressive strength"),
                                                checkboxInput("scatter_trend", "Add a trendline")),
                                conditionalPanel(condition="input.plot_type == 'Box_plot'",
                                    selectizeInput("box_var", "Select a Variable for this Boxplot",
                                                   choices = var_names,
                                                selected="Cement"),
                                                checkboxInput("group_box", "Group by compressive strength")) 
                            ), #End sidebarPanel
                                     
                                mainPanel(
                                    box(width = 10, 
                                        plotlyOutput("summary_plot")
                                    )    
                                ) #End mainPanel
                            ) #End sidebar layout
                ),#End Tab Panel
                
                #Tab for Numerical Summaries
                tabPanel("Numerical Summaries",
                    sidebarLayout(
                             
                        #Sidebar for selecting type of numerical summary
                            sidebarPanel(
                                 selectizeInput("numerical_var", "Select a variable for numerical summary",
                                             choices = var_names, selected="Cement"),
                                 checkboxInput("group_by_CompressiveStrength", "Group this summary by compressive strength")
                             ),
                             
                             #Main panel
                             mainPanel(
                                 box(width = 12, 
                                     tableOutput("numerical_summary") 
                                 )  
                             )
                         ) #End sidebar layout
                ) #End Tab Panel
            )#End tabsetPanel
            ),#End Data Exploration Page Content        
            
            
            #Modeling Page Content
            tabItem(tabName = "model",
                tabsetPanel(
                        
                    #Modeling Information tab
                    tabPanel("Modeling Info",
                        tabsetPanel(
                                 tabPanel("Multiple Linear Regression",
                                 h4("Linear Regression modeling is one of the supervised learning methods where the output is known, and the goal is to establish a function that best approximates the relationship between desired outputs and the provided sample data. Specifically, linear regression accomplishes
                                 this by learning a model that best fits the linear relationship between the predictor and response variables.The model is fit by minimizing the sum of squared residuals (difference between the observed and predicted responses). Multiple Linear Regression (MLR) has more than one
                                 predictor, and the although relationship between predictors and response remains linear in terms of the model parameters, the MLR model could contain interaction, quadratic and polynomial terms.An example of an MLR below is below:"),
                                 br(),
                                 withMathJax(
                                     helpText('$$y_{i}=\\beta_{0}+\\beta_{1}x_{1}+...+\\beta_{p}x_{p}+\\epsilon$$')
                                 ),
                                 br(),
                                 h4("The beta terms are regression coefficients (the exception is beta zero, the intercept), the x terms are the independent variables,and y is the predicted value of the dependent variable."),
                                 h4("Underlying assumptions for the linear regression model are:"),
                                 h4("Linearity: The model is linear in model parameters, this assumption can be checked using histogram or Q-Q plots"),
                                 h4("Normality: The predictor and response variables are multivariate normal"),
                                 h4("Multicollinearity: There is little to no multicollinearity among the predictor variables. (can be checked using Variance Inflation Factor"),
                                 h4("Homoscedasticity: Residuals are randomly distributed across the regression line (Can be checked using the Residual vs. Fitted value scatter plot. The plot must have to discernable pattern)"),
                                 h4("Autocorrelation: Residuals must be independent of each other (Can be checked using Durbin-Watson’s test)."),
                                 br(),
                                 h4(em("Advantages")),
                                 h4("-It is very useful in identifying and understanding the correlation between the response and predictor variables."),
                                 h4("-It requires relatively low computational power compared to other supervised learning models"),
                                 br(),
                                 h4(em("Disdvantages")),
                                 h4("-It is very sensitive to outliers, which leads to low accuracy."),
                                 h4("-It is prone to underfitting of the data."),
                                 h4("-It sometimes lacks practicality because most real world problems are not linear."),
                             
                                 ),
                        tabPanel("Regression Tree",
                                 h4("Regression tree is a tree based method where the predictor space is split into distinct and non-overlapping regions, with each region having different predictions."),
                                 h4("The mean of the observations in a given region us usually used as the prediction "),
                                 h4("The aim here is to find regions that minimize the Residual Sum of Squares:"),
                                 br(),
                                 withMathJax(
                                     helpText('$$\\sum_{j=1}^J\\sum_{i\\in Rj} (y_i - \\hat{y}_{R_j})^2$$')
                                 ),
                                 br(),
                                 h4("Rj is the jth region, y hat is the mean on the observations in a given region."),
                                 br(),
                                 h4(em("Advantages")),
                                 h4("-It requires less data preparation during pre-processing."),
                                 h4("-It is easy to explain to stakeholders."),
                                 h4("-Data normalization and scaling is not required "),
                                 br(),
                                 h4(em("Disdvantages")),
                                 h4("-It requires more time to train the model."),
                                 h4("-It is relatively expensive and time consuming."),
                                 h4("-It is prone to overfitting of the data."),
                                 
                        ),       
                        tabPanel("Random Forest Model",
                                 h4("Random Forests uses a large number of decision trees to produce a model prediction. It can improve prediction by using bootstrapping to generate a number of prediction candidates from each tree node, and then uses the entire ensemble to produce
                                    a robust prediction model. The prediction advantage from random forests is further enhanced by random forests resulting in prediction models having lower variance than a bagged tree model."),
                                 br(),
                                 h4(em("Advantages")),
                                 h4("-It can be applied to both classification and regression problems."),
                                 h4("-It improves accuracy by reducing overfitting in regression trees."),
                                 br(),
                                 h4(em("Disdvantages")),
                                 h4("-It requires a lot of computational power to build the trees."),
                                 h4("-It suffers from lack ot interpretability."),
                                 
                        )       
                        ),
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