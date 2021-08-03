#Required Packages
packages <- c("shiny","shinydashboard","shinybusy","tidyverse", "ggplot2","DT","readr","knitr","dplyr","gbm","randomForest","caret","tree", "plotly","class", "rpart", "rpart.plot")
lapply(packages, library, character.only = TRUE)

#Read in the dataset and categorize the compressive strength
data  <-  read_csv("Concrete_Dataset.csv")
data  <-  data %>% mutate(data,Concrete_Comp_strength_cat =
                            ifelse(Concrete_compressive_strength <45, "Low Compressive Strength",
                            ifelse(Concrete_compressive_strength >45, "High Compressive Strength", "")))
data$Concrete_Comp_strength_cat <- factor(data$Concrete_Comp_strength_cat, levels=c("Low Compressive Strength", "High Compressive Strength"))


shinyServer(function(input, output, session) {
  #Create the data table for the Data page
  getData_Data_Page <- reactive({
    newData <- data %>% filter(data$Concrete_Comp_strength_cat == input$comp_strength_cat)
  })
  
  output$Data_Tables <- renderDataTable({
    newdata <- getData_Data_Page()
  })
  
  #Make a button that downloads a csv file of the data
  output$Download_Data_Table <- downloadHandler(
    filename = function(){
      paste(input$comp_strength_cat, ".csv", sep="")
    },
    content = function(file){
      write.csv(getData_Data_Page(), file, row.names = FALSE)
    }
  )
  
  
#Create outputs for the Data Exploration Page
  
  #Create the graphical summaries outputs
  output$summary_plot <- renderPlotly({
    
    if(input$plot_type == "Histogram"){
      #Create the Histogram plot 
      if(input$hist_color_code & input$hist_density){
        g  <- ggplot(data=data, aes_string(x=input$hist_var))+
          geom_histogram(aes(fill=Concrete_Comp_strength_cat, y=..density..), position="dodge", color="black", bins = input$nBins)+
          geom_density(aes(fill=Concrete_Comp_strength_cat), alpha=input$alpha_value, adjust=input$adjust_value)+
          scale_fill_manual(values=c("#56B4E9", "green"))+
          theme(legend.title = element_blank())
        ggplotly(g) %>%
          layout(legend = list(orientation = "v", x = 1))
      } else if(input$hist_color_code & !input$hist_density){
        g <- ggplot(data=data, aes_string(x=input$hist_var))+
          geom_histogram(aes(fill=Concrete_Comp_strength_cat, y=..density..), color="black", bins = input$nBins, position="dodge")+
          scale_fill_manual(values=c("#56B4E9", "green"))+
          theme(legend.title = element_blank())
        ggplotly(g) %>%
          layout(legend = list(orientation = "v", x = 1))
      } else if(!input$hist_color_code & input$hist_density){
        g <- ggplot(data=data, aes_string(x=input$hist_var))+
          geom_histogram(aes(y=..density..), fill="#56B4E9", color="black", bins = input$nBins)+
          geom_density(fill="#56B4E9", alpha=input$alpha_value, adjust=input$adjust_value)
        ggplotly(g) %>%
          layout(legend = list(orientation = "v", x = 1.5))
      } else if(!input$hist_color_code & !input$hist_density){
        g <- ggplot(data=data, aes_string(x=input$hist_var), aes(y=..density..))+
          geom_histogram(aes(y=..density..), fill="#56B4E9", color="black", bins = input$nBins)
        ggplotly(g) %>%
          layout(legend = list(orientation = "v", x = 1.5))
      }
      
      
    } else if(input$plot_type == "Scatter"){
      #Create Scatter Plots
      if(input$scatter_color_code & input$scatter_trend){
        g <- ggplot(data=data, aes_string(x=input$xScatter, y=input$yScatter))+
          geom_point(aes(color=Concrete_Comp_strength_cat), alpha=0.8)+
          geom_smooth(aes(group=Concrete_Comp_strength_cat, color=Concrete_Comp_strength_cat), se=TRUE)+
          scale_color_manual(values=c("#56B4E9", "green"))+
          theme(legend.title = element_blank())
        ggplotly(g)
      } else if(input$scatter_color_code & !input$scatter_trend){
        g <- ggplot(data=data, aes_string(x=input$xScatter, y=input$yScatter))+
          geom_point(aes(color=Concrete_Comp_strength_cat), alpha=0.8)+
          scale_color_manual(values=c("#56B4E9", "green"))+
          theme(legend.title = element_blank())
        ggplotly(g)
      } else if(!input$scatter_color_code & input$scatter_trend){
        g <- ggplot(data=data, aes_string(x=input$xScatter, y=input$yScatter))+
          geom_point(color="#56B4E9")+
          geom_smooth(se=TRUE, color="black")
        ggplotly(g)
      } else if(!input$scatter_color_code & !input$scatter_trend){
        g <- ggplot(data=data, aes_string(x=input$xScatter, y=input$yScatter))+
          geom_point(color="#56B4E9")
        ggplotly(g)
      }
    }else if(input$plot_type == "Box_plot"){
      #Create Box plots
      if(input$group_box){
        g <- ggplot(data = data, aes_string(y=input$box_var))+
          geom_boxplot(aes(x=Concrete_Comp_strength_cat, color=Concrete_Comp_strength_cat))+
          scale_color_manual(values=c("#56B4E9", "green"))+
          theme(legend.title = element_blank())
        ggplotly(g)
      } else {
        g <- ggplot(data = data, aes_string(y=input$box_var))+
          geom_boxplot(color="#56B4E9")
        ggplotly(g)
      }
    }
    
  })  

  #Create Numerical Summaries outputs
  output$numerical_summary <-  renderTable({
    
    if(input$group_by_CompressiveStrength){
      Sum_Lowcomp <- data %>% select(input$numerical_var, Concrete_Comp_strength_cat) %>% filter(Concrete_Comp_strength_cat=="Low Compressive Strength")
      Sum_Lowcomp <- Sum_Lowcomp %>% summarize(
        N = n(),
        Min = min(Sum_Lowcomp[[input$numerical_var]]),
        IQR = IQR(Sum_Lowcomp[[input$numerical_var]]),
        Med = median(Sum_Lowcomp[[input$numerical_var]]),
        Max = max(Sum_Lowcomp[[input$numerical_var]]),
        Mean = round(mean(Sum_Lowcomp[[input$numerical_var]]), 3),
        SD = round(sd(Sum_Lowcomp[[input$numerical_var]]), 3)
      )
      Sum_Lowcomp$Concrete_Comp_strength_cat <- "Low Compressive Strength"
      
      Sum_Highcomp <- data %>% select(input$numerical_var, Concrete_Comp_strength_cat) %>% filter(Concrete_Comp_strength_cat=="High Compressive Strength")
      Sum_Highcomp <- Sum_Highcomp %>% summarize(
        N = n(),
        Min = min(Sum_Highcomp[[input$numerical_var]]),
        IQR = IQR(Sum_Highcomp[[input$numerical_var]]),
        Med = median(Sum_Highcomp[[input$numerical_var]]),
        Max = max(Sum_Highcomp[[input$numerical_var]]),
        Mean = round(mean(Sum_Highcomp[[input$numerical_var]]), 3),
        SD = round(sd(Sum_Highcomp[[input$numerical_var]]), 3)
      )
      Sum_Highcomp$Concrete_Comp_strength_cat <- "High Compressive Strength"
      
      Num_Sum_Tab <- rbind(Sum_Lowcomp, Sum_Highcomp)
      Num_Sum_Tab
      
    } else {
      Num_Sum_Tab <- data %>% select(input$numerical_var) %>% summarize(
        N = n(),
        Min = min(data[[input$numerical_var]]),
        IQR = IQR(data[[input$numerical_var]]),
        Med = median(data[[input$numerical_var]]),
        Max = max(data[[input$numerical_var]]),
        Mean = round(mean(data[[input$numerical_var]]), 3),
        SD = round(sd(data[[input$numerical_var]]), 3)
      ) 
    }
    
    Num_Sum_Tab
  }, rownames = TRUE, colnames = TRUE, striped=TRUE, width = NULL)
  
  
  #Doing a split for training/testing based on the user's choice
  
    data_split <- reactive({
      set.seed(365)
      train <- sample(1:nrow(data),size = input$data_train*nrow(data))
      data_Train <- data[train, ]
      data_Test <- data[test, ]
      return(list(Train=data_Train, Test=data_Test))
    })
  
    # Regression model
 
    reg_model <- reactive({
      if (length(input$variables)==0){
        return(formula(paste0(input$Response,"~", "Cement +Blast_Furnace_Slag +Fly_Ash +Water +Superplasticizer +Coarse_Aggregate  +Fine_Aggregate+Age")))
      } else{
        n <- length(input$variables)
        red_model <- paste0(input$variables,c(rep("+",n-1),""))
        red_model <- paste0(red_model, collapse = "")
        return(formula(paste0(input$Response, '~', red_model)))
      }
    })
    
    # Fit the Multiple Linear Regression model
    fit_mlr <- eventReactive(input$run_mlr,{
      fit.mlr <- lm(reg_model(), data = data_split()[["Train"]])
      return(fit.mlr)
    })
    
    # Output for mlr
    output$output_mlr <- renderPrint({
      if (input$run_mlr){
        summary(fit_mlr())
      }
    })
    
    # Fit the Random Forest model
    
    fit_rf <- eventReactive(input$run_rf,{
      trctrl <- trainControl(method = "repeatedcv", number=input$cv_fold, repeats=3)
      rf_grid <- expand.grid(mtry = 1:11)
      rf_train <- train(reg_model(), 
                        data = data_split()[["Train"]], 
                        method='rf', 
                        trControl=trctrl,
                        tuneGrid = rf_grid,
                        preProcess=c("center", "scale"))
      return(rf_train)
    })
    
    
    # Output for rf
    output$output_rf <- renderPrint({
      if (input$run_rf){
        fit_rf()[["results"]]
      }
    })
  
    # Fit the Regression Tree model
    
    fit_rt <- eventReactive(input$run_rt,{
      tree_fit <- rpart(
        formula = reg_model(),
        data = data_split()[["Train"]],
        method="anova", #for regression tree
        control=rpart.control(minsplit=30,cp=0.001))
        tree_cp <- printcp(tree_fit)
      
      return(tree_cp)
      
    })
    
    
    # Output for rt
    output$output_rt <- renderPrint({
      if (input$run_rt){
        fit_rt()
      }
    })
})


