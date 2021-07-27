#Required Packages
packages <- c("shiny","shinydashboard","shinybusy","tidyverse", "ggplot2","DT","readr","knitr","dplyr","gbm","randomForest","caret","tree", "plotly","class")
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
  
})


