This app enables the user to run exploratory data analyses, fit supervised learning models and run predictions on the target variable from the[Concrete Compressive Strength Data Set] (https://archive.ics.uci.edu/ml/datasets/Concrete+Compressive+Strength), donated by Prof. I-Cheng Yeh (Chung-Hua University, Taiwan) to the UCI Machine Learning Repository.

The __Data__ page contains the dataset to be used for analyses. The data can be sorted, scrolled through, and saved as a csv file. The page contains two tabs for  creating numerical graphs and summaries. You can change the plot and summary types, and also change the variables and data subset used in the plot and summaries. The __Modeling__ page contains 3 tabs: _Modeling Info_ contains information on three supervised learning models, _Model Fitting_ contains a choice of model settings, model comparisons, and appropriate summaries, the _Prediction tab_ provides the user a way to use one of the models for prediction.

The list of codes needed to run the app are `shiny`,`shinydashboard`,`shinybusy`,`tidyverse`, `ggplot2`,`DT`,`readr`,`knitr`,`dplyr`,`gbm`,`randomForest`,`caret`,`tree`, `plotly`, and `class`.

## Code needed to install and load the packages used:
```{r,eval = T}
packages <- c("shiny","shinydashboard","shinybusy","tidyverse", "ggplot2","DT","readr","knitr","dplyr","gbm","randomForest","caret","tree", "plotly","class","png")
install.packages(packages)
lapply(packages, library, character.only = TRUE)
```


## Code needed to run the app in RStudio
```{r, eval=T}
shiny::runGitHub(repo="yemoray/ST-558-Project-3", username = "yemoray",ref = "main", subdir = "ST_558_Project_3")
```
