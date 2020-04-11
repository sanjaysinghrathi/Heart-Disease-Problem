library(shiny)
library(DT)

ui <- fluidPage(
  
  titlePanel(title=div("Heart Disease Data Set Analysis by S.Rathee"), windowTitle = ""),  
  
  
  fluidRow(
    
    column(3,
           
           wellPanel(
             
             h4("Heart Disease Data"),
             hr(),
             h6("There are 14 variables provided in the data set and the last one is the dependent variable that we want to be able to predict. Here is a summary of what the other variables mean:"),
             h6("1. Age: Age of subject"),
             h6("2. Sex: The person’s sex (1 = male, 0 = female)"),
             h6("3. Chest-pain type: The chest pain experienced (Value 1: typical angina, Value 2: atypical angina, Value 3: non-anginal pain, Value 4: asymptomatic)"),
             h6("4. Resting Blood Pressure: Resting blood pressure in mm Hg"),
             h6("5. Serum Cholesterol: Serum cholesterol in mg/dl"),
             h6("6. Fasting Blood Sugar: Fasting blood sugar level:(if > 120 mg/dl, 1 = true; 0 = false)"),
             h6("7. Resting ECG: Resting electrocardiographic results:(0 = normal, 1 = having ST-T wave abnormality, 2 = showing probable or definite left ventricular hypertrophy by Estes’ criteria)"),
             h6("8. Max Heart Rate Achieved: Max heart rate of subject"),
             h6("9. Exercise Induced Angina:(1 = yes; 0 = no)"),
             h6("10. ST Depression Induced by Exercise Relative to Rest: ST Depression of subject"),
             h6("11. Peak Exercise ST Segment:(Value 1: upsloping, Value 2: flat, Value 3: downsloping)"),
             h6("12. Number of Major Vessels (0-3) Visible on Flouroscopy: Number of visible vessels under flouro"),
             h6("13. Thal: Form of thalassemia:(3 = normal; 6 = fixed defect; 7 = reversable defect)"),
             h6("14. Diagnosis of Heart Disease: Indicates whether subject is suffering from heart disease or not:(0 = no, 1,2,3,4 = yes)"),
             # fileInput("file1", "Upload CSV File having expression data (rows as probeid and columns as sample names)", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
             # h4("or"),
             # fileInput("file3", "Upload CDF File (.CDF ext only)", accept = c(".CDF"), multiple = FALSE),
             # fileInput("file2", "Upload CEL Files", accept = c(".CEL", ".CEL.gz"), multiple = TRUE)
           ),
           wellPanel(
             
             h4("Plot Original Data"),
             # hr(),
             # h6("The workflow below breaks out the categorical variables and visualizes them on a faceted bar plot.
             #    I’m recoding the factors levels from numeric back to text-based so the labels are easy to interpret on the plots and 
             #    stripping the y-axis labels since the relative differences are what matters. 
             #    We prefer boxplots for evaluating the numeric variables."),
             # hr(),
             # #selectInput('xcol', 'Plot Attribute (Alone or w.r.t. Response)', choices = c("Plot Attribute Only", "Plot Attribute wrt Response"), selected = c("Plot Attribute wrt Response")),
             selectInput('xcol1', 'Select Attribute', choices = names),
             h6("The faceted plots for categorical and numeric variables suggest the following conditions are associated with increased prevalence of heart disease (note: this does not mean the relationship is causal)."),
             h6("1. Asymptomatic angina chest pain (relative to typical angina chest pain, atypical angina pain, or non-angina pain)"),
             h6("2. Presence of exercise induced angina"),
             h6("3. Lower fasting blood sugar"),
             h6("4. Flat or down-sloaping peak exercise ST segment"),
             h6("5. Presence of left ventricle hypertrophy"),
             h6("6. Male"),
             h6("7. Higher thelassemia score"),
             h6("8. Higher age"),
             h6("9. Lower max heart rate achieved"),
             h6("10. Higher resting blood pressure"),
             h6("11. Higher cholesterol"),
             h6("12. Higher ST depression induced by exercise relative to rest"),
           ),
           wellPanel(
             
             h4("Plot Processed Data"),
             hr(),
             h6("The data cleaning pipeline below deals with NA values,
                converts some variables to factors, lumps the dependent variable into two buckets,
                removes the rows that had “?” for observations, and reorders the variables within the dataframe."),
             h6("It also converts target as binary variable. Heart disease (0 = no, 1 = yes)"),
             h6("Since any value above 0 in ‘Diagnosis_Heart_Disease’ (column 14) indicates the presence of heart disease, we can lump all levels > 0 together so the classification predictions are binary – Yes or No (1 or 0).
                The total count of positive heart disease results is less than the number of negative results so the fct_lump() call with default arguments will convert that variable from 4 levels to 2."),
             hr(),
              selectInput('ycol1', 'Select Attribute', choices = names),
           ),
           wellPanel(
             
             h4("Correlation Matrix"),
             hr(),
             h5("Highly correlated variables can lead to overly complicated models or wonky predictions. The ggcorr() function from GGally package provides a nice, clean correlation matrix of the numeric variables. The default method is Pearson which I use here first. Pearson isn’t ideal if the data is skewed or has a lot of outliers so I’ll check using the rank-based Kendall method as well."),
             hr(),
             numericInput('corr_cutoff', 'Correlation Matrix Cutoff (if type logical)', 0.7, min = 0, max = 1),
             selectInput('corr', 'Correlation Matrix Type', choices = c("Integer", "Logical")),
             hr(),
             h5("There are very minor differences between the Pearson and Kendall results. No variables appear to be highly correlated. As such, it seems reasonable to stay with the original 14 variables as we proceed into the modeling section."),
             h4("."),
             h4("."),
             h4("."),
             h4("."),
             h4("."),
             h4("."),
             h4("."),
             h4("."),
             h4("."),
             h4("."),
             h4("."),
             h4("."),
             h4("."),
             h4("."),
             h4("."),
             h4("."),
           ),
           wellPanel(
             
             h4("Principal Component Analysis"),
             hr(),
             
             checkboxInput("pca", "Plot PCA1 vs PCA2 vs PCA3"),
             hr(),
             checkboxInput("pca_select", "Plot Selected PCA"),
             selectInput('pca1', 'X-axis PCA', choices = c(1:length(names)) ),
             selectInput('pca2', 'Y-axis PCA', choices = c(1:length(names)) ),
           ),
           wellPanel(
             
             h4("Model Training & Testing"),
             hr(),
             
             numericInput('model_cutoff', 'Distribution factor for Training and Testing', 0.8, min = 0, max = 1),
             selectInput('model', 'Select ML Model(It will take few Minutes)', choices = c("No Model", "Elastic-Net", "Support Vector Machines", "Neural Networks", "Random Forest", 
                                                                 "Elastic-Net with feature selection", "Support Vector Machines with feature selection",
                                                                 "Neural Networks with feature selection", "Random Forest with feature selection"), selected ="No Model" ),
             #checkboxInput("pca", "Plot PCA1 vs PCA2"),
           ),
           
    ),
    
    
  
  fluidRow(
    column(8, wellPanel(
      "OUTPUT BOX",
      hr(),
      "Table for Heart Disease Data",
      #tableOutput("e"),
      DT::dataTableOutput('e2'),
      
      hr(),
      "Plot for an attribute in original data",
      plotOutput("plot", width = "1150px"),
      
      hr(),
      "Plot for an attribute in processed data",
      plotOutput("plot_processed", width = "1150px"),
      
      hr(),
      "Table for correlation matrix",
      DT::dataTableOutput('corre'),
      "Plot for correlation matrix",
      plotOutput("correplot", width = "1150px"),
      
      hr(),
      "Principal Component Analysis Plot",
      plotOutput("pca", width = "1150px"),
      tableOutput("finish"),
 
      
      hr(),
      "Confusion Table for Model",
      DT::dataTableOutput('ots1'),
      "Results Table for Model",
      DT::dataTableOutput('ots3'),
      "Plot for ROC Curve",
      plotOutput("ots2", width = "1150px"),
      
      hr(),
    
    )),
    
    
  )
  )
  
  
  
)