# install.packages("shinydashboard")
# install.packages("rsconnect")
# install.packages("shiny")
# install.packages("shinythemes")
# install.packages("caTools")
# install.packages("randomForest")

library(caTools)
library(shinydashboard)
library(rsconnect)
library(shiny)
library(shinythemes)
library(readr)
library(ggplot2)
library(readxl)
library(caret)
library(dplyr)
library(randomForest)

# Importing required packages and dataset 
# setwd("~/R/app")

d = read.table('processed_cleveland.csv',sep=",",header=FALSE,
               col.names = c('age','sex','cp','trestbps','chol','fbs','restecg',
                             'thalach','exang','oldpeak','slope','ca','thal','target'))
d$ca <- as.integer(d$ca)
d$thal <- as.integer(d$thal)
head(d,n=3)

# Data Pre-processing
d[d == '?'] <- NA
d <- transform(
  d,
  age=as.integer(age),
  sex=as.factor(sex),
  cp=as.factor(cp),
  trestbps=as.integer(trestbps),
  chol=as.integer(chol),
  fbs=as.factor(fbs),
  restecg=as.factor(restecg),
  thalach=as.integer(thalach),
  exang=as.factor(exang),
  oldpeak=as.numeric(oldpeak),
  slope=as.factor(slope),
  ca=as.factor(ca),
  thal=as.factor(thal),
  target=as.factor(target)
)


#Dropping rows with nulls
d <- d[!(d$ca %in% c(NA)),]
d <- d[!(d$thalach %in% c(NA)),]
head(d)

table(d['target'])
d[,'target'] <- ifelse(d[,'target']==0,0,1)
d[,'target'] = as.factor(d[,'target'])
levels(d$target) <- make.names(levels(factor(d$target)))
levels(d$target)
d$target <- relevel(d$target,"X1")
head(d)
str(d)

# Renaming target variable
names(d)[names(d) == "target"] <- "y"

#Removing null values
d <- na.omit(d)


#Prediction model
sample = sample.split(d$y, SplitRatio = .75)
train = subset(d, sample == TRUE)
test  = subset(d, sample == FALSE)
set.seed(123)
myModel1 <- glm(
  y ~ ., family = 'binomial',
  data=train
)

# Building dataset for EDA
df = read.csv(file = 'processed.cleveland.data')
head(df)

names(df) <- c('age','sex','cp','trestbps','chol','fbs','restecg',
               'thalach','exang','oldpeak','slope','ca','thal','target')
head(df)

for (row in 1:nrow(df)) {
  if (df[row, "target"]>0){
    df[row, "target"] <- 1
  }
  
}

head(df)

for (row in 1:nrow(df)) {
  if (df[row, "target"]>0){
    df[row, "heart_disease"] <- "Yes"
  }
  else{
    df[row, "heart_disease"] <- "No"
  }
  
}

for (row in 1:nrow(df)) {
  if (df[row, "fbs"]>0){
    df[row, "fbs_range"] <- "> 120"
  }
  else{
    df[row, "fbs_range"] <- "< 120"
  }
  
}

head(df)

for (row in 1:nrow(df)) {
  if (df[row, "sex"]>0){
    df[row, "Gender"] <- "Male"
  }
  else{
    df[row, "Gender"] <- "Female"
  }
  
}

for (row in 1:nrow(df)) {
  if (df[row, "exang"]>0){
    df[row, "exercise_angina"] <- "Yes"
  }
  else{
    df[row, "exercise_angina"] <- "No"
  }
  
}

for (row in 1:nrow(df)) {
  if (df[row, "slope"]==1){
    df[row, "slope_segment"] <- "Upslope"
  }
  else if (df[row, "slope"]==2) {
    df[row, "slope_segment"] <- "Flat"
  }
  else{
    df[row, "slope_segment"] <- "Downslope"
  }
}

for (row in 1:nrow(df)) {
  if (df[row, "restecg"]==0){
    df[row, "restecg_report"] <- "Normal"
  }
  else if (df[row, "restecg"]==1){
    df[row, "restecg_report"] <- "ST-T wave abnormality"
  }
  else{
    df[row, "restecg_report"] <- "Probable or definite left ventricular hypertrophy"
  }
  
}

head(df)
for (row in 1:nrow(df)) {
  if (df[row, "cp"]==1){
    df[row, "Chest_Pain_type"] <- "typical angina"
  }
  else if (df[row, "cp"]==2){
    df[row, "Chest_Pain_type"] <- "atypical angina"
  }
  else if (df[row, "cp"]==3){
    df[row, "Chest_Pain_type"] <- "non-anginal pain"
  }
  else{
    df[row, "Chest_Pain_type"] <- "asymptomatic"
  }
}

for (row in 1:nrow(df)) {
  if (df[row, "thal"]=="3.0"){
    df[row, "thal_report"] <- "Normal"
  }
  else if (df[row, "thal"]== "6.0"){
    df[row, "thal_report"] <- "Fixed Defect"
  }
  else{
    df[row, "thal_report"] <- "Reversible Defect"
  }
  
}

for (row in 1:nrow(df)) {
  if (df[row, "ca"]=="0.0"){
    df[row, "ca_number"] <- 0
  }
  else if (df[row, "ca"]== "1.0"){
    df[row, "ca_number"] <- 1
  }
  else if (df[row, "ca"]== "2.0"){
    df[row, "ca_number"] <- 2
  }
  else{
    df[row, "ca_number"] <- 3
  }
  
}

head(df)


frow1 <- fluidRow(
  box(
    
    plotOutput("Age", height = "300px")
    ,numericInput("Age", "Enter the age:",  50)
  ),
  box(
    plotOutput("Gender", height = "300px")
    ,checkboxGroupInput("Gender", "Gender",
                        c("Male" = "Male",
                          "Female" = "Female")
                        
    )
  ))

frow2 <- fluidRow(
  box(
    
    plotOutput("trestbps", height = "300px")
    ,sliderInput("trestbps", "Select range for Trestbps:", min(df['trestbps']),max(df['trestbps']), 130)
  ),
  box(
    plotOutput("ChestPaintype", height = "300px")
    ,checkboxGroupInput("ChestPaintype", "Chest_Pain_type",
                        c("typical angina" = "typical angina",
                          "atypical angina" = "atypical angina",
                          "non-anginal pain" = "non-anginal pain",
                          "asymptomatic" = "asymptomatic")
                        
    )
  ))

frow3 <- fluidRow(
  box(
    
    plotOutput("Chol", height = "300px")
    ,sliderInput("Chol", "Select range for Chol:", min(df['chol']),max(df['chol']), 210)
  ),
  box(
    
    plotOutput("Thalach", height = "300px")
    ,numericInput("Thalach", "Enter value for thalach:",  120)
  ))

frow4 <- fluidRow(
  box(
    title="Heart Disease vs Chest Pain ",
    plotOutput("fbs_range", height = "300px")
    ,checkboxGroupInput("fbs_range", "Chest_Pain_type",
                        c("< 120" = "< 120",
                          "> 120" = "> 120"))
  ),
  box(
    title="Heart Disease vs restecg report",
    plotOutput("restecg_report", height = "300px")
    ,checkboxGroupInput("restecg_report", "restecg_report",
                        c("Probable or definite left ventricular hypertrophy" = "Probable or definite left ventricular hypertrophy",
                          "Normal" = "Normal",
                          "ST-T wave abnormality"="ST-T wave abnormality"))
  ))

frow5 <- fluidRow(
  box(
    title="Heart Disease vs Exercise Angina",
    plotOutput("exercise_angina", height = "300px")
    ,checkboxGroupInput("exercise_angina", "exercise_angina",
                        c("Yes" = "Yes",
                          "No" = "No"))
  ),
  box(
    title="Heart Disease vs Slope Segment",
    plotOutput("slope_segment", height = "300px")
    ,checkboxGroupInput("slope_segment", "slope_segment",
                        c("Downslope" = "Downslope",
                          "Upslope" = "Upslope",
                          "Flat"="Flat"))
  ))

frow6 <- fluidRow(
  box(
    
    plotOutput("oldpeak", height = "300px")
    ,sliderInput("oldpeak", "Select range for oldpeak:", min(df['oldpeak']),max(df['oldpeak']), 2.5)
  ),
  box(
    
    plotOutput("thal_report", height = "300px")
    ,checkboxGroupInput("thal_report", "thal_report",
                        c("Normal" = "Normal",
                          "Reversible Defect" = "Reversible Defect",
                          "Fixed Defect"="Fixed Defect"
                        ))
  ))

frow7 <- fluidRow(
  box(
    
    plotOutput("ca_number", height = "300px")
    ,numericInput("ca_number", "Enter value for thalach:",  2)
  ))

frow8 <- fluidRow(
  numericInput(inputId = "age", 
               label = "Enter your age",
               value = 55
  ),
  radioButtons(inputId = "sex", 
               label = "Select your gender",choices = list("Male" = 1, "Female" = 0
               ),selected = 1),
  radioButtons(inputId = "cp", 
               label = "Select the type of your chest pain",choices = list("Typical angina" = 1,"Atypical angina" = 2, 
                                           "Non-anginal pain" = 3, "Asymptomatic" = 4),
               selected = 1
  ),
  numericInput(inputId = "trestbps", 
               label = "Enter resting blood pressure (in mm Hg on admission to the hospital)",
               value = 130
  ),
  numericInput(inputId = "chol", 
               label = "Enter serum cholestoral in mg/dl",
               value = 250
  ),
  radioButtons(inputId = "fbs", 
               label = "Select if fasting blood sugar > 120mg/dL",
               choices = list("No" = 0,"Yes" = 1),
               selected = 1
  ),
  radioButtons(inputId = "restecg", 
               label = "Enter your resting electrocardiographic results",
               choices = list("Normal" = 0,"Having ST-T wave abnormality" = 1, 
                                                "Showing probable or definite left ventricular hypertrophy" = 2),
               selected = 1
  ),
  numericInput(inputId = "thalach", 
               label = "What was your maximum heart rate achieved?",
               value = 150
  ),
  radioButtons(inputId = "exang", 
               label = "Do you have exercise induced angina? ",
               choices = list("No" = 0,"Yes" = 1),
               selected = 1
  ),
  numericInput(inputId = "oldpeak", 
               label = "oldpeak",
               value=1
  ),
  radioButtons(inputId = "slope", 
               label = "What is the slope of your peak exercise ST segment?",
               choices = list("Upsloping" = 1,"Flat" = 2, 
                                              "Downsloping" = 3),
               selected = 1
  ),  
  radioButtons(inputId = "ca", 
               label = "What is the number of major vessels (0-3) colored by flouroscopy?",
               choices = list("0" = 0,"1" = 1, "2" = 2, "3" = 3),
               selected = 1
  ),
  radioButtons(inputId = "thal", 
               label = "What is your thalassemic status?",
               choices = list("Normal" = 3,"Fixed defect" = 6,
                                             "Reversable defect" = 7),
               selected = 3
  ),
  
  actionButton(inputId = "go", 
               label = "Get Prediction"),
  htmlOutput ("pred"),
  htmlOutput ("feedback"))


header <- dashboardHeader(title = "Heart Disease Classification")  
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("EDA-1", tabName = "EDA-1", icon = icon("chart-bar")),
    menuItem("EDA-2", tabName = "EDA-2", icon = icon("chart-line")),
    menuItem("EDA-3", tabName = "EDA-3", icon = icon("columns")),
    menuItem("Prediction", tabName = "Prediction", icon = icon("bullseye"))
  )
)


body <- dashboardBody(tabItems(tabItem(tabName = "EDA-1",frow1,frow2),
                               tabItem(tabName = "EDA-2",frow3,frow4),
                               tabItem(tabName = "EDA-3",frow5,frow6,frow7),
                               tabItem(tabName = "Prediction",frow8)))


ui <- dashboardPage(title = 'Application for Heart Disease Classification',header, sidebar, body, skin='red')


server = function(input, output) {
  data1 <- reactive({
    subset(df, df$age < input$Age)})
  output$Age <- renderPlot({
    ggplot(as.data.frame(data1()), aes(x = age, fill = heart_disease)) +
      geom_density() +
      labs(x = "Age", y= "Heart Disease")
    
  })
  
  data2 <- reactive({
    subset(df, df$Gender %in% input$Gender)})
  output$Gender <- renderPlot({
    ggplot(as.data.frame(data2()), aes(x = Gender, fill = heart_disease)) +
      geom_bar() +
      labs(x = "Gender", y= "Heart Disease")
  })
  data3 <- reactive({
    subset(df, df$trestbps < input$trestbps)})
  output$trestbps <- renderPlot({
    ggplot(as.data.frame(data3()), aes(x = trestbps, fill = heart_disease)) +
      geom_histogram() +
      labs(x = "Trestbps", y= "Heart Disease")
  })
  data4 <- reactive({
    subset(df, df$Chest_Pain_type %in% input$ChestPaintype)})
  output$ChestPaintype <- renderPlot({
    ggplot(as.data.frame(data4()), aes(x = Chest_Pain_type, fill = heart_disease)) +
      geom_bar() +
      labs(x = "ChestPaintype", y= "Heart Disease")
  })
  data5 <- reactive({
    subset(df, df$chol < input$Chol)})
  output$Chol <- renderPlot({
    ggplot(as.data.frame(data5()), aes(x = chol, fill = heart_disease)) +
      geom_density() +
      labs(x = "Chol", y= "Heart Disease")
  })
  data6 <- reactive({
    subset(df, df$thalach < input$Thalach)})
  output$Thalach <- renderPlot({
    ggplot(as.data.frame(data6()), aes(x = thalach, fill = heart_disease)) +
      geom_histogram() +
      labs(x = "thalach", y= "Heart Disease")
  })
  
  data7 <- reactive({
    subset(df, df$fbs_range %in% input$fbs_range)})
  output$fbs_range <- renderPlot({
    ggplot(as.data.frame(data7()), aes(x = fbs_range, fill = heart_disease)) +
      geom_bar() +
      labs(x = "fbs_range", y= "Heart Disease")
  })
  
  data8 <- reactive({
    subset(df, df$restecg_report %in% input$restecg_report)})
  output$restecg_report <- renderPlot({
    ggplot(as.data.frame(data8()), aes(x = heart_disease, fill = restecg_report)) +
      geom_bar() +
      labs(x = "Heart Disease", y= "restecg_report")
  })
  
  data9 <- reactive({
    subset(df, df$exercise_angina %in% input$exercise_angina)})
  output$exercise_angina <- renderPlot({
    ggplot(as.data.frame(data9()), aes(x = exercise_angina, fill = heart_disease)) +
      geom_bar() +
      labs(x = "exercise_angina", y= "Heart Disease")
  })
  
  data10 <- reactive({
    subset(df, df$slope_segment %in% input$slope_segment)})
  output$slope_segment <- renderPlot({
    ggplot(as.data.frame(data10()), aes(x = slope_segment, fill = heart_disease)) +
      geom_bar() +
      labs(x = "slope_segment", y= "Heart Disease")
  })
  
  data11 <- reactive({
    subset(df, df$oldpeak < input$oldpeak)})
  output$oldpeak <- renderPlot({
    ggplot(as.data.frame(data11()), aes(x = oldpeak, fill = heart_disease)) +
      geom_density() +
      labs(x = "oldpeak", y= "Heart Disease")
  })
  
  data12 <- reactive({
    subset(df, df$thal_report == input$thal_report)})
  output$thal_report <- renderPlot({
    ggplot(as.data.frame(data12()), aes(x = thal_report, fill = heart_disease)) +
      geom_bar() +
      labs(x = "thal_report", y= "Heart Disease")
  })
  
  data13 <- reactive({
    subset(df, df$ca_number < input$ca_number)})
  output$ca_number <- renderPlot({
    ggplot(as.data.frame(data13()), aes(x = ca_number, fill = heart_disease)) +
      geom_density() +
      labs(x = "ca_number", y= "Heart Disease")
  })
  
  
  ee <- eventReactive(input$go, {
    sample.obs <- cbind(input$age,input$sex,input$cp,input$trestbps,input$chol,
                        input$fbs,input$restecg,input$thalach,input$exang,
                        input$oldpeak,input$slope,input$ca,input$thal)
    colnames(sample.obs) <- c('age','sex','cp','trestbps','chol','fbs','restecg',
                              'thalach','exang','oldpeak','slope','ca','thal')
    cat.cols = c('sex','cp','fbs','restecg','exang','slope','ca','thal')
    sample.obs <- data.frame(sample.obs)
    sample.obs <- transform(
      sample.obs,
      age=as.integer(age),
      sex=factor(sex, levels = levels(d$sex)),
      cp=factor(cp, levels = levels(d$cp)),
      trestbps=as.integer(trestbps),
      chol=as.integer(chol),
      fbs=factor(fbs, levels = levels(d$fbs)),
      restecg=factor(restecg,levels = levels(d$restecg)),
      thalach=as.integer(thalach),
      exang=factor(exang, levels = levels(d$exang)),
      oldpeak=as.numeric(oldpeak),
      slope=factor(slope, levels = levels(d$slope)),
      ca=factor(ca, levels = levels(d$ca)),
      thal=factor(thal, levels = levels(d$thal))
    )
    round(1-predict(myModel1, newdata=sample.obs,type='response'),digits = 2)
  })
  
  
  # output$pred <-
  observeEvent(input$go,output$pred <- renderUI({
    # predict(myModel1, newdata=ee(),type='prob')[,1]
    str1 <- paste("<br>","<b>","Based on your current reports, you have a ", ee()*100,"% probability of having a heart disease.",sep="","<b>")
    HTML(paste(str1))
  }))
  
  output$feedback <- renderUI({
    if (ee()>0.5){
      "We recommend you to see your doctor for an immediate follow-up consultation. Meanwhile inculcating some habits like eating less fatty foods, avoiding salt and exercising daily may help alleviate some immediate symptoms"
    } else{
      "It seems like you are fit and have lower chances of having a heart disease. Continue healthy eating habits with light exercise everyday to keep your health in check. Good job!"
    }
  })
  
}

shinyApp(ui, server)
# rsconnect::deployApp(server='shinyapps.io')
