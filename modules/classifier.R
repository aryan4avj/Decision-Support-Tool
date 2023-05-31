classifierui <- function(id) {
  ns <- NS(id)
  tabItem("classifier",
          column(12,
                column(3,
                       selectInput(
                         inputId = ns("classsel"),
                         label = "Select Classifier:",
                         choices = c("Survival Classifier","Remission Classifier","Survival Regression","RT Treatment time Regression"),
                         selected = "Survival Classifier"
                       )
                )
          ),
          column(12,
          conditionalPanel(
            condition = "input.classsel == 'Survival Classifier'",
            ns = ns,
              column(2,
                     selectInput(label = "Sex:", 
                                 inputId = ns("sexselectd"),
                                 choices = unique(filtereddata$`Sex`),
                                 selected = "Male"),
                     numericInput(label = "Age:", 
                                 inputId = ns("ageselectd"),
                                 value = 45,
                                 step = 0.1),
                     numericInput(label = "Height:", 
                                  inputId = ns("heightselectd"),
                                  value = 1.8,
                                  step = 0.01),
                     numericInput(label = "BMI:", 
                                 inputId = ns("bmiselectd"),
                                 value = 25,
                                 step = 0.1),
                     selectInput(label = "Diagnosis:", 
                                 inputId = ns("diagselectd"),
                                 choices = unique(filtereddata$Diag),
                                 selected = "CA soft palate"),
                     selectInput(label = "Grade:", 
                                 inputId = ns("gradeselectd"),
                                 choices = unique(filtereddata$`Grade`),
                                 selected = "moderately diff.")
                     ),
              column(2,
                     selectInput(label = "HPV Status:", 
                                 inputId = ns("hpvselectd"),
                                 choices = unique(filtereddata$`HPV status`),
                                 selected = "unknown"),
                     selectInput(label = "Induction Chemptherapy:", 
                                 inputId = ns("indchselectd"),
                                 choices = unique(filtereddata$`Induction Chemotherapy`),
                                 selected = "No"),
                     selectInput(label = "Concurrent Chemptherapy:", 
                                 inputId = ns("conchselectd"),
                                 choices = unique(filtereddata$`Received Concurrent Chemoradiotherapy?`),
                                 selected = "No"),
                     selectInput(label = "Chemotherapy Regimen:", 
                                 inputId = ns("chemoselectd"),
                                 choices = unique(filtereddata$`Chemotherapy Regimen`),
                                 selected = "No"),
                     selectInput(label = "Platinum-based chemotherapy:", 
                                 inputId = ns("platselectd"),
                                 choices = unique(filtereddata$`Platinum-based chemotherapy`),
                                 selected = "No")
              ),
              column(2,
                     selectInput(label = "Stage:", 
                                 inputId = ns("stageselectd"),
                                 choices = unique(filtereddata$Stage),
                                 selected = "II"),
                     selectInput(label = "CCRT Chemotherapy Regime:", 
                                 inputId = ns("ccrtselectd"),
                                 choices = unique(filtereddata$`CCRT Chemotherapy Regimen`),
                                 selected = "No"),
                     selectInput(label = "Potential Surgery:", 
                                 inputId = ns("surgselectd"),
                                 choices = unique(filtereddata$`Surgery Summary`),
                                 selected = "No"),
                     selectInput(label = "Smoking History:", 
                                 inputId = ns("shselectd"),
                                 choices = unique(filtereddata$`Smoking History`),
                                 selected = 1),
                     selectInput(label = "Current Smoker:", 
                                 inputId = ns("csselectd"),
                                 choices = unique(filtereddata$`Current Smoker`),
                                 selected = 0)
                ),
            column(5,
                   h1("Predicted Patient Outcome: ", textOutput(ns("surv")),
                      style = "font-size:15px;"),
                   br(),
                   h1("This Outcome is the prediction of the classifier",
                      style = "font-size:15px; color: red"),
                   br(),
                   h1("Please consult with your medical professional before making an decision regarding treatment options available",
                      style = "font-size:15px; color: red")
            )
            )
          ),
          column(12,
            conditionalPanel(
              condition = "input.classsel == 'Remission Classifier'",
              ns = ns,
              column(2,
                     selectInput(label = "Sex:", 
                                 inputId = ns("sexselectr"),
                                 choices = unique(filtereddata$`Sex`),
                                 selected = "Female"),
                     numericInput(label = "Age:", 
                                  inputId = ns("ageselectr"),
                                  value = 45,
                                  step = 0.1),
                     numericInput(label = "Height:", 
                                  inputId = ns("heightselectr"),
                                  value = 1.8,
                                  step = 0.01),
                     numericInput(label = "BMI:", 
                                  inputId = ns("bmiselectr"),
                                  value = 25,
                                  step = 0.1),
                     selectInput(label = "Diagnosis:", 
                                 inputId = ns("diagselectr"),
                                 choices = unique(filtereddata$Diag),
                                 selected = "CA soft palate"),
                     selectInput(label = "Grade:", 
                                 inputId = ns("gradeselectr"),
                                 choices = unique(filtereddata$`Grade`),
                                 selected = "moderately diff.")
              ),
              column(2,
                     selectInput(label = "HPV Status:", 
                                 inputId = ns("hpvselectr"),
                                 choices = unique(filtereddata$`HPV status`),
                                 selected = "unknown"),
                     selectInput(label = "Induction Chemptherapy:", 
                                 inputId = ns("indchselectr"),
                                 choices = unique(filtereddata$`Induction Chemotherapy`),
                                 selected = "No"),
                     selectInput(label = "Concurrent Chemptherapy:", 
                                 inputId = ns("conchselectr"),
                                 choices = unique(filtereddata$`Received Concurrent Chemoradiotherapy?`),
                                 selected = "No"),
                     selectInput(label = "Chemotherapy Regimen:", 
                                 inputId = ns("chemoselectr"),
                                 choices = unique(filtereddata$`Chemotherapy Regimen`),
                                 selected = "No"),
                     selectInput(label = "Platinum-based chemotherapy:", 
                                 inputId = ns("platselectr"),
                                 choices = unique(filtereddata$`Platinum-based chemotherapy`),
                                 selected = "No")
              ),
              column(2,
                     selectInput(label = "Stage:", 
                                 inputId = ns("stageselectr"),
                                 choices = unique(filtereddata$Stage),
                                 selected = "I"),
                     selectInput(label = "CCRT Chemotherapy Regime:", 
                                 inputId = ns("ccrtselectr"),
                                 choices = unique(filtereddata$`CCRT Chemotherapy Regimen`),
                                 selected = "No"),
                     selectInput(label = "Potential Surgery:", 
                                 inputId = ns("surgselectr"),
                                 choices = unique(filtereddata$`Surgery Summary`),
                                 selected = "No"),
                     selectInput(label = "Smoking History:", 
                                 inputId = ns("shselectr"),
                                 choices = unique(filtereddata$`Smoking History`),
                                 selected = 1),
                     selectInput(label = "Current Smoker:", 
                                 inputId = ns("csselectr"),
                                 choices = unique(filtereddata$`Current Smoker`),
                                 selected = 0)
              ),
              column(5,
                     h1("Predicted Patient Outcome: ",textOutput(ns("rem")),
                        style = "font-size:15px;"),
                     br(),
                     h1("This Outcome is the prediction of the classifier",
                        style = "font-size:15px; color: red"),
                     br(),
                     h1("Please consult with your medical professional before making an decision regarding treatment options available",
                        style = "font-size:15px; color: red")
              )
            )
        ),
        column(12,
               conditionalPanel(
                 condition = "input.classsel == 'Survival Regression'",
                 ns = ns,
                 column(2,
                        selectInput(label = "Sex:", 
                                    inputId = ns("sexselectsr"),
                                    choices = unique(filtereddata$`Sex`),
                                    selected = "Male"),
                        numericInput(label = "Age:", 
                                     inputId = ns("ageselectsr"),
                                     value = 45,
                                     step = 0.1),
                        numericInput(label = "Height:", 
                                     inputId = ns("heightselectsr"),
                                     value = 1.8,
                                     step = 0.01),
                        numericInput(label = "BMI:", 
                                     inputId = ns("bmiselectsr"),
                                     value = 25,
                                     step = 0.1),
                        selectInput(label = "Diagnosis:", 
                                    inputId = ns("diagselectsr"),
                                    choices = unique(filtereddata$Diag),
                                    selected = "CA soft palate"),
                        selectInput(label = "Grade:", 
                                    inputId = ns("gradeselectsr"),
                                    choices = unique(filtereddata$`Grade`),
                                    selected = "moderately diff.")
                 ),
                 column(2,
                        selectInput(label = "HPV Status:", 
                                    inputId = ns("hpvselectsr"),
                                    choices = unique(filtereddata$`HPV status`),
                                    selected = "unknown"),
                        selectInput(label = "Induction Chemptherapy:", 
                                    inputId = ns("indchselectsr"),
                                    choices = unique(filtereddata$`Induction Chemotherapy`),
                                    selected = "No"),
                        selectInput(label = "Concurrent Chemptherapy:", 
                                    inputId = ns("conchselectsr"),
                                    choices = unique(filtereddata$`Received Concurrent Chemoradiotherapy?`),
                                    selected = "No"),
                        selectInput(label = "Chemotherapy Regimen:", 
                                    inputId = ns("chemoselectsr"),
                                    choices = unique(filtereddata$`Chemotherapy Regimen`),
                                    selected = "No"),
                        selectInput(label = "Platinum-based chemotherapy:", 
                                    inputId = ns("platselectsr"),
                                    choices = unique(filtereddata$`Platinum-based chemotherapy`),
                                    selected = "No")
                 ),
                 column(2,
                        selectInput(label = "Stage:", 
                                    inputId = ns("stageselectsr"),
                                    choices = unique(filtereddata$Stage),
                                    selected = "II"),
                        selectInput(label = "CCRT Chemotherapy Regime:", 
                                    inputId = ns("ccrtselectsr"),
                                    choices = unique(filtereddata$`CCRT Chemotherapy Regimen`),
                                    selected = "No"),
                        selectInput(label = "Potential Surgery:", 
                                    inputId = ns("surgselectsr"),
                                    choices = unique(filtereddata$`Surgery Summary`),
                                    selected = "No"),
                        selectInput(label = "Smoking History:", 
                                    inputId = ns("shselectsr"),
                                    choices = unique(filtereddata$`Smoking History`),
                                    selected = 1),
                        selectInput(label = "Current Smoker:", 
                                    inputId = ns("csselectsr"),
                                    choices = unique(filtereddata$`Current Smoker`),
                                    selected = 0)
                 ),
                 column(5,
                        h1(textOutput(ns("sreg")),
                           style = "font-size:15px;"),
                        br(),
                        h1("This Outcome is the prediction of the regressor
                           Due to lack of data regressor couldnt fit well so this intended as example.",
                           style = "font-size:15px; color: red"),
                        br(),
                        h1("Please consult with your medical professional before making an decision regarding treatment options available",
                           style = "font-size:15px; color: red")
                 )
               )
        ),
        column(12,
               conditionalPanel(
                 condition = "input.classsel == 'RT Treatment time Regression'",
                 ns = ns,
                 column(2,
                        selectInput(label = "Sex:", 
                                    inputId = ns("sexselectrt"),
                                    choices = unique(filtereddata$`Sex`),
                                    selected = "Female"),
                        numericInput(label = "Age:", 
                                     inputId = ns("ageselectrt"),
                                     value = 45,
                                     step = 0.1),
                        numericInput(label = "Height:", 
                                     inputId = ns("heightselectrt"),
                                     value = 1.8,
                                     step = 0.01),
                        numericInput(label = "BMI:", 
                                     inputId = ns("bmiselectrt"),
                                     value = 25,
                                     step = 0.1),
                        selectInput(label = "Diagnosis:", 
                                    inputId = ns("diagselectrt"),
                                    choices = unique(filtereddata$Diag),
                                    selected = "CA soft palate"),
                        selectInput(label = "Grade:", 
                                    inputId = ns("gradeselectrt"),
                                    choices = unique(filtereddata$`Grade`),
                                    selected = "moderately diff.")
                 ),
                 column(2,
                        selectInput(label = "HPV Status:", 
                                    inputId = ns("hpvselectrt"),
                                    choices = unique(filtereddata$`HPV status`),
                                    selected = "unknown"),
                        selectInput(label = "Induction Chemptherapy:", 
                                    inputId = ns("indchselectrt"),
                                    choices = unique(filtereddata$`Induction Chemotherapy`),
                                    selected = "No"),
                        selectInput(label = "Concurrent Chemptherapy:", 
                                    inputId = ns("conchselectrt"),
                                    choices = unique(filtereddata$`Received Concurrent Chemoradiotherapy?`),
                                    selected = "No"),
                        selectInput(label = "Chemotherapy Regimen:", 
                                    inputId = ns("chemoselectrt"),
                                    choices = unique(filtereddata$`Chemotherapy Regimen`),
                                    selected = "No"),
                        selectInput(label = "Platinum-based chemotherapy:", 
                                    inputId = ns("platselectrt"),
                                    choices = unique(filtereddata$`Platinum-based chemotherapy`),
                                    selected = "No")
                 ),
                 column(2,
                        selectInput(label = "Stage:", 
                                    inputId = ns("stageselectrt"),
                                    choices = unique(filtereddata$Stage),
                                    selected = "I"),
                        selectInput(label = "CCRT Chemotherapy Regime:", 
                                    inputId = ns("ccrtselectrt"),
                                    choices = unique(filtereddata$`CCRT Chemotherapy Regimen`),
                                    selected = "No"),
                        selectInput(label = "Potential Surgery:", 
                                    inputId = ns("surgselectrt"),
                                    choices = unique(filtereddata$`Surgery Summary`),
                                    selected = "No"),
                        selectInput(label = "Smoking History:", 
                                    inputId = ns("shselectrt"),
                                    choices = unique(filtereddata$`Smoking History`),
                                    selected = 1),
                        selectInput(label = "Current Smoker:", 
                                    inputId = ns("csselectrt"),
                                    choices = unique(filtereddata$`Current Smoker`),
                                    selected = 0)
                 ),
                 column(5,
                        h1(textOutput(ns("rtreg")),
                           style = "font-size:15px;"),
                        br(),
                        h1("This Outcome is the prediction of the regressor
                           Due to lack of data regressor couldnt fit well so this intended as example.",
                           style = "font-size:15px; color: red"),
                        br(),
                        h1("Please consult with your medical professional before making an decision regarding treatment options available",
                           style = "font-size:15px; color: red")
                 )
               )
        ),
  )
}

classifierserver <- function(input,output,session) {
  ns <- session$ns
  
  survtext <- reactive({
    py$predict_classification(py$dt_model, rel()$inputlistsurv, xcolumns, py$le_alive_dead)
  })
  
  output$surv <- renderText({
    if (survtext() == "Dead"){
      "Low Predicition Of Survival"
    } else {
      "High Predicition Of Survival"
    }
  })
  
  remtext <- reactive({
    py$predict_classification(py$lr_model, rel()$inputlistrem, xcolumns, py$le_response_recurrence)
  })
  
  output$rem <- renderText({
    if (remtext() == "Complete response"){
      "Complete response"
    } else {
      "Remission"
    }
  })
  
  sregtext <- reactive({
    py$predict_regression(py$lgbm_model_survival, rel()$inputlistsreg, xcolumns)
  })
  
  output$sreg <- renderText({
    paste0("Predicted Survival Time: ",round(sregtext(),2)," months")
  })
  
  rtregtext <- reactive({
    py$predict_regression(py$lgbm_model_rt_treatment_time, rel()$inputlistrtreg, xcolumns)
  })
  
  output$rtreg <- renderText({
    paste0("Predicted Survival Time: ",round(rtregtext(),2)," months")
  })
  
  
  
  rel <- reactive({
    
    inputlistsurv <- c(input$sexselectd,
                       as.numeric(input$ageselectd),
                       as.numeric(input$heightselectd),
                       as.numeric(input$bmiselectd),
                       input$diagselectd,
                       input$gradeselectd,
                       input$hpvselectd,
                       input$indchselectd,
                       input$chemoselectd,
                       input$platselectd,
                       input$conchselectd,
                       input$ccrtselectd,
                       input$surgselectd,
                       as.numeric(input$shselectd),
                       as.numeric(input$csselectd),
                       input$stageselectd)
    
    inputlistrem <- c(input$sexselectr,
                       as.numeric(input$ageselectr),
                       as.numeric(input$heightselectr),
                       as.numeric(input$bmiselectr),
                       input$diagselectr,
                       input$gradeselectr,
                       input$hpvselectr,
                       input$indchselectr,
                       input$chemoselectr,
                       input$platselectr,
                       input$conchselectr,
                       input$ccrtselectr,
                       input$surgselectr,
                       as.numeric(input$shselectr),
                       as.numeric(input$csselectr),
                       input$stageselectr)
    
    inputlistsreg <- c(input$sexselectsr,
                      as.numeric(input$ageselectsr),
                      as.numeric(input$heightselectsr),
                      as.numeric(input$bmiselectsr),
                      input$diagselectsr,
                      input$gradeselectsr,
                      input$hpvselectsr,
                      input$indchselectsr,
                      input$chemoselectsr,
                      input$platselectsr,
                      input$conchselectsr,
                      input$ccrtselectsr,
                      input$surgselectsr,
                      as.numeric(input$shselectsr),
                      as.numeric(input$csselectsr),
                      input$stageselectsr)
    
    inputlistrtreg <- c(input$sexselectrt,
                       as.numeric(input$ageselectrt),
                       as.numeric(input$heightselectrt),
                       as.numeric(input$bmiselectrt),
                       input$diagselectrt,
                       input$gradeselectrt,
                       input$hpvselectrt,
                       input$indchselectrt,
                       input$chemoselectrt,
                       input$platselectrt,
                       input$conchselectrt,
                       input$ccrtselectrt,
                       input$surgselectrt,
                       as.numeric(input$shselectrt),
                       as.numeric(input$csselectrt),
                       input$stageselectrt)
    
    
    return(list(inputlistsurv = inputlistsurv,
                inputlistrem = inputlistrem,
                inputlistsreg = inputlistsreg,
                inputlistrtreg = inputlistrtreg))
  })

}