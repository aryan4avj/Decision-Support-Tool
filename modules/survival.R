survivalui <- function(id) {
  ns <- NS(id)
  tabItem("survival",
          column(3,
                 column(7,
                        h5("Variable Filter 1")
                 ),
                 column(1, 
                        checkboxInput(
                          ns("af1"),
                          label = NULL)
                 ),
                 column(11,
                        selectInput(
                          inputId = ns("sel1"),
                          label = NULL,
                          choices = colnames(survdata))
                 ),
                 column(11,
                        conditionalPanel(
                          condition = paste0(paste0("[",toString(paste0("'",numvecsurv,"'")),"]"),".includes(input.sel1)"),
                          ns = ns,
                          uiOutput(ns("nfil1"))),
                        conditionalPanel(
                          condition = paste0(paste0("[",toString(paste0("'",charvecsurv,"'")),"]"),".includes(input.sel1)"),
                          ns = ns,
                          uiOutput(ns("cfil1")))
                 ),
                 column(7,
                        h5("Variable Filter 2")
                 ),
                 column(2, 
                        checkboxInput(
                          ns("af2"),
                          label = NULL)
                 ),
                 column(11,
                        selectInput(
                          inputId = ns("sel2"),
                          label = NULL,
                          choices = colnames(survdata))
                 ),
                 column(11,
                        conditionalPanel(
                          condition = paste0(paste0("[",toString(paste0("'",numvecsurv,"'")),"]"),".includes(input.sel2)"),
                          ns = ns,
                          uiOutput(ns("nfil2"))),
                        conditionalPanel(
                          condition = paste0(paste0("[",toString(paste0("'",charvecsurv,"'")),"]"),".includes(input.sel2)"),
                          ns = ns,
                          uiOutput(ns("cfil2")))
                 ), column(7,
                           h5("Variable Filter 3")
                 ),
                 column(3, 
                        checkboxInput(
                          ns("af3"),
                          label = NULL)
                 ),
                 column(11,
                        selectInput(
                          inputId = ns("sel3"),
                          label = NULL,
                          choices = colnames(survdata))
                 ),
                 column(11,
                        conditionalPanel(
                          condition = paste0(paste0("[",toString(paste0("'",numvecsurv,"'")),"]"),".includes(input.sel3)"),
                          ns = ns,
                          uiOutput(ns("nfil3"))),
                        conditionalPanel(
                          condition = paste0(paste0("[",toString(paste0("'",charvecsurv,"'")),"]"),".includes(input.sel3)"),
                          ns = ns,
                          uiOutput(ns("cfil3")))
                 )
          ),
          column(9,
                 column(4,
                         selectInput(inputId = ns("selsurvival"),
                                     label = "Survival Endpoint",
                                     choices = survec,
                                     selected = survec[1]),
                        ),
                 column(5,
                         selectInput(inputId = ns("grpsurvival"),
                                     label = "Grouping Endpoint",
                                     choices = charvecsurv,
                                     selected = charvecsurv[2])
                        ),                 
                 # column(
                 #          width = 3,
                 #          downloadButton(
                 #            outputId = "Survdownload",
                 #            label = "Download PDF"),
                 #          br(),
                 #          br()
                 #        )
                 ),
          column(9,
                 column(12,
                         plotlyOutput(
                           ns("survgg")
                         ),
                         plotOutput(
                           ns("survtb")
                         )
                 )
                 # textOutput(
                 #   ns("texttest")
                 # )
          )
  )
}

survivalserver <- function(input,output,session) {
  ns <- session$ns
  
  output$survgg <- renderPlotly({
    ggplotly(ggsurvplot(fit = runSur(), data = rel()$datafil, risk.table = TRUE)$plot)
  })
  
  output$survtb <- renderPlot({
    ggsurvplot(fit = runSur(), data = rel()$datafil, risk.table = TRUE)$table
  })
  

  # output$texttest <- renderText({
  #     runtest()
  #   })

  
  output$nfil1 <- renderUI({
    sliderInput(ns("nfil1"),
                label = NULL,
                min = min(relsel()$fvec1),
                max = max(relsel()$fvec1),
                value = c(min(relsel()$fvec1),
                          max(relsel()$fvec1)),
                step = 0.5)
  })
  
  output$cfil1 <- renderUI({
    selectInput(ns("cfil1"),
                label = NULL,
                choices = unique(relsel()$fvec1),
                selected = unique(relsel()$fvec1),
                multiple = TRUE)
  })
  
  output$nfil2 <- renderUI({
    sliderInput(ns("nfil2"),
                label = NULL,
                min = min(relsel()$fvec2),
                max = max(relsel()$fvec2),
                value = c(min(relsel()$fvec2),
                          max(relsel()$fvec2)),
                step = 0.5)
  })
  
  output$cfil2 <- renderUI({
    selectInput(ns("cfil2"),
                label = NULL,
                choices = unique(relsel()$fvec2),
                selected = unique(relsel()$fvec2),
                multiple = TRUE)
  })
  
  output$nfil3 <- renderUI({
    sliderInput(ns("nfil3"),
                label = NULL,
                min = min(relsel()$fvec3),
                max = max(relsel()$fvec3),
                value = c(min(relsel()$fvec3),
                          max(relsel()$fvec3)),
                step = 0.5)
  })
  
  output$cfil3 <- renderUI({
    selectInput(ns("cfil3"),
                label = NULL,
                choices = unique(relsel()$fvec3),
                selected = unique(relsel()$fvec3),
                multiple = TRUE)
  })
  
  relsel <- reactive({
    fvec1 <- survdata %>% select(input$sel1)
    fvec2 <- survdata %>% select(input$sel2)
    fvec3 <- survdata %>% select(input$sel3)
    
    return(list(fvec1 = fvec1,fvec2 = fvec2,fvec3 = fvec3))
  })
  
  rel <- reactive({
    datafil <- survdata
    if(input$af1){
      if(input$sel1 %in% numvecsurv){
        datafil <- datafil %>% filter(get(input$sel1) > input$nfil1[1] & get(input$sel1) < input$nfil1[2])
      } 
      else if(input$sel1 %in% charvecsurv){
        datafil <- datafil %>% filter(get(input$sel1) %in% input$cfil1)
      }
    }
    
    if(input$af2){
      if(input$sel2 %in% numvecsurv){
        datafil <- datafil %>% filter(get(input$sel2) > input$nfil2[1] & get(input$sel2) < input$nfil2[2])
      } 
      else if(input$sel2 %in% charvecsurv){
        datafil <- datafil %>% filter(get(input$sel2) %in% input$cfil2)
      }
    }
    
    if(input$af3){
      if(input$sel3 %in% numvecsurv){
        datafil <- datafil %>% filter(get(input$sel3) > input$nfil3[1] & get(input$sel3) < input$nfil3[2])
      } 
      else if(input$sel3 %in% charvecsurv){
        datafil <- datafil %>% filter(get(input$sel3) %in% input$cfil3)
      }
    }

    if(input$selsurvival == "Survivalmonths"){
      datafil <- datafil %>% filter(!is.na(`OverallSurvivalCensor`))
    } else if (input$selsurvival == "Diseasefreeintervalmonths"){
      datafil <- datafil %>% filter(!is.na(`DiseaseSpecificSurvivalCensor`))
    }
    
    datafil$`Survivalmonths` <- as.numeric(datafil$`Survivalmonths`)
    datafil$`OverallSurvivalCensor` <- as.numeric(datafil$`OverallSurvivalCensor`)
    datafil$`Disease-freeintervalmonths` <- as.numeric(datafil$`Diseasefreeintervalmonths`)
    datafil$`DiseaseSpecificSurvivalCensor` <- as.numeric(datafil$`DiseaseSpecificSurvivalCensor`)
    
    return(list(datafil = datafil))
  })
  
  runSur <- reactive({
    
    if(input$selsurvival == "Survivalmonths"){
      form <- formula(paste0("Surv(`Survivalmonths`, `OverallSurvivalCensor`) ~ ",input$grpsurvival))
    } else     if(input$selsurvival == "Diseasefreeintervalmonths"){
      form <- formula(paste0("Surv(`Diseasefreeintervalmonths`, `DiseaseSpecificSurvivalCensor`) ~ ",input$grpsurvival))
    }
    eval(bquote(survfit(.(form),data=rel()$datafil)))
  })
}
