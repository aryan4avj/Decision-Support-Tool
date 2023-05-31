dataviewerui <- function(id) {
  ns <- NS(id)
  tabItem("dataviewer",
              column(3,
                     selectInput(label = "Select Variables to Display", 
                                 inputId = ns("varselect"),
                                 choices = colnames(pooleddata),
                                 selected = colnames(pooleddata)[1:10],
                                 multiple = TRUE),
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
                           choices = colnames(datasel))
                           ),
                    column(11,
                           conditionalPanel(
                             condition = paste0(paste0("[",toString(paste0("'",numvec,"'")),"]"),".includes(input.sel1)"),
                             ns = ns,
                             uiOutput(ns("nfil1"))),
                           conditionalPanel(
                             condition = paste0(paste0("[",toString(paste0("'",charvec,"'")),"]"),".includes(input.sel1)"),
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
                             choices = colnames(datasel))
                    ),
                    column(11,
                           conditionalPanel(
                             condition = paste0(paste0("[",toString(paste0("'",numvec,"'")),"]"),".includes(input.sel2)"),
                             ns = ns,
                             uiOutput(ns("nfil2"))),
                           conditionalPanel(
                             condition = paste0(paste0("[",toString(paste0("'",charvec,"'")),"]"),".includes(input.sel2)"),
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
                             choices = colnames(datasel))
                    ),
                    column(11,
                           conditionalPanel(
                             condition = paste0(paste0("[",toString(paste0("'",numvec,"'")),"]"),".includes(input.sel3)"),
                             ns = ns,
                             uiOutput(ns("nfil3"))),
                           conditionalPanel(
                             condition = paste0(paste0("[",toString(paste0("'",charvec,"'")),"]"),".includes(input.sel3)"),
                             ns = ns,
                             uiOutput(ns("cfil3")))
                    )
              ),
         column(9,
                column(
                  width = 3,
                  downloadButton(
                    outputId = ns("downloadData"),
                    label = "Download csv"),
                  br(),
                  br()
                  ),
                column(12,
                    dataTableOutput(
                      ns("datatable")
                      )
                )
          )
  )
}

dataviewerserver <- function(input,output,session) {
  ns <- session$ns
  
  output$datatable <- renderDataTable({
    datatable(rel()$datafil)
    })
  
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
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(Sys.time(), ' Table.csv', sep='')
    },
    content = function(file) {
      write.csv(rel()$datafil, file, row.names = FALSE)
    }
  )
  
  relsel <- reactive({
    fvec1 <- datasel %>% select(input$sel1)
    fvec2 <- datasel %>% select(input$sel2)
    fvec3 <- datasel %>% select(input$sel3)
    
    return(list(fvec1 = fvec1,fvec2 = fvec2,fvec3 = fvec3))
  })
  
  rel <- reactive({
    datafil <- datasel
    if(input$af1){
          if(input$sel1 %in% numvec){
            datafil <- datafil %>% filter(get(input$sel1) > input$nfil1[1] & get(input$sel1) < input$nfil1[2])
            } 
          else if(input$sel1 %in% charvec){
            datafil <- datafil %>% filter(get(input$sel1) %in% input$cfil1)
            }
    }
    
    if(input$af2){
      if(input$sel2 %in% numvec){
        datafil <- datafil %>% filter(get(input$sel2) > input$nfil2[1] & get(input$sel2) < input$nfil2[2])
      } 
      else if(input$sel2 %in% charvec){
        datafil <- datafil %>% filter(get(input$sel2) %in% input$cfil2)
      }
    }
    
    if(input$af3){
      if(input$sel3 %in% numvec){
        datafil <- datafil %>% filter(get(input$sel3) > input$nfil3[1] & get(input$sel3) < input$nfil3[2])
      } 
      else if(input$sel3 %in% charvec){
        datafil <- datafil %>% filter(get(input$sel3) %in% input$cfil3)
      }
    }
    
    datafil <- datafil %>% select(input$varselect)
    
    return(list(datafil = datafil))
  })
}
