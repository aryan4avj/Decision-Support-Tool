patientprofileui <- function(id) {
  ns <- NS(id)
  tabItem("patientprofile",
          column(3,
                 selectInput(label = "Select Patient:", 
                             inputId = ns("patientselect"),
                             choices = pooleddata$`TCIA code`,
                             selected = pooleddata$`TCIA code`[1]),
                 selectInput(label = "Variables Displayed:",
                             inputId = ns("varppsel"),
                             choices = colnames(pooleddata),
                             selected = colnames(pooleddata)[1:10],
                             multiple = TRUE)
                ),
          column(9,
                 column(
                   width = 3,
                   downloadButton(
                     outputId = ns("PPdownload"),
                     label = "Download PDF"),
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

patientprofileserver <- function(input,output,session) {
  ns <- session$ns
  
  output$datatable <- renderDataTable({
    datatable(rel()$datafil,
              options = list(searching = FALSE,
                             lengthChange = FALSE,
                             dom = 't',
                             sDom = 't',
                             pageLength =  nrow(rel()$datafil)
                             ),
              colnames = c(""))
  })
  
  output$PPdownload <- downloadHandler(

    filename = function() {
      paste0(input$patientselect," Patient Profile.png")
    },
    content = function(file)
    {
      png(file)

      patienttab<- flextable(relprint()$datafilprint)

      plot(patienttab)

      dev.off()
    }
  )
  
  rel <- reactive({
    datafil1 <- datasel %>%
      filter(`TCIA code` == input$patientselect) %>%
      select(input$varppsel)
    
    datafil <- data.table::transpose(datafil1)
    
    rownames(datafil) <- colnames(datafil1)
    colnames(datafil) <- NULL
  
    return(list(datafil = datafil))
  })
  
  relprint <- reactive({
    datafilprint1 <- datasel %>%
      filter(`TCIA code` == input$patientselect) %>%
      select(input$varppsel)
    
    datafilprint <- data.table::transpose(datafilprint1)
  
    datafilprint[2] <- colnames(datafilprint1)
    colnames(datafilprint) <- c("Value","Characteristic")
    datafilprint <- select(datafilprint,Characteristic,Value)
    
    return(list(datafilprint = datafilprint))
  })
}
        