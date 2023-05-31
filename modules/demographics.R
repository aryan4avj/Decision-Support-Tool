demographicsui <- function(id) {
  ns <- NS(id)
  tabItem("demographics",
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
          #        column(
          #          width = 3,
          #          downloadButton(
          #            outputId = ns("downloadPlot"),
          #            label = "Download PDF"),
          #          br(),
          #          br()
          #        ),
                 column(12,
                   column(3,
                           selectInput(
                             inputId = ns("chartsel"),
                             label = "Select Plot:",
                             choices = c("Barplot","Scatterplot","Histogram","Waterfall Plot","Boxplot","Violin Plot"),
                             selected = "Barplot"
                           )
                         ),
                   conditionalPanel(condition = "input.chartsel == 'Barplot'",
                                    ns = ns,
                                    column(9,uiOutput(ns("barplotfil"))),
                                    column(12,plotlyOutput(ns("ggbarplot")))
                   ),
                   conditionalPanel(condition = "input.chartsel == 'Scatterplot'",
                                    ns = ns,
                                    column(9,uiOutput(ns("scatterplotfil"))),
                                    column(12,plotlyOutput(ns("ggscatterplot")))
                   ),
                   conditionalPanel(condition = "input.chartsel == 'Histogram'",
                                    ns = ns,
                                    column(9,uiOutput(ns("histogramfil"))),
                                    column(12,plotlyOutput(ns("gghistogram")))
                   ),
                   conditionalPanel(condition = "input.chartsel == 'Waterfall Plot'",
                                    ns = ns,
                                    column(9,uiOutput(ns("waterfallplotfil"))),
                                    column(12,plotlyOutput(ns("ggwaterfallplot")))
                   ),
                   conditionalPanel(condition = "input.chartsel == 'Boxplot'",
                                    ns = ns,
                                    column(9,uiOutput(ns("boxplotfil"))),
                                    column(12,plotlyOutput(ns("ggboxplot")))
                   ),
                   conditionalPanel(condition = "input.chartsel == 'Violin Plot'",
                                    ns = ns,
                                    column(9,uiOutput(ns("violinplotfil"))),
                                    column(12,plotlyOutput(ns("ggviolinplot")))
                   ),
                  )
          )
    )
}

demographicsserver <- function(input,output,session) {
  ns <- session$ns
  
  output$barplotfil <- renderUI({
    fluidRow(
            column(3,
                   selectInput(inputId = ns("selxbr"),
                               label = "X Variable:",
                               choices = charvec,
                               selected = charvec[2]
                               )
            ),
            # column(3,
            #        selectInput(inputId = ns("selybr"),
            #                    label = "Y Variable:",
            #                    choices = charvec,
            #                    selected = charvec[2]
            #        )
            # ),
            column(4,
                   selectInput(inputId = ns("selgbr"),
                               label = "Group Variable:",
                               choices = c("",charvec),
                               selected = ""
                   )
            )
    )
  })
  
  output$ggbarplot <- renderPlotly({
    barplot_fun(rel()$datafil,input$selxbr,input$selgbr)
    })
  
  output$scatterplotfil <- renderUI({
    fluidRow(
      column(3,
             selectInput(inputId = ns("selxsc"),
                         label = "X Variable:",
                         choices = numvec,
                         selected = numvec[1]
             )
      ),
      column(3,
             selectInput(inputId = ns("selysc"),
                         label = "Y Variable:",
                         choices = numvec,
                         selected = numvec[2]
             )
      ),
      column(4,
             selectInput(inputId = ns("selgsc"),
                         label = "Group Variable:",
                         choices = c("",charvec),
                         selected = ""
             )
      )
    )
  })

  output$ggscatterplot <- renderPlotly({

    scatterplot_fun(rel()$datafil,input$selxsc,input$selysc,input$selgsc)
  })
  
  output$histogramfil <- renderUI({
    fluidRow(
      column(3,
             selectInput(inputId = ns("selxh"),
                         label = "X Variable:",
                         choices = numvec,
                         selected = numvec[1]
             )
      ),
      column(3,
             selectInput(inputId = ns("selgh"),
                         label = "Group Variable:",
                         choices = c("",charvec),
                         selected = ""
             )
      )
    )
  })

  output$gghistogram <- renderPlotly({

    histogram_fun(rel()$datafil,input$selxh,input$selgh)
  })
  
  output$waterfallplotfil <- renderUI({
    fluidRow(
      column(3,
             selectInput(inputId = ns("selxw"),
                         label = "X Variable:",
                         choices = numvec,
                         selected = numvec[1]
             )
      ),
      column(3,
             selectInput(inputId = ns("selgw"),
                         label = "Group Variable:",
                         choices = c("",charvec),
                         selected = ""
             )
      )
    )
  })
   
  output$ggwaterfallplot <- renderPlotly({
    waterfallplot_fun(rel()$datafil,input$selxw,input$selgw)
  })
  
  output$boxplotfil <- renderUI({
    fluidRow(
      column(3,
             selectInput(inputId = ns("selxbx"),
                         label = "X Variable:",
                         choices = charvec,
                         selected = charvec[2]
             )
      ),
      column(3,
             selectInput(inputId = ns("selybx"),
                         label = "Y Variable:",
                         choices = numvec,
                         selected = numvec[1]
             )
      ),
      column(3,
             selectInput(inputId = ns("selgbx"),
                         label = "Group Variable:",
                         choices = c("",charvec),
                         selected = ""
             )
      )
    )
  })

  output$ggboxplot <- renderPlotly({

    boxplot_fun(rel()$datafil,input$selxbx,input$selybx,input$selgbx)
  })

  output$violinplotfil <- renderUI({
    fluidRow(
      column(3,
             selectInput(inputId = ns("selxv"),
                         label = "X Variable:",
                         choices = charvec,
                         selected = charvec[2]
             )
      ),
      column(3,
             selectInput(inputId = ns("selyv"),
                         label = "Y Variable:",
                         choices = numvec,
                         selected = numvec[1]
             )
      ),
      column(3,
             selectInput(inputId = ns("selgv"),
                         label = "Group Variable:",
                         choices = c("",charvec),
                         selected = ""
             )
      )
    )
  })
  
  output$ggviolinplot <- renderPlotly({

    violinplot_fun(rel()$datafil,input$selxv,input$selyv,input$selgv)
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
    
    return(list(datafil = datafil))
  })
  
  # output$downloadPlot <- downloadHandler(
  #   filename = function() { paste(input$chartsel, '.pdf') },
  #   content = function(file) {
  #     pdf(file)
  #     
  #     p = barplot_funpdf(rel()$datafil,input$selxbx,input$selybx,input$selgbx)
  #     
  #     plot()
  #     
  #     dev.off()
  #   }
  # )
  
  
}
