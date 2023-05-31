wdset <- "C:/Users/emmet/OneDrive/Documents/GitHub/Cancer_Decision_Support_Tool/"
source(paste0(wdset, "App/global.R"))

ui = source(paste0(wdset, "App/ui.R")) 
server = source(paste0(wdset, "App/server.R"))

shinyApp(ui = ui, server = server)

