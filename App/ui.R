pdf(NULL)

ui <- dashboardPage(
  skin = "purple-light",
  dashboardHeader(
    title = "Decision Support Tool"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Front Page", tabName = "frontpage", icon = icon("house-medical")),
      menuItem("Data Viewer", tabName = "dataviewer", icon = icon("book-medical")),
      menuItem("Demographics Plots", tabName = paste0("demographics"), icon = icon("notes-medical")),
      menuItem("Patient Profile", tabName = paste0("patientprofile"), icon = icon("bed-pulse")),
      menuItem("Survival Analysis", tabName = paste0("survival"), icon = icon("heart-pulse", lib="font-awesome")),
      menuItem("Classifier", tabName = paste0("classifier"), icon = icon("laptop-medical"))
      )
  ),
  dashboardBody(
    tabItems(
      frontpageui("frontpage1"),
      dataviewerui("dataviewer1"),
      demographicsui("demographics1"),
      patientprofileui("patientprofile1"),
      survivalui("survival1"),
      classifierui("classifier1")
    ),
    tags$head(tags$style(
      HTML('
        /* body */
        .content-wrapper, .right-side { 
            font-family: "Calibri";
            background-color: #FFFFFF;
        }')))
  )
)
