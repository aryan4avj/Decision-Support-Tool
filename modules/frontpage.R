frontpageui <- function(id) {
  ns <- NS(id)
  tabItem("frontpage",
          h1("This tool is designed to support decision making and should be used in conjunction with other diagnostic tools and expertise",
             style = "font-size:22px; color: red"),
          h1("Please consult with a medical professional before making any decisions",
             style = "font-size:22px; color: red"),
          h1(
          "The aim of this decision support tool is to provide patients with a clearer understanding of their diagnosis which can aid shared decision-making,
                 and also to assist medical team members in decision making.",
          style = "font-size:15px;"),
          h1("The app contains 5 tabs,Data Viewer, Demographics Plots, Patient Profile, Survival Analysis and Classifier.",
             style = "font-size:15px;"),
          h1("The Data Viewer displays a data table of the columns in the dataset and can be filtered."
             ,style = "font-size:15px;"),
          h1("The demographics plots tab enables user to display various visualisations of filtered subgroups based on a factor."
             ,style = "font-size:15px;"), 
          h1("The patient profile tab displays a datatable of selected information for a selected patient. "
             ,style = "font-size:15px;"),
          h1("The survival analysis tab displays a kaplan-meier plot for filtered patients for selected outcomes. "
             ,style = "font-size:15px;"),
          h1("The classifier tab displays a prediction based on input for patients.",
             style = "font-size:15px;"),
  )
}

server <- function(input,output,session) {

}
