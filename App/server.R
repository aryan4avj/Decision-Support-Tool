server <- function(input,output,session) {
  callModule(dataviewerserver,"dataviewer1",session=session)
  callModule(demographicsserver,"demographics1",session=session)
  callModule(patientprofileserver,"patientprofile1",session=session)
  callModule(survivalserver,"survival1",session=session)
  callModule(classifierserver,"classifier1",session=session)
  }