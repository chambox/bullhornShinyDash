#As of now I can only think one filter for all recruitment menus
output$vacancyRightSideBar <- renderUI({
  fluidRow(
  column(width = 12,
    uiOutput("vacancy_clientCopFilter"),
    uiOutput("vacancy_businessUnitFilter")
  ))
})
