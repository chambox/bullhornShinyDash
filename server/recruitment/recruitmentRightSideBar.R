#As of now I can only think one filter for all recruitment menus
output$recuitmentRightSideBar <- renderUI({
   div(
    uiOutput("recruitment_businessUnitFilter")
   )
})
