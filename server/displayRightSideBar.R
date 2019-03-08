output$rightSidebar <- renderUI({
  if (is.null(input$sidebar)) 
    uiOutput("recuitmentRightSideBar")
  
  else if (input$sidebar %in% c("allRecruitments","perRecruiter")) 
    uiOutput("recuitmentRightSideBar")

  else if(input$sidebar == "vacancy_request")
    uiOutput("vacancyRightSideBar")
})