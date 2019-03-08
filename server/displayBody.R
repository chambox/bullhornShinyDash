output$body <- renderUI({
  if (is.null(input$sidebar)) {
    uiOutput("perRecruiterBody")
  }
  else if (input$sidebar == "allRecruitments") {
    uiOutput("allRecruitmentsBody")
  }
  else if (input$sidebar == "perRecruiter") {
    uiOutput("perRecruiterBody")
  }
  else if (input$sidebar == "vacancy_request"){
    uiOutput("vacancy_requestBody")
  }
})


