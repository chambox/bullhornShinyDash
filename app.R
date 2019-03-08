library(shiny)
library(tidyverse)
library(lubridate)
library(timevis)
library(readr)
library(shinyjqui)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyAce)
library(styler)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(fullcalendar)



pl_rdata = read_csv(file.path("../entityData", "placement_rdata.csv"))
recruitment_rdata = read_csv(file.path("../dataCubesForShiny", "recruitment_rdata.csv"))
vacancy_rdata = read_csv(file.path("../dataCubesForShiny", "vacancy_rdata.csv"))
shinyApp(#UI
  {
    ui = dashboardPagePlus(
      #Header
      {
        dashboardHeaderPlus(
          fixed = TRUE,
          title = "peppaPig",
          enable_rightsidebar = TRUE,
          rightSidebarIcon = "gears"
        )
      },
      ### Right sidebar contains information about settings
      rightsidebar = rightSidebar(
        background = "light",
        width = 230,
        rightSidebarTabContent(
          id = 1,
          icon = "desktop",
          title = "Filters",
          active = TRUE,
          fluidRow(column(
            width = 12,
            dateRangeInput(
              "daterange",
              "Date range:",
              start = Sys.Date() - 30,
              end = Sys.Date()
            ),
            uiOutput("rightSidebar")
          )
         
        )
      )),
      ###Left sidebar contains information on functionalities
      {
        dashboardSidebar(
          sidebarMenu(id="sidebar",
          ###Recruitments
          {
            menuItem(
              "Recruitments",
              menuSubItem(
                "All recruitments",
                tabName = "allRecruitments",
                href = NULL,
                newtab = TRUE,
                icon = shiny::icon("angle-double-right"),
                selected = NULL
              ),
              menuSubItem(
                "Per recruiter",
                tabName = "perRecruiter",
                href = NULL,
                newtab = TRUE,
                icon = shiny::icon("angle-double-right"),
                selected = NULL
              ),
              tabName = NULL,
              icon = icon("briefcase")
            )
          },
          
          ###Vacancies
          {
            menuItem(
              "Vacancies",
              tabName = "header",
              icon = icon("folder-open"),
              menuSubItem(
                "Request",
                tabName = "vacancy_request",
                href = NULL,
                newtab = TRUE,
                icon = shiny::icon("angle-double-right"),
                selected = NULL
              )
            )
          },
          ###Bench
          {
            menuItem(
              "Bench",
              tabName = "bench",
              badgeLabel = "new",
              badgeColor = "green",
              icon = icon("briefcase")
            )
          },
          ###Placements
          {
            menuItem(
              "Placements",
              menuSubItem(
                "Timeline",
                tabName = "buttons",
                href = NULL,
                newtab = TRUE,
                icon = shiny::icon("angle-double-right"),
                selected = NULL
              ),
              tabName = NULL,
              icon = icon("cubes")
            )
          },
          ###Others
          {
            menuItem(
              "Others",
              tabName = "boxelements",
              badgeLabel = "new",
              badgeColor = "green",
              icon = icon("th")
            )
          },
          menuItem(
            "Extra CS§S effects",
            tabName = "extracss",
            badgeLabel = "new",
            badgeColor = "green",
            icon = icon("magic")
          ),
          menuItem(
            "New extra elements",
            tabName = "extraelements",
            badgeLabel = "new",
            badgeColor = "green",
            icon = icon("plus-circle")
          )
        ))
      },
      ###Body
      {
        dashboardBody(uiOutput("body"))
      },
      title = "shinyDashboardPlus"
    )
  },
  #Server
  {
    server = function(input, output) {
      ##Recruitment 
      source(file.path("server/recruitment","recruitmentServer.R"),local = TRUE)$value
      source(file.path("server/recruitment","recruitmentBody.R"),local = TRUE)$value
      source(file.path("server/recruitment","recruitmentRightSideBar.R"),local = TRUE)$value
      
      ##Vacancy
      source(file.path("server/vacancy","vacancyServer.R"),local = TRUE)$value
      source(file.path("server/vacancy","vacancyBody.R"),local = TRUE)$value
      source(file.path("server/vacancy","vacancyRightSideBar.R"),local = TRUE)$value
      
      ##Right sidebar and body contents
      source(file.path("server","displayBody.R"),local = TRUE)$value
      source(file.path("server","displayRightSideBar.R"),local = TRUE)$value
    }
  })