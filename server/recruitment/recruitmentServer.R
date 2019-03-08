##This function creates the postates
per_recruiter_renderUI <- function(data, stages) {
  #stages<-unique(data$boards)
  recruiter <- unique(data$recruiter)
  l1 <- lapply(stages, function(x)
    list(stage = x))
  for (i in 1:length(l1))
    l1[[i]]$recruiter <- lapply(recruiter, function(x)
      list(recruiter = x))
  
  for (i in 1:length(l1))
    for (k in 1:length(l1[[i]]$recruiter))
      l1[[i]]$recruiter[[k]]$item <-
    data$items[data$boards == l1[[i]]$stage &
                 data$recruiter == l1[[i]]$recruiter[[k]]$recruiter]
  
  
  
  
  len_s <- length(l1)
  
  perTimeStage <- list()
  length(perTimeStage) <- len_s
  
  for (j in 1:len_s)
  {
    perTimeStage[[j]] <- timelineItem(lapply(seq_len(length(l1[[j]]$recruiter)), function(i0) {
      width <- round(12 / length(l1[[j]]$recruiter))+1
      if (width <= 1)
        width = 2
      box(
        width = width,
        title = l1[[j]]$recruiter[[i0]]$recruiter,
        status = "warning",
        todoList(lapply(seq_len(
          length(l1[[j]]$recruiter[[i0]]$item)
        ), function(i1) {
          todoListItem(label = l1[[j]]$recruiter[[i0]]$item[i1])
        }))
      )
    }))
    #merge HTML for the jth stage
    # for(i in 1:length(list1)){
    #   perTimeStage[[j]]<-paste(perTimeStage[[j]],list1[[i]])
    # }
  }
  cols <- c("red", "maroon", "yellow", "orange", "grey", "blue", "green")
  fluidRow(column(
    width = 12,
    timelineBlock(
      reversed = FALSE,
      timelineEnd(color = "danger"),
      lapply(seq_len(len_s), function(i) {
        #browser()
        HTML(paste(timelineLabel(stages[i], color = cols[i]), perTimeStage[[i]]))
      })
    )
  ))
}

# filter recruiter
output$recruitment_recruiterFilter <- renderUI({
  subset <-
    recruitment_rdata$group[recruitment_rdata$BizUnit == input$recruitment_businessUnitFilter]
  recruiter <- as.list(subset)
  selectInput(
    "placement_acc",
    "Select key account owner",
    choices = recruiter,
    selected = recruiter[[1]]
  )
})
#filter business unit
output$recruitment_businessUnitFilter <- renderUI({
  chs <- as.list(unique(recruitment_rdata$BizUnit))
  selectInput(
    "recruitment_businessUnitFilter",
    "Select business unit",
    choices = chs,
    selected = "EBS"
  )
})
# Data filtering based on date and business unit
# all done here in this reactive setting
recruitment_filteredData <- reactive({
  if (is.null(input$recruitment_businessUnitFilter)) {
    bu <- "EBS"
  }
  else{
    bu <- input$recruitment_businessUnitFilter
  }
  
  if (is.null(input$daterange)) {
    startEnd = c(Sys.Date() - 30, Sys.Date())
  }
  else{
    startEnd <- input$daterange
  }
  subset0 <- recruitment_rdata$BizUnit == bu
  subset1 <-
    recruitment_rdata$start %within% interval(ymd(startEnd[1]),
                                              ymd(startEnd[2]))
  
  r0 <- recruitment_rdata[subset1 & subset0,]
  
  len <- length(unique(r0$group))
  style = rep(c("background-color:gainsboro;", "background-color:white;"),
              len / 2)
  if (len %% 2 > 0)
    style <- c(style, "background-color:gainsboro;")
  
  group_recruitment <- data.frame(
    id = unique(r0$group),
    content = unique(r0$group),
    subgroupOrder = rep("content", len),
    style = style
  )
  
  
  data.frame(table(r0$group[r0$BizUnit == bu],
                   r0$action[r0$BizUnit == bu])) %>%
    mutate(Recruiter = Var1,
           freq = ifelse(
             Freq > 0,
             sprintf(
               '<a id="%s" href="#" class="action-button" onclick="Shiny.onInputChange(&quot;select_button&quot;,  this.id)">%s</a>',
               paste(Var1, Var2, sep = "@"),
               Freq
             )
             ,
             Freq
           )) %>%
    select(-Var1) %>%
    select(-Freq) %>%
    reshape(idvar = "Recruiter",
            timevar = "Var2",
            direction = "wide") -> r2
  names(r2) <- gsub("freq.", "", names(r2))
  r1 <- data.frame(table(r0$group[r0$BizUnit == bu]))
  
  names(r1) <- c("variable", "counts")
  
  r1 %>% mutate(
    percentage = counts / sum(counts),
    group = "orange",
    label = paste0(round((
      counts / sum(counts)
    ) * 100), "%"),
    title = gsub(" ", "\n", variable)
  ) -> r1
  list(
    r2 = unique(r2),
    r1 = unique(r1),
    r0 = unique(r0),
    group_recruitment = unique(group_recruitment)
  )
  
  #r0 has action (first ITV,...,css style, content (candidate ID), start date, title=action, bizUnit,
  #group=recruiter,subgroup=content)
})
#initialise a reactive value for the results of
#the recruitment table on click
myValue <- reactiveValues(results = NULL)

observeEvent(input$select_button, {
  recruitment_filteredData()$r0 %>% filter(action == strsplit(input$select_button, "@")[[1]][2]) %>%
    filter(group == strsplit(input$select_button, "@")[[1]][1]) %>%
    select(Action = action,
           Candidate = content,
           Date = start) ->myValue$results
  showModal(modalDialog(
    title = gsub("@", "--", input$select_button),
    DT::renderDataTable({
      myValue$results
    })
  ))
})



#Summary statistics area
output$prescreen <- renderValueBox({
  total <- sum(recruitment_filteredData()$r0$action == "Prescreen")
  #perc<-sum(recruitment_filteredData()$r0$action=="Prescreen")/length(recruitment_filteredData()$r0$action)
  valueBox(total,
           "Prescreen",
           icon = icon("phone"),
           color = "purple")
})

output$firstITV <- renderValueBox({
  total <- sum(recruitment_filteredData()$r0$action == "First ITV")
  #perc<-total/length(recruitment_filteredData()$r0$action)
  valueBox(total,
           "First ITV",
           icon = icon("battery-empty"),
           color = "blue")
})
output$secondITV <- renderValueBox({
  total <- sum(recruitment_filteredData()$r0$action == "Second ITV")
  #perc<-total/length(recruitment_filteredData()$r0$action)
  valueBox(total,
           "Second ITV",
           icon = icon("battery-half"),
           color = "purple")
})

output$thirdITV <- renderValueBox({
  total <- sum(recruitment_filteredData()$r0$action == "Third ITV")
  #perc<-sum(recruitment_filteredData()$r0$action=="Third ITV")/length(recruitment_filteredData()$r0$action)
  valueBox(total,
           "Third ITV",
           icon = icon("battery-full"),
           color = "purple")
})

output$referenceCheck <- renderValueBox({
  total <- sum(recruitment_filteredData()$r0$action == "Reference Check")
  #perc<-sum(recruitment_filteredData()$r0$action=="Reference Check")/length(recruitment_filteredData()$r0$action)
  valueBox(total,
           "Reference Check",
           icon = icon("check"),
           color = "red")
})


output$proposal <- renderValueBox({
  total <- sum(recruitment_filteredData()$r0$action == "Proposal")
  #perc<-sum(recruitment_filteredData()$r0$action=="Proposal")/length(recruitment_filteredData()$r0$action)
  valueBox(total,
           "Proposal",
           icon = icon("location-arrow"),
           color = "orange")
})

output$contracted <- renderValueBox({
  total <- sum(recruitment_filteredData()$r0$action == "Contracted")
  #perc<-sum(recruitment_filteredData()$r0$action=="Contracted")/length(recruitment_filteredData()$r0$action)
  valueBox(total,
           "Contracted",
           icon = icon("thumbs-up"),
           color = "green")
})
#postates
output$postates <- renderUI({
  recruitment_filteredData()$r0 %>%
    select(boards = action,
           items = content,
           recruiter = group) -> data
  per_recruiter_renderUI(
    data,
    stages = c(
      "Prescreen",
      "First ITV",
      "Second ITV",
      "Third ITV",
      "Reference Check",
      "Proposal",
      "Contracted"
    )
  )
})


#Data table output
output$recruitment_table <- DT::renderDataTable({
  if (!is.null(recruitment_filteredData()$r2))
    recruitment_filteredData()$r2
},
options = list(pageLength = 10, scrollX = TRUE),
escape = FALSE, server = FALSE, selection = 'none')


##Recruitment performance output
output$recruitment_perfo <- renderPlotly({
  recruitment_filteredData()$r0 %>%
    select(
      "Recruitment stage" = action,
      items = content,
      recruiter = group
    ) -> data
  
  p <-
    ggplot(data = data, aes(x = recruiter, fill = `Recruitment stage`)) +
    geom_bar(position = "dodge")
  
  ggplotly(p) %>%
    layout(xaxis = list(title = "", tickangle = -45))
})
