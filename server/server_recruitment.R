# filter recruiter
output$recruitment_recruiter <- renderUI({
  subset<-recruitment_rdata$group[recruitment_rdata$BizUnit==input$businessUnit]
  recruiter<-as.list(subset)
  selectInput("pl_acc",
              "Select key account owner",
              choices = recruiter,
              selected = recruiter[[1]]
  )
})
#filter business unit
output$businessUnit <- renderUI({

  chs<-as.list(unique(recruitment_rdata$BizUnit))
  selectInput("businessUnit",
              "Select business unit",
              choices = chs,
              selected = "EBS"
  )
})
# Data filtering based on date and business unit
# all done here
dataCom_recruitment <- reactive({
  if(is.null(input$businessUnit)){
    bu<-"EBS"
  }
  else{
    bu<-input$businessUnit
  }
  
  if(is.null(input$daterange)){
    startEnd = c(Sys.Date()-30,Sys.Date())
  }
  else{
    startEnd<-input$daterange
  }
  subset0<-recruitment_rdata$BizUnit==bu
  subset1<-recruitment_rdata$start%within%interval(ymd(startEnd[1]), 
                                                   ymd(startEnd[2]))

  r0<-recruitment_rdata[subset1&subset0, ]

  len<-length(unique(r0$group))
  style=rep(c("background-color:gainsboro;","background-color:white;"),len/2)
  if(len%%2>0)
    style<-c(style,"background-color:gainsboro;")

  group_recruitment<-data.frame(id=unique(r0$group),
                                content=unique(r0$group),
                                subgroupOrder=rep("content",len),
                                style=style)


  data.frame(table(r0$group[r0$BizUnit==bu],
                   r0$action[r0$BizUnit==bu]))%>%
    mutate(Recruiter=Var1,freq=ifelse(Freq>0,
                                      sprintf(
                                        '<a id="%s" href="#" class="action-button" onclick="Shiny.onInputChange(&quot;select_button&quot;,  this.id)">%s</a>',
                                        paste(Var1,Var2,sep = "@"),Freq)
                                      ,Freq))%>%
    select(-Var1)%>%
    select(-Freq)%>%
    reshape(idvar = "Recruiter", timevar = "Var2", direction = "wide")->r2
  names(r2)<-gsub("freq.","",names(r2))
  r1<-data.frame(table(r0$group[r0$BizUnit==bu]))

  names(r1)<-c("variable","counts")

  r1%>%mutate(percentage=counts/sum(counts),
              group="orange",
              label=paste0(round((counts/sum(counts))*100),"%"),
              title=gsub(" ","\n",variable))->r1
  list(r2=unique(r2),r1=unique(r1),r0=unique(r0),group_recruitment=unique(group_recruitment))

  #r0 has action (first ITV,...,css style, content (candidate ID), start date, title=action, bizUnit,
  #group=recruiter,subgroup=content)
})
#initialise a reactive value for the results of
#the recruitment table on click
myValue <- reactiveValues(results = NULL)

observeEvent(input$select_button,{
  dataCom_recruitment()$r0%>%filter(action==strsplit(input$select_button,"@")[[1]][2])%>%
    filter(group==strsplit(input$select_button,"@")[[1]][1])%>%
    select(Action=action,Candidate=content,Date=start)->>myValue$results
  showModal(modalDialog(
    title = gsub("@","--",input$select_button),
    DT::renderDataTable(
      {
        myValue$results
      }
    )
  ))
})



#Summary statistics area
output$prescreen <- renderValueBox({
  total<-sum(dataCom_recruitment()$r0$action=="Prescreen")
  #perc<-sum(dataCom_recruitment()$r0$action=="Prescreen")/length(dataCom_recruitment()$r0$action)
  valueBox(total, "Prescreen", icon = icon("phone"),
    color = "purple"
  )
})

output$firstITV <- renderValueBox({
  total<-sum(dataCom_recruitment()$r0$action=="First ITV")
  #perc<-total/length(dataCom_recruitment()$r0$action)
  valueBox(total, "First ITV", icon = icon("battery-empty"),
    color = "blue"
  )
})
output$secondITV <- renderValueBox({
  total<-sum(dataCom_recruitment()$r0$action=="Second ITV")
  #perc<-total/length(dataCom_recruitment()$r0$action)
  valueBox(total, "Second ITV", icon = icon("battery-half"),
    color = "purple"
  )
})

output$thirdITV <- renderValueBox({
  total<-sum(dataCom_recruitment()$r0$action=="Third ITV")
  #perc<-sum(dataCom_recruitment()$r0$action=="Third ITV")/length(dataCom_recruitment()$r0$action)
  valueBox(total, "Third ITV", icon = icon("battery-full"),
    color = "purple"
  )
})

output$referenceCheck <- renderValueBox({
  total<-sum(dataCom_recruitment()$r0$action=="Reference Check")
  #perc<-sum(dataCom_recruitment()$r0$action=="Reference Check")/length(dataCom_recruitment()$r0$action)
  valueBox(total, "Reference Check", icon = icon("check"),
    color = "red"
  )
})


output$proposal <- renderValueBox({
  total<-sum(dataCom_recruitment()$r0$action=="Proposal")
  #perc<-sum(dataCom_recruitment()$r0$action=="Proposal")/length(dataCom_recruitment()$r0$action)
  valueBox(total, "Proposal", icon = icon("location-arrow"),
    color = "orange"
  )
})

output$contracted <- renderValueBox({
  total<-sum(dataCom_recruitment()$r0$action=="Contracted")
  #perc<-sum(dataCom_recruitment()$r0$action=="Contracted")/length(dataCom_recruitment()$r0$action)
  valueBox(total, "Contracted", icon = icon("thumbs-up"),
    color = "green"
  )
})
#postates
output$postates<-renderUI({
    dataCom_recruitment()$r0%>%
    select(boards=action,items=content,recruiter=group)->data
    per_recruiter_renderUI(data,stages=c("Prescreen","First ITV",
                                         "Second ITV","Third ITV","Reference Check",
                                         "Proposal","Contracted"))
})


#Data table output
output$recruitment_table<-DT::renderDataTable({
  if(!is.null(dataCom_recruitment()$r2))
  dataCom_recruitment()$r2
  },
  options = list(pageLength = 10,scrollX=TRUE),
  escape = FALSE,server = FALSE, selection = 'none'
)


##Recruitment performance output
output$recruitment_perfo<-renderPlotly({
  dataCom_recruitment()$r0%>%
    select("Recruitment stage" = action,items=content,recruiter=group)->data
  
  p <- ggplot(data = data, aes(x = recruiter, fill = `Recruitment stage`)) +
    geom_bar(position = "dodge")
  
  ggplotly(p)%>%
    layout(xaxis = list(title = "", tickangle = -45))
})

