library(shiny)
library(caret)
library(caretEnsemble)
library(LiblineaR)
library(leaps)
library(kknn)
library(rsconnect)
library(rhandsontable)
library(dplyr)

rsconnect::setAccountInfo(name='hadi',
                          token='F6A716C64B6ED9D28F3BD16910E0458F',
                          secret='UyBYJuzMulvcFesJHQJyEgyZKuhxEsmQsbcAG+mz')

filename = "table.RData"
if(file.exists(filename)){
  load(filename)
}else
  DF <- data.frame(TimeKeeperName=character(3), Percentage = numeric(3), RateType = character(3), stringsAsFactors = F)

##load the model and test data globally
load("model_partner_v3.RData")
load("data_te_partner_v3.RData")

load("model_associate_v2.RData")
load("data_te_associate_v2.RData")

load("model_other_v2.RData")
load("data_te_other_v2.RData")

### user input form
ui <- fluidPage(
  numericInput(inputId = "mattNum", value=254184, label="Please enter the matter number"),
  textInput(inputId = "lead_partner", value="Wong, E", label= "Please enter the name (last, first) of the partner expected to put in the most hours for this deal."),
  numericInput(inputId = "PartnerCount", value=0, label="Please enter the number of partners/principals/counsels that will be involved in the deal."),
  numericInput(inputId = "AssociateCount", value=0, label="Please enter the number of associates that will be involved in the deal."),
  numericInput(inputId = "OtherCount", value=0, label="Please enter the number of clerks/students/paralegals that will be involved in the deal."),
  selectInput(inputId = "DollarVal", selected = NULL, label="What is the Dollar value of the deal?",
              choices = c('<$10M','$10MM-$50M', '$50M-$250M', '>$250M')),
  selectInput(inputId = "buyer_seller", selected = NULL, label="Are we representing the vendor or the purchaser?",
              choices = c('Vendor','Purchaser')),
  selectizeInput(inputId = "jurisdiction", selected = 'ON', multiple=T, label="What Jurisdictions are likely to be involved?",
                 choices = c('AB','BC','ON','QC','A Canadian province/territory other than the above','USA','Other Foreign')),
  selectInput(inputId = "sellerfrom", selected = NULL, multiple=F, label="Is the immidiate seller a Canadian or foreign business?",
              choices = c('Canadian','Foreign')),
  selectInput(inputId = "clientfrom", selected = NULL, multiple=F, label="What is the client's country of origin?",
              choices = c('Canada',
                          'Europe',
                          'US',
                          'Asia',
                          'Other')),
  selectizeInput(inputId = "share_asset", selected = 'Asset', multiple=T, label="What kind of deal is being considered?",
                 choices = c('Share','Asset')),
  
  selectInput(inputId = "oneoff", selected = NULL, multiple=F, label="Is this a normal-course transaction or a one-off for the client?",
              choices = c('Normal-course','One-off')),
  selectInput(inputId = "reps_warranties", selected = NULL, multiple=F, label="Is this an as-is-where-is deal or normal reps and warranties?",
              choices = c('Normal reps and warranties','As-is-where-is')),
  selectInput(inputId = "cocounsel", selected = NULL, multiple=F, label="Is there a co-counsel leading the deal or supporting McMillan?",
              choices = c('Co-counsel lead', 'Co-counsel co-lead', 'Co-counsel support McMillan','No co-counsel')),
  selectInput(inputId = "num_seller", selected = NULL, multiple=F, label="How many sellers (individuals, trusts, partnerships, corporations) are involved?",
              choices = c('Single seller','Multiple sellers (10 or fewer)','Multiple sellers (11-20)','Multiple sellers (21 or more)')),
  selectInput(inputId = "num_purchaser", selected = NULL, multiple=F, label="How many purchasers (individuals, trusts, partnerships, corporations) are involved?",
              choices = c('Single purchaser','Mulitple purchasers')),
  selectInput(inputId = "purchaser_from", selected = NULL, multiple=F, label="Is the purhcaser a Canadian or a foreign business?",
              choices = c('Canadian',
                          'Foreign')),
  selectInput(inputId = "due_dil_role", selected = NULL, multiple=F, label="In the due diligence process, do you anticipate that McMillan will be leading or play a supporting role?",
              choices = c('Lead','Support','Take no part in it')),
  selectInput(inputId = "closing_role", selected = NULL, multiple=F, label="Do you anticipate that the McMillan team will be leading or supporting the closing?",
              choices = c('Lead',
                          'Support')),
  selectInput(inputId = "buyer_type", selected = NULL, multiple=F, label="What type of purchasers are involved?",
              choices = c('Corporate',
                          'Individual',
                          'Institutional investors',
                          'Private Equity')),
  selectInput(inputId = "ica_not", selected=NULL, label="Is Investment Canada Act notification required for this deal?",
              choices = c("yes","No")),
  actionButton("run", "Estimate Hours"),
  h2(""),
  h2(textOutput("out1")),
  h2(textOutput("out2")),
  h2(textOutput("out3")),
  conditionalPanel(
    condition = "input.run>0",
    htmlOutput("Insturctions"),
    rHandsontableOutput("hot"),
    actionButton("addRow", "Add Row"),
    actionButton("getFees", "Estimate Fees")
  )
    
  # ),
  # conditionalPanel(
  #   condition = "errors == FALSE",
  #   rHandsontableOutput("hot")
  # ),
  # conditionalPanel(
  #   condition = "errors == FALSE",
  #   actionButton("addRow", "Add Row")
  # ),
  # conditionalPanel(
  #   condition = "errors == FALSE",
  #   actionButton("getFees", "Estimate Fees")
  # ),
  # h2(""),
  # h2(textOutput("out4")),
  # h2(textOutput("out5"))
)

server <- function(input, output) {
  
  observeEvent(input$run, {
    
    #get lead partner info
    source("get_tkprinfo.R")
    result = get_tkprinfo(input$lead_partner, "Standard")
    lead_partner = result[[1]]
    
    if(input$lead_partner != "" & dim(lead_partner)[1]>0 ){

      source("getMatterType.R")
      matterType = getMatterType(input$mattNum)
      data_te_partner$MatterType = matterType
      buyer_seller = ifelse(input$buyer_seller=="Vendor","seller","buyer")
      data_te_partner$PPLcount = input$PartnerCount
      data_te_partner$office = lead_partner$office
      if(input$DollarVal=="<$10M")
        map = 1
      else if (input$DollarVal=="$10MM-$50M")
        map=2
      else if (input$DollarVal=="$50M-$250M")
        map=3
      else
        map=4
      data_te_partner$dolla_val = map
      data_te_partner$Did.we.represent.buyer.or.seller.= buyer_seller
      data_te_partner$Non_MCM = length(grep('A Canadian province/territory other than the above',input$jurisdiction))
      data_te_partner$AB = length(grep("AB",input$jurisdiction))
      data_te_partner$BC = length(grep("BC",input$jurisdiction))
      data_te_partner$ON = length(grep("ON",input$jurisdiction))
      data_te_partner$QC = length(grep("QC", input$jurisdiction))
      data_te_partner$Was.the.seller.a.Canadian.or.a.foreign.business. = input$sellerfrom
      data_te_partner$What.was.the.client.s.country.of.origin.= input$clientfrom
      data_te_partner$Was.this.a..normal.course..transaction.or.a..one.off..for.the.client..i.e..the.client.has.done.many.similar.deals.. = input$oneoff
      if(input$cocounsel == 'Co-counsel lead')
        cocounsel = 'Led the deal'
      else if(input$cocounsel == 'Co-counsel co-lead')
        cocounsel = 'Co-led'
      else if (input$cocounsel == 'Co-counsel support McMillan')
        cocounsel = 'Supported McMillan'
      else if(input$cocounsel == 'No co-counsel')
        cocounsel = 'There was no co-counsel'
      data_te_partner$Did.the.co.counsel.lead.the.deal.or.support.McMillan.s.role. = cocounsel
      data_te_partner$How.many.sellers..individuals..trusts..partnerships..corporations..were.involved.=input$num_seller
      data_te_partner$How.many.purchasers..individuals..trusts..partnerships..corporations..were.involved. = input$num_purchaser
      data_te_partner$Was.the.purhcaser.a.Canadian.or.a.foreign.business.=input$purchaser_from
      data_te_partner$Due.diligence..did.McMillan.lead.or.play.supporting.role.=input$due_dil_role
      data_te_partner$Closing..Did.McMillan.lead.it.or.played.supporting.role.=input$closing_role
      data_te_partner$What.type.of.buyers.were.involved. = input$buyer_type
      our_role = case_when(
        cocounsel == 'There was no co-counsel' ~ 'Primary',
        cocounsel == 'Supported McMillan' ~ 'Primary',
        cocounsel == 'Co-led' ~ 'Secondary',
        cocounsel == 'Led the deal' ~ 'Secondary'
      )
      data_te_partner$Was.McMillan.the.primary.counsel.for.the.client.s.transaction.or.did.we.play.a.secondary.role.supporting.the.primary.counsel.=our_role
      data_te_partner$What.kind.of.deal.was.it.= ifelse(length(input$share_asset)==2,"Share;#Asset",input$share_asset)
      data_te_partner$deparment = lead_partner$department
      data_te_partner$yrsExperience = 8 #ifelse(is.na(lead_partner$yrsExperience),20,lead_partner$yrsExperience)
      data_te_partner$ica_not = input$ica_not
      pred_partner = predict(model_partner,data_te_partner)
      pred_partner = exp(pred_partner)*ifelse(input$PartnerCount==0,0,1)
  
      # checked output against ground truth
  
      ##########################################
      ## associate data
      # isolate(data_te_associate)
      data_te_associate$PPLcount = input$AssociateCount
      data_te_associate$office=lead_partner$office
      data_te_associate$MatterType = matterType
      data_te_associate$dolla_val = map
      data_te_associate$Did.we.represent.buyer.or.seller. = buyer_seller
      data_te_associate$Non_MCM = length(grep('A Canadian province/territory other than the above',input$jurisdiction))
      data_te_associate$US = length(grep("USA",input$jurisdiction))
      data_te_associate$Other.foreign.jursidiction = length(grep("Other Foreign",input$jurisdiction))
      data_te_associate$AB = length(grep("AB",input$jurisdiction))
      data_te_associate$BC = length(grep("BC",input$jurisdiction))
      data_te_associate$ON = length(grep("ON",input$jurisdiction))
      data_te_associate$QC = length(grep("QC", input$jurisdiction))
      data_te_associate$Was.the.seller.a.Canadian.or.a.foreign.business.= input$sellerfrom
      data_te_associate$What.was.the.client.s.country.of.origin.=input$clientfrom
      data_te_associate$What.kind.of.deal.was.it.= ifelse(length(input$share_asset)==2,"Share;#Asset",input$share_asset)
      data_te_associate$Was.this.a..normal.course..transaction.or.a..one.off..for.the.client..i.e..the.client.has.done.many.similar.deals.. = input$oneoff
      data_te_associate$Was.this.an..as.is.where.is..deal.or.normal.reps.and.warranties. = input$reps_warranties
      data_te_associate$Did.the.co.counsel.lead.the.deal.or.support.McMillan.s.role. = cocounsel
      data_te_associate$How.many.sellers..individuals..trusts..partnerships..corporations..were.involved. = input$num_seller
      data_te_associate$How.many.purchasers..individuals..trusts..partnerships..corporations..were.involved. = input$num_purchaser
      data_te_associate$Was.the.purhcaser.a.Canadian.or.a.foreign.business. = input$purchaser_from
      data_te_associate$Due.diligence..did.McMillan.lead.or.play.supporting.role. = input$due_dil_role
      data_te_associate$Closing..Did.McMillan.lead.it.or.played.supporting.role. = input$closing_role
      data_te_associate$What.type.of.buyers.were.involved.=input$buyer_type
      data_te_associate$Was.McMillan.the.primary.counsel.for.the.client.s.transaction.or.did.we.play.a.secondary.role.supporting.the.primary.counsel.= our_role
      data_te_associate$department = lead_partner$department
      data_te_associate$yrsExperience = 8#ifelse(is.na(lead_partner$yrsExperience),20,lead_partner$yrsExperience)
      data_te_associate$ica_not = input$ica_not
      # browser()
      # save(data_te_assoc,file="data_te_assoc.RData")
      pred_assoc = predict(model_associate,data_te_associate)
      pred_assoc = exp(pred_assoc)*ifelse(input$AssociateCount==0,0,1)
  
      # checked output against ground truth
      ##################################################################
      ## Other data
  
      data_te_other$PPLcount = input$OtherCount
      data_te_other$office=lead_partner$office
      data_te_other$MatterType = matterType
      data_te_other$dolla_val = map
      data_te_other$Did.we.represent.buyer.or.seller. = buyer_seller
      data_te_other$Non_MCM = length(grep('A Canadian province/territory other than the above',input$jurisdiction))
      data_te_other$US = length(grep("USA",input$jurisdiction))
      data_te_other$Other.foreign.jursidiction = length(grep("Other Foreign",input$jurisdiction))
      data_te_other$AB = length(grep("AB",input$jurisdiction))
      data_te_other$BC = length(grep("BC",input$jurisdiction))
      data_te_other$ON = length(grep("ON",input$jurisdiction))
      data_te_other$QC = length(grep("QC", input$jurisdiction))
      data_te_other$Was.the.seller.a.Canadian.or.a.foreign.business.= input$sellerfrom
      data_te_other$What.was.the.client.s.country.of.origin.=input$clientfrom
      data_te_other$What.kind.of.deal.was.it.= ifelse(length(input$share_asset)==2,"Share;#Asset",input$share_asset)
      data_te_other$Was.this.a..normal.course..transaction.or.a..one.off..for.the.client..i.e..the.client.has.done.many.similar.deals.. = input$oneoff
      data_te_other$Was.this.an..as.is.where.is..deal.or.normal.reps.and.warranties. = input$reps_warranties
      data_te_other$Did.the.co.counsel.lead.the.deal.or.support.McMillan.s.role. = cocounsel
      data_te_other$How.many.sellers..individuals..trusts..partnerships..corporations..were.involved. = input$num_seller
      data_te_other$How.many.purchasers..individuals..trusts..partnerships..corporations..were.involved. = input$num_purchaser
      data_te_other$Was.the.purhcaser.a.Canadian.or.a.foreign.business. = input$purchaser_from
      data_te_other$Due.diligence..did.McMillan.lead.or.play.supporting.role. = input$due_dil_role
      data_te_other$Closing..Did.McMillan.lead.it.or.played.supporting.role. = input$closing_role
      data_te_other$What.type.of.buyers.were.involved.=input$buyer_type
      data_te_other$Was.McMillan.the.primary.counsel.for.the.client.s.transaction.or.did.we.play.a.secondary.role.supporting.the.primary.counsel.= our_role
      data_te_other$department = lead_partner$department
      data_te_other$ica_not = input$ica_not
      data_te_other$yrsExperience = 8
  
      pred_other = predict(model_other,data_te_other)
      pred_other = exp(pred_other)*ifelse(input$OtherCount==0,0,1)
    }
    else{
      pred_partner=NA
      pred_assoc = NA
      pred_other = NA
    }

    output$out1 <- renderText({
      if(dim(lead_partner)[1] == 0)
        paste("Error: no timekeeper was found with the name ", input$lead_partner)
      else if(input$lead_partner=="")
        paste("Error: no partner name was entered. A partner name must be entered.")
      else
        paste("Projected hours for partners: ", round(pred_partner), " hours")

    })
    output$out2 <- renderText({
        paste("Projected hours for associates: ", round(pred_assoc), " hours")
    })
    output$out3 <- renderText({
        paste("Projected hours for students/clerks/paralegals: ", round(pred_other), " hours")
    })
  })
   
}

shinyApp(ui = ui, server = server)