library(shiny)
library(caret)
library(caretEnsemble)
library(LiblineaR)
library(leaps)
library(rsconnect)

rsconnect::setAccountInfo(name='hadi',
                          token='F6A716C64B6ED9D28F3BD16910E0458F',
                          secret='UyBYJuzMulvcFesJHQJyEgyZKuhxEsmQsbcAG+mz')

##load the model and test data globally
load("model_partner.RData")
load("data_te_partner.RData")

load("model_associates.RData")
load("data_te_assoc.RData")

load("model_other.RData")
load("data_te_other.RData")

### user input form
ui <- fluidPage(
  numericInput(inputId = "PartnerCount", value=1, label = "Number of Partners/Counsels/Principals:"),
  numericInput(inputId = "AssociateCount", value=0, label = "Number of Associates:"),
  numericInput(inputId = "OtherCount", value=0, label = "Number of Students/Clerks/Paralegals:"),
  selectInput(inputId="office", label="Where was the office of the highest billing timekeeper?", 
              choices=c("McMillan LLP - Toronto",
                        "McMillan LLP - Montreal",
                        "McMillan LLP - Vancouver",
                        "McMillan LLP - Calgary",
                        "McMillan LLP - Ottawa"),
              selected = NULL, multiple = FALSE,
              selectize = TRUE, width = NULL, size = NULL),
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
selectInput(inputId = "primary", selected = NULL, multiple=F, label="What type of purchasers are involved?",
            choices = c("Primary","Secondary")),

selectInput(inputId = "department", selected = NULL, multiple=F, label="What is the practice area of the highest billing lawyer?",
            choices = c("Business Law","Capital Markets",'Commercial Real Estate',
                        'Financial Services','Regulatory', 'Tax')),
numericInput(inputId = "yrsExperience", value=1, label="How long ago did the highest billing lawyer obtain their JD?"),
selectInput(inputId = "ica_not", selected=NULL, label="Is Investment Canada Act notification required for this deal?",
            choices = c("yes","No")),
actionButton("run", "Estimate Hours"),
  h2(""),
  h2(textOutput("out1")),
  h2(textOutput("out2")),
  h2(textOutput("out3"))
)

server <- function(input, output) {

  observeEvent(input$run, {
    
    # browser()
    ########################################
    ### partner data
    data_te_partner$PPLcount = input$PartnerCount
    data_te_partner$department = input$department
    data_te_partner$yrsExperience = input$yrsExperience
    data_te_partner$What.kind.of.deal.was.it. = ifelse(length(input$share_asset)==2,"Share;#Asset",input$share_asset)
    buyer_seller = ifelse(input$buyer_seller=="Vendor","seller","buyer")
    data_te_partner$Did.we.represent.buyer.or.seller. = buyer_seller
    data_te_partner$ica_not = input$ica_not
    # browser()
    pred_partner = predict(model_partner,data_te_partner)
    pred_partner = exp(pred_partner)*ifelse(input$PartnerCount==0,0,1)
    # checked output against ground truth

    ##########################################
    ## associate data
    data_te_assoc$PPLcount=input$AssociateCount
    data_te_assoc$office=input$office
    if(input$DollarVal=="<$10M")
      map = 1
    else if (input$DollarVal=="$10MM-$50M")
      map=2
    else if (input$DollarVal=="$50M-$250M")
      map=3
    else
      map=4
    
    data_te_assoc$dolla_val = map
    # browser()
    data_te_assoc$Did.we.represent.buyer.or.seller..x=buyer_seller
    data_te_assoc$Did.we.represent.buyer.or.seller..y=buyer_seller
    data_te_assoc$Non_MCM = length(grep('A Canadian province/territory other than the above',input$jurisdiction))
    data_te_assoc$US = length(grep("USA",input$jurisdiction))
    data_te_assoc$Other.foreign.jursidiction = length(grep("Other Foreign",input$jurisdiction))
    data_te_assoc$AB = length(grep("AB",input$jurisdiction))
    data_te_assoc$BC = length(grep("BC",input$jurisdiction))
    data_te_assoc$ON = length(grep("ON",input$jurisdiction))
    data_te_assoc$QC = length(grep("QC", input$jurisdiction))
    data_te_assoc$Was.the.seller.a.Canadian.or.a.foreign.business.= input$sellerfrom
    data_te_assoc$What.was.the.client.s.country.of.origin.=input$clientfrom
    data_te_assoc$What.kind.of.deal.was.it..x = ifelse(length(input$share_asset)==2,"Share;#Asset",input$share_asset)
    data_te_assoc$Was.this.a..normal.course..transaction.or.a..one.off..for.the.client..i.e..the.client.has.done.many.similar.deals.. = input$oneoff
    data_te_assoc$Was.this.an..as.is.where.is..deal.or.normal.reps.and.warranties. = input$reps_warranties
    # browser()
    if(input$cocounsel == 'Co-counsel lead')
      cocounsel = 'Led the deal'
    else if(input$cocounsel == 'Co-counsel co-lead')
      cocounsel = 'Co-led'
    else if (input$cocounsel == 'Co-counsel support McMillan')
      cocounsel = 'Supported McMillan'
    else if(input$cocounsel == 'No co-counsel')
      cocounsel = 'There was no co-counsel'
    data_te_assoc$Did.the.co.counsel.lead.the.deal.or.support.McMillan.s.role. = cocounsel
    data_te_assoc$How.many.sellers..individuals..trusts..partnerships..corporations..were.involved. = input$num_seller
    data_te_assoc$How.many.purchasers..individuals..trusts..partnerships..corporations..were.involved. = input$num_purchaser
    data_te_assoc$Was.the.purhcaser.a.Canadian.or.a.foreign.business. = input$purchaser_from
    data_te_assoc$Due.diligence..did.McMillan.lead.or.play.supporting.role. = input$due_dil_role
    data_te_assoc$Closing..Did.McMillan.lead.it.or.played.supporting.role. = input$closing_role
    data_te_assoc$What.type.of.buyers.were.involved.=input$buyer_type
    data_te_assoc$Was.McMillan.the.primary.counsel.for.the.client.s.transaction.or.did.we.play.a.secondary.role.supporting.the.primary.counsel.= input$primary
    data_te_assoc$department = input$department
    data_te_assoc$yrsExperience = input$yrsExperience
    data_te_assoc$ica_not = input$ica_not
    data_te_assoc$What.kind.of.deal.was.it..y = ifelse(length(input$share_asset)==2,"Share;#Asset",input$share_asset)
    # browser()
    # save(data_te_assoc,file="data_te_assoc.RData")
    pred_assoc = predict(model_associates,data_te_assoc)
    pred_assoc = exp(pred_assoc)*ifelse(input$AssociateCount==0,0,1)
    # checked output against ground truth
    ##################################################################
    ## Other data
    data_te_other$PPLcount=input$OtherCount
    data_te_other$office=input$office
    data_te_other$dolla_val = map
    data_te_other$Did.we.represent.buyer.or.seller..x=buyer_seller
    data_te_other$Did.we.represent.buyer.or.seller..y=buyer_seller
    data_te_other$Non_MCM = length(grep('A Canadian province/territory other than the above',input$jurisdiction))
    data_te_other$US = length(grep("USA",input$jurisdiction))
    data_te_other$Other.foreign.jursidiction = length(grep("Other Foreign",input$jurisdiction))
    data_te_other$AB = length(grep("AB",input$jurisdiction))
    data_te_other$BC = length(grep("BC",input$jurisdiction))
    data_te_other$ON = length(grep("ON",input$jurisdiction))
    data_te_other$QC = length(grep("QC", input$jurisdiction))
    data_te_other$Was.the.seller.a.Canadian.or.a.foreign.business.= input$sellerfrom
    data_te_other$What.was.the.client.s.country.of.origin.=input$clientfrom
    data_te_other$What.kind.of.deal.was.it..x = ifelse(length(input$share_asset)==2,"Share;#Asset",input$share_asset)
    data_te_other$Was.this.a..normal.course..transaction.or.a..one.off..for.the.client..i.e..the.client.has.done.many.similar.deals.. = input$oneoff
    data_te_other$Was.this.an..as.is.where.is..deal.or.normal.reps.and.warranties. = input$reps_warranties
    data_te_other$Did.the.co.counsel.lead.the.deal.or.support.McMillan.s.role. = cocounsel
    data_te_other$How.many.sellers..individuals..trusts..partnerships..corporations..were.involved. = input$num_seller
    data_te_other$How.many.purchasers..individuals..trusts..partnerships..corporations..were.involved. = input$num_purchaser
    data_te_other$Was.the.purhcaser.a.Canadian.or.a.foreign.business. = input$purchaser_from
    data_te_other$Due.diligence..did.McMillan.lead.or.play.supporting.role. = input$due_dil_role
    data_te_other$Closing..Did.McMillan.lead.it.or.played.supporting.role. = input$closing_role
    data_te_other$What.type.of.buyers.were.involved.=input$buyer_type
    data_te_other$Was.McMillan.the.primary.counsel.for.the.client.s.transaction.or.did.we.play.a.secondary.role.supporting.the.primary.counsel.= input$primary
    data_te_other$department = input$department
    data_te_other$yrsExperience = input$yrsExperience
    data_te_other$ica_not = input$ica_not
    data_te_other$What.kind.of.deal.was.it..y = ifelse(length(input$share_asset)==2,"Share;#Asset",input$share_asset)

    pred_other = predict(model_other,data_te_other)
    pred_other = exp(pred_other)*ifelse(input$OtherCount==0,0,1)
    # checked output against ground truth
    
    output$out1 <- renderText({
      # browser()
      paste("Projected hours for partners: ", round(pred_partner), " hours")
    })
    output$out2 <- renderText({
      # browser()
      paste("Projected hours for associates: ", round(pred_assoc), " hours")
    })
    output$out3 <- renderText({
      # browser()
      paste("Projected hours for students/clerks/paralegals: ", round(pred_other), " hours")
    })
  })
}

shinyApp(ui = ui, server = server)