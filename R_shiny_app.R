  library(shiny)
  library(survival)
  library(DT)
  library(shinythemes)
  library(SPARQL)
  library(shinycssloaders)
  library(shinyWidgets)
  
  # Define UI ----
  ui <- fluidPage(theme = shinytheme("yeti"),
    sidebarLayout(
      sidebarPanel(
        selectInput("endpoint", label = h4(tags$b("Select an SPARQL endpoint")), 
                    choices = list(Blazegraph = "http://sparql.cancerdata.org", Others = "NIL")),
        htmlOutput("myEndpoint"),
        tags$hr(),
        checkboxGroupInput("", h4(tags$b("Select variables to display:"))),
        checkboxGroupInput("patientID", "", c("Patient ID" = "?patientID"), selected = "?patientID"),
        checkboxGroupInput("gender", "", c("Gender" = "?value"), selected = "?value"),
        checkboxGroupInput("age_at_diagnosis", "", c("Age at Diagnosis" = "?age_at_diagnosis")),
        checkboxGroupInput("death", "", c("Death" = "?Death")),
        checkboxGroupInput("survival_days", "", c("Survival Days" = "?Overall_survival_days")),
        checkboxGroupInput("ct_scan", "", c("CT Scan" = "?CT_Scan")),
        checkboxGroupInput("clinical_Tstage", "", c("Clinical T stage value" = "?TstageValue")),
        checkboxGroupInput("clinical_Nstage", "", c("Clinical N stage value" = "?NstageValue")),
        checkboxGroupInput("clinical_Mstage", "", c("Clinical M stage value" = "?MstageValue")),
      
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Query", verbatimTextOutput("sparqlquery")),
          tabPanel("Result", withSpinner(dataTableOutput("result"),type = 6), downloadButton("downloadData", "Download"))
        )
      )
    )
  )
  
  
  # Define server logic ----
  server <- function(input, output) {
    
    query <- reactive({
     
    prefix <-paste0(
"prefix rr: <http://www.w3.org/ns/r2rml#>
prefix ex: <http://example.com/ns#>
prefix map: <http://mapping.local/>
prefix sty: <http://purl.bioontology.org/ontology/STY/>
prefix sio: <http://semanticscience.org/resource/SIO_>
prefix foaf: <http://xmlns.com/foaf/0.1/>
prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
prefix xsd: <http://www.w3.org/2001/XMLSchema#>
prefix ncit: <http://ncicb.nci.nih.gov/xml/owl/EVS/Thesaurus.owl#>
prefix roo: <http://www.cancerdata.org/roo/>
prefix icd: <http://purl.bioontology.org/ontology/ICD10/>
prefix skos: <http://www.w3.org/2008/05/skos#>
prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
prefix uo: <http://purl.obolibrary.org/obo/UO_>
prefix time: <http://www.w3.org/2006/time#>
prefix MEDDRA: <http://purl.bioontology.org/ontology/MEDDRA/>
prefix ro: <http://www.radiomics.org/RO/>", sep="\n")

      select<-paste0("SELECT", sep = " ", input$patientID, sep = " ", input$gender, sep = " ", input$age_at_diagnosis, sep = " ", input$death, sep = " ", input$survival_days, sep = " ", input$ct_scan, sep = " ", input$clinical_Tstage, sep = " ", input$clinical_Nstage, sep = " ", input$clinical_Mstage, sep = "\n")
         
      where <- paste0(" WHERE { 
        ?patient a ncit:C16960.
        ?patient roo:P100042 ?patientID.
                   
        ?patient roo:P100018 ?biologicalSex.
        ?biologicalSex roo:P100042 ?value.
                   
        ?patient roo:P100016 ?ageatdiagnosis.
        ?ageatdiagnosis roo:P100042 ?age_at_diagnosis.
                   
        ?patient roo:P100254 ?death.
        ?death roo:P100042 ?Death.
                   
        ?patient roo:P100311 ?overallsurvivaldays.
        ?overallsurvivaldays roo:P100042 ?Overall_survival_days.
                   
        ?patient roo:P100024 ?ctscan.
        ?ctscan roo:P100042 ?CT_Scan.
              	   
        ?patient roo:P100029 ?neoplasm.
        ?neoplasm roo:P100244 ?Tstage.
        ?Tstage roo:P100042 ?TstageValue.
                   
        ?neoplasm roo:P100242 ?Nstage.
        ?Nstage roo:P100042 ?NstageValue.
              	
        ?neoplasm roo:P100241 ?Mstage.
        ?Mstage roo:P100042 ?MstageValue.
        }")
    
    query <- paste(prefix,select,where, sep = "\n")
    return(query) })
    
    test <- reactive({
      query <- query()
      endpoint <- "http://sparql.cancerdata.org/namespace/Abhinandan_workspace/sparql"
      output <- SPARQL(endpoint, query)
      outputList <- output$results
    })
    
    output$myEndpoint <- renderUI(tags$a(input$endpoint))
    
  
    output$sparqlquery <- renderText({query()})
    
    output$result <- renderDataTable({test()},options = list(pageLength = 20) ,filter="bottom")

    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$result, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(test(), file, row.names = FALSE)
      }
    )
  }
  
  # Run the app ----
  shinyApp(ui = ui, server = server)