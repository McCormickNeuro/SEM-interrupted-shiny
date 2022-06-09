#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(fontawesome)
library(shinyBS)
library(DT)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  #shinyjs::useShinyjs(),
  
  # Application Title
  dashboardHeader(title = "SEM, interrupted"),
  
  # Navigation Sidebar
  dashboardSidebar(
    
    # Create Sidebar Menu Options
    tags$style("@import url(https://use.fontawesome.com/releases/v6.0.0/css/all.css);"),
    sidebarMenu(
      #style = "position:fixed",
      id = "tabs",
      menuItem(text = "Upload Data", 
               tabName = "fileupload", 
               icon = icon("circle-arrow-up", lib = "glyphicon")),
      menuItem(text = "Analysis Options",
               tabName = "analysisoptions",
               icon = icon("gear", verify_fa = FALSE)),
      menuItem(text = "Individual Item Mediation",
               tabName = "individualitem",
               icon = icon("chart-gantt", verify_fa = FALSE)),
      menuItem(text = "Latent Variable Mediation",
               tabName = "lvmodel",
               icon = icon("hat-wizard", verify_fa = FALSE)),
      menuItem(text = "Latent Variable Diagnostics",
               tabName = "lvdiag",
               icon = icon("wand-magic-sparkles", verify_fa = FALSE)),
      menuItem(text = "Download Outputs",
               tabName = "downloads",
               icon = icon("circle-arrow-down", lib = "glyphicon"))
    )
  ),
  
  # Shiny App Content  
  dashboardBody(
    
    
    # Custom Font for Title
    tags$head(tags$style(HTML('
    .main-header .logo {
    font-family: Arial, Helvetica, sans-serif;
    font-weight: bold;
    font-size: 24px;
    }'))),
    
    # Fixed Position for Sidebar + Title
    tags$script(HTML("$('body').addClass('fixed');")),
    
    tabItems(  
      # Page 1: Upload Data 
      tabItem(
        tabName = "fileupload",
        tags$a(href="javascript:history.go(0)", 
               popify(tags$i(class="fa fa-refresh fa"),
                      title = "Reload", 
                      content = "Click here to reset the Shiny app",
                      placement = "right")),
        
        # File Upload Field & Nickname
        fluidRow(
          shiny::column(width = 3,
                        radioButtons(inputId = "inputType",
                                     label = "Select Data Source",
                                     choices = c("User Data", "Example Data"),
                                     selected = "User Data")),
          shiny::column(width = 6,
                        uiOutput("DataSource")
          )
        ),
        fluidRow(
          shiny::column(width = 6,
            textInput(inputId = "nickname",
                      label = "Analysis Nickname (Optional)")
          )
        ),
        
        # View User Data Table
        fluidRow(
          uiOutput("UserData")
        ),
        
        # Variable Selection
        fluidRow(
          shiny::column(width = 6,
            uiOutput("IDVar"),
            textOutput("Nsub")
          )
        ),
        shiny::br(),
        fluidRow(
          shiny::column(width = 6,
            uiOutput("XVar"),
            uiOutput("YVar")
          ),
          shiny::column(width = 6,
            uiOutput("compVar"),
            uiOutput("userCompName")
          )
        ),
        fluidRow(
          shiny::column(width = 6,
          ),
          shiny::column(width = 6,
            uiOutput("ItemVars"),
            textOutput("Nitems")
          )
        ),
        
        # Warnings for Improper Input
        fluidRow(
          shiny::column(width = 6,
            textOutput("xyWarning"),
            tags$head(tags$style("#xyWarning{color: red;
                                 font-size: 15px;
                                 font-style: italic;
                                 }"))
            ),
          shiny::column(width = 6,
            textOutput("itemWarning"),
            tags$head(tags$style("#itemWarning{color: red;
                                 font-size: 15px;
                                 font-style: italic;
                                 }"))
            )
        ),
        shiny::br(),
        
        # View Final Data Frame
        fluidRow(
          uiOutput("FinalData")
        ),
        shiny::br(),
        
        # Press to Continue to Analysis Options
        fluidRow(
          shiny::column(width = 6,
            uiOutput("AnalysisOptionsButton")
          )
        )
      ),
      
      # Page 2: Analysis Options
      tabItem(
        tabName = "analysisoptions",
        fluidRow(
          uiOutput("AnalysisOptions")
        ),
        shiny::br(),
        shiny::br(),
        
        # Press to Continue to Analysis Results
        fluidRow(
          shiny::column(width = 6,
            uiOutput("AnalyzeButton")
          )
        )
      ),
      
      # Page 3: Individual Item Mediation Results
      tabItem(
        tabName = "individualitem",
        uiOutput("FitData")
      ),
      
      # Page 4: Latent Variable Mediation Results
      tabItem(
        tabName = "lvmodel",
        uiOutput("LatentModels")
      ),
      
      # Page 5: Latent Variable Diagnostics
      tabItem(
        tabName = "lvdiag",
        uiOutput("LatentDiagnostics")
      ),
      
      # Page 6: Download Outputs/Reports
      tabItem(
        tabName = "downloads",
        uiOutput("DownloadOutputs")
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  `%>%` <- magrittr::`%>%`
  
  # Page 1: Upload Data 
  #-----------------------------------------------------------------------------  

  ## Select Data Source
  output$DataSource <- renderUI({
    if (input$inputType == "User Data"){
      fileInput(inputId = "userfile", 
                label = "Upload a file: (supports CSV, Excel, & SPSS files)",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv",
                           ".sav",
                           ".xls",
                           ".xlsx"))
    } else if (input$inputType == "Example Data"){
      selectInput(inputId = "selectexample",
                  label = "Select Dataset",
                  choices = c("",
                              "Proportional Mediation: 4-Item Sum Score" = "propSum",
                              "Interrupted Mediation: Grey Matter Measures" = "gmvolume",
                              "Suppressed Mediation: 10 Item Scale Score" = "scale10"),
                  selected = "",
                  multiple = FALSE)
    }
  })
  
  ## Read In User-File (need to write function for other data types)
  userdata <- reactive({
    if(input$inputType == "User Data"){
      if (is.null(input$userfile$datapath)) return(NULL)
      if (grepl(".sav", input$userfile$datapath)){
        foreign::read.spss(input$userfile$datapath, to.data.frame = TRUE)
      } else if (grepl(".xls", input$userfile$datapath)){
        read.table(input$userfile$datapath, sep="\t", header = TRUE)
      } else {
        read.csv(input$userfile$datapath, header = TRUE)
      }
    } else if (input$inputType == "Example Data"){
      if (is.null(input$selectexample)) return(NULL)
      if (input$selectexample == ""){
        return(NULL)
      } else if (input$selectexample == "gmvolume"){
        read.csv("./data/example01-2items.csv")
      } else if (input$selectexample == "scale10"){
        read.csv("./data/example02-10items.csv")
      } else if (input$selectexample == "propSum"){
        read.csv("./data/example03-4items.csv")
      }
    }
  })
  
  ## Display User Data in Table
  output$UserData <- renderUI({
    if(is.null(userdata())) return(NULL)
    req(userdata())
    box(
      title = "User Data", width = NULL, status = "primary",
      collapsible = TRUE, collapsed = FALSE,
      div(style = "overflow-x: scroll", tableOutput("dataTable"))
    )
  })
  output$dataTable <- renderTable({
    head(userdata(), row = 10)
  })
  
  ## Select Variables UI
  output$IDVar <- renderUI({
    if (is.null(userdata())) return(NULL)
    items <- colnames(userdata())
    selectInput("id-dropdown", "ID Variable:", items)
  })
  output$XVar <- renderUI({
    if (is.null(userdata())) return(NULL)
    items <- colnames(userdata() %>% dplyr::select(-input$`id-dropdown`))
    selectInput("x-dropdown", "Predictor (X) Variable:", 
                choices = c("", items), selected = "")
  })
  output$YVar <- renderUI({
    if (is.null(userdata())) return(NULL)
    items <- colnames(userdata() %>% dplyr::select(-input$`id-dropdown`))
    selectInput("y-dropdown", "Outcome (Y) Variable:", 
                choices = c("", items), selected = "")
  })
  output$ItemVars <- renderUI({
    if (is.null(input$compSelect)) return(NULL)
    if (!is.null(input$compSelect) & input$compSelect == "") return(NULL)
    items <- colnames(userdata() %>% dplyr::select(-input$`id-dropdown`,
                                            -input$`x-dropdown`,
                                            -input$`y-dropdown`))
    if (input$compSelect == "Sum Score" | input$compSelect == "Mean Score"){
      selectInput("item-dropdown", 
                  HTML("Select Mediator Item Variables<br>(select as many as apply):"), 
                  choices = items, multiple = TRUE)
    } else if (input$compSelect == "Difference Score" |
               input$compSelect == "Product Score" |
               input$compSelect == "Ratio Score"){
      list(selectInput("item1-dropdown", "Select Mediator Item 1:", 
                       choices = items, selected = items[1],
                       multiple = FALSE),
           selectInput("item2-dropdown", "Select Mediator Item 2:", 
                       choices = items, selected = items[2],
                       multiple = FALSE))
    }
  })
  
  ## Process Item Inputs
  
    ### Identify & Print Number of Subjects
    N <- reactive({
      req(userdata())
      userdata() %>% dplyr::select(input$`id-dropdown`) %>% dplyr::n_distinct()
    })
    output$Nsub <- renderText({
      paste0("Number of Subjects: ", N())
    })
    
    ### Identify & Print Number of Items
    Nitems <- reactive({
      length(input$`item-dropdown`)
    })
    output$Nitems <- renderText({
      if (Nitems() == 0) return(NULL)
      if (!is.null(input$compSelect) & 
          !(input$compSelect == "Sum Score" |
            input$compSelect == "Mean Score")) return(NULL)
      paste0("Number of Items: ", Nitems())
    })
  
    ### Store Relevant Variables
    xDat <- reactive({
      req(userdata())
      userdata() %>% dplyr::select(input$`x-dropdown`)
    })
    yDat <- reactive({
      userdata() %>% dplyr::select(input$`y-dropdown`)
    })
    itemDat <- reactive({
      if (is.null(input$compSelect)) return(NULL)
      if (input$compSelect == "Sum Score" |
          input$compSelect == "Mean Score"){
        userdata() %>% dplyr::select(input$`item-dropdown`)
      } else {
        userdata() %>% dplyr::select(input$`item1-dropdown`, 
                              input$`item2-dropdown`)
      }
    })
  
    ### Store Relevant Variable Names
    nickname <- reactive({
      if (is.null(input$nickname) | input$nickname == "") {Sys.Date()
      } else {input$nickname}
    })
    xName <- reactive({
      colnames(xDat())
    })
    yName <- reactive({
      colnames(yDat())
    })
    itemNames <- reactive({
      if (is.null(input$`item-dropdown`) & 
          (is.null(input$`item1-dropdown`) |
           is.null(input$`item2-dropdown`))) return(NULL)
      colnames(itemDat())
    })
  
    ### Generate Warnings for Duplicated Item Selections
    output$xyWarning <- renderText({
      if (is.null(input$`x-dropdown`) | is.null(input$`y-dropdown`)) return(NULL)
      if (input$`x-dropdown` == "" | input$`y-dropdown` == "") return(NULL)
      if (input$`x-dropdown` == input$`y-dropdown`){
        paste0("X and Y variables cannot be the same, 
               please select another variable.")
      }
    })
    output$itemWarning <- renderText({
      if (is.null(itemNames()) |
          is.null(input$`item1-dropdown`)) return(NULL)
      if (input$`item1-dropdown` == input$`item2-dropdown`){
        paste0("The two mediator variables cannot be the same,
               please select another variable.")
      }
    })
  
  ## Select Composite Type (with optional name input)
  compLabels = c("",
                 "Sum Score",
                 "Mean Score",
                 "Difference Score",
                 "Product Score",
                 "Ratio Score")
  
  output$compVar <- renderUI({
    if (is.null(input$`y-dropdown`)) return(NULL)
    if (input$`y-dropdown` == "") return(NULL)
    selectInput(inputId = "compSelect", 
                label = "Type of Mediator Composite",
                choices = compLabels,
                selected = compLabels[1])
  })
  output$userCompName <- renderUI({
    if (is.null(input$`y-dropdown`)) return(NULL)
    if (input$`y-dropdown` == "") return(NULL)
    textInput("userCompText", 
              label = "Custom Mediator Name (Optional)",
              value = NULL)
  })
  compName <- reactive({
    if (!is.null(input$userCompText) &
        input$userCompText != ""){
      input$userCompText
    } else {
      if (input$compSelect == "Sum Score"){"SumScore"
      } else if (input$compSelect == "Mean Score"){"MeanScore"
      } else if (input$compSelect == "Difference Score"){"DiffScore"
      } else if (input$compSelect == "Product Score"){"ProductScore"
      } else if (input$compSelect == "Ratio Score"){"RatioScore"}
    }
  })
  
  ## Generate & Display Final Data for Analysis
  MediationData <- reactive({
    M <- cbind(userdata() %>% dplyr::select(input$`id-dropdown`),
               xDat(), yDat(), itemDat())
    if (input$compSelect == "Sum Score"){
      M$SumScore <- rowSums(itemDat(), na.rm =  TRUE)
      if (!is.null(input$userCompText) &
          input$userCompText != ""){
        M <- M %>% 
          plyr::rename(c("SumScore" = input$userCompText))
      }
    } else if (input$compSelect == "Mean Score"){
      M$MeanScore <- rowMeans(itemDat(), na.rm = TRUE)
      if (!is.null(input$userCompText) &
          input$userCompText != ""){
        M <- M %>%  
          plyr::rename(c("MeanScore" = input$userCompText))
      }
    } else if (input$compSelect == "Difference Score"){
      M$DiffScore <- itemDat()[,1] - itemDat()[,2]
      if (!is.null(input$userCompText) &
          input$userCompText != ""){
        M <- M %>%  
          plyr::rename(c("DiffScore" = input$userCompText))
      }
    } else if (input$compSelect == "Product Score"){
      M$ProductScore <- itemDat()[,1] * itemDat()[,2]
      if (!is.null(input$userCompText) &
          input$userCompText != ""){
        M <- M %>% 
          plyr::rename(c("ProductScore" = input$userCompText))
      }
    } else if (input$compSelect == "Ratio Score"){
      M$RatioScore <- itemDat()[,1] / itemDat()[,2]
      if (!is.null(input$userCompText) &
          input$userCompText != ""){
        M <- M %>% 
          plyr::rename(c("RatioScore" = input$userCompText))
      }
    }
    return(M)
  })
  output$FinalData <- renderUI({
    if (is.null(userdata())) return(NULL)
    if (is.null(input$`x-dropdown`) | is.null(input$`y-dropdown`)) return(NULL)
    if (!is.null(input$`x-dropdown`) &
        input$`x-dropdown` == input$`y-dropdown`) return(NULL)       
    if (!is.null(input$`item1-dropdown`)){
      if (input$`item1-dropdown` == input$`item2-dropdown`) return(NULL)
    }
    if (is.null(input$`item1-dropdown`) & 
                is.null(input$`item-dropdown`)) return(NULL)
    
    box(
      title = "Final Data", width = NULL, status = "primary",
      collapsible = TRUE, collapsed = FALSE,
      div(style = "overflow-x: scroll", tableOutput("medTable"))
    )
  })
  output$medTable <- renderTable({
    head(MediationData(), row = 10)
  })
  
  ## Button to Advance to Analysis Options
  analysisOpts <- reactiveValues(flag = FALSE)
  observeEvent(input$userfile, {analysisOpts$flag <- FALSE})
  observeEvent(input$selectexample, {analysisOpts$flag <- FALSE})
  output$AnalysisOptionsButton <- renderUI({
    if (is.null(input$`x-dropdown`)) return(NULL)
    if (!is.null(input$`x-dropdown`) &
        input$`x-dropdown` == input$`y-dropdown`) return(NULL)       
    if (!is.null(input$`item1-dropdown`)){
      if (input$`item1-dropdown` == input$`item2-dropdown`) return(NULL)
    }
    if (is.null(input$`item1-dropdown`) & 
        is.null(input$`item-dropdown`)) return(NULL)
    
    actionButton(inputId = "GoToOptions", label = "Go to Analysis Options",
                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
  })
  observeEvent(input$GoToOptions, {
    updateTabItems(inputId = "tabs", selected = "analysisoptions")
    analysisOpts$flag <- TRUE
  })
  #-----------------------------------------------------------------------------
  # Page 2: Analysis Options
  #-----------------------------------------------------------------------------
  ## Generate Radio Buttons for Analysis Options
  output$AnalysisOptions <- renderUI({
    if(!analysisOpts$flag) return(NULL)
    box(
      title = "Analysis Options", width = NULL, status = "primary",
      collapsible = FALSE, collapsed = FALSE,
      uiOutput("CheckBoxes")
    )
  })
  output$CheckBoxes <- renderUI({
    div(
      fluidRow(
        column(width = 4,
               radioButtons(
                 inputId = "estimatorOpt",
                 label = "Estimator",
                 choices = list("Normal-theory ML" = "ML",
                                "Robust ML" = "MLR"),
                 selected = "ML"
               ),
               tipify(bsButton(inputId = "help1", 
                               label = shiny::icon("circle-question", 
                                                   verify_fa = FALSE),
                               style = "inverse", size = "small"),
                      title = strwrap(paste("Normal-theory Maximum Likelihood (ML) 
                                      assumes normality of the residuals for 
                                      outcome (endogenous) variables when 
                                      computing standard errors. 
                                      Robust ML (MLR) corrects these standard 
                                      errors for non-normality in the
                                      outcome variables."),
                                      width=1e10, simplify = TRUE),
                      placement = "right",
                      trigger = "hover")
        ),
        column(width = 4,
               radioButtons(
                 inputId = "missingOpt",
                 label = "Missing Data for Endogenous Variables",
                 choices = list("List-wise Deletion" = "listwise",
                                "Full Information" = "FIML"),
                 selected = "FIML"
               ),
               tipify(bsButton(inputId = "help2", 
                               label = shiny::icon("circle-question", 
                                                   verify_fa = FALSE),
                               style = "inverse", size = "small"),
                      title = strwrap(paste("Listwise deletion results in any 
                                            individual with any missing data 
                                            being removed from the analysis. 
                                            Full Information Maximum Likelihood 
                                            (FIML) allows for missing data in 
                                            endogenous (i.e., outcome) variables 
                                            ."),
                                      width=1e10, simplify = TRUE),
                      placement = "right",
                      trigger = "hover")
        ),
        column(width = 4,
               radioButtons(
                 inputId = "fixedXOpt",
                 label = "Missing Data for Exogenous Variables",
                 choices = list("No Missing Data on X variable" = TRUE,
                                "Missing Data Allowed on X variable" = FALSE),
                 selected = TRUE
               ),
               tipify(bsButton(inputId = "help3", 
                               label = shiny::icon("circle-question", 
                                                   verify_fa = FALSE),
                               style = "inverse", size = "small"),
                      title = strwrap(paste("Most models do not make 
                                            distributional assumptions 
                                            (e.g., continuity, normality) for 
                                            exogenous (i.e., predictor) variables, 
                                            however, this means that the model 
                                            also cannot accomodate missing data 
                                            on these variables. If we choose to 
                                            invoke these assumptions, we can 
                                            allow for missing data. Likely not 
                                            appropriate for discrete predictors 
                                            (e.g., binary, count, ordinal)."),                                          
                                      width=1e10, simplify = TRUE),
                      placement = "right",
                      trigger = "hover")
        )
      ),
      shiny::br(),
      shiny::br(),
      fluidRow(
        column(width = 4,
               radioButtons(
                 inputId = "seOpt",
                 label = "Standard Errors",
                 choices = list("Standard SEs" = "standard",
                                "Robust SEs" = "robust",
                                "Boostrap SEs" = "bootstrap"),
                 selected = "standard"
               ),
               uiOutput("BootstrapSamples"),
               tags$head(tags$style("#BootstrapWarning{color: black;
                                 font-size: 15px;
                                 font-style: italic;
                                 }")),
               textOutput("BootstrapWarning")
        ),
        column(width = 4,
               radioButtons(
                 inputId = "stdOpt",
                 label = "Standardized Effects",
                 choices = list("None" = "None",
                                "Standardize" = "std",
                                "Standardize All Except X" = "std.nox"),
                 selected = "None"
               )
        )
      ),
    )
  })
  
  ## Field for Manual Specification of Number of Bootstrap Samples
  output$BootstrapSamples <- renderUI({
    if (input$seOpt != "bootstrap") return(NULL)
    numericInput(inputId = "userBootsamples",
                 label = "Number of Bootstrap Samples",
                 value = 1000, step = 500)
  })
  output$BootstrapWarning <- renderText({
    if (input$seOpt != "bootstrap") return(NULL)
    paste0("Be aware that increasing the number of samples will 
           increase analysis run time.")
  })
  
  ## Button to Advance to Analysis Results
  output$AnalyzeButton <- renderUI({
    if(!analysisOpts$flag) return(NULL)
    actionButton(inputId = "RunAnalyses", label = "Analyze",
                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
  })
  observeEvent(input$RunAnalyses, {
    updateTabItems(inputId = "tabs", selected = "individualitem")
  })
  
  
  #-----------------------------------------------------------------------------  
  # Page 3: Individual Item Mediation Results
  #-----------------------------------------------------------------------------
  
  ## Individual Item Mediation Functions
  ItemFits <- eventReactive(input$RunAnalyses, {
    req(MediationData())
    source("item-mediation.R")
    
    itemMediation(data = MediationData(),
                  idvar = input$`id-dropdown`,
                  xvar = xName(), yvar = yName(),
                  itemvars = itemNames(), compvar = compName(),
                  estimator = input$estimatorOpt,
                  missing = input$missingOpt,
                  fixed.x = as.logical(input$fixedXOpt),
                  se = input$seOpt,
                  bootstrap = input$userBootsamples)
  })
  paramTabs <- reactive({
    req(ItemFits())
    tableParameters(ItemFits())
  })
  Effplot <- reactive({
    req(paramTabs())
    plotItemMediation(paramTabs(), compvar = compName())
  })
  compResults <- reactive({
    lavaan::summary(ItemFits()[[compName()]],
                    standardize = ifelse(is.null(input$stdOpt),
                                         FALSE,
                                         ifelse(input$stdOpt != "None",
                                                TRUE,
                                                FALSE)),
                    std.nox = (input$stdOpt == "std.nox"),
                    rsquare = TRUE)
  })
  
  ## Display Plots and Lavaan Output (+ Download Buttons)
  output$FitData <- renderUI({
    if(is.null(ItemFits())) return(NULL)
    
    tabItem(
      shiny::br(),
      fluidRow(box(title = h3(tags$b("Effects Plot")), width = NULL, status = "primary",
                   collapsible = TRUE, collapsed = FALSE,
                   div(style = "overflow-x: scroll", 
                       uiOutput("EffDistribution")))),
      shiny::br(),
      shiny::br(),
      fluidRow(
        box(
          title = h3(tags$b("Proportional Constraint Test")), width = NULL, status = "primary",
          collapsible = TRUE, collapsed = FALSE,
          div(style = "overflow-x: scroll", 
              uiOutput("PropTestResults"))
        )
      ),
      shiny::br(),
      shiny::br(),
      fluidRow(
        box(
          title = h3(tags$b("Composite Mediator Results")), width = NULL, status = "primary",
          collapsible = TRUE, collapsed = FALSE,
          div(style = "overflow-x: scroll", 
              uiOutput("CompositeMResults"))
        )
      ),
      shiny::br(),
      fluidRow(
          box(
            title = h3(tags$b("Individual Item Results")), width = NULL, status = "primary",
            collapsible = TRUE, collapsed = FALSE,
            div(style = "overflow-x: scroll", 
                uiOutput("indivItemOut")
          )
        )
      )
    )
    
  })
  
    ### Effect Distribution Plot
    output$EffDistribution <- renderUI({
      fixedRow(
        column(
          width = 10,
          plotOutput("EffDistPlot"),
          shiny::br(),
          shiny::br(),
          downloadButton(outputId = "DLEffPlot", 
                         label = "Download Effect Plot",
                         class = "download"),
          tags$head(tags$style(
            ".download{color: #fff; background-color: #337ab7; border-color: #2e6da4}")),
          shiny::br(),
          shiny::br()
        )
      )
    })
    output$EffDistPlot <- renderPlot(width = 1200, height = 400, {
      req(paramTabs())
      Effplot()
    })
    output$DLEffPlot <- downloadHandler(
      filename = paste0("MediationEffectPlot-", nickname(), ".png"),
      content = function(file){
        ggplot2::ggsave(filename = file,
                        plot = Effplot(),
                        width = 17,
                        height = 5,
                        scale = 1.25,
                        dpi = 'retina')}
    )
    
    ### Proportionality Model Comparison Test
    output$PropTestResults <- renderUI({
      fluidPage(
        fluidRow(
          column(
            width = 8,
            verbatimTextOutput("propLRT")
          )
        ),
        fluidRow(
          column(
            width = 8,
            verbatimTextOutput("propFit")
          ),
          column(
            width = 4,
            selectInput(inputId = "selectSpec", 
                        label = "Select Specification:",
                        choices = c("Freely-Estimated" = "free.fit",
                                    "Constrained" = "constrain.fit"),
                        selected = "free"),
            shiny::br(),
            shiny::br(),
            downloadButton(outputId = "DLSpec", 
                           label = "Download Proportional Test Results",
                           class = "download")
          )
        )
      )
    })
    
    propTest <- reactive({
      
      proportionalTest(data = MediationData(),
                       idvar = input$`id-dropdown`,
                       xvar = xName(), yvar = yName(),
                       itemvars = itemNames(), compvar = compName(),
                       estimator = input$estimatorOpt,
                       missing = input$missingOpt,
                       fixed.x = as.logical(input$fixedXOpt),
                       se = input$seOpt,
                       bootstrap = input$userBootsamples)
      
    })
    
    output$propLRT <- renderPrint(width = 1000, {
      
      propTest()$LRT
      
    })
    
    output$propFit <- renderPrint(width = 1000, {
      
      lavaan::summary(propTest()[[input$selectSpec]],
                      standardize = ifelse(is.null(input$stdOpt),
                                           FALSE,
                                           ifelse(input$stdOpt != "None",
                                                  TRUE,
                                                  FALSE)),
                      std.nox = (input$stdOpt == "std.nox"),
                      rsquare = TRUE)
      
    })
    
    output$DLSpec <- downloadHandler(
      filename = function(){paste0("ProportionalConstraintTest-", 
                                   nickname(), 
                                   ".zip")},
      content = function(zipname){
        fs <- c()
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        
        fs = c(paste0("ProportionalConstraintTest-FreelyEstimated-", nickname(),".csv"),
               paste0("ProportionalConstraintTest-FreelyEstimated-", nickname(),".txt"),
               paste0("ProportionalConstraintTest-Constrained-", nickname(),".csv"),
               paste0("ProportionalConstraintTest-Constrained-", nickname(),".txt"),
               paste0("ProportionalConstraintTest-LRT-", nickname(),".txt"))
        
        write.csv(lavaan::summary(propTest()[["free.fit"]],
                                  standardize = ifelse(is.null(input$stdOpt),
                                                       FALSE,
                                                       ifelse(input$stdOpt != "None",
                                                              TRUE,
                                                              FALSE)),
                                  std.nox = (input$stdOpt == "std.nox"),
                                  rsquare = TRUE), 
                  file = fs[1], 
                  row.names=FALSE)
        capture.output(lavaan::summary(propTest()[["free.fit"]],
                                       standardize = ifelse(is.null(input$stdOpt),
                                                            FALSE,
                                                            ifelse(input$stdOpt != "None",
                                                                   TRUE,
                                                                   FALSE)),
                                       std.nox = (input$stdOpt == "std.nox"),
                                       rsquare = TRUE),
                       file = fs[2],
                       type = "output")
        write.csv(lavaan::summary(propTest()[["constrain.fit"]],
                                  standardize = ifelse(is.null(input$stdOpt),
                                                       FALSE,
                                                       ifelse(input$stdOpt != "None",
                                                              TRUE,
                                                              FALSE)),
                                  std.nox = (input$stdOpt == "std.nox"),
                                  rsquare = TRUE), 
                  file = fs[3], 
                  row.names=FALSE)
        capture.output(lavaan::summary(propTest()[["constrain.fit"]],
                                       standardize = ifelse(is.null(input$stdOpt),
                                                            FALSE,
                                                            ifelse(input$stdOpt != "None",
                                                                   TRUE,
                                                                   FALSE)),
                                       std.nox = (input$stdOpt == "std.nox"),
                                       rsquare = TRUE),
                       file = fs[4],
                       type = "output")
        capture.output(propTest()[["LRT"]],
                       file = fs[5],
                       type = "output")
        
        
        zip::zip(zipfile=zipname, files=fs)
      },
      contentType = "application/zip"
    )
    
    output$DLMplusDat <- downloadHandler(
      filename = paste0("mplusData-", nickname(), ".dat"),
      content = function(file){
        MplusAutomation::prepareMplusData(df = MediationData(),
                                          filename = file,
                                          inpfile = FALSE)
      }
    )
    
    ### Composite Mediation Model Output
    output$CompositeMResults <- renderUI({
      fixedRow(
        column(width = 10,
          verbatimTextOutput("CompositeMFit"),
          shiny::br(),
          shiny::br(),
          downloadButton(outputId = "DLCompositeMFit", 
                         label = "Download Composite Results",
                         class = "download"),
          shiny::br(),
          shiny::br()
        )
      )
    })
    output$CompositeMFit <- renderPrint(width = 1000, {
      compResults()
    })
    output$DLCompositeMFit <- downloadHandler(
      filename = function(){
        paste0(compName(), 
               "-CompositeResults-",
               nickname(),
               ".zip")
      },
      content = function(zipname){
        fs <- c(paste0(compName(), 
                       "-CompositeResults-", 
                       nickname(), 
                       ".csv"),
                paste0(compName(), 
                       "-CompositeResults-", 
                       nickname(),
                       ".txt")
                )
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        
        write.csv(compResults(), file = fs[1],
                  row.names=FALSE)
        capture.output(lavaan::summary(ItemFits()[[compName()]],
                            standardize = ifelse(is.null(input$stdOpt),
                                                 FALSE,
                                                 ifelse(input$stdOpt != "None",
                                                        TRUE,
                                                        FALSE)),
                            std.nox = (input$stdOpt == "std.nox"),
                            rsquare = TRUE), 
            file = fs[2], 
            type = "output")
        
        zip::zip(zipfile=zipname, files=fs)
      },
      contentType = "application/zip"
    )
    
    ### Individual Item Mediation Model Output
    output$indivItemOut <- renderUI({
      fluidRow(
        column(
          width = 8,
          verbatimTextOutput("ItemMFit")
        ),
        column(
          width = 4,
          selectInput(inputId = "indivItem", 
                      label = "Select Item:",
                      itemNames()),
          shiny::br(),
          shiny::br(),
          downloadButton(outputId = "DLIndivMFit", 
                         label = "Download All Individual Item Results",
                         class = "download")
        )
      )
    })
    output$ItemMFit <- renderPrint(width = 1000, {
      
      lavaan::summary(ItemFits()[[input$indivItem]],
                      standardize = ifelse(is.null(input$stdOpt),
                                           FALSE,
                                           ifelse(input$stdOpt != "None",
                                                  TRUE,
                                                  FALSE)),
                      std.nox = (input$stdOpt == "std.nox"),
                      rsquare = TRUE)
      
    })
    output$DLIndivMFit <- downloadHandler(
      filename = paste0("ItemMediatorResults-", nickname(), ".zip"),
      content = function(zipname){
        fs <- c()
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        for (name in itemNames()){
          fs <- c(fs, 
                  paste0(name, "-ItemMediatorResults-", nickname(),".csv"))
          write.csv(lavaan::summary(ItemFits()[[name]],
                                    standardize = ifelse(is.null(input$stdOpt),
                                                         FALSE,
                                                         ifelse(input$stdOpt != "None",
                                                                TRUE,
                                                                FALSE)),
                                    std.nox = (input$stdOpt == "std.nox"),
                                    rsquare = TRUE),
                    file = paste0(name,
                                  "-ItemMediatorResults-",
                                  nickname(),
                                  ".csv"),
                    row.names=FALSE)
          fs <- c(fs, 
                  paste0(name, "-ItemMediatorResults-", nickname(), ".txt"))
          capture.output(lavaan::summary(ItemFits()[[name]],
                                         standardize = ifelse(is.null(input$stdOpt),
                                                              FALSE,
                                                              ifelse(input$stdOpt != "None",
                                                                     TRUE,
                                                                     FALSE)),
                                         std.nox = (input$stdOpt == "std.nox"),
                                         rsquare = TRUE),
                         file = paste0(name,
                                       "-ItemMediatorResults-",
                                       nickname(),
                                       ".txt"), 
                         type = "output")
        }
        zip::zip(zipfile=zipname, files=fs)
      },
      contentType = "application/zip"
    )
  
  #-----------------------------------------------------------------------------
  # Page 4: Latent Variable Mediation Results
  #-----------------------------------------------------------------------------
  ## Latent Variable Syntax Functions
  lvCompSyntax <- eventReactive(input$RunAnalyses, {
    req(MediationData())
    source("lv-mediation.R")
    
    generateSyntax(xvar = xName(), yvar = yName(),
                   itemvars = itemNames(), 
                   compvar = compName(),
                   compType = input$compSelect,
                   type = "composite",
                   estimator = input$estimatorOpt,
                   fixed.x = as.logical(input$fixedXOpt),
                   se = input$seOpt,
                   bootstrap = input$userBootsamples)
  })
  lvFullSyntax <- eventReactive(input$RunAnalyses, {
    req(MediationData())
    
    generateSyntax(xvar = xName(), yvar = yName(),
                   itemvars = itemNames(), 
                   compvar = compName(),
                   compType = input$compSelect,
                   type = "latentVar",
                   estimator = input$estimatorOpt,
                   fixed.x = as.logical(input$fixedXOpt),
                   se = input$seOpt,
                   bootstrap = input$userBootsamples)
    
  })
  
  ## Latent Variable Mediation Functions
  LVCompFits <- eventReactive(input$RunAnalyses, {
    if (grepl("Warning:", lvCompSyntax()$lavaan_syntax)){
      return(lvCompSyntax()$lavaan_syntax)
    }
    req(lvCompSyntax())
    
    lvMediation(model = lvCompSyntax()$lavaan_syntax,
                data = MediationData(),
                idvar = input$`id-dropdown`,
                xvar = xName(), yvar = yName(),
                itemvars = itemNames(), compvar = compName(),
                estimator = input$estimatorOpt,
                missing = "FIML",
                fixed.x = as.logical(input$fixedXOpt),
                se = input$seOpt,
                bootstrap = input$userBootsamples)
    
  })
  LVFullFits <- eventReactive(input$RunAnalyses, {
    if (grepl("Warning:", lvFullSyntax()$lavaan_syntax)){
      return(lvFullSyntax()$lavaan_syntax)
    }
    req(lvFullSyntax())
    
    lvMediation(model = lvFullSyntax()$lavaan_syntax,
                data = MediationData(),
                idvar = input$`id-dropdown`,
                xvar = xName(), yvar = yName(),
                itemvars = itemNames(), compvar = compName(),
                estimator = input$estimatorOpt,
                missing = "FIML",
                fixed.x = as.logical(input$fixedXOpt),
                se = input$seOpt,
                bootstrap = input$userBootsamples)
    
  })
  
  ## Display Plots and Lavaan/Mplus Output (+ Download Buttons)
  output$LatentModels <- renderUI({
    if(is.null(LVCompFits()) | is.null(LVFullFits())) return(NULL)
    
    tabItem(
      shiny::br(),
      fluidRow(
               box(
                 title = h3(tags$b("Model Syntax")), width = NULL, 
                 status = "primary", collapsible = TRUE, collapsed = FALSE,
                 div(style = "overflow-x: hidden", 
                     uiOutput("ModelSyntax"))
               )
      ),
      shiny::br(),
      shiny::br(),
      fluidRow(
        box(
          title = h3(tags$b("Latent Variable Composite Model Results")), 
          width = NULL, 
          status = "primary", collapsible = TRUE, collapsed = FALSE,
          div(style = "overflow-x: hidden", 
              uiOutput("LVCompResults"))
        )
      ),
      shiny::br(),
      shiny::br(),
      fluidRow(
        box(
          title = h3(tags$b("Full Latent Variable Model Results")), 
          width = NULL, 
          status = "primary", collapsible = TRUE, collapsed = FALSE,
          div(style = "overflow-x: hidden", 
              uiOutput("FullLVResults"))
        )
      )
    )
  })
  
    ### Model Syntax
    output$ModelSyntax <- renderUI({
      fluidRow(
        column(width = 8,
          verbatimTextOutput("ModelSyntaxText")
        ),
        column(width = 4,
          selectInput(inputId = "syntaxprogram", 
                      label = "Select Program:",
                      c("lavaan", "Mplus"),
                      selected = "lavaan"),
          textOutput("abbrWarning"),
          tags$head(tags$style("#abbrWarning{font-style: italic;}")),
          shiny::br(),
          shiny::br(),
          uiOutput("LVtypeSyntaxOpt"),
          shiny::br(),
          shiny::br(),
          uiOutput("mplusSyntaxOpt"),
          shiny::br(),
          shiny::br(),
          downloadButton(outputId = "DLSyntax", 
                         label = "Download All Syntax Files",
                         class = "download"),
          shiny::br(),
          shiny::br(),
          downloadButton(outputId = "DLMplusDat",
                         label = "Download Mplus-compatible Data File",
                         class = "download")
        )
      )
    })
    output$LVtypeSyntaxOpt <- renderUI({
      selectInput(inputId = "lvTypesyntaxOpt",
                  label = "Select LV Specification:",
                  c("Composite Latent Variable", "Full Latent Variable"),
                  selected = "Composite Latent Variable")
    })
    output$mplusSyntaxOpt <- renderUI({
      if (input$syntaxprogram != "Mplus") return(NULL)
      
      selectInput(inputId = "mplussyntaxOpt",
                  label = "Select Mplus Option:",
                  c("Modification Indices", "Indirect Effect"),
                  selected = "Modification Indices")
    })
    output$abbrWarning <- renderText({
      if (input$syntaxprogram != "Mplus") return(NULL)
      paste0("Note: Names may be abbreviated to be compatible with Mplus settings. 
             Be aware of per-line character limits. You may need to adjust 
             syntax before running to avoid runtime errors.")
    })
    output$ModelSyntaxText <- renderPrint(width = 10, {
      if(is.null(LVCompFits()) | is.null(LVFullFits())) return(NULL)
      if (input$lvTypesyntaxOpt == "Composite Latent Variable"){
        if (input$syntaxprogram == "lavaan"){
          cat(lvCompSyntax()$lavaan_syntax)
        } else if (input$syntaxprogram == "Mplus"){
          if (!is.null(input$mplussyntaxOpt) & input$mplussyntaxOpt == "Modification Indices"){
            cat(lvCompSyntax()$mplus_syntax$modifications)
          } else if (!is.null(input$mplussyntaxOpt) & input$mplussyntaxOpt == "Indirect Effect"){
            cat(lvCompSyntax()$mplus_syntax$indirect) 
          }
        }
      } else if (input$lvTypesyntaxOpt == "Full Latent Variable"){
        if (input$syntaxprogram == "lavaan"){
          cat(lvFullSyntax()$lavaan_syntax)
        } 
        if (input$syntaxprogram == "Mplus"){
          if (input$mplussyntaxOpt == "Modification Indices"){
            cat(lvFullSyntax()$mplus_syntax$modifications)
          }
          if (input$mplussyntaxOpt == "Indirect Effect"){
            cat(lvFullSyntax()$mplus_syntax$indirect) 
          }
        }
      }
      
    })
    output$DLSyntax <- downloadHandler(
      filename = function(){paste0("LatentVariableModelSyntax-", 
                                   nickname(), 
                                   ".zip")},
      content = function(zipname){
        fs <- c()
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        
        fs = c("LVComposite-lavaan-syntax.txt", 
               "LVComposite-mplus-mod-syntax.inp",
               "LVComposite-mplus-indirect-syntax.inp",
               "LVFull-lavaan-syntax.txt", 
               "LVFull-mplus-mod-syntax.inp",
               "LVFull-mplus-indirect-syntax.inp")
        
        cat(lvCompSyntax()$lavaan_syntax,
            file = fs[1])
        cat(lvCompSyntax()$mplus_syntax$modifications,
            file = fs[2])
        cat(lvCompSyntax()$mplus_syntax$indirect,
            file = fs[3])
        cat(lvFullSyntax()$lavaan_syntax,
            file = fs[4])
        cat(lvFullSyntax()$mplus_syntax$modifications,
            file = fs[5])
        cat(lvFullSyntax()$mplus_syntax$indirect,
            file = fs[6])
        
        zip::zip(zipfile=zipname, files=fs)
      },
      contentType = "application/zip"
    )
    
    output$DLMplusDat <- downloadHandler(
      filename = paste0("mplusData-", nickname(), ".dat"),
      content = function(file){
        MplusAutomation::prepareMplusData(df = MediationData(),
                                          filename = file,
                                          inpfile = FALSE)
      }
    )
    
    ### Latent Variable Composite Model Results
    output$LVCompResults <- renderUI({
      fluidRow(
        column(width = 8,
               verbatimTextOutput("LVCompMFit")),
        column(width = 4,
               downloadButton(outputId = "DLLVCompMFit", 
                              label = "Download Latent Variable Composite Model Results",
                              class = "download"))
      )
    })
    output$LVCompMFit <- renderPrint(width = 1000, {
      if (grepl("Warning:", lvCompSyntax()$lavaan_syntax)){
        return(cat(lvCompSyntax()$lavaan_syntax))
      }
      lavaan::summary(LVCompFits(),
                      standardize = ifelse(is.null(input$stdOpt),
                                           FALSE,
                                           ifelse(input$stdOpt != "None",
                                                  TRUE,
                                                  FALSE)),
                      std.nox = (input$stdOpt == "std.nox"),
                      rsquare = TRUE)
    })
    output$DLLVCompMFit <- downloadHandler(
      filename = paste0("CompositeLatentVariableResults-", nickname(), ".zip"),
      content = function(zipname){
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        if (grepl("Warning:", lvCompSyntax()$lavaan_syntax)){
          fs <- paste0("Warning-", nickname(),".txt")
          capture.output(print(lvCompSyntax()$lavaan_syntax),
                         file = fs,
                         type = "output")
        } else {
          fs <- c(
            paste0("CompositeLatentVariableResults-", nickname(), ".csv"),
            paste0("CompositeLatentVariableResults-", nickname(), ".txt")
          )
          
          write.csv(lavaan::summary(LVCompFits(),
                                    standardize = ifelse(is.null(input$stdOpt),
                                                         FALSE,
                                                         ifelse(input$stdOpt != "None",
                                                                TRUE,
                                                                FALSE)),
                                    std.nox = (input$stdOpt == "std.nox"),
                                    rsquare = TRUE), 
                    file = fs[1], 
                    row.names=FALSE)
          capture.output(lavaan::summary(LVCompFits(),
                                         standardize = ifelse(is.null(input$stdOpt),
                                                              FALSE,
                                                              ifelse(input$stdOpt != "None",
                                                                     TRUE,
                                                                     FALSE)),
                                         std.nox = (input$stdOpt == "std.nox"),
                                         rsquare = TRUE),
                         file = fs[2],
                         type = "output")
        }
        zip::zip(zipfile=zipname, files=fs)
      },
      contentType = "application/zip"
    )
    
    
    ### Full Latent Variable Model Results
    output$FullLVResults <- renderUI({
      fluidRow(
        column(width = 8,
               verbatimTextOutput("LVFullMFit")),
        column(width = 4,
               downloadButton(outputId = "DLLVFullMFit", 
                              label = "Download Full Latent Variable Model Results",
                              class = "download"))
      )
    })
    output$LVFullMFit <- renderPrint(width = 1000, {
      if (grepl("Warning:", lvFullSyntax()$lavaan_syntax)){
        return(cat(lvFullSyntax()$lavaan_syntax))
      }
      lavaan::summary(LVFullFits(),
                      standardize = ifelse(is.null(input$stdOpt),
                                           FALSE,
                                           ifelse(input$stdOpt != "None",
                                                  TRUE,
                                                  FALSE)),
                      std.nox = (input$stdOpt == "std.nox"),
                      rsquare = TRUE)
    })
    output$DLLVFullMFit <- downloadHandler(
      filename = paste0("FullLatentVariableResults-", nickname(), ".zip"),
      content = function(zipname){
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        
        if (grepl("Warning:", lvFullSyntax()$lavaan_syntax)){
          fs <- paste0("Warning-", nickname(),".txt")
          capture.output(print(lvFullSyntax()$lavaan_syntax),
                         file = fs,
                         type = "output")
        } else {
          fs <- c(
            paste0("FullLatentVariableResults-", nickname(), ".csv"),
            paste0("FullLatentVariableResults-", nickname(), ".txt")
          )
          write.csv(lavaan::summary(LVFullFits(),
                                    standardize = ifelse(is.null(input$stdOpt),
                                                         FALSE,
                                                         ifelse(input$stdOpt != "None",
                                                                TRUE,
                                                                FALSE)),
                                    std.nox = (input$stdOpt == "std.nox"),
                                    rsquare = TRUE), 
                    file = fs[1], 
                    row.names=FALSE)
          capture.output(lavaan::summary(LVFullFits(),
                                         standardize = ifelse(is.null(input$stdOpt),
                                                              FALSE,
                                                              ifelse(input$stdOpt != "None",
                                                                     TRUE,
                                                                     FALSE)),
                                         std.nox = (input$stdOpt == "std.nox"),
                                         rsquare = TRUE),
                         file = fs[2],
                         type = "output")
        }
        zip::zip(zipfile=zipname, files=fs)
      },
      contentType = "application/zip"
    )
    
  #-----------------------------------------------------------------------------
  # Page 5: Latent Variable Diagnostics
  #-----------------------------------------------------------------------------
  ## Generate MI Tables
  lvCompMIs <- eventReactive(input$RunAnalyses, {
    if (grepl("Warning:", lvCompSyntax()$lavaan_syntax) |
        grepl("Warning:", lvFullSyntax()$lavaan_syntax)) return(NULL)
    req(MediationData())
    source("lv-diagnostics.R")
    
    tableMIs(fitObj = LVCompFits(),
             standardize = (input$stdOpt == "std" | input$stdOpt == "std.nox"), 
             std.nox = (input$stdOpt == "std.nox"),
             pretty = TRUE)
  })
  lvFullMIs <- eventReactive(input$RunAnalyses, {
    if (grepl("Warning:", lvCompSyntax()$lavaan_syntax) |
        grepl("Warning:", lvFullSyntax()$lavaan_syntax)) return(NULL)
    req(MediationData())
    source("lv-diagnostics.R")
    
    tableMIs(fitObj = LVFullFits(),
             standardize = (input$stdOpt == "std" | input$stdOpt == "std.nox"), 
             std.nox = (input$stdOpt == "std.nox"),
             pretty = TRUE)
  })
  
  ## Display MI Tables
  output$LatentDiagnostics <- renderUI({
    if(is.null(lvCompMIs()) | is.null(lvCompMIs())) return(NULL)
    if (grepl("Warning:", lvCompSyntax()$lavaan_syntax) |
        grepl("Warning:", lvFullSyntax()$lavaan_syntax)) return(NULL)
    
    tabItem(
      shiny::br(),
      fluidRow(
        box(
          title = h3(tags$b("Proportional Test Modification Indices")), width = NULL,
          status = "primary", collapsible = TRUE, collapsed = FALSE,
          div(style = "overflow-x: hidden", uiOutput("propMIs"))
        )
      ),
      shiny::br(),
      shiny::br(),
      fluidRow(
        box(
          title = h3(tags$b("Composite Model Modification Indices")), width = NULL,
          status = "primary", collapsible = TRUE, collapsed = FALSE,
          div(style = "overflow-x: hidden", uiOutput("compMIs"))
        )
      ),
      shiny::br(),
      shiny::br(),
      fluidRow(
        box(
          title = h3(tags$b("Full Latent Variable Model Modification Indices")), 
          width = NULL, 
          status = "primary", collapsible = TRUE, collapsed = FALSE,
          div(style = "overflow-x: hidden", uiOutput("fullMIs"))
        )
      )
    )
  })
  
  output$propMIs <- renderUI({
    fixedRow(
      column(width = 10,
             DT::DTOutput("printPropMI"),
             shiny::br(),
             shiny::br(),
             downloadButton(outputId = "DLPropMI", 
                            label = "Download Proportional Test MIs",
                            class = "download"),
             shiny::br(),
             shiny::br()
      )
    )
  })
  
  output$printPropMI <- DT::renderDataTable({
    DT::datatable(propTest()$MIs, filter = "top")
  }, server = TRUE)
  
  output$DLPropMI <- downloadHandler(
    filename = paste0("ProportionalTestMIs-", nickname(), ".csv"),
    content = function(file){
      write.csv(propTest()$MIs,
                file = file,
                row.names = FALSE)
    }
  )
  
  output$compMIs <- renderUI({
    fluidPage(
      fluidRow(
        column(width = 8,
               textOutput("MINotes1"),
               tags$head(tags$style("#MINotes1{font-style: italic;}")),
               shiny::br(),
               shiny::br(),
               plotOutput("comptidySEMinteractive"),
               shiny::br(),
               shiny::br()
        ),
        column(width = 4,
               radioButtons(inputId = "compMIfilter",
                            label = "Filter MIs",
                            choices = c("Potential Item-level Mediations" = "itemMed", 
                                        "All" = "all"),
                            selected = "itemMed",
                            inline = TRUE),
               shiny::br(),
               shiny::br(),
               DT::DTOutput("compMIDT"),
               shiny::br(),
               shiny::br()
        )
      ),
      fluidRow(
        column(width = 8,
               downloadButton(outputId = "DLcompMIplot", 
                              label = "Download Composite MI Plot",
                              class = "download")
               ),
        column(width = 4,
               downloadButton(outputId = "DLcompMItable", 
                              label = "Download Composite MI Table",
                              class = "download")
               )
      )
    )
    
  })
  
  output$MINotes1 <- renderText({
    paste0("Note: Modification indices indicate the predicted improvement in model 
           fit if a path was included in the model. When selecting \"All\", the 
           displayed paths may violate the reasonable causal structure of the model. 
           When selecting \"Potential Item-Level Mediators\", only paths which 
           point in the \"proper\" (i.e., X -> M -> Y) direction are shown. 
           While the table includes all potential MIs, the paths displayed here 
           are restricted to those with values > 5.00.")
  })
  
  output$compMIDT <- DT::renderDataTable({
    if (input$compMIfilter == "itemMed"){
      DT::datatable(lvCompMIs()[
        grepl(paste0(" ~ ", xName()), lvCompMIs()$Parameter) |
          grepl(paste0(yName(), " ~ "), lvCompMIs()$Parameter),
      ], filter = "top")
    } else if (input$compMIfilter == "all"){
      DT::datatable(lvCompMIs(), filter = "top")
    }
  }, server = TRUE)
  
  comptidyPlot <- reactive({
    plotTidyMIs(fitObj = LVCompFits(),
                xName = xName(),
                yName = yName(),
                mName = compName(), 
                itemNames = itemNames(),
                nItems = length(itemNames()),
                filter = input$compMIfilter)
  })
  
  output$comptidySEMinteractive <- renderPlot(height = 500, {
    plot(comptidyPlot())
  })
  
  output$DLcompMIplot <- downloadHandler(
    filename = paste0("CompositeMIPlot-", nickname(), ".png"),
    content = function(file){
      ggplot2::ggsave(
        filename = file,
        plot = ggplotify::as.ggplot(plot(comptidyPlot())) + 
          ggplot2::theme(
            panel.background = ggplot2::element_rect(
              fill = "white", color = "white"),
            panel.border = ggplot2::element_rect(
              color = "white")),
        width = 10,
        height = 5,
        scale = 1.25,
        dpi = 'retina')}
  )
  
  output$DLcompMItable <- downloadHandler(
    filename = paste0("CompositeMITable-", nickname(), ".csv"),
    content = function(file){
      write.csv(lvCompMIs(),
                file = file,
                row.names = FALSE)
    }
  )
  
  
  output$fullMIs <- renderUI({
    fluidPage(
      fluidRow(
        column(width = 8,
               textOutput("MINotes2"),
               tags$head(tags$style("#MINotes2{font-style: italic;}")),
               shiny::br(),
               shiny::br(),
               plotOutput("fulltidySEMinteractive"),
               shiny::br(),
               shiny::br()
        ),
        column(width = 4,
               radioButtons(inputId = "fullMIfilter",
                            label = h4("Filter MIs"),
                            choices = c("Potential Item-level Mediations" = "itemMed", 
                                        "All" = "all"),
                            selected = "itemMed",
                            inline = TRUE),
               shiny::br(),
               shiny::br(),
               DT::DTOutput("fullMIDT"),
               shiny::br(),
               shiny::br()
        )
      ),
      fluidRow(
        column(width = 8,
               downloadButton(outputId = "DLfullMIplot", 
                              label = "Download Full Latent Variable MI Plot",
                              class = "download")),
        column(width = 4,
               downloadButton(outputId = "DLfullMItable", 
                              label = "Download Full Latent Variable MI Table",
                              class = "download"))
      )
    )
    
  })
  
  output$MINotes2 <- renderText({
    paste0("Note: Modification indices indicate the predicted improvement in model 
           fit if a path was included in the model. When selecting \"All\", the 
           displayed paths may violate the reasonable causal structure of the model. 
           When selecting \"Potential Item-Level Mediators\", only paths which 
           point in the \"proper\" (i.e., X -> M -> Y) direction are shown. 
           While the table includes all potential MIs, the paths displayed here 
           are restricted to those with values > 5.00.")
  })
  
  output$fullMIDT <- DT::renderDataTable({
    if (input$fullMIfilter == "itemMed"){
      DT::datatable(lvFullMIs()[
        grepl(paste0(" ~ ", xName()), lvFullMIs()$Parameter) |
          grepl(paste0(yName(), " ~ "), lvFullMIs()$Parameter),
      ], filter = "top")
    } else if (input$fullMIfilter == "all"){
      DT::datatable(lvFullMIs(), filter = "top")
    }
  }, server = TRUE)
  
  fulltidyPlot <- reactive({
    plotTidyMIs(fitObj = LVFullFits(),
                xName = xName(),
                yName = yName(),
                mName = compName(), 
                itemNames = itemNames(),
                nItems = length(itemNames()),
                filter = input$fullMIfilter)
  })
  output$fulltidySEMinteractive <- renderPlot(height = 500, {
    plot(fulltidyPlot())
  })
  
  output$DLfullMIplot <- downloadHandler(
    filename = paste0("FullMIPlot-", nickname(), ".png"),
    content = function(file){
      ggplot2::ggsave(
        filename = file,
        plot = ggplotify::as.ggplot(plot(fulltidyPlot())) + 
          ggplot2::theme(
            panel.background = ggplot2::element_rect(
              fill = "white", color = "white"),
            panel.border = ggplot2::element_rect(
              color = "white")),
        width = 10,
        height = 5,
        scale = 1.25,
        dpi = 'retina')}
  )
  
  output$DLfullMItable <- downloadHandler(
    filename = paste0("FullMITable-", nickname(), ".csv"),
    content = function(file){
      write.csv(lvFullMIs(),
                file = file,
                row.names = FALSE)
    },
    contentType = "application/zip"
  )
  
  
  #-----------------------------------------------------------------------------
  # Page 6: Download All Outputs
  #-----------------------------------------------------------------------------
  
  output$DownloadOutputs <- renderUI({
    fluidPage(
      fluidRow(
        downloadButton(outputId = "DownloadAll",
                       label = "Download All Outputs",
                       class = "download")
      )
    )
  })
  
  output$DownloadAll <- downloadHandler(
    filename = paste0("SEMinterrupted-AllOutputs-", nickname(), ".zip"),
    content = function(zipname){
      owd <- setwd(tempdir())
      on.exit({
        unlink(zipdir, recursive = TRUE)
        setwd(owd)
        })
      
      # Create Parent Directory
      zipdir <- paste0("SEMinterrupted-AllOutputs-", nickname())
      dir.create(zipdir, showWarnings = FALSE)
      
      # Individual Item Output
      subdir <- file.path(zipdir, "Item-Mediator-Results"); dir.create(subdir)
      fname <- file.path(subdir, 
                         paste0("MediationEffectPlot-", nickname(), ".png"))
      ggplot2::ggsave(filename = fname,
                      plot = Effplot(),
                      width = 17,
                      height = 5,
                      scale = 1.25,
                      dpi = 'retina')
      
      subsubdir <- file.path(subdir, "Proportional-Constraints"); dir.create(subsubdir)
      fname <- file.path(subsubdir,
                         paste0(compName(), 
                                "ProportionalConstraintTest-FreelyEstimated-",
                                nickname(), 
                                ".csv"))
      write.csv(lavaan::summary(propTest()[["free.fit"]],
                                standardize = ifelse(is.null(input$stdOpt),
                                                     FALSE,
                                                     ifelse(input$stdOpt != "None",
                                                            TRUE,
                                                            FALSE)),
                                std.nox = (input$stdOpt == "std.nox"),
                                rsquare = TRUE), 
                file = fname, 
                row.names=FALSE)
      fname <- file.path(subsubdir,
                         paste0(compName(), 
                                "ProportionalConstraintTest-FreelyEstimated-",
                                nickname(), 
                                ".txt"))
      capture.output(lavaan::summary(propTest()[["free.fit"]],
                                     standardize = ifelse(is.null(input$stdOpt),
                                                          FALSE,
                                                          ifelse(input$stdOpt != "None",
                                                                 TRUE,
                                                                 FALSE)),
                                     std.nox = (input$stdOpt == "std.nox"),
                                     rsquare = TRUE),
                     file = fname,
                     type = "output")
      fname <- file.path(subsubdir,
                         paste0(compName(), 
                                "ProportionalConstraintTest-Constrained-",
                                nickname(), 
                                ".csv"))
      write.csv(lavaan::summary(propTest()[["constrain.fit"]],
                                standardize = ifelse(is.null(input$stdOpt),
                                                     FALSE,
                                                     ifelse(input$stdOpt != "None",
                                                            TRUE,
                                                            FALSE)),
                                std.nox = (input$stdOpt == "std.nox"),
                                rsquare = TRUE), 
                file = fname, 
                row.names=FALSE)
      fname <- file.path(subsubdir,
                         paste0(compName(), 
                                "ProportionalConstraintTest-Constrained-",
                                nickname(), 
                                ".txt"))
      capture.output(lavaan::summary(propTest()[["constrain.fit"]],
                                     standardize = ifelse(is.null(input$stdOpt),
                                                          FALSE,
                                                          ifelse(input$stdOpt != "None",
                                                                 TRUE,
                                                                 FALSE)),
                                     std.nox = (input$stdOpt == "std.nox"),
                                     rsquare = TRUE),
                     file = fname,
                     type = "output")
      fname <- file.path(subsubdir,
                         paste0(compName(), 
                                "ProportionalConstraintTest-LRT-",
                                nickname(), 
                                ".txt"))
      capture.output(propTest()[["LRT"]],
                     file = fname,
                     type = "output")
      
      
      subsubdir <- file.path(subdir, "Composite-Results"); dir.create(subsubdir)
      fname <- file.path(subsubdir,
                         paste0(compName(), "-CompositeResults-", nickname(), 
                                ".csv"))
      write.csv(compResults(), file = fname, row.names=FALSE)
      fname <- file.path(subsubdir,
                         paste0(compName(), "-CompositeResults-", nickname(), 
                                ".txt"))
      capture.output(lavaan::summary(ItemFits()[[compName()]],
                                     standardize = ifelse(is.null(input$stdOpt),
                                                          FALSE,
                                                          ifelse(input$stdOpt != "None",
                                                                 TRUE,
                                                                 FALSE)),
                                     std.nox = (input$stdOpt == "std.nox"),
                                     rsquare = TRUE), 
                     file = fname, 
                     type = "output")
      
      subsubdir <- file.path(subdir, "Item-Results"); dir.create(subsubdir)
      for (name in itemNames()){
        fname <- file.path(subsubdir, 
                           paste0(name, "-ItemMediatorResults-", nickname(),".csv"))
        write.csv(lavaan::summary(ItemFits()[[name]],
                                  standardize = ifelse(is.null(input$stdOpt),
                                                       FALSE,
                                                       ifelse(input$stdOpt != "None",
                                                              TRUE,
                                                              FALSE)),
                                  std.nox = (input$stdOpt == "std.nox"),
                                  rsquare = TRUE),
                  file = fname,
                  row.names=FALSE)
        fname <- file.path(subsubdir, 
                           paste0(name, "-ItemMediatorResults-", nickname(), ".txt"))
        capture.output(lavaan::summary(ItemFits()[[name]],
                                       standardize = ifelse(is.null(input$stdOpt),
                                                            FALSE,
                                                            ifelse(input$stdOpt != "None",
                                                                   TRUE,
                                                                   FALSE)),
                                       std.nox = (input$stdOpt == "std.nox"),
                                       rsquare = TRUE),
                       file = fname, 
                       type = "output")
      }
      
      # Latent Variable Model Output
      subdir <- file.path(zipdir, "Latent-Variable-Mediator-Results"); dir.create(subdir)
      subsubdir <- file.path(subdir, "Latent-Variable-Syntax"); dir.create(subsubdir)
      fnames <- file.path(subsubdir,
                          c("LVComposite-lavaan-syntax.txt", 
                            "LVComposite-mplus-mod-syntax.inp",
                            "LVComposite-mplus-indirect-syntax.inp",
                            "LVFull-lavaan-syntax.txt", 
                            "LVFull-mplus-mod-syntax.inp",
                            "LVFull-mplus-indirect-syntax.inp"))
      cat(lvCompSyntax()$lavaan_syntax,
          file = fnames[1])
      cat(lvCompSyntax()$mplus_syntax$modifications,
          file = fnames[2])
      cat(lvCompSyntax()$mplus_syntax$indirect,
          file = fnames[3])
      cat(lvFullSyntax()$lavaan_syntax,
          file = fnames[4])
      cat(lvFullSyntax()$mplus_syntax$modifications,
          file = fnames[5])
      cat(lvFullSyntax()$mplus_syntax$indirect,
          file = fnames[6])
      
      subsubdir <- file.path(subdir, "Mplus-Data"); dir.create(subsubdir)
      fname <- file.path(subsubdir,
                         paste0("mplusData-", nickname(), ".dat"))
      MplusAutomation::prepareMplusData(df = MediationData(),
                                        filename = fname,
                                        inpfile = FALSE)
      
      if (!grepl("Warning:", lvCompSyntax()$lavaan_syntax)){
        subsubdir <- file.path(subdir, "Composite-LV-Results"); dir.create(subsubdir)
        fnames <- file.path(subsubdir,
                            c(paste0("CompositeLatentVariableResults-", 
                                     nickname(), ".csv"),
                              paste0("CompositeLatentVariableResults-", 
                                     nickname(), ".txt")))
        write.csv(lavaan::summary(LVCompFits(),
                                  standardize = ifelse(is.null(input$stdOpt),
                                                       FALSE,
                                                       ifelse(input$stdOpt != "None",
                                                              TRUE,
                                                              FALSE)),
                                  std.nox = (input$stdOpt == "std.nox"),
                                  rsquare = TRUE), 
                  file = fnames[1], 
                  row.names=FALSE)
        capture.output(lavaan::summary(LVCompFits(),
                                       standardize = ifelse(is.null(input$stdOpt),
                                                            FALSE,
                                                            ifelse(input$stdOpt != "None",
                                                                   TRUE,
                                                                   FALSE)),
                                       std.nox = (input$stdOpt == "std.nox"),
                                       rsquare = TRUE),
                       file = fnames[2],
                       type = "output")
      }
      
      if (!grepl("Warning:", lvFullSyntax()$lavaan_syntax)){
        subsubdir <- file.path(subdir, "Full-LV-Results"); dir.create(subsubdir)
        fnames <- file.path(subsubdir,
                            c(paste0("FullLatentVariableResults-", 
                                     nickname(), ".csv"),
                              paste0("FullLatentVariableResults-", 
                                     nickname(), ".txt")))
        
        write.csv(lavaan::summary(LVFullFits(),
                                  standardize = ifelse(is.null(input$stdOpt),
                                                       FALSE,
                                                       ifelse(input$stdOpt != "None",
                                                              TRUE,
                                                              FALSE)),
                                  std.nox = (input$stdOpt == "std.nox"),
                                  rsquare = TRUE), 
                  file = fnames[1], 
                  row.names=FALSE)
        capture.output(lavaan::summary(LVFullFits(),
                                       standardize = ifelse(is.null(input$stdOpt),
                                                            FALSE,
                                                            ifelse(input$stdOpt != "None",
                                                                   TRUE,
                                                                   FALSE)),
                                       std.nox = (input$stdOpt == "std.nox"),
                                       rsquare = TRUE),
                       file = fnames[2],
                       type = "output")
      }
      
      # Latent Variable Diagnostic Output
      if (!grepl("Warning:", lvCompSyntax()$lavaan_syntax)){
        subdir <- file.path(zipdir, "Latent-Variable-Diagnostics"); dir.create(subdir)
        subsubdir <- file.path(subdir, "ProportionalTestMIs"); dir.create(subsubdir)
        fname <- file.path(subsubdir,
                           paste0("ProportionalTestMIs-", nickname(), ".csv"))
        write.csv(propTest()$MIs,
                  file = fname,
                  row.names = FALSE)
        
        subsubdir <- file.path(subdir, "Composite-LV-Diagnostics"); dir.create(subsubdir)
        fname <- file.path(subsubdir,
                           paste0("CompositeMIPlot-", nickname(), ".png"))
        ggplot2::ggsave(
          filename = fname,
          plot = ggplotify::as.ggplot(plot(comptidyPlot())) +
            ggplot2::theme(panel.background = ggplot2::element_rect(
                fill = "white", color = "white"),
              panel.border = ggplot2::element_rect(color = "white")),
          width = 10,
          height = 5,
          scale = 1.25,
          dpi = 'retina')
        
        fname <- file.path(subsubdir,
                           paste0("CompositeMITable-", nickname(), ".csv"))
        write.csv(lvCompMIs(),
                  file = fname,
                  row.names = FALSE)
      }
      
      if (!grepl("Warning:", lvFullSyntax()$lavaan_syntax)){
        subsubdir <- file.path(subdir, "Full-LV-Diagnostics"); dir.create(subsubdir)
        fname <- file.path(subsubdir,
                           paste0("FullMIPlot-", nickname(), ".png"))
        ggplot2::ggsave(
          filename = fname,
          plot = ggplotify::as.ggplot(plot(fulltidyPlot())) + 
            ggplot2::theme(panel.background = ggplot2::element_rect(
                fill = "white", color = "white"),
              panel.border = ggplot2::element_rect(color = "white")),
          width = 10,
          height = 5,
          scale = 1.25,
          dpi = 'retina')
        
        fname <- file.path(subsubdir,
                           paste0("FullMITable-", nickname(), ".csv"))
        write.csv(lvFullMIs(),
                  file = fname,
                  row.names = FALSE)
      }
      
      zip::zip(zipfile=zipname, files=zipdir)
    },
    contentType = "application/zip"
  )
  
}


  



# Run the application 
shinyApp(ui = ui, server = server)
