
library(shiny)
library(dplyr)
library(readxl)
library(readr)
library(purrr)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Synthesis utilities"),
  
  # Tabbed layout
  tabsetPanel(
    tabPanel("Usage Instructions", 
             mainPanel(
               h3("Usage Instructions"),
               p("This application is designed to perform two main functions:"),
               
               h4("1. File Merging App (CSV & Excel)"),
               p("In this tab, you can upload multiple CSV or Excel files. The application will merge the data from these files into a single dataset. You can preview the merged data and download it as a CSV file."),
               
               h4("2. Article Title Evaluation App"),
               p("In this tab, you can upload a CSV file containing article titles. The application will allow you to evaluate each title with a 'Yes' or 'No' response. You can navigate through the titles, and the last recorded evaluation will be displayed. You also have the option to download the evaluation results as a CSV file."),
               
               h4("3. Abstract Evaluation App"),
               p("In this tab, you can upload a CSV file containing abstracts. The application will allow you to evaluate each abstract with a 'Yes' or 'No' response. You can navigate through the abstracts, and the last recorded evaluation will be displayed. You also have the option to download the evaluation results as a CSV file."),
               
               h5("Instructions:"),
               p("1. To use the File Merging App, select the file type and choose the files you want to merge."),
               p("2. To use the Article Title Evaluation App, upload a CSV file with article titles, and use the buttons to evaluate them."),
               p("3. To use the Abstract Evaluation App, upload a CSV file with abstracts, and use the buttons to evaluate them."),
               p("4. Download buttons are available to save the results of both functionalities."),
               p("Alicia Foxx")
             )),
    
    tabPanel("File Merging App (CSV & Excel)", 
             sidebarLayout(
               sidebarPanel(
                 radioButtons("file_type", "Choose File Type:", 
                              choices = c("CSV" = "csv", "Excel" = "excel"), 
                              selected = "csv"),
                 
                 fileInput("files", "Select Files", multiple = TRUE, 
                           accept = c(".xls", ".xlsx", ".csv")),
                 
                 downloadButton("downloadMerged", "Download Merged CSV")
               ),
               mainPanel(
                 h4("Preview of Merged Data"),
                 tableOutput("mergedTable")
               )
             )),
    
    tabPanel("Article Title Evaluation App", 
             sidebarLayout(
               sidebarPanel(
                 fileInput("file", "Upload CSV File", accept = ".csv"),
                 actionButton("yesButton", "Yes", class = "btn-success", style = 'font-size:150%'),
                 actionButton("noButton", "No", class = "btn-danger", style = 'font-size:150%'),
                 actionButton("nextTitle", "Next Title"),
                 selectInput("playbackSpeed", "Playback Speed", 
                             choices = c("Normal (1X)" = "1", "1.2X" = "1.2", "1.5X" = "1.5", "1.75X" = "1.75","0.75X" = "0.75"), 
                             selected = "1"),
                 downloadButton("downloadData", "Download Results"),
                 br(),
                 h4("Last Recorded Evaluation:"),
                 verbatimTextOutput("lastEvaluation")
               ),
               mainPanel(
                 h4("Current Article Title"),
                 textOutput("titleText"),
                 br(),
                 h4("CSV Data with Evaluations"),
                 tableOutput("csvTable")
               )
             ),
             tags$script(HTML("
               function speakText(text, speed) {
                 var utterance = new SpeechSynthesisUtterance(text);
                 utterance.rate = parseFloat(speed);
                 speechSynthesis.speak(utterance);
               }

               function stopSpeech() {
                 speechSynthesis.cancel();  // Stops all speech synthesis
               }

               Shiny.addCustomMessageHandler('speakText', function(message) {
                 speakText(message.text, message.speed);
               });
             "))
    ),
    
    tabPanel("Abstract Evaluation App", 
             sidebarLayout(
               sidebarPanel(
                 fileInput("abstractFile", "Upload CSV File", accept = ".csv"),
                 actionButton("abstractYesButton", "Yes", class = "btn-success", style = 'font-size:150%'),
                 actionButton("abstractNoButton", "No", class = "btn-danger", style = 'font-size:150%'),
                 actionButton("nextAbstract", "Next Abstract"),
                 actionButton("stopAbstract", "Stop Reading", class = "btn-warning"),
                 selectInput("abstractPlaybackSpeed", "Playback Speed", 
                             choices = c("Normal (1X)" = "1", "1.2X" = "1.2", "1.5X" = "1.5", "1.75X" = "1.75", "0.75X" = "0.75"), 
                             selected = "1"),
                 downloadButton("downloadAbstractData", "Download Results"),
                 br(),
                 h4("Last Recorded Evaluation:"),
                 verbatimTextOutput("lastAbstractEvaluation")
               ),
               mainPanel(
                 h4("Current Abstract"),
                 textOutput("abstractText"),
                 br(),
                 h4("CSV Data with Evaluations"),
                 tableOutput("abstractCsvTable")
               )
             ),
             tags$script(HTML("
               function speakText(text, speed) {
                 var utterance = new SpeechSynthesisUtterance(text);
                 utterance.rate = parseFloat(speed);
                 speechSynthesis.speak(utterance);
               }

               function stopSpeech() {
                 speechSynthesis.cancel();
               }

               Shiny.addCustomMessageHandler('speakText', function(message) {
                 speakText(message.text, message.speed);
               });

               Shiny.addCustomMessageHandler('stopSpeech', function(message) {
                 stopSpeech();
               });
             "))
    )
  )
)

server <- function(input, output, session) {
  # Reactive expression for the file merging app
  mergedData <- reactive({
    req(input$files)
    
    files <- input$files$datapath
    
    if (input$file_type == "excel") {
      df_list <- map(files, ~ read_excel(.x, sheet = 1) %>% 
                       mutate(across(everything(), as.character)))
    } else {
      df_list <- map(files, ~ read_csv(.x, show_col_types = FALSE) %>% 
                       mutate(across(everything(), as.character)))
    }
    
    combined_df <- bind_rows(df_list)
    
    if (all(c("Authors", "Article Title", "Abstract", "Source Title", "Publication Date") %in% names(combined_df))) {
      combined_df <- combined_df %>%
        select(Authors, `Article Title`, Abstract, `Source Title`, `Publication Date`) %>%
        rename(Journal = `Source Title`)
      
      combined_df <- distinct(combined_df, Abstract, .keep_all = TRUE)
    }
    
    combined_df
  })
  
  output$mergedTable <- renderTable({
    mergedData()
  })
  
  output$downloadMerged <- downloadHandler(
    filename = function() {
      paste("merged_results_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(mergedData(), file)
    }
  )
  
  # Reactive variables for the article title evaluation app
  dataRV <- reactiveVal(NULL)
  indexRV <- reactiveVal(NULL)
  lastEvalRV <- reactiveVal("")
  originalFilename <- reactiveVal("evaluation_results")
  
  options(shiny.maxRequestSize=35*1024^2)
  
  findFirstNA <- function(df) {
    first_na <- which(is.na(df$Evaluation))[1]
    if (!is.na(first_na)) return(first_na)
    return(NULL)
  }
  
  observeEvent(input$file, {
    req(input$file)
    df <- read_csv(input$file$datapath, show_col_types = FALSE)
    
    if (!"Evaluation" %in% names(df)) {
      df$Evaluation <- NA
    }
    
    dataRV(df)
    originalFilename(tools::file_path_sans_ext(input$file$name))
    
    first_na_index <- findFirstNA(df)
    indexRV(first_na_index)
    
    if (!is.null(first_na_index)) {
      session$sendCustomMessage("speakText", list(text = df[["Article.Title"]][first_na_index], speed = input$playbackSpeed))
    } else {
      indexRV(NULL)
    }
  })
  
  output$titleText <- renderText({
    df <- dataRV()
    req(df)
    idx <- indexRV()
    
    if (!is.null(idx) && idx <= nrow(df)) {
      paste("Article Title:", df[["Article.Title"]][idx])
    } else {
      "All articles have been evaluated."
    }
  })
  
  output$csvTable <- renderTable({
    df <- dataRV()
    req(df)
    df
  })
  
  output$lastEvaluation <- renderText({
    lastEvalRV()
  })
  
  recordTitleEvaluation <- function(value) {
    df <- dataRV()
    req(df)
    idx <- indexRV()
    
    if (!is.null(idx) && idx <= nrow(df)) {
      df[idx, "Evaluation"] <- value
      dataRV(df)
      lastEvalRV(value)
    }
  }
  
  observeEvent(input$yesButton, {
    recordTitleEvaluation("Yes")
    session$sendCustomMessage("stopSpeech", list());  # Stop speech when 'Yes' is clicked
  })
  
  observeEvent(input$noButton, {
    recordTitleEvaluation("No")
    session$sendCustomMessage("stopSpeech", list());  # Stop speech when 'No' is clicked
  })
  
  observeEvent(input$nextTitle, {
    df <- dataRV()
    req(df)
    
    next_na_index <- findFirstNA(df)
    indexRV(next_na_index)
    
    if (!is.null(next_na_index)) {
      session$sendCustomMessage("speakText", list(text = df[["Article.Title"]][next_na_index], speed = input$playbackSpeed))
    }
  }, ignoreInit = TRUE)
  
  output$downloadData <- downloadHandler(
    filename = function() {
      timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
      paste0(originalFilename(), "_", timestamp, ".csv")
    },
    content = function(file) {
      write_csv(dataRV(), file)
    }
  )
  
  # Reactive variables for the abstract evaluation app
  abstractDataRV <- reactiveVal(NULL)
  abstractIndexRV <- reactiveVal(NULL)
  lastAbstractEvalRV <- reactiveVal("")
  abstractOriginalFilename <- reactiveVal("abstract_evaluation_results")
  
  findFirstNA <- function(df) {
    first_na <- which(is.na(df$Evaluation))[1]
    if (!is.na(first_na)) return(first_na)
    return(NULL)
  }
  
  observeEvent(input$abstractFile, {
    req(input$abstractFile)
    df <- read_csv(input$abstractFile$datapath, show_col_types = FALSE)
    
    if (!"Evaluation" %in% names(df)) {
      df$Evaluation <- NA
    }
    
    abstractDataRV(df)
    abstractOriginalFilename(tools::file_path_sans_ext(input$abstractFile$name))
    
    first_na_index <- findFirstNA(df)
    abstractIndexRV(first_na_index)
    
    if (!is.null(first_na_index)) {
      session$sendCustomMessage("speakText", list(text = df[["Abstract"]][first_na_index], speed = input$abstractPlaybackSpeed))
    } else {
      abstractIndexRV(NULL)
    }
  })
  
  output$abstractText <- renderText({
    df <- abstractDataRV()
    req(df)
    idx <- abstractIndexRV()
    
    if (!is.null(idx) && idx <= nrow(df)) {
      paste("Abstract:", df[["Abstract"]][idx])
    } else {
      "All abstracts have been evaluated."
    }
  })
  
  output$abstractCsvTable <- renderTable({
    df <- abstractDataRV()
    req(df)
    df
  })
  
  output$lastAbstractEvaluation <- renderText({
    lastAbstractEvalRV()
  })
  
  recordAbstractEvaluation <- function(value) {
    df <- abstractDataRV()
    req(df)
    idx <- abstractIndexRV()
    
    if (!is.null(idx) && idx <= nrow(df)) {
      df[idx, "Evaluation"] <- value
      abstractDataRV(df)
      lastAbstractEvalRV(value)
    }
  }
  
  observeEvent(input$abstractYesButton, {
    recordAbstractEvaluation("Yes")
    session$sendCustomMessage("stopSpeech", list())
  })
  
  observeEvent(input$abstractNoButton, {
    recordAbstractEvaluation("No")
    session$sendCustomMessage("stopSpeech", list())
  })
  
  observeEvent(input$stopAbstract, {
    session$sendCustomMessage("stopSpeech", list())
  })
  
  observeEvent(input$nextAbstract, {
    df <- abstractDataRV()
    req(df)
    
    next_na_index <- findFirstNA(df)
    abstractIndexRV(next_na_index)
    
    if (!is.null(next_na_index)) {
      session$sendCustomMessage("speakText", list(text = df[["Abstract"]][next_na_index], speed = input$abstractPlaybackSpeed))
    }
  }, ignoreInit = TRUE)
  
  output$downloadAbstractData <- downloadHandler(
    filename = function() {
      timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
      paste0(abstractOriginalFilename(), "_", timestamp, ".csv")
    },
    content = function(file) {
      write_csv(abstractDataRV(), file)
    }
  )
}

shinyApp(ui, server)


