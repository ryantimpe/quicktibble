


quicktibble <- function(dat = NULL){
  ################
  # Non-Reactive functions ----
  ################
  collapse_col2 <- function(index, data){
    #Check to see if its a numeric column...
    check.num <- suppressWarnings(sum(is.na(as.numeric(as.character(data[, index])))) > 0)

    if(!check.num){
      paste0(names(data)[index], " = c( ", paste0(data[, index], collapse = ", "), ")")
    } else {
      paste0(names(data)[index], " = c( '", paste0(data[, index], collapse = "', '"), "')")
    }
  }

  ################
  # UI ----
  ################
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("quicktibble"),
    conditionalPanel(
      condition = "input.selInputBuild == 0",
      miniUI::miniButtonBlock(
        fluidRow(
          column(width = 3,
                 selectInput("selInputData", label = "From data frame",
                             choices = c("None", names(eapply(.GlobalEnv,is.data.frame))[unlist(eapply(.GlobalEnv,is.data.frame))])),
                 selectInput("selInputColumn", label = "From column",
                             choices = NULL)
          ),
          column(width = 3,
                 strong("Column names"),
                 helpText("Leave blank to not include"),
                 textInput("setCol2", label = NULL, value ="Col2", placeholder = "Column 2 name"),
                 textInput("setCol3", label = NULL, value ="", placeholder = "Column 3 name")

          ),
          column(width = 3,
                 br(),
                 br(),
                 textInput("setCol4", label = NULL, value ="", placeholder = "Column 4 name"),
                 textInput("setCol5", label = NULL, value ="", placeholder = "Column 5 name")
          ),
          column(width = 3,
                 checkboxInput("setWeight", "Weight column?", value = FALSE),
                 checkboxInput("setUnique", "Unique values", value = TRUE),
                 hr(),
                 actionButton("selInputBuild", label = "Create")
          )
        )
      )
    ),
    conditionalPanel(
      condition = "input.selInputBuild != 0",
      miniUI::miniButtonBlock(
        textInput("setOPname", label = "Output table name", value = "QuickTibble", placeholder = "Output Name"),
        conditionalPanel(
          condition = "input.setWeight == true",
          p("When complete, scale weights?"),
          radioButtons("setWeightScale", label = NULL,
                       choices = "none",
                       selected = "none",
                       inline = TRUE),
          helpText("Choose a column name to scale each group to 100%.")
        )
      )
    ),
    miniUI::miniContentPanel(
      rhandsontable::rHandsontableOutput("hot"),
      verbatimTextOutput("opPreview")
    )

  ) #End UI

  ################
  # Server ----
  ################

  server <- function(input, output, session) {

    ###
    # Update selects
    ###
    # Column/DF selection
    observe({
      if(input$selInputData == "None"){
        updateSelectInput(session, "selInputColumn",
                          choices = "Select a data frame")
      } else{
        updateSelectInput(session, "selInputColumn",
                          choices = names(get(input$selInputData)))
      }
    })

    # Weight scaling
    observe({
      wghtChoices <- c(input$setCol2, input$setCol3, input$setCol4, input$setCol5)
      wghtChoices <- wghtChoices[wghtChoices != ""]

      updateRadioButtons(session, "setWeightScale",
                         choices = c("No"="no", "All to 100%"="total", input$selInputColumn, wghtChoices),
                         selected = "no",
                         inline = TRUE)
    })

    ###
    # Build starting data frame
    ###
    df.start <- eventReactive(input$selInputBuild, {
      starting.column <- tibble::as.tibble(get(input$selInputData)[, input$selInputColumn]) %>% dplyr::pull()

      if(input$setUnique){
        starting.column <- unique(starting.column)
      }

      dat <- tibble::tibble(starting.column, V2 = "")
      names(dat)[1] <- input$selInputColumn
      names(dat)[2] <- input$setCol2

      #Additional columns
      if(input$setCol3 != ""){
        dat <- dat %>%
          dplyr::mutate(.col = "")
        names(dat)[names(dat) == ".col"] <- input$setCol3
      }
      if(input$setCol4 != ""){
        dat <- dat %>%
          dplyr::mutate(.col = "")
        names(dat)[names(dat) == ".col"] <- input$setCol4
      }
      if(input$setCol5 != ""){
        dat <- dat %>%
          dplyr::mutate(.col = "")
        names(dat)[names(dat) == ".col"] <- input$setCol5
      }

      #Weight Column
      if(input$setWeight){
        dat <- dat %>%
          dplyr::mutate(.Weight = 1)
      }

      return(dat)
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    output$hot = rhandsontable::renderRHandsontable({
      if (!is.null(input$hot)) {
        DF = rhandsontable::hot_to_r(input$hot)
      } else {
        DF = df.start()
      }

      rhandsontable::rhandsontable(DF) %>%
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
        rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE) %>%
        rhandsontable::hot_col(col = input$selInputColumn,  strict = FALSE, allowInvalid = TRUE)
    })

    # Finalize the output

    #Scale Weights
    tbScaled <- reactive({
      hot = input$hot
      if (!is.null(hot)) {
        dat <- rhandsontable::hot_to_r(hot)

        #Check to see if weight column
        if(input$setWeight){
          #Scale to total...
          if(input$setWeightScale == "total"){
            dat <- dat %>%
              dplyr::mutate(.Weight = .Weight / sum(.Weight))
            #Scale to a column....
          } else if(input$setWeightScale != "no"){
            dat <- dat %>%
              dplyr::group_by(!!!rlang::syms(c(input$setWeightScale))) %>%
              dplyr::mutate(.Weight = round(.Weight / sum(.Weight)), 5) %>%
              dplyr::ungroup() %>%
              as.data.frame()
          } else {dat <- dat} #Other don't scale
        }

      } else {dat <- df.start()}

      return(dat)
    })

    # Preview the output
    codePreview <- reactive({
      hot = input$hot

      if (!is.null(hot)) {

        opName <- if(input$setOPname == ""){"QuickTibble"}else{input$setOPname}
        # dat <- rhandsontable::hot_to_r(hot)
        dat <- tbScaled()

        op <- paste0(opName, " <- tibble::tibble(",
                     paste(purrr::map_chr(1:ncol(dat), collapse_col2, dat), collapse = ",\n\t\t\t\t"),
                     ")")


      } else {op <- "Print Preview"}

      return(op)

    })
    output$opPreview <- renderText({
      return(codePreview())
    })

    # Listen for 'done' events.
    observeEvent(input$done, {
      hot = isolate(input$hot)
      if (!is.null(hot)) {
        rstudioapi::insertText(Inf, codePreview())
      }
      stopApp()
    })

    session$onSessionEnded(function() {
      stopApp()
    })

  } #End Server

  ####
  # Addin settings ----
  ####

  viewer <- dialogViewer(paste("quicktibble"), width = 1200, height= 1200)


  runGadget(ui, server, viewer = viewer)
}

