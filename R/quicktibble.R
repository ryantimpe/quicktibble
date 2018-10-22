


quicktibble <- function(dat = NULL){
  ################
  # Non-Reactive functions ----
  ################

  ################
  # UI ----
  ################
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("quicktibble"),
    miniUI::miniButtonBlock(
      fluidRow(
        column(width = 3,
               selectInput("selInputData", label = "From data frame",
                           choices = c("None", names(eapply(.GlobalEnv,is.data.frame))[unlist(eapply(.GlobalEnv,is.data.frame))]))
               ),
        column(width = 3,
               selectInput("selInputColumn", label = "From column",
                           choices = NULL)
               ),
        column(width = 3,
               textInput("setCol2", label = NULL, value ="Col2", placeholder = "Column 2 name"),
               textInput("setCol3", label = NULL, value ="", placeholder = "Column 3 name"),
               textInput("setCol4", label = NULL, value ="", placeholder = "Column 4 name")
               ),
        column(width = 3,
               checkboxInput("setUnique", "Unique values", value = TRUE),
               hr(),
               actionButton("selInputBuild", label = "Create")
               )
      )
    ),
    miniUI::miniContentPanel(
      # DT::dataTableOutput("rhot")
      rhandsontable::rHandsontableOutput("hot")
    )

    ) #End UI

  ################
  # Server ----
  ################

  server <- function(input, output, session) {

    ###
    # Update selects
    ###
    observe({
      if(input$selInputData == "None"){
        updateSelectInput(session, "selInputColumn",
                          choices = "Select a data frame")
      } else{
        updateSelectInput(session, "selInputColumn",
                          choices = names(get(input$selInputData)))
      }
    })

    ###
    # Build starting data frame
    ###
    df.start <- eventReactive(input$selInputBuild, {
      starting.column <- get(input$selInputData)[, input$selInputColumn]

      if(input$setUnique){
        starting.column <- unique(starting.column)
      }

      dat <- tibble::tibble(starting.column, V2 = "")
      names(dat)[1] <- input$selInputColumn
      names(dat)[2] <- input$setCol2

      if(input$setCol3 != ""){
        dat <- dat %>%
          dplyr::mutate(.col3 = "")
        names(dat)[names(dat) == ".col3"] <- input$setCol3
      }
      if(input$setCol4 != ""){
        dat <- dat %>%
          dplyr::mutate(.col4 = "")
        names(dat)[names(dat) == ".col4"] <- input$setCol4
      }

      return(dat)
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # output$rhot <- DT::renderDataTable(df.start())

    output$hot = rhandsontable::renderRHandsontable({
      if (!is.null(input$hot)) {
        DF = rhandsontable::hot_to_r(input$hot)
      } else {
        DF = df.start()
      }

      rhandsontable::rhandsontable(DF) %>%
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
        rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
    })

  } #End Server

  ####
  # Addin settings ----
  ####

  viewer <- dialogViewer(paste("quicktibble -", deparse(substitute("hi"))), width = 1400, height= 2000)


  runGadget(ui, server, viewer = viewer)
}

