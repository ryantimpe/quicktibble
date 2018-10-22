


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
               checkboxInput("setUnique", "Unique values", value = TRUE)
               ),
        column(width = 3,
               actionButton("selInputBuild", label = "Create")
               )
      )
    ),
    miniUI::miniContentPanel(
      DT::dataTableOutput("rhot")
      # rhandsontable::rHandsontableOutput("rhot")
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

      return(dat)
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    output$rhot <- DT::renderDataTable(df.start())

    # output$rhot <- rhandsontable::renderRHandsontable(df.start())
  } #End Server

  ####
  # Addin settings ----
  ####

  viewer <- dialogViewer(paste("quicktibble -", deparse(substitute("hi"))), width = 1400, height= 2000)


  runGadget(ui, server, viewer = viewer)
}

