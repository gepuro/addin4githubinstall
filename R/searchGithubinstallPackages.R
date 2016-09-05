#' Search R packages with githubinstall::gh_search_packages()
#' This addin can be used to interactively search R Packages.
#' @export
SearchGithubInstallPackages <- function() {
  
  # Get the document context.
  context <- rstudioapi::getActiveDocumentContext()
  
  # Set the default data to use based on the selection.
  text <- context$selection[[1]]$text
  defaultData <- text
  
  # Generate UI for the gadget.
  ui <- miniPage(
    gadgetTitleBar("Search R packages",
                   right = miniTitleBarButton("done", "Install", primary = TRUE)),
    miniContentPanel(
      stableColumnLayout(
        textInput("keyword", "Key Word")
      ),
      uiOutput("pending"),
      DT::dataTableOutput('output')
    )
  )
  
  
  # Server code for the gadget.
  server <- function(input, output, session) {
    
    reactiveData <- reactive({
      
      # Collect inputs.
      keywordString <- input$keyword
      pkg_df <- gh_search_packages(keywordString)
      
      # Check to see if there is data called 'data',
      # and access it if possible.
      if (!nzchar(keywordString))
        return(errorMessage("keyword", "Unavailable keyword"))
      
      if (nrow(pkg_df) == 0)
        return(errorMessage("keyword", paste("No Packages")))
      
      return(pkg_df)
      
      call <- as.call(list(
        as.name("Package List"),
        pkg_df
      ))
      
      eval(call, envir = .GlobalEnv)
    })
    
    output$pending <- renderUI({
      data <- reactiveData()
      if (isErrorMessage(data))
        h4(style = "color: #AA7732;", data$message)
    })
    
    output$output <- renderDataTable({
      data <- reactiveData()
      if (isErrorMessage(data))
        return(NULL)
      data
    })
    
    # Listen for 'done'.
    observeEvent(input$done, {
      info <- input$output_cell_clicked
      if(length(info) != 0){
        pkg_info <- reactiveData()[info$col,]
        pkg_name <- paste0(pkg_info[1], "/", pkg_info[2])
        print(paste("Install", pkg_name))
        devtools::install_github(pkg_name,
                                 ask = FALSE,
                                 force = TRUE)
      }
      invisible(stopApp())
    })
    
  }
  
  # Use a modal dialog as a viewr.
  viewer <- dialogViewer("GithubInstall", width = 1000, height = 800)
  runGadget(ui, server, viewer = viewer)
  
}
