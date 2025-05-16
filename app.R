library(shiny)
library(bslib)
library(chromote)
library(magick)
library(openssl)  # Add this library for base64_dec function

# Start a new Chrome session
chrome_session <- function() {
  # Try to find Chrome
  chrome_path <- tryCatch({
    chromote::find_chrome()
  }, error = function(e) {
    return(NULL)
  })
  
  # Check if Chrome was found
  if (is.null(chrome_path) || length(chrome_path) == 0) {
    return(NULL)
  }
  
  # Start a chromote session
  tryCatch({
    session <- ChromoteSession$new()
    return(session)
  }, error = function(e) {
    message("Error starting ChromoteSession: ", e$message)
    return(NULL)
  })
}

ui <- page_sidebar(
  title = "Headless Chrome in Shiny",
  sidebar = sidebar(
    width = 350,
    titlePanel("Website Screenshot Tool"),
    textInput("url", "Enter URL:", 
              value = "https://www.r-project.org/"),
    sliderInput("width", "Viewport Width:", 
                min = 320, max = 1920, value = 1280),
    sliderInput("height", "Viewport Height:", 
                min = 320, max = 1080, value = 800),
    checkboxInput("fullpage", "Capture Full Page", value = FALSE),
    actionButton("capture", "Capture Screenshot", 
                 class = "btn-primary"),
    hr(),
    downloadButton("download", "Download Image")
  ),
  
  card(
    full_screen = TRUE,
    card_header("Screenshot Preview"),
    card_body(
      uiOutput("chrome_status"),
      imageOutput("screenshot", height = "auto")
    )
  )
)

server <- function(input, output, session) {
  # Initialize reactive values
  values <- reactiveValues(
    chrome = NULL,
    screenshot = NULL
  )
  
  # Attempt to initialize Chrome on app startup
  observe({
    values$chrome <- chrome_session()
  })
  
  # Show status message
  output$chrome_status <- renderUI({
    if (is.null(values$chrome)) {
      div(
        class = "alert alert-warning",
        icon("exclamation-triangle"),
        " Chrome not available. This app requires Chrome to be installed."
      )
    } else {
      # When Chrome is available, don't show any message
      NULL
    }
  })
  
  # Take screenshot when button is clicked
  observeEvent(input$capture, {
    # Verify Chrome is available
    if (is.null(values$chrome)) {
      showNotification("Chrome is not available", type = "error")
      return()
    }
    
    # Check URL
    if (nchar(input$url) == 0) {
      showNotification("Please enter a valid URL", type = "warning")
      return()
    }
    
    # Add http:// if missing
    url <- input$url
    if (!grepl("^https?://", url)) {
      url <- paste0("http://", url)
    }
    
    # Show loading indicator
    withProgress(message = 'Taking screenshot...', {
      tryCatch({
        # Navigate to the URL
        values$chrome$Page$navigate(url)
        values$chrome$Page$loadEventFired()
        
        # Set viewport size
        values$chrome$Emulation$setDeviceMetricsOverride(
          width = input$width,
          height = input$height,
          deviceScaleFactor = 1,
          mobile = FALSE
        )
        
        # Wait for page to load
        Sys.sleep(2)
        
        # Capture screenshot
        if (input$fullpage) {
          # Get page dimensions
          metrics <- values$chrome$Page$getLayoutMetrics()
          height <- metrics$contentSize$height
          
          # Set viewport to full height
          values$chrome$Emulation$setDeviceMetricsOverride(
            width = input$width,
            height = as.integer(height),
            deviceScaleFactor = 1,
            mobile = FALSE
          )
          
          # Take screenshot
          img_data <- values$chrome$Page$captureScreenshot()
        } else {
          # Take visible viewport screenshot
          img_data <- values$chrome$Page$captureScreenshot()
        }
        
        # Convert base64 image to magick image
        img <- image_read(base64_dec(img_data$data))
        values$screenshot <- img
        
        showNotification("Screenshot captured successfully", type = "message")
      }, error = function(e) {
        showNotification(paste("Error capturing screenshot:", e$message), type = "error")
      })
    })
  })
  
  # Display the screenshot
  output$screenshot <- renderImage({
    req(values$screenshot)
    
    # Create a temporary file for the image
    temp_file <- tempfile(fileext = ".png")
    image_write(values$screenshot, temp_file)
    
    list(
      src = temp_file,
      contentType = "image/png",
      width = "100%",
      alt = "Website screenshot"
    )
  }, deleteFile = TRUE)
  
  # Download handler
  output$download <- downloadHandler(
    filename = function() {
      paste0("screenshot-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".png")
    },
    content = function(file) {
      req(values$screenshot)
      image_write(values$screenshot, file)
    }
  )
  
  # Cleanup when the session ends
  onSessionEnded(function() {
    if (!is.null(values$chrome)) {
      try(values$chrome$close(), silent = TRUE)
    }
  })
}

shinyApp(ui, server)
