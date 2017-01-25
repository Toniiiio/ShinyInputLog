# Related to this question on stackoverflow: http://stackoverflow.com/questions/41718948/analysing-shiny-server-log-to-create-statistics-on-usage
# 
# 
# A small overview on the work
# 
# Challenge 1 - Track the inputs:
# First, I thought looping through the inputs and add a listener. Then i found Shiny already has something similar:
# "shiny:inputchanged". It looked like a quick win, as you could collect the changes in JS and then send them to R.
# How to send them to R? Right: "Shiny.onInputChange". Calling that within "shiny:inputchanged" sounds strange and 
# well suprise, that didnt work.
# 
# Challenge 2 - Send results back to R
# So I thought about saving results in a variable and calling "shiny:inputchanged" outside "shiny:inputchanged" as soon
# as there was a change in that variable. JS does not really have a variable change listener, but you can find a workaround
# HERE.
# 
# Note:JS part could use some refactoring, I just taught myself a bit JS for Shiny purposes only. 
# 


addListener <- '
  function checkVariable() {
    if (window.ip == true) {
      sendToUI.setinputUpdate(["connected", "connected", window.ip]);
    }
  }

  function Create(callback) {
    var inputUpdate = false;
    return {
      getinputUpdate   : function()  { return inputUpdate; },
      setinputUpdate   : function(p) { inputUpdate = p; callback(inputUpdate);}
    };
  }
    
  var sendToUI = Create(function(inputUpdate) {
    if (inputUpdate) {
      $.getJSON("//freegeoip.net/json/?callback=?", function(data) {
        window.ip = JSON.stringify(data.ip, null, 2);
      });
      var val = inputUpdate[1].toString()
      // dont allow other input types than the ones in UI-input to be logged
      if(val.charAt(0) != "." && val != "logger"){
        // datatableoutput gives input changes on itial start
        if(val.substring(0,7) != "values_"){
          Shiny.onInputChange("logger", [inputUpdate, window.ip]);
        }
      }
    }
  });

  $(document).on("shiny:inputchanged", function(event) {
    sendToUI.setinputUpdate([event.value, event.name]);
  });

  $(document).on("shiny:connected", function(event) {
      $.getJSON("//freegeoip.net/json/?callback=?", function(data) {
        window.ip = JSON.stringify(data.ip, null, 2);
      });
      setTimeout(checkVariable, 100);
  });

  $(document).on("shiny:disconnected", function(event) {
    sendToUI.setinputUpdate(["disconnected", "disconnected"]);
  });

'

if(!file.exists("log.csv")){
  log <- data.frame(inputVal = "", inputId = "", UserIp = "", time = "")
  write.table(log, "log.csv", sep = ";", append = TRUE,  row.names = FALSE, col.names = TRUE)
}


setwd("C:/Users/d91115/Dropbox/Application/Stackoverflow")

library(shiny)

# Define UI for slider demo application
ui <- fluidPage(
  tags$head(tags$script(HTML(addListener))),
  #  Application title
  titlePanel("Input Logger"),
  
  # Sidebar with sliders that demonstrate various available
  # options
  sidebarLayout(
    sidebarPanel(
      # Simple integer interval
      sliderInput("integer", "Integer:",
                  min=0, max=1000, value=500),
      
      # Decimal interval with step value
      sliderInput("decimal", "Decimal:",
                  min = 0, max = 1, value = 0.5, step= 0.1),
      
      # Specification of range within an interval
      sliderInput("range", "Range:",
                  min = 1, max = 1000, value = c(200,500)),
      
      # Provide a custom currency format for value display,
      # with basic animation
      sliderInput("format", "Custom Format:",
                  min = 0, max = 10000, value = 0, step = 2500,
                  pre = "$", sep = ",", animate=TRUE),
      
      # Animation with custom interval (in ms) to control speed,
      # plus looping
      sliderInput("animation", "Looping Animation:", 1, 2000, 1,
                  step = 10, animate=
                    animationOptions(interval=300, loop=TRUE))
    ),
    
    # Show a table summarizing the values entered
    mainPanel(
      dataTableOutput("values")
    )
  )
)


server <- function(input, output, session) {
  
  observe({
    input$logger
    if(!is.null(input$logger)){
      inputLog <- c(input$logger, as.character(Sys.time()))
      if(length(inputLog) == 5) inputLog <- c(paste(input$logger[1:2], collapse = "-"), input$logger[3:4], as.character(Sys.time()))
      print(as.data.frame(rbind(inputLog)))
      write.table(as.data.frame(rbind(inputLog)), "log.csv", sep = ";", append = TRUE,  row.names = FALSE, col.names = FALSE)
    }
  })

  sliderValues <- reactive({
    
    # Compose data frame
    data.frame(
      Name = c("Integer", 
               "Decimal",
               "Range",
               "Custom Format",
               "Animation"),
      Value = as.character(c(input$integer, 
                             input$decimal,
                             paste(input$range, collapse=' '),
                             input$format,
                             input$animation)), 
      stringsAsFactors=FALSE)
  }) 
  
  output$values <- renderDataTable({
    input$logger
    data <- as.data.frame(read.table("log.csv", sep = ";", header = TRUE))
    return(data[dim(data)[1]:1, ])
  })
}

runApp(shinyApp(ui, server), launch.browser = TRUE, display.mode = "showcase")



