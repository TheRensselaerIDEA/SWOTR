# set working directory - only works in RStudio (with rstudioapi)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("campfire_lib.R")

campfireApp(
  
  controller = div(
    h1("Controller"),
    selectInput(inputId = "fruitSelection",
      label = "Pick a fruit",
      choices = c("Apple", "Banana", "Pear")),
    style="position: absolute; 
           top: 50%; left: 50%; 
           margin-right: -50%; 
           transform: translate(-50%, -50%)"
  ),
  
  wall = div(
    h1("Wall"),
    textOutput("wallText"),
    style="position: absolute; 
           top: 50%; left: 50%;
           margin-right: -50%; 
           transform: translate(-50%, -50%);
           background: rgb(255, 255, 255);"
  ),
  
  floor = div(
    h1("Floor"),
    textOutput("floorText"),
    style="position: absolute; 
           top: 50%; left: 50%;
           margin-right: -50%; 
           transform: translate(-50%, -50%);
           background: rgb(255, 255, 255);"
  ),

  monitor = div(
    h1("External Monitor"),
    textOutput("monitorText"),
    style="position: absolute; 
           top: 50%; left: 50%;
           margin-right: -50%; 
           transform: translate(-50%, -50%);
           background: rgb(255, 255, 255);"
  ),
  
  serverFunct = function(serverValues, output) {
    output$wallText <- renderText({ serverValues$fruitSelection })
    output$floorText <- renderText({ serverValues$fruitSelection })
    output$monitorText <- renderText({ serverValues$fruitSelection })
  }
  
)

