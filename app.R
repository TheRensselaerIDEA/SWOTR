# A small example that shows multipage shiny in action and an embedded image

library(plotly)
library(shiny)
library(shinyjs)

source("./R/multipage_shiny_lib.R")

disease <- "Microcephaly"


shinyAppPages = createMultipageServer(
  list(
    # Below inputs are drop down input values in the control window on the
    # shiny app. The above input is for stage of development and below that is
    # the disease selection. 
    selectInput(inputId = "frames",
                label = "Select stage of development to highlight:",
                choices = c('Pluripotency', 'Neuroectoderm',
                            'Neural Differentiation', 'Cortical Specification',
                            'Deep Layers', 'Upper Layers','Original'),
                selected = 'Original'),
    selectInput(inputId = "dis",
                label = "Select disease:",
                choices = c("Antisocial_personality_disease",
                            "Atypical_autism","Microcephaly", "Gout"
                            ),
                selected = "Microcephaly")
  ),
  Floor = function(input) {
    stages = c('Pluripotency', 'Neuroectoderm',
                'Neural Differentiation', 'Cortical Specification',
                'Deep Layers', 'Upper Layers','Original')
    index = 1
    for (i in 1:7)  {
      if (input$frames == stages[i]) {
        index <- i;
        break;
      }
    }
    disease <- input$dis
    return(plot_ly() %>% layout(title=input$example,
                                images = list(
                                  list(
                                    source = paste0(disease, paste0(toString(index),"_floor.png" )),
                                    xref = "paper",
                                    yref = "paper",
                                    x = 0,
                                    y = 1,
                                    sizex = 1,
                                    sizey = 1,
                                    opacity = 1
                                  )
                                ),
                                paper_bgcolor = 'black', plot_bgcolor = 'black'))
  },
  Wall = function(input) {
    disease <- input$dis
    return(plot_ly() %>% layout(title=input$example, 
                                images= list(
                                  list(
                                    source = paste0(disease, "_wall.png"),
                                    xref = "paper",
                                    yref = "paper",
                                    x= 0,
                                    y= 1,
                                    sizex = 10,
                                    sizey = 1,
                                    opacity = 1
                                  )
                                ), paper_bgcolor = 'black', plot_bgcolor = 'black'))
  },
  CampFire2D = function(input) {
    stages = c('Pluripotency', 'Neuroectoderm',
                'Neural Differentiation', 'Cortical Specification',
                'Deep Layers', 'Upper Layers','Original')
    index = 1
    for (i in 1:7)  {
      if (input$frames == stages[i]) {
        index <- i;
        break;
      }
    }
    disease <- input$dis
    return(plot_ly() %>% layout(title=input$example,
                                images = list(
                                  list(
                                    source = paste0(disease, paste0(toString(index),"_2d.png" )),
                                    xref = "paper",
                                    yref = "paper",
                                    x = 0,
                                    y = 1,
                                    sizex = 10,
                                    sizey = 1,
                                    opacity = 1
                                  )
                                ),
                                paper_bgcolor = 'black', plot_bgcolor = 'black'))
  }
)

shinyApp(ui = shinyAppPages$ui, server = shinyAppPages$server)
