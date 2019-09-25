library(shiny)

# Simple shiny layout for demo sake
ui<-fluidPage(
  sidebarLayout(
    sidebarPanel(
      h5("use case - button based navigation"),
      actionButton(inputId = "buttonuno", label = "buttonuno"),
      br(),
      actionButton(inputId = "buttondos", label = "buttondos"),
      br(),
      actionButton(inputId = "buttontres", label = "buttontres")
    ), 
    mainPanel(
      textOutput("uno"),
      textOutput("dos"),
      textOutput("tres")
      
    )
  )
)

server<-function(input, output,session){
   but1 = reactiveValues(but1=FALSE)
   but2 = reactiveValues(but2=FALSE)
   but3 = reactiveValues(but3=FALSE)
   
   observeEvent(input$buttonuno,
                isolate({but1$but1=TRUE
                but2$but2=FALSE
                but3$but3=FALSE
                }))
   
   observeEvent(input$buttondos,
                isolate({but1$but1=FALSE
                but2$but2=TRUE
                but3$but3=FALSE
                }))
   
   observeEvent(input$buttontres,
                isolate({but1$but1=FALSE
                but2$but2=FALSE
                but3$but3=TRUE
                }))
   
   output$uno <- renderText({
     if(but1$but1)
       paste("ustedes selectado button uno") ## cambiarlo esos con renderUI
     else
       return()
   })
   
   output$dos <- renderText({
     if(but2$but2)
       paste("ustedes selectado button dos") ## cambiarlo esos con renderUI
     else
       return()
   })
   
   
   output$tres <- renderText({
     if(but3$but3)
       paste("ustedes selectado button tres") ## cambiarlo esos con renderUI
     else
       return()
   })
}

shinyApp(ui = ui, server = server)