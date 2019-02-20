library(shiny)
library(miniUI)
library(tidyverse)
library(DT)

x <- data.frame(stefan = c(0,0), jenny = c(0,0))
time_df <- tibble()

interTable <- function(df) {

  ui <- miniPage(
    gadgetTitleBar("WritR"),
    miniContentPanel(
      dataTableOutput("dt")
    )
  )
  
  server <- function(input, output, session) {
    
    
    output$dt <- renderDT(df, selection = 'none', rownames = F, editable = T)
    
    proxy = dataTableProxy('dt')
    
    observeEvent(input$dt_cell_edit, {
      info = input$dt_cell_edit
      #str(info)
      i = info$row
      j = info$col +1
      v = info$value
      time_df <<- bind_rows(time_df, tibble(row = i, col = j, value = v, time = Sys.time()))
      
      df[i, j] <<- DT::coerceValue(v, df[i, j])
      replaceData(proxy, df, resetPaging = FALSE, rownames = FALSE)  # important
    })
    
    observeEvent(input$done, {
      
      stopApp(df)
    }) 
  
  }

  runGadget(ui, server)
  
}


hasse <- interTable(x)


