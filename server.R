library(shiny)
library(shinyGlobe)
library(ggplot2)
library(dplyr)
library(ggmap)
library(maptools)
library(maps)
library(plotly)
library(shinydashboard)
library(DT)
library(leaflet)
library(shinyjs)
library(V8)
library(reshape)
library(googleVis)


test <- readRDS("test.rds")
max_growth <- readRDS("max_growth.rds")
comb_table <- readRDS("comb_table.rds")
data_us <- readRDS("data_us.rds")
data_state <- readRDS("data_state.rds")
Montana <- readRDS("Montana.rds")
Texas <- readRDS("Texas.rds")
Oklahoma <- readRDS("Oklahoma.rds")
Washington <- readRDS("Washington.rds")


shinyServer(function(input, output, session) {
  
  chemical <- reactive({
                          if (input$sector == 1) {
                            sect = "chemical"
                          } else {
                            sect = "food"
                          }
                          chemical <- data_us[grep(sect, data_us$NAICS.display.label),]
                          chemical <- chemical[,-grep("_S", colnames(chemical))]
                          chemical <- chemical[chemical$YEAR.id > 2011,]
                          chemical
                        })
  
  
  tot_cost <- reactive({
                        chemical <- chemical()
                        fringe <- sum(chemical$BENEFIT,na.rm = T) / sum(chemical$EMP,na.rm = T)  # $1000 / emp
                        ele <- sum(chemical$CSTELEC,na.rm = T) / sum(chemical$ELECPCH, na.rm = T)   # $ / kWh / emp
                        contract <- sum(chemical$CSTCNT, na.rm = T) / sum(chemical$EMP,na.rm = T)  # $1000 / emp
                        material <- sum(chemical$CSTMTOT, na.rm = T) / sum(chemical$EMP,na.rm = T)  # $1000 / emp
                        emp <- mean(chemical$EMP)
                        
                        
                        tot_cost <- (fringe * emp) + (ele * emp * 16986848) + (contract * 42449) + (material * emp)
                        
                        tot_cost
                        
                      })
  
  state <- reactive({
    
                      
                      tot_cost <- tot_cost()
                      chemical <- chemical()
                      emp <- mean(chemical$EMP)
                      data_state1 <- data_state
                      data_state1$tot_cost2 <- (data_state1$BENEFIT * emp) + (data_state1$ele_avg * emp * 16986848) + (data_state1$CSTMTOT * emp) + (data_state1$CSTCNT * emp)
                      data_state1$per <- 100 - ( 100 * data_state1$tot_cost2 / tot_cost)
                      data_state1 <- data_state1[order(-data_state1$per),]
                      
                      if (sum(data_state1$per > 0) > 4) {
                        data_state1 <- data_state1[-grep("Montana", data_state1$GEO.display.label), ]
                        data_state1 <- data_state1[-grep("Washington", data_state1$GEO.display.label), ]
                      }
                      state <- data_state1[1:2,]
                      state$per <- round(state$per,1)
                      
                      
                      state
                      
                    })
  
  
  output$per_state1 <- renderGvis({
    
                    state <- state()
                    Gauge <-  gvisGauge(state[1,c(1,8)], 
                                        options=list(min=-10, max=50, greenFrom=5,
                                                     greenTo=50, yellowFrom=0, yellowTo=5,
                                                     redFrom=-10, redTo=0, width=300, height=300,legend='none'))
                    Gauge
                  })
  
  output$per_state2 <- renderGvis({
    
                      state <- state()
                      Gauge2 <-  gvisGauge(state[2,c(1,8)], 
                                          options=list(min=-10, max=50, greenFrom=5,
                                                       greenTo=50, yellowFrom=0, yellowTo=5,
                                                       redFrom=-10, redTo=0, width=300, height=300))
                      Gauge2
                    })
  
  
  
  output$fringe_ind <- renderValueBox({
    
                      chemical <- chemical()
                      fringe_ind <- sum(chemical$BENEFIT,na.rm = T) / sum(chemical$EMP,na.rm = T)  # $1000 / emp
                      valueBox(paste("$",round(fringe_ind,1)),HTML(paste("Avg. Fringe Benefit ($1000)",tags$sup(1))),icon = icon("flag-o"), color = 'green') })
  
  output$ele_ind <- renderValueBox({
    
                      chemical <- chemical()
                      ele_ind <- sum(chemical$CSTELEC,na.rm = T) / sum(chemical$ELECPCH, na.rm = T)   # $ / kWh / emp
                      valueBox(paste("$",round(ele_ind,3)),HTML(paste("Avg. Electricity Cost",tags$sup(2))),icon = icon("flag-o"), color = 'red') })
  
  output$contract_ind <- renderValueBox({
    
                      chemical <- chemical()
                      contract_ind <- sum(chemical$CSTCNT, na.rm = T) / sum(chemical$EMP,na.rm = T)  # $1000 / emp
                      valueBox(paste("$",round(contract_ind,1)),HTML(paste("Avg. Contract Cost ($1000)",tags$sup(3))),icon = icon("flag-o"), color = 'blue') })
  
  output$material_ind <- renderValueBox({
    
                      chemical <- chemical()
                      material_ind <- sum(chemical$CSTMTOT, na.rm = T) / sum(chemical$EMP,na.rm = T)  # $1000 / emp
                      valueBox(paste("$",round(material_ind,1)),HTML(paste("Avg. Material Cost ($1000)",tags$sup(4))),icon = icon("flag-o"), color = 'purple') })
  
  
  output$levelQueryUi <- renderUI({
                          radioButtons("sector", label = "Manufacturing Sector",
                                       choices = list("Chemical" = 1, "Food" = 2), 
                                       selected = 1)
                        })
  
  
  output$Design1 <- renderText({
                        if (input$sector == 1) {
                          name = paste("Chemical Manufacturing")
                        } else {
                          name = paste("Food Manufacturing")
                        }
                        name
                      })
  
  output$Design2 <- renderText({
                      if (input$sector == 1) {
                        name2 = paste("Recommended States for Chemical Manufacturing")
                      } else {
                        name2 = paste("Recommended States for Food Manufacturing")
                      }
                      name2
  })
  
  output$state1 <- renderText({
                      state <- state()
                      name3 = paste(state$GEO.display.label[1])
                      name3
                    })
  
  
  output$state2 <- renderText({
                      state <- state()
                      name4 = paste(state$GEO.display.label[2])
                      name4
                    })
  
  
  
  
   output$plot1 <- renderPlotly({

    m <- list(l = 0,r = 0,b = 0,t = 30, pad = 0, autoexpand = TRUE)
    
    p1 <- plot_ly(
              x = c("2015","2014","2013","2012"),y = as.vector(test$Chemical), type = 'bar',name = "Chemical") %>%
              add_trace(y = as.vector(test$Transportation), name = "Transportation") %>%
              add_trace(y = as.vector(test$Pharmaceutical), name = "Pharmaceutical") %>%
              add_trace(y = as.vector(test$Food), name = "Food") %>%
              add_trace(y = as.vector(test$Aerospace), name = "Aerospace") %>%
              add_trace(y = as.vector(test$Coal), name = "Coal") %>%
              add_trace(y = as.vector(test$Refineries), name = "Refineries") %>%
              layout( title = "Highest Earning Manufacturing Industries",
                      #font = list(size = 15),
                      #margin = m,
                      yaxis = list(title = "Earnings (Billion US$)"), xaxis = list(title = paste("Year","",sep = "<br>")), 
                      barmode = 'group',legend = list(orientation = 'v')) %>%
              config(displayModeBar = F)
    
    p1  })
  
  
  output$plot2 <- renderPlotly({
    
   
              p2 <- plot_ly(x= max_growth$Label,y = max_growth$grow_avg,type='bar') %>%
                    layout(title = "Fastest Growing Manufacturing Industries",font = list(size = 13),
                       margin = list(l = 50, r = 50, b = 50, t = 100, pad = 4),
                       yaxis = list(title = "Growth (%)"), xaxis = list(title = "Industry"), barmode = 'group',
                       annotations = list(
                         x = 0.5 , y = 1.17, text = "(The Average Growth across All Manufacturing Sectors is 1.84%)", 
                         showarrow = F, xref='paper', yref='paper')) %>%
                     config(displayModeBar = F)
    
    p2  })
  
  
  output$plot3 <- renderPlotly({
    
    
    p3 <- plot_ly(comb_table, x = ~Data) %>%
      add_markers(y = ~earning, color = ~grow_avg, marker = list(size = 10, colorbar = list(title = paste("Average", "Growth", sep = "<br>")
      ))) %>%
      layout(title = "Earning Vs. Spending on Data Science", margin = list(l = 100, r = 50, b = 100, t = 100, pad = 4), font = list(size = 13),
             xaxis = list(title = "Data Science Spending per Employee ($1000 US)", range = c(0,4.5)), 
             yaxis = list(title = "Earning per Employee ($1000 US)", range = c(0,1000)),
             annotations = list(
               x = 0.50 , y = 1.08, text = "(Across all Manufacturing Industries in the US)", showarrow = F, xref='paper', yref='paper')) %>%
      config(displayModeBar = F)
    
    p3  })
  
  
  output$plot4 <- renderPlotly({
    
    state <- state()
    if (paste(state$GEO.display.label[1]) == "Montana") {
      data1 <- Montana
    } else {
      data1 <- Texas
    }
    
    m <- list(l = 0,r = 0,b = 0,t = 0, pad = 0, autoexpand = TRUE)
    
    p4 <- plot_ly(data1, labels =  ~ind, values = ~cost, type = 'pie',hole = 0.7,
                  textinfo = 'value',insidetextfont = list(color = '#FFFFFF'),
                  hoverinfo = 'text',text = ~paste(ind,': $', cost),showlegend = T) %>%
      layout(margin = m,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             legend = list(x = 0.35, y = 0.5)) %>%
      config(displayModeBar = F)
    
    p4  })
  
  
  output$plot5 <- renderPlotly({
    
                    state <- state()
                    if (paste(state$GEO.display.label[2]) == "Washington") {
                      data1 <- Washington
                    } else {
                      data1 <- Oklahoma
                    }
                    
                    m <- list(l = 0,r = 0,b = 0,t = 0, pad = 0, autoexpand = TRUE)
                    
                    p5 <- plot_ly(data1, labels =  ~ind, values = ~cost, type = 'pie',hole = 0.7,
                                  textinfo = 'value',insidetextfont = list(color = '#FFFFFF'),
                                  hoverinfo = 'text',text = ~paste(ind,': $', cost),showlegend = T) %>%
                      layout(margin = m,
                             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                             legend = list(x = 0.35, y = 0.5)) %>%
                      config(displayModeBar = F)
    
    p5  })
  
  
  
  
})