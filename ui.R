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

dashboardPage(skin = "yellow",
              dashboardHeader(
                  title = "Make in USA"),
              
              dashboardSidebar(
                sidebarMenu(
                  menuItem("Proposal", tabName = "proposal", icon = icon("dashboard")),
                  menuItem("Exploratory Analysis", tabName = "expo", icon = icon("area-chart")),
                  menuItem("Interactive App", tabName = "app", icon = icon("gears"),badgeLabel = "new", badgeColor = "green"),
                  menuItem("Future Plans", tabName = "plan", icon = icon("tasks")),
                  hr(),
                  sidebarUserPanel(name = a("Sarthak Dasadia", target = "_blank_",
                                            href = "ADD"), 
                                   subtitle = "PhD Physics",
                                   image = "sd.png")
                )
              ),
              
              dashboardBody(
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                ),
                tabItems(
                tabItem(tabName = "proposal",
                        fluidPage(
                          title = "DB",
                          fluidRow(
                            column( width = 12,
                                    box(
                                      style = "font-size: 120%;",
                                      width = 5,
                                      #height = 490,
                                      background = "light-blue",
                                      solidHeader = FALSE,
                                      collapsible = FALSE,
                                      collapsed = FALSE,
                                      h1(" Make in USA"),
                                      tags$br(),
                                      p("The massive tax cut to businesses proposed by the current administration is expected to 
                                         ignite US manufacturing sector. Additionally, heavy import tax and withdrawal from the 
                                         Trans-Pacific Partnership (TPP) will inspire American companies to bring their manufacturing facilities back to 
                                         the US. With no standard tools available to help these companies, I propose a project which will help these 
                                         companies make smarter decisions and cut costs."),
                                      p("The manufacturing sector in the US accounts for 12.1% of the total GDP in 2015 contributing over 
                                         $2.17 trillion to the economy (source: Bureau of Economic Analysis). Output per hour for all workers 
                                         in the manufacturing sector has increased by more than 2.5 times since 1987. Which points towards more 
                                         automation and more productivity rise. Over the next decade, nearly 3½ million manufacturing jobs will 
                                         likely be needed. My project aims to draw helpful correlations between manufacturing industry, operating 
                                         efficiency and labor skill requirements across all possible dimensions.")
                                      
                                    ),
                                    
                                    box(
                                      width = 7,
                                      tags$iframe(width="750", height="450", src="//www.youtube.com/embed/szQlPUoh2a4", frameborder="0", allowfullscreen = "true")
                                     )
                            
                          )),
                          fluidRow(
                            column(width = 12,
                                   box(
                                     style = "font-size: 120%;",
                                     width = 5,
                                     #height = 490,
                                     background = "olive",
                                     solidHeader = FALSE,
                                     collapsible = FALSE,
                                     collapsed = FALSE,
                                     h2(" Datasets"),
                                     p("In this project, I plan to use following data sources."),
                                       tags$ul(
                                         tags$li("Annual Survey of Manufactures (ASM) dataset published by the US Census Bureau. 
                                                 The dataset contains over 110 features on information related to the various types of 
                                                 expenses such as electricity, labor, tax, materials, data analysis, and instruments etc. 
                                                 for over 500 different manufacturing sectors nationwide. There is also a state-level data 
                                                 on the overall manufacturing sector. ASM data tables can be obtained using Census Bureau API 
                                                 or by downloading online tables."),
                                         p(""),
                                         tags$li("For more detailed state wise manufacturing data, I plan to use publicly available dataset 
                                                  maintained by the National Associate of Manufactures (NAM). This dataset contains state wise 
                                                  information on different manufacturing sectors and their exports. NAM dataset is available to 
                                                  download online."),
                                         p(""))
                                         
                                     
                                     ),
                                   box(
                                     style = "font-size: 120%; background-color : #e77e7e;",
                                     width = 7,
                                     #height = 490,
                                     background = "olive",
                                     solidHeader = FALSE,
                                     collapsible = FALSE,
                                     collapsed = FALSE,
                                     h2("Priliminary Results"),
                                     p("The exploratory analysis has revealed a tremendous potential for this project. 
                                        Some of the most interesting preliminary results are:"),
                                     tags$ul(
                                       tags$li("Chemical, transportation, pharmaceutical, food, aerospace, and traditional fuel manufacturing 
                                                industries have highest earning and a steady average growth of ~2%."),
                                       p(""),
                                       tags$li("The smaller scale sectors such as dairy, motor-vehicle, Gypsum mining, truss, and truck manufacturing 
                                               have highest growth rate since 2012."),
                                       p(""),
                                       tags$li("The profit-loss analysis across all manufacturing sectors suggests that the net earning is correlated 
                                                with the data science-related spending (e.g., expenses related to data analysis, purchasing computational 
                                                instruments and software). However, this needs to be better examined due to other contributing factors."),
                                       p(""),
                                       tags$li("In this project, I plan to build an app which can help reduce cost e.g., if a plant is going to need 
                                                heavy consumption of electricity, consider building it to states where electricity is cheap such as 
                                                Montana, Washington, Oregon or Kentucky. While, to reduce material and labor costs consider states 
                                                like Louisiana, Delaware, Texas or Kentucky. This can be further expanded to include many features to 
                                                build a great recommendation tool companies. "))
                                     
                                   )
                            )
                          )
                        )
                ),
                tabItem(tabName = "expo",
                        fluidPage(
                          title = "Expo",
                          fluidPage(
                            column(width = 12,
                                   height = 1800,
                                   box(
                                     title = "Preliminary Results",
                                     status = "primary",
                                     width = 12,
                                     solidHeader = TRUE,
                                     collapsible = TRUE,
                                     fluidRow(
                                       box(
                                         style = "font-size: 140%;",
                                         status = "primary",
                                         width = 12,
                                         solidHeader = FALSE,
                                         collapsible = TRUE,
                                         p("1. Chemical, transportation, pharmaceutical, food, aerospace, and traditional fuel manufacturing 
                                                industries have highest earning and a steady average growth of ~2%."),
                                         tags$br(),
                                         plotlyOutput("plot1"),
                                         tags$br(),
                                         tags$br(),
                                         p("2. The smaller scale sectors such as dairy, motor-vehicle, Gypsum mining, truss, and truck manufacturing 
                                               have highest growth rate since 2012."),
                                         #tags$br(),
                                         plotlyOutput("plot2"),
                                         tags$br(),
                                         tags$br(),
                                         p("3. In this project, I plan to build an app which can help reduce cost e.g., if a plant is going to need 
                                                heavy consumption of electricity, consider building it to states where electricity is cheap such as 
                                                Montana, Washington, Oregon or Kentucky. While, to reduce material and labor costs consider states 
                                                like Louisiana, Delaware, Texas or Kentucky. This can be further expanded to include many features to 
                                                build a great recommendation tool companies."),
                                         tags$br(),
                                         img(src='1.jpg', align = "center", width = 1100),
                                         tags$br(),
                                         tags$br(),
                                         p("4. The profit-loss analysis across all manufacturing sectors suggests that the net earning is correlated 
                                                with the data science-related spending (e.g., expenses related to data analysis, purchasing computational 
                                           instruments and software). However, this needs to be better examined due to other contributing factors."),
                                         tags$br(),
                                         plotlyOutput("plot3", height = 600)
                                         
                                         
                                       )
                                     )
                                   )
                              
                            )
                          )
                        )
                ),
                tabItem(tabName = "app",
                        fluidPage(
                          title = "MN",
                          column(width = 2,
                          box(
                            title = "Query Builder",
                            status = "primary",
                            width = 12,
                            solidHeader = TRUE,
                            background = "navy",
                            box(
                              width = 12,
                              status = "primary",
                              solidHeader = FALSE,
                              background = "navy",
                              uiOutput("levelQueryUi")
                            ))),
                            column( width = 10,
                                    box(
                                      title = textOutput("Design1"), 
                                      status = "primary",
                                      width = 12,
                                      #height = 1500,
                                      solidHeader = TRUE,
                                      collapsible = TRUE,
                                      fluidRow(
                                        box(
                                          width = 12,
                                          title = "Nationwide Average Costs",
                                          status = "warning",
                                          solidHeader = FALSE,
                                          collapsible = TRUE,
                                          valueBoxOutput("fringe_ind", width = 3),
                                          valueBoxOutput("ele_ind", width = 3),
                                          valueBoxOutput("contract_ind", width = 3),
                                          valueBoxOutput("material_ind", width = 3),
                                          p("1. This is the employer's costs for social security tax, unemployment tax, workers' compensation insurance, 
                                            state disability insurance pension plans, stock purchase plans, union-negotiated benefits, 
                                            life insurance premiums, and insurance premiums on hospital and medical plans per employees", 
                                            style = "margin-left: 15px; font-size: 110%;"),
                                          p("2. Average cost of electricity per kWh",style = "margin-left: 15px; font-size: 110%;"),
                                          p("3. Average cost of contract work per employee",style = "margin-left: 15px; font-size: 110%;"),
                                          p("4. Average cost of items (material) consumed or put into production per employee",
                                            style = "margin-left: 15px;font-size: 110%;")),
                                        box(
                                          width = 12,
                                          title = textOutput("Design2"),
                                          status = "success",
                                          solidHeader = FALSE,
                                          collapsible = TRUE,
                                          p("The recommendations were made by accounting fringe benefits, electricity, contract and material costs. Average 
                                            saving in % is the difference between the average cost of operation for the sector calculated nationwide 
                                            and the average cost in each state. Below are the top two states with the highest saving.", style = "margin-left: 15px;font-size: 110%;"),
                                          tags$br(),
                                          box(width = 6,
                                              solidHeader = TRUE,
                                              collapsible = FALSE,
                                              
                                              title = textOutput("state1"),
                                              h3("Average Saving in %", style = "text-align: center"),
                                              tags$style(type="text/css", HTML("#per_state1 { margin: 0px 85px}")), htmlOutput("per_state1"),
                                              tags$br(),
                                              h3("Top Manufacturing Sectors in the State", style = "text-align: center"),
                                              h4("(in Millions of Dollars, 2014)", style = "text-align: center"),
                                              tags$br(),
                                              plotlyOutput("plot4", height = 300)
                                              ),
                                          box(width = 6,
                                              solidHeader = TRUE,
                                              collapsible = FALSE,
                                              title = textOutput("state2"),
                                              h3("Average Saving in %", style = "text-align: center"),
                                              tags$style(type="text/css", HTML("#per_state2 { margin: 0px 85px}")), htmlOutput("per_state2"),
                                              tags$br(),
                                              h3("Top Manufacturing Sectors in the State", style = "text-align: center"),
                                              h4("(in Millions of Dollars, 2014)", style = "text-align: center"),
                                              tags$br(),
                                              plotlyOutput("plot5", height = 300)
                                        )
                                      )
                                    )
                              
                            )
                            )
                          )
                        ),
                tabItem(tabName = "plan",
                        fluidPage(
                          column(
                            width = 12,
                            box(
                              width = 12,
                              title = "Future Plans",
                              status = "primary",
                              collapsible = TRUE,
                              solidHeader = TRUE,
                              p("Following are some of the ideas that I would like to pursue in future:", style = "font-size: 130%;"),
                              tags$ul(
                               
                                tags$li("Expand current analysis to include different expense categories. For example, fuel expenses, health insurance, electricity
                                        generated by the sector etc.",style = "font-size: 130%;"),
                                p(""),
                                tags$li("Incorporate sector wise data for more precise recommendations.",style = "font-size: 130%;"),
                                p(""),
                                tags$li("Include uncertanities into the calculations.",style = "font-size: 130%;"),
                                p(""),
                                tags$li("Use machine learning techniques (e.g., regression, matrix factorization) to build a more advanced recommender system.",style = "font-size: 130%;"),
                                p("")
                                
                                )
                              
                                
                              
                              
                            )
                          )
                        ))
                
                      
                  
                )
                
                )
)
 


            
                
              