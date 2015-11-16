#########################################################################################################
#
# Title : Literacy in India : A DeepR dive
# Designed and developed by: Tinniam V Ganesh
# Date : 5 Nov 2015
# File: ui.R
# More details: https://gigadom.wordpress.com/
#
#########################################################################################################
library(shiny)
library(shinyjs)

shinyUI(navbarPage(useShinyjs(),
                   "Literacy in India",
                   # Create a tab for State literacy
                   tabPanel("State literacy",                           
                            titlePanel("Literacy in each state"),                          
                            fluidRow(
                                column(3,
                                       code(id = "inputBox1", "Please wait, loading ..."),
                                       hidden(selectizeInput(
                                           
                                           "state1", label = "States", choices = NULL,multiple=FALSE,selected="Assam",
                                           options = list(create = TRUE,placeholder = 'Choose the state'))
                                           
                                       ),
                                       radioButtons("region1", label = h3("Region"),
                                                    choices = list("Urban+Rural" = "Total",
                                                                   "Rural" = "Rural", 
                                                                   "Urban" = "Urban"),                                                   
                                                    selected = "Total"),
                                       radioButtons("type1", label = h3("Who"),
                                                    choices = list("Persons" = "Persons",
                                                                   "Males" = "Males", 
                                                                   "Females" = "Females",
                                                                   "All (Persons,Males & Females)" = "All"),                                                                  
                                                    selected = "Persons"),
                                       radioButtons("literacy1", label = h3("Literacy"),
                                                    choices = list("Attending Edn" = "Edu",
                                                                   "Illiterate" = "Illiterate", 
                                                                   "Literate" = "Literate",
                                                                   "Literate without Edn" = "LiterateWithoutEdu",
                                                                   "Below Primary" = "BelowPrimary",
                                                                   "Primary" = "Primary",
                                                                   "Middle" = "Middle",
                                                                   "Matric-Sec" = "MatricSec",
                                                                   "Higher Sec-Intermediate" = "HigherSecIntmdt",
                                                                   "Non-Technical Diploma" = "NonTechnicalDiploma",
                                                                   "Technical Diploma" = "TechnicalDiploma",
                                                                   "Graduate" = "Graduate",
                                                                   "Unclassified" = "Unclassified"
                                                                   ),                                                                  
                                                    selected = "Edu")
                                ),                                
                                column(8,
                                       plotOutput("statePlot")
                                ),
                                column(7, offset=4,
                                       tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                       tags$h5((tags$i("Nov 5,2015"))),
                                       tags$h6("Data source:https://data.gov.in")
                                )
                                
                            )                            
                   ),
                   # Create a tab for Age vs Educational instituations attendance
                   tabPanel("Educational inst. attendance",
                            titlePanel("India Literacy"),                            
                            fluidRow(
                                column(3,
                                       code(id = "inputBox", "Please wait, loading ..."),
                                       hidden(selectizeInput(
                                           "state", label = "States", choices = NULL,multiple=FALSE,selected="Assam",
                                           options = list(create = TRUE,placeholder = 'Choose the state'))
                                           
                                       ),
                                       radioButtons("region", label = h3("Region"),
                                                    choices = list("Urban+Rural" = "Total",
                                                                   "Rural" = "Rural", 
                                                                   "Urban" = "Urban"),                                                   
                                                    selected = "Total"),
                                       radioButtons("type", label = h3("Who"),
                                                    choices = list("Persons" = "Persons",
                                                                   "Males" = "Males", 
                                                                   "Females" = "Females"),                                                                  
                                                    selected = "Persons"),
                                       radioButtons("status", label = h3("Plot type"),
                                                    choices = list("Normal" = "Normal",
                                                                   "Flipped" = "Flipped"),                                                                  
                                                    selected = "Normal")
                                ),                                
                                column(9,
                                       plotOutput("distPlot")
                                ),
                                column(7, offset=4,
                                       tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                       tags$h5((tags$i("Nov 5,2015"))),
                                       tags$h6("Data source:https://data.gov.in")
                                )
                                                               
                            )      
                                                        
                   ),
                   # Create a tab for literacy across India
                   tabPanel("Literacy across India",
                            # Application title
                            titlePanel("Literacy across India"),
                            
                            
                            fluidRow(
                                
                                column(3,
                                       radioButtons("region2", label = h3("Region"),
                                                    choices = list("Urban+Rural" = "Total",
                                                                   "Rural" = "Rural", 
                                                                   "Urban" = "Urban"),                                                   
                                                    selected = "Total"),
                                       radioButtons("type2", label = h3("Who"),
                                                    choices = list("Persons" = "Persons",
                                                                   "Males" = "Males", 
                                                                   "Females" = "Females"),                                                                  
                                                    selected = "Persons"),
                                       radioButtons("literacyLevel", label = h3("Literacy Level"),
                                                    choices = list("Attending Education Inst" = "AttendingEdu",
                                                                   "Illiterate" = "Illiterate", 
                                                                   "Literate with no edn" = "LiterateNoEdu",
                                                                   "Below Primary" = "BelowPrimary",
                                                                   "Primary" = "Primary",
                                                                   "Middle school" = "Middle",
                                                                   "Matric or Secondary" = "MatricSecondary",
                                                                   "HigherSec-Intermdt-PU" = "HigherSecIntmdtPU",
                                                                   "Non Technical Diploma" = "NonTechnicalDiploma",
                                                                   "Technical Diploma" = "TechnicalDiploma",
                                                                   "Graduate and Above" = "GraduateAndAbove",
                                                                   "Unclassified" = "Unclassified"),
                                                    
                                                    selected = "AttendingEdu")
                                       
                                ),
                                
                                column(6,
                                       plotOutput("indiaLiteracy")
                                ),
                                column(7, offset=4,
                                       tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                       tags$h5((tags$i("Nov 5,2015"))),
                                       tags$h6("Data source:https://data.gov.in")
                                       
                                )                                
                                
                            )                   
                            
                   ),
                   # Create a tab for District wise literacy
                   tabPanel("Districtwise Literacy",
                            # Application title
                            titlePanel("Literacy within states"),
                            
                            
                            fluidRow(
                                column(3,
                                       code(id = "inputBox2", "Please wait, loading ..."),
                                       hidden(selectizeInput(
                                           "state2", label = "States", choices = NULL,multiple=FALSE,selected="Assam",
                                           options = list(create = TRUE,placeholder = 'Choose the state'))
                                           
                                       ),
                                       radioButtons("type3", label = h3("Who"),
                                                    choices = list("Persons" = "Persons",
                                                                   "Males" = "Males", 
                                                                   "Females" = "Females"),                                                                  
                                                    selected = "Persons"),
                                       radioButtons("literacy3", label = h3("Literacy"),
                                                    choices = list("Attending Edn" = "Edu",
                                                                   "School" = "School", 
                                                                   "College" = "College",
                                                                   "Vocational Inst" ="VocInst",
                                                                   "Other Inst" = "OthrInst",
                                                                   "Literacy Center" = "LitCntr"),                                                                  
                                                    selected = "Edu")
                                       
                                ),
                                
                                # Show a plot of the generated distribution
                                
                                column(6,
                                       plotOutput("districtPlot")
                                ),
                                column(7, offset=4,
                                       tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                       tags$h5((tags$i("Nov 5,2015"))),
                                       tags$h6("Data source:https://data.gov.in")
                                )
                                
                                
                            )                   
                            
                   )
              
))

