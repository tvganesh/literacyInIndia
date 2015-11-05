#########################################################################################################
#
# Title : Literacy in India : A DeepR dive
# Designed and developed by: Tinniam V Ganesh
# Date : 5 Nov 2015
# File: server.R
# More details: https://gigadom.wordpress.com/
#
#########################################################################################################
library(shiny)
library(shinyjs)
library(rgeos)
library(maptools)
library(ggplot2)
library(dplyr)
library(stringr)


source("./eduLevels.R", local=TRUE)
source("./stateLiteracy.R", local=TRUE)
source("./districtLiteracy.R", local=TRUE)
source("./indiaLiteracy.R", local=TRUE)

# Load the data
load(file="education.RData")

colnames(a) <- gsub("Educational.level...","",colnames(a))

# Clean the Area.Name
a$Area.Name <-gsub("State - ","",a$Area.Name)
a$Area.Name <- gsub("\\d+","",a$Area.Name)
# Remove trailing spaces
a$Area.Name <- gsub("[[:space:]]*$","",a$Area.Name)

# Get unique state names
states <- unique(a$Area.Name)

# Load the fortified INDIa shapefile
load(file="./INDIA_SHP/ind.RData")

shinyServer(function(input, output,session) {
    
    # 1. State plot
    # Update the drop down with all states
    updateSelectizeInput(session, 'state1', choices = states, server = TRUE,selected="INDIA")
    toggle("inputBox1")
    toggle("state1")
    
    # Draw the plot
    output$statePlot <- renderPlot({  
        
        # If the user requested "All' plot the combined plot else draw the barplot
        if(input$type1 == "All") {
            allPercent(a,input$region1,input$state1, input$literacy1)
        } else {
            stateLiteracy(a,input$type1,input$region1, input$state1,input$literacy1)
        }
    })
    
    # 2. Draw the educational institutions attended vs Age plot
    # Update the drop down with all states
    updateSelectizeInput(session, 'state', choices = states, server = TRUE,selected="INDIA")
    toggle("inputBox")
    toggle("state")
    output$distPlot <- renderPlot({        
        educationalLevels(a,input$type,input$region, input$state,input$status)    
    })
    
    # 3. India literacy plot
    # Draw the India Literacy plot
    output$indiaLiteracy <- renderPlot({         
        indiaLiteracy(input$region2,input$type2,input$literacyLevel)
    })
    
    # 4. Draw the literacy in the districts
    # Update the drop down with all states
    # Remove states for which the district data does not match the map data
    u <- c(-1,-2,-5,-8,-12,-14,-15,-16,-17,-18,-26,-27,-31,-32,-35,-36)
    updatedStates <- states[u]
    updateSelectizeInput(session, 'state2', choices = updatedStates, server = TRUE,selected="KARNATAKA")
    toggle("inputBox2")
    toggle("state2")
    output$districtPlot <- renderPlot({          
        districtEdu(input$state2,input$type3,input$literacy3)
    })
    
    
})
