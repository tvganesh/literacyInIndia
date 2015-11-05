#########################################################################################################
#
# Title : Literacy in India : A DeepR dive
# Designed and developed by: Tinniam V Ganesh
# Date : 5 Nov 2015
# File: indiaLiteracy
# More details: https://gigadom.wordpress.com/
#
#########################################################################################################
indiaLiteracy <- function(region,peopleType,literacyLevel)
{
    
    c <- filter(a,Age.group=="All ages" & Total..Rural..Urban==region)
    
    # Subset columns with persons
    people <- select(c,matches(peopleType,ignore.case=FALSE))
    
    peoplePercent <- people[,2:14]/people[,1]*100
    
    # Set column names
    names(peoplePercent) <- c("AttendingEdu","Illiterate","Literate","LiterateNoEdu",
                              "BelowPrimary","Primary","Middle","MatricSecondary","HigherSecIntmdtPU",
                              "NonTechnicalDiploma","TechnicalDiploma","GraduateAndAbove","Unclassified")
    
    
    # Add the age column
    peoplePercent <- cbind(c[5],peoplePercent)
    
    
    # Remove the row corresponding to India
    peoplePercent <- peoplePercent[2:length(rownames(peoplePercent)),]
    peoplePercent$Area.Name <-gsub("State - ","",peoplePercent$Area.Name)
    peoplePercent$Area.Name <- gsub("\\d+","",peoplePercent$Area.Name)
    
    # Remove trailing spaces
    peoplePercent$Area.Name <- gsub("[[:space:]]*$","",peoplePercent$Area.Name)
    
    
    # Set the names of states as it is in the map file
    peoplePercent[peoplePercent$Area.Name=="JAMMU & KASHMIR",]$Area.Name = "Jammu And Kashmir"    
    peoplePercent[peoplePercent$Area.Name=="HIMACHAL PRADESH",]$Area.Name = "Himachal Pradesh"
    peoplePercent[peoplePercent$Area.Name=="PUNJAB",]$Area.Name = "Punjab"
    peoplePercent[peoplePercent$Area.Name=='UTTARANCHAL',]$Area.Name = "Uttarakhand"
    peoplePercent[peoplePercent$Area.Name=="CHANDIGARH",]$Area.Name = "CHANDIGARH"    
    peoplePercent[peoplePercent$Area.Name=="HARYANA",]$Area.Name = "Haryana"   
    peoplePercent[peoplePercent$Area.Name=="DELHI",]$Area.Name = "Nct Of Delhi"
    peoplePercent[peoplePercent$Area.Name=="RAJASTHAN",]$Area.Name = "Rajasthan"
    peoplePercent[peoplePercent$Area.Name=="UTTAR PRADESH",]$Area.Name = "Uttar Pradesh"
    peoplePercent[peoplePercent$Area.Name=="BIHAR",]$Area.Name = "Bihar"
    peoplePercent[peoplePercent$Area.Name=="SIKKIM",]$Area.Name = "Sikkim"    
    peoplePercent[peoplePercent$Area.Name=="ARUNACHAL PRADESH",]$Area.Name = "Arunachal Pradesh"
    peoplePercent[peoplePercent$Area.Name=="NAGALAND",]$Area.Name = "Nagaland"
    peoplePercent[peoplePercent$Area.Name=="MANIPUR",]$Area.Name = "Manipur"
    peoplePercent[peoplePercent$Area.Name=="MIZORAM",]$Area.Name = "Mizoram"    
    peoplePercent[peoplePercent$Area.Name=="TRIPURA",]$Area.Name = "Tripura"
    peoplePercent[peoplePercent$Area.Name=="MEGHALAYA",]$Area.Name = "Meghalaya"
    peoplePercent[peoplePercent$Area.Name=="ASSAM",]$Area.Name = "Assam"
    peoplePercent[peoplePercent$Area.Name=="WEST BENGAL",]$Area.Name = "West Bengal"
    peoplePercent[peoplePercent$Area.Name=="JHARKHAND",]$Area.Name = "Jharkhand"    
    peoplePercent[peoplePercent$Area.Name=="ORISSA",]$Area.Name = "Orissa"
    peoplePercent[peoplePercent$Area.Name=="CHHATTISGARH",]$Area.Name = "Chhattisgarh"
    peoplePercent[peoplePercent$Area.Name=="MADHYA PRADESH",]$Area.Name = "Madhya Pradesh"    
    peoplePercent[peoplePercent$Area.Name=="GUJARAT",]$Area.Name = "Gujarat"
    peoplePercent[peoplePercent$Area.Name=="DAMAN & DIU",]$Area.Name = "DAMAN AND DIU"
    peoplePercent[peoplePercent$Area.Name=="DADRA & NAGAR HAVELI",]$Area.Name = "DADRA AND NAGAR HAVELI"    
    peoplePercent[peoplePercent$Area.Name=="MAHARASHTRA",]$Area.Name = "Maharashtra"
    peoplePercent[peoplePercent$Area.Name=="ANDHRA PRADESH",]$Area.Name = "Andhra Pradesh"
    peoplePercent[peoplePercent$Area.Name=="KARNATAKA",]$Area.Name = "Karnataka"
    peoplePercent[peoplePercent$Area.Name=="GOA",]$Area.Name = "Goa"   
    peoplePercent[peoplePercent$Area.Name=="LAKSHADWEEP",]$Area.Name = "LAKSHADWEEP"
    peoplePercent[peoplePercent$Area.Name=="KERALA",]$Area.Name = "Kerala"
    peoplePercent[peoplePercent$Area.Name=="TAMIL NADU",]$Area.Name = "Tamil Nadu"
    peoplePercent[peoplePercent$Area.Name=="PONDICHERRY",]$Area.Name = "Pondicherry"
    peoplePercent[peoplePercent$Area.Name=="ANDAMAN & NICOBAR ISLANDS",]$Area.Name = "ANDAMAN AND NICOBAR ISLANDS"
    
    # Get the colmn numbers which match with literacyLevel
    w <- which(names(peoplePercent) == literacyLevel)
    
    # Create a data frame with Area.Name and selected column based on user input
    df = data.frame(peoplePercent$Area.Name,peoplePercent[,w])
    names(df) <- c("Area.Name","percentages")
    
    # Show the bottom 5 for illiterate and Top 5 for all othrs
    if(literacyLevel == "Illiterate"){
        m <- head(arrange(df,percentages),5)
        
    } else {
        m <- head(arrange(df,desc(percentages)),5)
    }
    
    # Create a vector of labels
    literacy <- paste(m$Area.Name,"(",round(m$percentages,1),")",sep="")
    labels <- data.frame(
        xc = c(90,90,90,90,90), 
        yc = c(11,10,9,8,7), 
        label = as.vector(literacy) 
        
    )
    # Set title and plot
    atitle <- paste("Percentage",literacyLevel,"in",region,peopleType)
    ggplot() + geom_map(data=df, aes(map_id = Area.Name, fill = percentages),
                        map = ind,,color="black",size=0.25) + 
        expand_limits(x = ind$long, y = ind$lat) + 
        scale_fill_distiller(name="Percent", palette = "OrRd")+
        geom_text(data = labels, aes(x = xc, y = yc, label = label))+
        ggtitle(atitle) +
        geom_text(aes(label="Top 5",90,12),colour="blue")+
        geom_text(aes(label="Data source:https://data.gov.in",85,40)) +
        xlab("Longitude") + ylab("Latitude")
    
    
    
    
    
}
