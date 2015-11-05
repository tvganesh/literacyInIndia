#########################################################################################################
#
# Title : Literacy in India : A DeepR dive
# Designed and developed by: Tinniam V Ganesh
# Date : 5 Nov 2015
# File: stateLiteracy.R
# More details: https://gigadom.wordpress.com/
#
#########################################################################################################

stateLiteracy <- function(df,peopleType,type,state,literacyLevel) {
    # Set defaults values while input is being read to avoid error
    if(state ==""){
        state = "INDIA"
    }
    if(type ==""){
        type ="Total"
    }
    if(peopleType ==""){
        peopleType="Persons"
    }
    if(literacyLevel ==""){
        literacyLevel="Edu"
    }
    
    # Subset based on state and region
    b <- filter(a,Area.Name==state & Total..Rural..Urban==type)
       
    # Select colums from 7 - 19
    c <- b[,7:19]
    
    # Set names of columns
    names(c) <-c("Age","Persons","Males","Females","EduPersons","EduMales",
                 "EduFemales","IlliteratePersons","IlliterateMales",
                 "IlliterateFemales","LiteratePersons","LiterateMales","LiterateFemales")
    
    # Subset columns with peopleType
    people <- select(c,matches(peopleType,ignore.case=FALSE))
    
    # Calculate  percent 
    peoplePercent <- people[,2:4]/people[,1]*100
    
    # Add the age column
    peoplePercent <- cbind(c[1],peoplePercent)
    
    
    # Drop the 1st row
    peoplePercent <- peoplePercent[2:length(rownames(peoplePercent)),]
    
    # Do the same for the India to plot the national average
    m <- filter(a,Area.Name=="INDIA" & Total..Rural..Urban==type)
    
    # Select colums from 7 - 19
    n <- m[,7:19]
    # Set names
    names(n) <-c("Age","Persons","Males","Females","EduPersons","EduMales",
                 "EduFemales","IlliteratePersons","IlliterateMales",
                 "IlliterateFemales","LiteratePersons","LiterateMales","LiterateFemales")
    
    
    # Subset columns with persons
    natPeople <- select(n,matches(peopleType,ignore.case=FALSE))
    
    # Calculate males percent as percent of total males
    natPeoplePercent <- natPeople[,2:4]/natPeople[,1]*100
    
    # Add the age column
    natPeoplePercent <- cbind(c[1],natPeoplePercent)
    
    # Drop the 1st row
    natPeoplePercent <- natPeoplePercent[2:length(rownames(natPeoplePercent)),]
    
    
    #Use a color palette
    pal <- colorRampPalette(c("yellow","blue"))
    colors=pal(22)
    
    # Create the column from the literacyLevel input
    u <- literacyLevel
    v <- paste(literacyLevel,peopleType,sep="")
    w <- which(names(peoplePercent) == v)
    
    # Store the midpoints of bars for use in plotting later
    natPeople <- barplot(natPeoplePercent[,w],names.arg=natPeoplePercent$Age,
                         col="white",border=NA)
    # Expand Edu for the title
    if(literacyLevel =="Edu"){
        y<- "attending edu. Institutes"
    } else {
        y <- literacyLevel
    }
    # Draw the barplot
    atitle <- paste("Percentage of",type,peopleType,y,"in ",state)
    barplot(peoplePercent[,w],names.arg=peoplePercent$Age,col=colors,ylim=c(0,100),xlab="Age",
            ylab="Percent",main=atitle)
    
    # Draw the national average
    with(data=natPeoplePercent,lines(natPeople,natPeoplePercent[,w],col="black",lty=3,lwd=3))
    legend(x="topright",c("National Average"), lty=c(3),   
           lwd=c(3),col=c("black"))

    
    
    
    
}

# The below function plots all - Persons, Males and Females as a continous plot
allPercent <- function(df,region,state, literacyLevel) {
    
    b <- filter(a,Area.Name==state & Total..Rural..Urban==region)
    
    # Select colums from 7 - 19
    c <- b[,7:19]
    # Set names
    names(c) <-c("Age","Persons","Males","Females","EduPersons","EduMales",
                 "EduFemales","IlliteratePersons","IlliterateMales",
                 "IlliterateFemales","LiteratePersons","LiterateMales","LiterateFemales")
    
    
    
    
    males <- select(c,matches("Males",ignore.case=FALSE))
    females <- select(c,matches("Females",ignore.case=FALSE))
    persons <- select(c,matches("Persons",ignore.case=FALSE))
    
    
    # Calculate males percent as percent of total males
    malesPercent <- males[,2:4]/males[,1]*100
    # Calculate females percent as percent of total females
    femalesPercent <- females[,2:4]/females[,1]*100
    # Calculate persons percent as percent of total persons
    personsPercent <- persons[,2:4]/persons[,1]*100
    
    # Add the age column
    malesPercent <- cbind(c[1],malesPercent)
    femalesPercent <- cbind(c[1],femalesPercent)
    personsPercent <- cbind(c[1],personsPercent)
    
    # Drop the 1st row
    malesPercent <- malesPercent[2:length(rownames(malesPercent)),]
    femalesPercent <- femalesPercent[2:length(rownames(femalesPercent)),]
    personsPercent <- personsPercent[2:length(rownames(personsPercent)),]
    
    
    
    #Use a color palette
    pal <- colorRampPalette(c("yellow","blue"))
    colors=pal(22)
    
    # Create the column from the literacyLevel input
    u <- literacyLevel
    v <- paste(literacyLevel,"Persons",sep="")
    w <- which(names(personsPercent) == v)
    
    
    # Save the mid-points of the batplots as x-coordinates
    persons <- barplot(personsPercent[,w],names.arg=personsPercent$Age,
                       col="white",border=NA,ylim=c(0,100))
    atitle=paste('Percentages of',literacyLevel,"Persons,Males and Females in",state)
    plot(persons,personsPercent[,w],col="black",ylim=c(0,100),type="l",lty=1,lwd=2,
         xlab="Age",ylab="Percentage",main=atitle)
    lines(persons,malesPercent[,w],col="blue",lty=2,lwd=2)
    lines(persons,femalesPercent[,w],col="red",lty=3,lwd=2)
    legend(x="topright",c("Persons","Males","Females"), lty=c(1,2,3),   
           lwd=c(2,2,2),col=c("black","blue","red"))
    
}
