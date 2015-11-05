#########################################################################################################
#
# Title : Literacy in India : A DeepR dive
# Designed and developed by: Tinniam V Ganesh
# Date : 5 Nov 2015
# File: eduLevels.R
# More details: https://gigadom.wordpress.com/
#
#########################################################################################################
educationalLevels <- function(df,peopleType,region,state,status) {
    # Set the defaults values while the values are being read to prevent error
    if(state ==""){
        state = "INDIA"
    }
    if(region ==""){
        region ="Total"
    }
    if(peopleType ==""){
        peopleType="Persons"
    }
    if(status ==""){
        status="Normal"
    }
    
    # Filter by Area.Name and region
    b <- filter(df,Area.Name==state & Total..Rural..Urban==region)

    # Subset columns with the peopleType
    people <- select(b,matches(peopleType,ignore.case=FALSE))
    
    # Remove the "..."
    l <-paste("...",peopleType,sep="")
    # Set the columns
    names(people) <- gsub(l,"",names(people))

    
    # Compute the percentage of people in the age group (4,5,6..)  as a percentage of total people in group
    # The total people in the group are in the 1st row - people[1,]
    l <- dim(people)
    for(i in 2:l[1]) {
        for(j in 2:l[2]) {
            # people[5,8]  <- people[5,8]/people[1,8] *100 
            people[i,j] <- people[i,j]/people[1,j]*100
            
        }
    }
    
    # Add the age column to people
    age <- b[,7]
    people <- cbind(age,people)
    
    # Choose the columns and subset
    v <- c(1,7:15)
    m1 <- people[2:23,v]
    
    # Set the names
    names(m1) <- c("age","Below primary","Primary","Middle","MatricSecondary","HigherSecIntmdtPU",
                   "NonTechnicalDiploma","TechnicalDiploma","GraduateAndAbove","Unclassified")
  
    # Needed to add a '0' so that the numeric and lexicographic ordering is fine
    m1$age = as.character(m1$age)
    m1$age[2] ="05"
    m1$age[3] ="06"
    m1$age[4] ="07"
    m1$age[5] ="08"
    m1$age[6] ="09"
    
    
    # Change from wide to narrow and based on age
    m2 <- melt(m1,id.vars="age")
    
    # Add a title
    atitle <- paste("School/college",tolower(region),tolower(peopleType), "in", state,"vs. age groups")
    
    # Draw a normal plot if user selected normal else flip
    if(status == "Normal"){
        ggplot(m2,aes(x=age,y=value,fill=variable)) +     geom_bar(stat = "identity") +
            xlab("Age") + ylab("Percentage from different age groups") +
            ggtitle(atitle)
    } else {
        ggplot(m2,aes(x=variable,y=value,fill=age)) +     geom_bar(stat = "identity") +
            xlab("Age") + ylab("Percentage from different age groups") +
            ggtitle(atitle)
    }
    
}
