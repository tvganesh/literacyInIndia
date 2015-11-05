library(rgeos)
library(maptools)
library(ggplot2)
library(dplyr)
library(stringr)
library(reshape2)
library(RColorBrewer)


#a <- read.csv("india.csv")

educationalLevels2 <- function(df,peopleType,region,state,status) {
    #educationalLevels(a,input$type,input$region, input$state,input$status)
    #df <-a
    ##region<-"Total"
    ##peopleType <-"Persons"
    #state<-'INDIA'
    #status<-"Normal"
    #print(peopleType)
    #print(state)
    #print(type)
    #dim(df)
    #status="Flipped"
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

    b <- filter(df,Area.Name==state & Total..Rural..Urban==region)
    #b <- filter(a,Area.Name=="KERALA" & Total..Rural..Urban=="Rural")
    print("Reached11")
    # Subset columns with persons
    people <- select(b,matches(peopleType,ignore.case=FALSE))
    l <-paste("...",peopleType,sep="")
    names(people) <- gsub(l,"",names(people))
    print("Reached")
   
    # Compute the percentage of people in the age group (4,5,6..)  as a percentage of total people in group  
    l <- dim(people)
    for(i in 2:l[1]) {
        for(j in 2:l[2]) {
            # people[5,8]  <- people[5,8]/people[1,8] *100 - In
            people[i,j] <- people[i,j]/people[1,j]*100
                           
        }
    }
    
    age <- b[,7]
    print("Reached 13")
    print(length(age))
    print(dim(people))
    people <- cbind(age,people)
    
    print("Reached 14")
    v <- c(1,7:15)
    m1 <- people[2:23,v]
    names(m1) <- c("age","Below primary","Primary","Middle","MatricSecondary","HigherSecIntmdtPU",
                              "NonTechnicalDiploma","TechnicalDiploma","GraduateAndAbove","Unclassified")
  
    print("Reached 15")
    # Needed to add a '0' so that the numeric and lexicographic ordering is fine
    m1$age = as.character(m1$age)
    m1$age[2] ="05"
    m1$age[3] ="06"
    m1$age[4] ="07"
    m1$age[5] ="08"
    m1$age[6] ="09"
    print("Reached 16")
    m2 <- melt(m1,id.vars="age")
    
    # Add a title
    atitle <- paste("School/college",tolower(region),tolower(peopleType), "in", state,"vs. age groups")
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

################################################################################################
#
#
###############################################################################################

bar1 <- function(df,peopleType,type,state,literacyLevel) {
    dim(df)
    print(peopleType)
    print(type)
    print(state)
    print(literacyLevel)
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
    #a <- read.csv("education.csv")
    #colnames(a) <- gsub("Educational.level...","",colnames(a))
    
    
    #a$Area.Name <-gsub("State - ","",a$Area.Name)
    #a$Area.Name <- gsub("\\d+","",a$Area.Name)
    
    # Remove trailing spaces
    #a$Area.Name <- gsub("[[:space:]]*$","",a$Area.Name)
    
    b <- filter(a,Area.Name==state & Total..Rural..Urban==type)
    #b <- filter(a,Area.Name=="KERALA" & Total..Rural..Urban=="Rural")
    
  
    
    # Select colums from 8 - 21
    c <- b[,7:19]
    # Set names
    names(c) <-c("Age","Persons","Males","Females","EduPersons","EduMales",
                 "EduFemales","IlliteratePersons","IlliterateMales",
                 "IlliterateFemales","LiteratePersons","LiterateMales","LiterateFemales")
    
    # Subset columns with persons
    people <- select(c,matches(peopleType,ignore.case=FALSE))
    
    # Calculate males percent as percent of total males
    peoplePercent <- people[,2:4]/people[,1]*100
    
    # Add the age column
    peoplePercent <- cbind(c[1],peoplePercent)
    
    
    # Drop the 1st row
    peoplePercent <- peoplePercent[2:length(rownames(peoplePercent)),]
    
    #Do the same for the India to plot the national average
    m <- filter(a,Area.Name=="INDIA" & Total..Rural..Urban==type)
    
    # Select colums from 8 - 21
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
    
    natPeople <- barplot(natPeoplePercent[,w],names.arg=natPeoplePercent$Age,
                         col="white",border=NA)
    if(literacyLevel =="Edu"){
        y<- "attending edu. Institutes"
    } else {
        y <- literacyLevel
    }
    atitle <- paste("Percentage of",type,peopleType,y,"in ",state)
    barplot(peoplePercent[,w],names.arg=peoplePercent$Age,col=colors,ylim=c(0,100),xlab="Age",
            ylab="Percent",main=atitle)
    
    with(data=natPeoplePercent,lines(natPeople,natPeoplePercent[,w],col="black",lty=3,lwd=3))
    legend(x="topright",c("National Average"), lty=c(3),   
           lwd=c(3),col=c("black"))
   
    
    ################################################################################
    ################################################################################
   
   
    
    
    
}

#########################################################################################
allPercent1 <- function(df,region,state, literacyLevel) {
    #df =a
    #type="Rural"
    #state="INDIA"
    #literacyLevel="Literate"
    print(region)
    print(state)
    print(literacyLevel)
    b <- filter(a,Area.Name==state & Total..Rural..Urban==region)
    
    # Select colums from 8 - 21
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

##############################################

districtEdu1 <- function(state,peopleType,eduInst){
    #state <-"BIHAR"
    #peopleType="Persons"
    #eduInst ="Schools"
    
    # Manually chanhe with only the first letter 
    if(state=="PUNJAB"){
        state<-"Punjab"
    } else if (state == "HIMACHAL PRADESH"){
        state <- "Himachal Pradesh"
    } else if (state == "UTTARANCHAL"){
        state <- "Uttaranchal"
    } else if(state == "HARYANA"){
        state = "Haryana"
    } else if (state == "RAJASTHAN"){
        state <- "Rajasthan"
    } else if (state == "UTTAR PRADESH"){
        state <- "Uttar Pradesh"
    } else if (state == "BIHAR"){
        state <- "Bihar"
    } else if (state == "ARUNACHAL PRADESH"){
        state <- "Arunachal Pradesh"
    } else if (state == "ASSAM"){
        state <- "Assam"
    } else if (state == "WEST BENGAL"){
        state <- "West Bengal"
    } else if (state == "JHARKHAND"){
        state <- "Jharkhand"
    } else if (state == "ORISSA"){
        state <- "Orissa"
    } else if (state == "CHHATTISGARH"){
        state <- "Chhattisgarh"
    } else if (state == "MADHYA PRADESH"){
        state <- "Madhya Pradesh"
    } else if (state == "GUJARAT"){
        state <- "Gujarat"
    } else if (state == "MAHARASHTRA"){
        state <- "Maharashtra"
    }else if (state == "ANDHRA PRADESH"){
        state <- "Andhra Pradesh"
    } else if (state == "KARNATAKA"){
        state <- "Karnataka"
    } else if (state == "KERALA"){
        state <- "Kerala"
    }else if (state == "TAMIL NADU"){
        state <- "Tamil Nadu"
    }
    
    
    
    print(state)
    ind_dist <- readShapeSpatial("./IND_adm/IND_adm2.shp")
    district_df = ind_dist@data
    state_dist_df = data.frame(district_df[grep(state,district_df$NAME_1),])
    polygon_list = list()
    for (istr in rownames(state_dist_df)){
        i = as.numeric(istr) + 1
        tmp = ind_dist@polygons[i]
        polygon_list = c(polygon_list,tmp)
    }
    print("reached")
    # construct a new shape file with the  districts
    dist_spatial = SpatialPolygons(polygon_list,1:length(polygon_list))
    dist_spatial_frame = SpatialPolygonsDataFrame(dist_spatial,data=state_dist_df)
    print(state)
    shpFile <- paste( state,".shp",sep="")
    print(shpFile)
    print("reached1")
    
    districtDir <- paste("./district/",shpFile,sep="")
    writeSpatialShape(dist_spatial_frame,districtDir)
    dist_df = readShapePoly(districtDir)
    print("reached here")
    plot(dist_df)
    
    dist <- fortify(dist_df, region = "NAME_2")
    print("reached here11")
    csvFile <- paste(state,".csv",sep="")
    csvDir <- paste("./data/",csvFile,sep="")
    stateData <- read.csv(csvDir)
    
    print("reached here11")
    a <- filter(stateData,Age.group=="All ages")
    b <- filter(a,grepl("District",Area.Name))
    c <- filter(b,Total..Rural..Urban=="Total")
    c$Area.Name <-gsub("District - ","",c$Area.Name)
    c$Area.Name <- gsub("\\d+","",c$Area.Name)
    c$Area.Name <- gsub(" |\\*","",c$Area.Name)
    
    print("Here")
    df <- NULL
    df <- c[,5:28]
    names(df) <-c("Area.Name","Total..Rural..Urban", "Age.group", 
                  "Persons","Males","Females",
                  "PersonsEdu","MalesEdu", "FemalesEdu",
                  "PersonsSchool","MalesSchool","FemalesSchool",
                  "PersonsCollege","MalesCollege","FemalesCollege",
                  "PersonsVocInst","MalesVocInst","FemalesVocInst",
                  "PersonsOthrInst","MalesOthrInst","FemalesOthrInst",
                  "PersonsLitCntr","MalesLitCntr","FemalesLitCntr")
    
    # Subset columns with persons
    people <- select(df,matches(peopleType,ignore.case=FALSE))
    
    # Calculate males percent as percent of total males
    peoplePercent <- people[,2:7]/people[,1]*100
    
    # Add the age column
    peoplePercent <- cbind(df[1],peoplePercent)
    
  
    
    
    #i= max(peoplePercent[,w])
    ##j = min(peoplePercent[,w])
    #mid = (i+j)/2
    
    #df$PersonsEdu <- df$PersonsEdu/df$Persons * 100
    ##df$MalesEdu <- df$MalesEdu/df$Males * 100
    #df$FemalesEdu <- df$FemalesEdu/df$Females * 100
    #m= max(df$PersonsEdu)
    #n = min(df$PersonsEdu)
    #mid = (m+n)/2
    
    length(intersect(df$Area.Name,unique(dist$id)))
    setdiff(df$Area.Name,unique(dist$id))
    setdiff(unique(dist$id),df$Area.Name)
    
    if(state == "Tamil Nadu"){
        df[df$Area.Name=="TheNilgiris",]$Area.Name = "Nilgiris"
        df[df$Area.Name=="Viluppuram",]$Area.Name = "Villupuram"
        df[df$Area.Name=="Tiruchirappalli",]$Area.Name = "Tiruchchirappalli"
        df[df$Area.Name=="Thoothukkudi",]$Area.Name = "Thoothukudi"
        df[df$Area.Name=="Tirunelveli",]$Area.Name = "Tirunelveli Kattabo"
    } else if(state == "Kerala"){
        df[df$Area.Name=="Pathanamthitta",]$Area.Name = "Pattanamtitta"
    } else if(state == "Andhra Pradesh"){
        df[df$Area.Name=="Visakhapatnam",]$Area.Name = "Vishakhapatnam"
        df[df$Area.Name=="EastGodavari",]$Area.Name = "East Godavari"
        df[df$Area.Name=="WestGodavari",]$Area.Name = "West Godavari"
    } else if(state == "Arunachal Pradesh"){
        df[df$Area.Name=="WestKameng",]$Area.Name = "West Kameng"
        df[df$Area.Name=="EastKameng",]$Area.Name = "East Kameng"
        df[df$Area.Name=="PapumPare",]$Area.Name = "Papum Pare"
        df[df$Area.Name=="LowerSubansiri",]$Area.Name = "Lower Subansiri"
        df[df$Area.Name=="UpperSubansiri",]$Area.Name = "Upper Subansiri"
        df[df$Area.Name=="WestSiang",]$Area.Name = "West Siang"
        df[df$Area.Name=="EastSiang",]$Area.Name = "East Siang"
        df[df$Area.Name=="UpperSiang",]$Area.Name = "Upper Siang"
        df[df$Area.Name=="DibangValley",]$Area.Name = "DibangValley"
        
    } else if(state == "Assam"){
        df[df$Area.Name=="Dhubri",]$Area.Name = "Dhuburi"
        df[df$Area.Name=="KarbiAnglong",]$Area.Name = "Karbi Anglong"
        df[df$Area.Name=="NorthCacharHills",]$Area.Name = "North Cachar Hills"
    } else if(state == "Bihar") {
        df[df$Area.Name=="PashchimChamparan",]$Area.Name = "Pashchim Champaran"
        df[df$Area.Name=="PurbaChamparan",]$Area.Name = "Purba Champaran"
        df[df$Area.Name=="Kaimur(Bhabua)",]$Area.Name = "Bhabua"
    } else if(state == "Gujarat") {
        df[df$Area.Name=="BanasKantha",]$Area.Name = "Banas Kantha"
        df[df$Area.Name=="SabarKantha",]$Area.Name = "Sabar Kantha"
        df[df$Area.Name=="PanchMahals",]$Area.Name = "Panch Mahals"
        df[df$Area.Name=="Dohad",]$Area.Name = "Dahod"
        df[df$Area.Name=="TheDangs",]$Area.Name = "The Dangs"
    } else if(state == "Chhattisgarh") {
        df[df$Area.Name=="Rajnandgaon",]$Area.Name = "Raj Nandgaon"
    } else if(state == "Haryana") {
        df[df$Area.Name=="Yamunanagar",]$Area.Name = "Yamuna Nagar"
        df[df$Area.Name=="Sonipat",]$Area.Name = "Sonepat"
    } else if(state == "Himachal Pradesh") {
        df[df$Area.Name=="Lahul&Spiti",]$Area.Name = "Lahul and Spiti"
        
    }else if(state == "Jharkhand") {
        df[df$Area.Name=="Kodarma",]$Area.Name = "Koderma"
        df[df$Area.Name=="Pakaur",]$Area.Name = "Pakur"
        df[df$Area.Name=="PashchimiSinghbhum",]$Area.Name = "Pashchim Singhbhum"
        df[df$Area.Name=="PurbiSinghbhum",]$Area.Name = "Purba Singhbhum"
        
    } else if(state == "Karnataka") {
        df[df$Area.Name=="UttaraKannada",]$Area.Name = "Uttar Kannand"
        df[df$Area.Name=="Bangalore",]$Area.Name = "Bangalore Urban"
        df[df$Area.Name=="BangaloreRural",]$Area.Name = "Bangalore Rural"
        df[df$Area.Name=="DakshinaKannada",]$Area.Name = "Dakshin Kannad"
        df[df$Area.Name=="Chamarajanagar",]$Area.Name = "Chamrajnagar"
        
    } else if(state == "Madhya Pradesh") {
        df[df$Area.Name=="WestNimar",]$Area.Name = "West Nimar"
        df[df$Area.Name=="EastNimar",]$Area.Name = "East Nimar"
        df[df$Area.Name=="Narsimhapur",]$Area.Name = "Narsinghpur"
    } else if(state == "Mahrashtra") {
        df[df$Area.Name=="Mumbai(Suburban)",]$Area.Name = "Greater Bombay"
        df[df$Area.Name=="Mumbai",]$Area.Name = "Greater Bombay"
        df[df$Area.Name=="Ahmadnagar",]$Area.Name = "Ahmednagar"
        df[df$Area.Name=="Gadchiroli",]$Area.Name = "Garhchiroli"
    } else if(state == "Orissa") {
        df[df$Area.Name=="Bargarh",]$Area.Name = "Baragarh"
        df[df$Area.Name=="Debagarh",]$Area.Name = "Deogarh"
        df[df$Area.Name=="Kendujhar",]$Area.Name = "Keonjhar"
        df[df$Area.Name=="Jagatsinghapur",]$Area.Name = "Jagatsinghpur"
        df[df$Area.Name=="Jajapur",]$Area.Name = "Jajpur"
        df[df$Area.Name=="Anugul",]$Area.Name = "Angul"
        df[df$Area.Name=="Baudh",]$Area.Name = "Boudh"
        df[df$Area.Name=="Sonapur",]$Area.Name = "Sonepur"
        df[df$Area.Name=="Balangir",]$Area.Name = "Bolangir"
        df[df$Area.Name=="Nabarangapur",]$Area.Name = "Nabarangpur"
    } else if(state == "Punjab") {
        df[df$Area.Name=="Nawanshahr",]$Area.Name = "Nawan Shehar"
        df[df$Area.Name=="FatehgarhSahib",]$Area.Name = "Fatehgarh Sahib"
    } else if(state == "Punjab") {
        df[df$Area.Name=="SawaiMadhopur",]$Area.Name = "Sawai Madhopur"
    } else if(state == "Uttar Pradesh") {
        df[df$Area.Name=="JyotibaPhuleNagar",]$Area.Name = "Jyotiba Phule Nagar"
        df[df$Area.Name=="GautamBuddhaNagar",]$Area.Name = "Gautam Buddha Nagar"
        df[df$Area.Name=="Budaun",]$Area.Name = "Badaun"
        df[df$Area.Name=="Kheri",]$Area.Name = "Lakhimpur Kheri"
        df[df$Area.Name=="RaeBareli",]$Area.Name = "Rae Bareli"
        df[df$Area.Name=="KanpurDehat",]$Area.Name = "Kanpur Dehat"
        df[df$Area.Name=="KanpurNagar",]$Area.Name = "Kanpur"
        df[df$Area.Name=="Barabanki",]$Area.Name = "Bara Banki"
        df[df$Area.Name=="AmbedkarNagar",]$Area.Name = "Ambedkar Nagar"
        df[df$Area.Name=="Shrawasti",]$Area.Name = "Shravasti"
        df[df$Area.Name=="Siddharthnagar",]$Area.Name = "Siddharth Nagar"
        df[df$Area.Name=="SantKabirNagar",]$Area.Name = "Sant Kabir Nagar"
        df[df$Area.Name=="SantRavidasNagar",]$Area.Name = "Sant Ravi Das Nagar"
        
    } else if(state == "Uttaranchal") {
        df[df$Area.Name=="Rudraprayag",]$Area.Name = "Rudra Prayag"
        df[df$Area.Name=="TehriGarhwal",]$Area.Name = "Tehri Garhwal"
        df[df$Area.Name=="Dehradun",]$Area.Name = "Dehra Dun"
        df[df$Area.Name=="Garhwal",]$Area.Name = "Pauri Garhwal"
        df[df$Area.Name=="Nainital",]$Area.Name = "Naini Tal" 
        df[df$Area.Name=="UdhamSinghNagar",]$Area.Name = "Udham Singh Nagar"
        df[df$Area.Name=="Hardwar",]$Area.Name = "Haridwar"
    } else if(state == "West Bengal") {
        df[df$Area.Name=="KochBihar",]$Area.Name = "Kochbihar"
        df[df$Area.Name=="UttarDinajpur",]$Area.Name = "Uttar Dinajpu"
        df[df$Area.Name=="DakshinDinajpur",]$Area.Name = "Dakshin Dinajpur"
        df[df$Area.Name=="NorthTwentyFourParganas",]$Area.Name = "North 24 Parganas"
        df[df$Area.Name=="SouthTwentyFourParganas",]$Area.Name = "South 24 Parganas"
        df[df$Area.Name=="Medinipur",]$Area.Name = "West Midnapore"
        #df[df$Area.Name=="Medinipur",]$Area.Name = "East Midnapore"
    }
    
    # Create the column from the literacyLevel input
    u <- eduInst
    v <- paste(peopleType,eduInst,sep="")
    print(v)
    w <- which(names(peoplePercent) == v)
    print(w)
    
    # Select the districts with lowest literacy
    m <- head(arrange(peoplePercent,peoplePercent[,w]),5)
    lowestLiteracy <- paste(m$Area.Name,"(",round(m[,w],1),")",sep="")
    print(lowestLiteracy)
    
    # Get the min/max latitude and longitude for plotting districts with lowest literacy
    # This is obtained from the fortified data frame 
    minLat= min(dist$lat)
    maxLat =max(dist$lat)
    minLong = min(dist$long)
    maxLong = max(dist$long)
    x = minLong+0.5
    y= minLat + 1.0
    # Create a data frame to primt the top 5 ofenders
    labels <- data.frame(
        xc = c(x,x,x,x,x), 
        yc = c(y,y-.2,y-0.4,y-0.6,y-.8), 
        label = as.vector(lowestLiteracy) 
        
    )
    
    
    #Create the title
    if(eduInst == "Edu"){
        inst <- "educational inst"
    } else if(eduInst == "School"){
        inst <- "schools"
    } else if(eduInst == "College"){
        inst <- "colleges"
    } else if(eduInst == "VocInst"){
        inst <- "vocational inst."
    } else if(eduInst == "OthrInst"){
        inst <- "other inst"
    } else if(eduInst == "LitCntr"){
        inst <- "literacy centers"
    }
    atitle=paste(peopleType,"in", state, "going to",inst)
    #print(dim(df))
    #print(df$PersonsEdu)
    #Create a dataframe with the selected column and the district
    
    d <- data.frame(peoplePercent$Area.Name,peoplePercent[,w])
    names(d) <-c("Area.Name","EduInst")
    ggplot() + geom_map(data = d, aes(map_id = Area.Name, fill = EduInst),  
                        ,map = dist,color="black",size=0.25) + 
        expand_limits(x = dist$long, y = dist$lat) +  
        scale_fill_distiller(name="Percent", palette = "YlGn")+
        labs(title=atitle)+
        geom_text(data = labels, aes(x = xc, y = yc, label = label))+
        #geom_text(aes(label="Data source:https://data.gov.in",maxLong-1,minLat+0.1)) +
        xlab("Longitude") + ylab("Latitude")
    
    
    
    
}


##################################################
literacyIndia <- function(region,peopleType,literacyLevel)
{
    
    
   
    c <- filter(a,Age.group=="All ages" & Total..Rural..Urban==region)
    
    
    # Subset columns with persons
    people <- select(c,matches(peopleType,ignore.case=FALSE))
    
    peoplePercent <- people[,2:14]/people[,1]*100
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
    
    #ind <- readShapeSpatial("./India_SHP/INDIA.shp")
    #plot(ind)
    
    #ind <- fortify(ind, region = "ST_NAME")
    
    
    # Set the names as in the map
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
    
    w <- which(names(peoplePercent) == literacyLevel)
    
    
    i= max(peoplePercent[,w])
    j = min(peoplePercent[,w])
    mid = (i+j)/2
    

    
    df = data.frame(peoplePercent$Area.Name,peoplePercent[,w])
    names(df) <- c("Area.Name","percentages")
    
    if(literacyLevel == "Illiterate"){
        m <- head(arrange(df,percentages),5)
        
    } else {
        m <- head(arrange(df,desc(percentages)),5)
    }
    literacy <- paste(m$Area.Name,"(",round(m$percentages,1),")",sep="")
    labels <- data.frame(
        xc = c(90,90,90,90,90), 
        yc = c(11,10,9,8,7), 
        label = as.vector(literacy) 
        
    )
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
