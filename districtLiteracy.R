#########################################################################################################
#
# Title : Literacy in India : A DeepR dive
# Designed and developed by: Tinniam V Ganesh
# Date : 5 Nov 2015
# File: districLiteracy.R
# More details: https://gigadom.wordpress.com/
#
#########################################################################################################
districtEdu <- function(state,peopleType,eduInst){
    
    # Manually change the state from all capitals to only 1st letter capital.
    # Note capitalize in HMisc package does this but conflicts with functions of other packages
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
    
    # Read India district file
    ind_dist <- readShapeSpatial("./IND_adm/IND_adm2.shp")
    district_df = ind_dist@data
    
    # Select the data associated with the state
    state_dist_df = data.frame(district_df[grep(state,district_df$NAME_1),])
    
    # Create shape file for the state and districts alone
    polygon_list = list()
    for (istr in rownames(state_dist_df)){
        i = as.numeric(istr) + 1
        tmp = ind_dist@polygons[i]
        polygon_list = c(polygon_list,tmp)
    }
    
    
    # construct a new shape file with the  districts
    dist_spatial = SpatialPolygons(polygon_list,1:length(polygon_list))
    dist_spatial_frame = SpatialPolygonsDataFrame(dist_spatial,data=state_dist_df)
    
    # Write shape file
    shpFile <- paste( state,".shp",sep="")   
    districtDir <- paste("./district/",shpFile,sep="")
    writeSpatialShape(dist_spatial_frame,districtDir)
    # Read the shape file
    dist_df = readShapePoly(districtDir)
    dist <- fortify(dist_df, region = "NAME_2")
    
    # Read the data for state and literacy levels in each district
    csvFile <- paste(state,".csv",sep="")
    csvDir <- paste("./data/",csvFile,sep="")
    stateData <- read.csv(csvDir)
    
    # Subset
    a <- filter(stateData,Age.group=="All ages")
    b <- filter(a,grepl("District",Area.Name))
    c <- filter(b,Total..Rural..Urban=="Total")
    c$Area.Name <-gsub("District - ","",c$Area.Name)
    c$Area.Name <- gsub("[0-9]+","",c$Area.Name)
    c$Area.Name <- gsub(" |\\*","",c$Area.Name)
   
    # Name colums for manageability
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
    
    # Calculate people percent as percent of total people
    peoplePercent <- people[,2:7]/people[,1]*100
    
    # Add the age column
    peoplePercent <- cbind(df[1],peoplePercent)
    
    # Code below is used to identify names of districts that are spelled differently in
    # the data file and in the shape file
    length(intersect(df$Area.Name,unique(dist$id)))
    setdiff(df$Area.Name,unique(dist$id))
    setdiff(unique(dist$id),df$Area.Name)
    
    # Make the names of the district in data frame to match the spelling in the map file
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
    
    # Find this column
    w <- which(names(peoplePercent) == v)
    
    
    # Select the districts with lowest literacy for the select column
    m <- head(arrange(peoplePercent,peoplePercent[,w]),5)
    lowestLiteracy <- paste(m$Area.Name,"(",round(m[,w],1),")",sep="")
    
    
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
    
    # Create a data frame which can be used with ggplot with Area.Name and selected column
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
