backupData <-tempData

myData <- tempData

#temp variables
xAxisName <- "Listening_Situation"
yAxisName <- "Number_of_Actuators"

xTargetName <- "Listening_Situation"
yTargetName <- "Number_of_Actuators"


#Parse Y Axis elements
nrOfItems <- lengths(regmatches(Table[, yTargetName], gregexpr(",", Table[yTargetName])))
nrOfItems <- sum(nrOfItems) + length(Table[yTargetName])
tempTable <- data.frame(xAxisName = character(), yAxisName = character(), Wearable = character())
colnames(tempTable)[1] <- xAxisName
colnames(tempTable)[2] <- yAxisName
rowDest <- 1
for(i in 1:nrOfItems)
{
  collumn <- Table[i, yTargetName]
  nrOfElements <- lengths(regmatches(collumn, gregexpr(",", collumn))) + 1
  colComponents <- as.vector(str_split_fixed(collumn, pattern = ", ", n = nrOfItems))
  for(j in 1:nrOfElements)
  {
    tempTable[rowDest, xAxisName] <- Table[i,xTargetName]
    tempTable[rowDest, yAxisName] <- colComponents[j]
    tempTable[rowDest,"Wearable"] <- Table[i,"Wearable"]
    rowDest <- rowDest + 1
  }
}
tempTable <- tempTable[rowSums(is.na(tempTable)) == 0,]

#Parse X Axis elements

nrOfItemsX <- lengths(regmatches(Table[, xTargetName], gregexpr(",", Table[xTargetName])))
nrOfItemsX <- sum(nrOfItems) + length(Table[xTargetName])

rowDest <- 1
for(i in 1:nrOfItemsX)
{
  collumn <- tempTable[i, xTargetName]
  nrOfElements <- lengths(regmatches(collumn, gregexpr(",", collumn))) + 1
  colComponents <- as.vector(str_split_fixed(collumn, pattern = ", ", n = nrOfItemsX))
  for(j in 1:nrOfElements)
  {
    tempTable[rowDest, xAxisName] <- colComponents[j]
    tempTable[rowDest, yAxisName] <- tempTable[i,yTargetName]
    tempTable[rowDest,"Wearable"] <- tempTable[i,"Wearable"]
    rowDest <- rowDest + 1
  }
}




if(TRUE)
{
  myData$Number_of_Participants <- gsub('\\s+', '', myData$Number_of_Participants)
  levels =  c("0", "1", "2","3","4","5","6","7","8","9",
              "10", "11", "12","13","14","15","16","17","18","19",
              "20", "21", "22","23","24","25","26","27","28","29",
              "30", "31", "32","33","34","35","36","37","38","39",
              "40", "41", "42","43","44","45","46","47","48","49",
              "50", "51", "52","53","54","55","56","57","58","59",
              "60", "61", "62","63","64","65","66","67","68","69",
              "70", "71", "72","73","74","75","76","77","78","79",
              "80", "81", "82","83","84","85","86","87","88","89",
              "90", "91", "92","93","94","95","96","97","98","99",
              "N/A", "Unspecified")
  
  levels(myData$val)
  
  level_order <- factor(myData$Number_of_Participants, level = levels);
  
  myData$Number_of_Participants <- factor(myData$Number_of_Participants, level = levels)
  
  #which(is.na(myData$Wearable))
  
  temp <- as.numeric(myData$Number_of_Participants) - 1
  temp = temp[temp!= 100]
  temp = temp[temp!= 101]
  
  #level <- factor(myData$val, level = levelOrder)
  
  scatter <- ggplot(data = myData, aes(x = Year, y = level_order)) +
    geom_count(aes(colour = Number_of_Participants), show.legend = FALSE, alpha = 0.4) +
    theme_solarized(light = FALSE) + 
    scale_size_area() + 
    theme(plot.margin = unit(c(0,0, 0, 0), "cm"), text=element_text(colour="white", size=14), axis.text.y = element_text(colour="grey"), axis.text.x = element_text(colour="grey"), ) 
  scatter
  
  hist_right <- ggplot(data = myData, aes(x = Number_of_Participants)) + 
    geom_bar(width = 0.5, aes(colour = Number_of_Participants, fill = Number_of_Participants), show.legend = FALSE) + 
    geom_vline(aes(xintercept = mean(as.numeric(temp))), colour = "orange", size = 2 )+
    coord_flip() +
    stat_count(aes(y=..count.., label=..count..), geom="text", hjust= 1.2) + 
    theme_solarized(light = FALSE) + 
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank(), plot.margin = unit(c(0, 0, 0, 0), "cm"),
          text=element_text(colour="white", size = 14),axis.text.x = element_text(colour="grey")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0)))
  
  hist_right
  
  dualGraph <- grid.arrange(scatter, hist_right, ncol = 2, nrow = 1, widths=c(3, 1))
}


#   
# geom_vline(aes(xintercept = 22), data = data.frame(x = "2"), colour = "red")
# 
# myData %>%
#   mutate(name = fct_reorder(val, name)) %>%
#   ggplot( aes(x=name, y=val)) +
#   geom_count(aes(colour = val), show.legend = FALSE, alpha = 0.4) +
#   theme_bw()