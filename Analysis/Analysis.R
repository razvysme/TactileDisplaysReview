#Copyright @ Razvan Paisa, 2022 - applies to the script and the data in "Table" 
#Free to use for research purposes
#Graphs generated can be published in articles only after agreement 
#Data in "Table" must not be published
#Contact rpa@create.aau.dk for permission request
#Provided as is; feel free to report bugs but don't expect immediate fixing.

library(readxl)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggExtra)
library(gridExtra)
library(stringr)
library(scales)
library('Cairo') 
library('cowplot') #not in use now
library(egg)
setwd("E:/OneDrive - Aalborg Universitet/PhD/Projects/2021 -Vibrotactile displays for music listening - a literature overview/Analysis")
#setwd("~/OneDrive - Aalborg Universitet/PhD/Projects/2021 -Vibrotactile displays for music listening - a literature overview/Analysis")
loadTable <- function(showTable)
  {
  rawTable <- read_excel("Paper logs and Table.xlsx", 
                         sheet = "Table of Devices For Analysis", 
                         col_types = c("skip", "text", "text", 
                                       "text", "text", "text", "text", "text", 
                                       "text", "text", "text", "text", "text", 
                                       "text", "text","skip", "skip", "skip", 
                                       "skip","skip"),n_max = 71)
  Table <- rawTable;
  Table <- Table[-c(53, 59, 60, 61),] #remove the specified rows
  
  
  for(i in 1:nrow(Table)){
    
    Table[i, "Device_Name"] <- sub('.*(\\d{4}).*', '\\1', gsub("[^0-9.-]", "", Table[i, "Device_Name"])) #remove all non digit elements and keep only the last 4 digits
    
    #if(!grepl('[^[:alnum:]]', Table[i, "Number_of_Actuators"]))
    #  {
    #    print(i)
    #    Table[i, "Number_of_Actuators"] <- gsub("[^0-9.-]", "", Table[i, "Number_of_Actuators"])
    #  }
    
    # if(Table[i, "Wearable"] == "Yes")
    # {
    #   Table[i, "Wearable"] <- "1"
    # }
    # else if(Table[i, "Wearable"] == "No")
    # {
    #   Table[i, "Wearable"] <- "0"
    # }
    # else
    # {
    #   Table[i, "Wearable"] <- ""    
    # }
    
  }
  
  Table[5, "Number_of_Actuators"] <- "0"
  Table[38, "Number_of_Actuators"] <- "11"
  Table[51, "Number_of_Actuators"] <- "12"
  
  Table["Wearable"][Table["Wearable"] == "N/A"] <- "No"
  
  # convert columns to numeric
  Table$Device_Name = as.numeric(Table$Device_Name)
  Table$Number_of_Actuators = as.numeric(Table$Number_of_Actuators)
  #Table$Number_of_Participants = as.numeric(Table$Number_of_Participants)
  #Table$Wearable = as.numeric(Table$Wearable) 
  
  
  colnames(Table)[1] <- "Year"
  
  if(showTable == TRUE)
  {
    View(Table)
  }
  return(Table)
  }

Table <- loadTable(0)

#sapply(Table, mode) #verify column type
#str(Table) #structure of the data

# qplot(data = Table, Year, Number_of_Actuators, color = Wearable ) #scatter plot
# 
# 
# 
# #plot nr of actuators over years, and regression line with polynomial interpolation
# 
nrOfActuatorsVsYearPlot <- ggplot(Table,aes(Year, Number_of_Actuators)) +
    geom_point(size = 5, aes(shape = Wearable, color = Wearable), show.legend = TRUE, alpha = 0.5) +
    geom_hline(yintercept = mean(Table$Number_of_Actuators),linetype="dashed",color = "orange", size = 1.5) +
    ylab("Number of Actuators") +
    theme_solarized(light = FALSE) + 
    theme(plot.margin = unit(c(0,0, 0, 0), "cm"), text=element_text(colour="black", size=14), axis.text.x = element_text(colour="black"), axis.text.y = element_text(colour="black"),
          panel.background = element_rect(fill="white", colour="black",size = 0.5, linetype = "solid"),
          panel.grid.major = element_line(size = 0.25, linetype = 'dashed',
                                          colour = "grey"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'dashed',
                                          colour = "grey"),
          plot.background = element_rect(fill = "white")) +
    #theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank(), plot.margin = unit(c(0, 0, 0, 0), "cm"),
    #text=element_text(colour="white", size = 14),axis.text.x = element_text(colour="grey")) +
    #scale_y_continuous(expand = expansion(mult = c(0, 0)))+
    geom_smooth(method = "lm" , formula = y ~ poly(x, 3))
nrOfActuatorsVsYearPlot
if (1)
{
  

# 
#ggsave(plot = nrOfActuatorsVsYearPlot, filename = "NrOfActuators Vs Year Prediction.png", dpi = 300, type = 'cairo')

#theme(plot.margin = unit(c(0,0, 0, 0), "cm"), text=element_text(colour="white", size=14), axis.text.x = element_text(colour="grey", angle = textAngle, vjust = textVjust, hjust = textHjust), axis.text.y = element_text(colour="grey"), ) 
# 
# collumn = Table[1, "Actuators_Type"]
# #calculate nr of elements in a column divided by ","
# nrOfElements <- lengths(regmatches(collumn, gregexpr(",", collumn))) + 1
# #split the column into a list of elements
# test <- as.vector(str_split_fixed(collumn,pattern = ", ", n = nrOfElements))
# 
# 
# #compute types of actuators over years
# 
# nrOfActuators <- lengths(regmatches(Table$Actuators_Type, gregexpr(",", Table$Actuators_Type)))
# nrOfActuators <- sum(nrOfActuators) + length(Table$Actuators_Type)
# 
# actuators <- data.frame(Year = numeric(),  Actuator = character())
# 
# rowDestination = 1;
# for(i in 1:nrOfActuators)
# {
#   collumn <- Table[i, "Actuators_Type"]
#   nrOfElements <- lengths(regmatches(collumn, gregexpr(",", collumn))) + 1
#   colComponents <- as.vector(str_split_fixed(collumn, pattern = ", ", n = nrOfElements))
#   for(j in 1:nrOfElements)
#   {
#     actuators[rowDestination, "Year"] <- Table[i,"Year"]
#     actuators[rowDestination, "Actuator"] <- colComponents[j]
#     actuators[rowDestination,"Wearable"] <- Table[i,"Wearable"]
#     rowDestination <- rowDestination + 1
#   }
# }
# #remove any N/A rows
# actuators <- actuators[rowSums(is.na(actuators)) == 0,]
# 
# qplot(data = actuators, x = Year, y = Actuator, color = Actuator, show.legend = FALSE)
# qplot(data = actuators, Actuator)
# 
# ##### Experiments here
# 
# N <- function(x){
#   return(data.frame(y = mean(x), label = length(x)))
# }
# 
# # Actuators type over years + histrogram
# scatter <- ggplot(actuators, aes(x = Year, y = Actuator)) +
#   geom_point(aes(colour = Actuator), show.legend = FALSE) +
#   geom_count(aes(colour = Actuator, shape = Wearable), show.legend = FALSE) +
#   scale_size_area() + 
#   theme(plot.margin = unit(c(0,0, 0, 0), "cm"), text=element_text(size=16))
# 
# hist_right <- ggplot(data = actuators, aes(x = Actuator)) + 
#   geom_bar(width = 0.5, aes(colour = Actuator, fill = Actuator), show.legend = FALSE) + 
#   coord_flip() +
#   stat_count(aes(y=..count.., label=..count..), geom="text", hjust= 1.5) + 
#   theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank(), plot.margin = unit(c(0, 0, 0, 0), "cm"), text=element_text(size=16)) +
#   scale_y_continuous(expand = expansion(mult = c(0, 0)))
# 
# actuatorTypeVsYearPlot <- grid.arrange(scatter, hist_right, ncol = 2, nrow = 1, widths=c(2, 1))
# 
# #ggsave(plot = actuatorTypeVsYearPlot, filename = "actuatorTypeVsYear.pdf")
# 
# 
# ####
# 
# nrOfAreas <- lengths(regmatches(Table$Body_Area_Actuated, gregexpr(",", Table$Body_Area_Actuated)))
# nrOfAreas <- sum(nrOfActuators) + length(Table$Body_Area_Actuated)
# 
# bodyArea <- data.frame(Year = numeric(),  Area = character(), nrOfActuators = numeric())
# 
# rowDestination = 1;
# for(i in 1:nrOfAreas)
# {
#   collumn <- Table[i, "Body_Area_Actuated"]
#   nrOfElements <- lengths(regmatches(collumn, gregexpr(",", collumn))) + 1
#   colComponents <- as.vector(str_split_fixed(collumn, pattern = ", ", n = nrOfAreas))
#   for(j in 1:nrOfElements)
#   {
#     bodyArea[rowDestination, "Year"] <- Table[i,"Year"]
#     bodyArea[rowDestination, "Area"] <- colComponents[j]
#     bodyArea[rowDestination, "nrOfActuators"] <- Table[i,"Number_of_Actuators"]
#     bodyArea[rowDestination,"Wearable"] <- Table[i,"Wearable"]
#     rowDestination <- rowDestination + 1
#   }
# }
# #remove any N/A rows
# bodyArea <- bodyArea[rowSums(is.na(bodyArea)) == 0,]
# 
# # convert columns to numeric
# bodyArea$Year = as.numeric(bodyArea$Year)
# bodyArea$nrOfActuators = as.numeric(bodyArea$nrOfActuators)
# 
# 
# scatter2 <- ggplot(bodyArea, aes(x = nrOfActuators, y = Area)) +
#   geom_point(aes(colour = Area), show.legend = FALSE) +
#   geom_count(aes(colour = Area, shape = Wearable), show.legend = FALSE) +
#   scale_size_area() + 
#   theme(plot.margin = unit(c(0,0, 0, 0), "cm"), text=element_text(size=16))
# 
# hist_right2 <- ggplot(data = bodyArea, aes(x = Area)) + 
#   geom_bar(width = 0.5, aes(colour = Area, fill = Area), show.legend = FALSE) + 
#   coord_flip() +
#   stat_count(aes(y=..count.., label=..count..), geom="text", hjust= 1.5) + 
#   theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank(), plot.margin = unit(c(0, 0, 0, 0), "cm"), text=element_text(size=16)) +
#   scale_y_continuous(expand = expansion(mult = c(0, 0)))
# 
# areaVsNrOfActuators <- grid.arrange(scatter2, hist_right2, ncol = 2, nrow = 1, widths=c(2, 1))
# 
# 
# #ggsave(plot = areaVsNrOfActuators, filename = "areaVsNrOfActuators.pdf")
# 
# #######
# 
# nrOfDSP <- lengths(regmatches(Table$DSP, gregexpr(",", Table$DSP)))
# nrOfDSP <- sum(nrOfDSP) + length(Table$DSP)
# DSP <- data.frame(Year = numeric(),  DSP = character(), nrOfActuators = numeric(), Wearable = character())
# 
# rowDestination = 1;
# for(i in 1:nrOfDSP)
# {
#   collumn <- Table[i, "DSP"]
#   nrOfElements <- lengths(regmatches(collumn, gregexpr(",", collumn))) + 1
#   colComponents <- as.vector(str_split_fixed(collumn, pattern = ", ", n = nrOfDSP))
#   for(j in 1:nrOfElements)
#   {
#     DSP[rowDestination, "Year"] <- Table[i,"Year"]
#     DSP[rowDestination, "DSP"] <- colComponents[j]
#     DSP[rowDestination, "nrOfActuators"] <- Table[i,"Number_of_Actuators"]
#     DSP[rowDestination,"Wearable"] <- Table[i,"Wearable"]
#     rowDestination <- rowDestination + 1
#   }
# }
# 
# DSP <- DSP[rowSums(is.na(DSP)) == 0,]
# 
# 
# scatter3 <- ggplot(DSP, aes(x = Year, y = DSP)) +
#   geom_point(aes(colour = DSP, shape = Wearable), show.legend = FALSE) +
#   geom_count(aes(colour = DSP, shape = Wearable), show.legend = FALSE) +
#   scale_size_area() + 
#   theme(plot.margin = unit(c(0,0, 0, 0), "cm"), text=element_text(size=16))
# 
# hist_right3 <- ggplot(data = DSP, aes(x = DSP)) + 
#   geom_bar(width = 0.5, aes(colour = DSP, fill = DSP), show.legend = FALSE) + 
#   coord_flip() +
#   stat_count(aes(y=..count.., label=..count..), geom="text", hjust= 1.5) + 
#   theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank(), plot.margin = unit(c(0, 0, 0, 0), "cm"), text=element_text(size=16)) +
#   scale_y_continuous(expand = expansion(mult = c(0, 0)))
# 
# DSPVsNrOfActuators <- grid.arrange(scatter3, hist_right3, ncol = 2, nrow = 1, widths=c(2, 1))
# 
# #ggsave(plot = DSPVsNrOfActuators, filename = "DSPVsNrOfActuators.pdf")
# 
# #ggsave(plot = DSPVsNrOfActuators, filename = "DSPVsYear.pdf")
# 
# nrOfSignals <- lengths(regmatches(Table$DSP, gregexpr(",", Table$Signals_Used)))
# nrOfSignals <- sum(nrOfSignals) + length(Table$Signals_Used)
# Signals <- data.frame(Year = numeric(), Signal = character(), DSP = character(), Wearable = character(), nrOfActuators = numeric())
# 
# rowDestination = 1;
# 
# for(i in 1:nrOfSignals)
# {
#   collumn <- Table[i, "Signals_Used"]
#   nrOfElements <- lengths(regmatches(collumn, gregexpr(",", collumn))) + 1
#   colComponents <- as.vector(str_split_fixed(collumn, pattern = ", ", n = nrOfDSP))
#   for(j in 1:nrOfElements)
#   {
#     Signals[rowDestination, "Year"] <- Table[i,"Year"]
#     Signals[rowDestination, "Signal"] <- colComponents[j]
#     Signals[rowDestination, "nrOfActuators"] <- Table[i,"Number_of_Actuators"]
#     Signals[rowDestination,"Wearable"] <- Table[i,"Wearable"]
#     Signals[rowDestination, "DSP"] <- Table[i, "DSP"]
#     rowDestination <- rowDestination + 1
#   }
# }
# 
# Signals <- Signals[rowSums(is.na(Signals)) == 0,]
# 
# scatter4 <- ggplot(Signals, aes(x = Year, y = Signal)) +
#   geom_point(aes(colour = Signal, shape = Wearable), show.legend = FALSE) +
#   geom_count(aes(colour = Signal, shape = Wearable), show.legend = FALSE) +
#   scale_size_area() + 
#   theme(plot.margin = unit(c(0,0, 0, 0), "cm"), text=element_text(size=16))
# 
# hist_right4 <- ggplot(data = Signals, aes(x = Signal)) + 
#   geom_bar(width = 0.5, aes(colour = Signal, fill = Signal), show.legend = FALSE) + 
#   coord_flip() +
#   stat_count(aes(y=..count.., label=..count..), geom="text", hjust= 1.5) + 
#   theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank(), plot.margin = unit(c(0, 0, 0, 0), "cm"), text=element_text(size=16)) +
#   scale_y_continuous(expand = expansion(mult = c(0, 0)))
# 
# SignalsVsYear <- grid.arrange(scatter4, hist_right4, ncol = 2, nrow = 1, widths=c(2, 1))
# 
# #ggsave(plot = SignalsVsYear, filename = "SignalsVsYear.pdf")
}
tempData <- data.frame()
tempData2 <- data.frame()
drawGraphs <- function(data, xAxisName, yAxisName, xTargetName, yTargetName, histogram)
  {
    if(xAxisName != "Year")
    {
      print(cat("X axis is not Year, it is: ", xAxisName ))
      nrOfItems <- lengths(regmatches(data[, yTargetName], gregexpr(",", data[yTargetName])))
      nrOfItems <- sum(nrOfItems) + length(Table[yTargetName])
      tempTable <- data.frame(xAxisName = character(), yAxisName = character(), Wearable = character())
      colnames(tempTable)[1] <- xAxisName
      colnames(tempTable)[2] <- yAxisName
      rowDest <- 1
      print("Sorting Y Axis")
      for(i in 1:nrOfItems)
      {
        collumn <- data[i, yTargetName]
        nrOfElements <- lengths(regmatches(collumn, gregexpr(",", collumn))) + 1
        #print(nrOfElements)
        colComponents <- as.vector(str_split_fixed(collumn, pattern = ", ", n = nrOfItems))
        #print(colComponents)
        for(j in 1:nrOfElements)
        {
          tempTable[rowDest, xAxisName] <- data[i,xTargetName]
          element <- trimws(colComponents[j], whitespace = "[ \t\r\n]")
          #print(element)
          tempTable[rowDest, yAxisName] <- element
          tempTable[rowDest,"Wearable"] <- data[i,"Wearable"]
          rowDest <- rowDest + 1
        }
      }
      tempTable <- tempTable[rowSums(is.na(tempTable)) == 0,]
      #tempData <<- tempTable
      
      nrOfItemsX <- lengths(gregexpr(",", tempTable[xTargetName]))
      nrOfItemsX <- sum(nrOfItemsX) + length(Table[xTargetName])
      finalTable <- data.frame(xAxisName = character(), yAxisName = character(), Wearable = character())
      colnames(finalTable)[1] <- xAxisName
      colnames(finalTable)[2] <- yAxisName

      if(nrOfItemsX < nrOfItems)
      {
        print("!!!!!!! WARNING!!!!, X Collumn is smaller than Y collumn. Reverse axises!!!!!!")
      }
      print("Sorting X axis")
      rowDest <- 1
      for(i in 1:nrOfItemsX)
      {
        collumn <- tempTable[i, xTargetName]
        nrOfElements <- lengths(regmatches(collumn, gregexpr(",", collumn))) + 1
        #print(nrOfElements)
        colComponents <- as.vector(str_split_fixed(collumn, pattern = ", ", n = nrOfItemsX))
        #print(colComponents)
        for(j in 1:nrOfElements)
        {
          finalTable[rowDest, yAxisName] <- tempTable[i, yTargetName]
          finalTable[rowDest, xAxisName] <- trimws(colComponents[j], whitespace = "[ \t\r\n]")
          finalTable[rowDest,"Wearable"] <- data[i,"Wearable"]
          rowDest <- rowDest + 1
        }
      }
      finalTable <- finalTable[rowSums(is.na(finalTable)) == 0,]
      tempData2 <<- finalTable
      tempTable <- finalTable
      textAngle <- 60
      textVjust <- 1 
      textHjust <- 1.01
    }
    else
    {
      print(cat("Y axis is not Year, it is: ", yAxisName ))
      nrOfItems <- lengths(regmatches(data[, yTargetName], gregexpr(",", data[yTargetName])))
      #print(nrOfItems)
      nrOfItems <- sum(nrOfItems) + length(Table[yTargetName])
      #print(nrOfItems)
      tempTable <- data.frame(Year = numeric(), yAxisName = character(), Wearable = character())
      colnames(tempTable)[1] <- xAxisName
      colnames(tempTable)[2] <- yAxisName
      rowDest <- 1
      for(i in 1:nrOfItems)
      {
        collumn <- data[i, yTargetName]
        nrOfElements <- lengths(regmatches(collumn, gregexpr(",", collumn))) + 1
        #print(nrOfElements)
        colComponents <- as.vector(str_split_fixed(collumn, pattern = ", ", n = nrOfItems))
        for(j in 1:nrOfElements)
        {
          tempTable[rowDest, xAxisName] <- data[i, xTargetName]
          tempTable[rowDest, yAxisName] <- trimws(colComponents[j], whitespace = "[ \t\r\n]")
          tempTable[rowDest,"Wearable"] <- data[i,"Wearable"]
          rowDest <- rowDest + 1
        }
      }
      tempTable <- tempTable[rowSums(is.na(tempTable)) == 0,]
      #tempData <<- tempTable
      
      textAngle <- 0
      textVjust <- 0 
      textHjust <- 0.5
    }
    #create the scatter plot
    scatter <- ggplot(data = tempTable, aes(x = tempTable[,xAxisName], y = tempTable[,yAxisName])) +
      #geom_point(aes(colour = tempTable[,yAxisName], shape = Wearable), show.legend = FALSE) +
      geom_count(aes(colour = tempTable[,yAxisName], shape = Wearable), show.legend = FALSE, alpha = 0.5) +
      theme_solarized(light = FALSE) + 
      scale_size_area() + 
      ylab(yAxisName) +
      xlab(xAxisName) +
      theme(plot.margin = unit(c(0,0, 0, 0), "cm"), text=element_text(colour="white", size=14), axis.text.y = element_text(colour="black"),
            axis.text.x = element_text(colour="black", angle = textAngle, vjust = textVjust, hjust = textHjust),
            panel.background = element_rect(fill="white", colour="white",size = 0.5, linetype = "solid"),
            panel.grid.major = element_line(size = 0.25, linetype = 'dashed',
                                            colour = "grey"), 
            panel.grid.minor = element_line(size = 0.25, linetype = 'dashed',
                                            colour = "grey"),
            plot.background = element_rect(fill = "white")) 
    
    #create the histogram  
    hist_right <- ggplot(data = tempTable, aes(x = tempTable[,yAxisName])) + 
      geom_bar(width = 0.5, aes(colour = tempTable[,yAxisName], fill = tempTable[,yAxisName]), show.legend = FALSE) + 
      coord_flip() +
      stat_count(aes(y=..count.., label=..count..), geom="text", hjust= 1.2) + 
      theme_solarized(light = FALSE) + 
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            axis.title.y = element_blank(), plot.margin = unit(c(0, 0, 0, 0), "cm"),
            axis.line.y = element_line(colour = "black", size = 1, linetype = "solid"),
            text=element_text(colour="white", size = 14),axis.text.x = element_text(colour="black"),
            panel.background = element_rect(fill="white", colour="white",size = 0.5, linetype = "solid"),
            panel.grid.major = element_line(size = 0.25, linetype = 'dashed',
                                            colour = "grey"), 
            panel.grid.minor = element_line(size = 0.25, linetype = 'dashed',
                                            colour = "grey"),
            plot.background = element_rect(fill = "white")) +
      scale_y_continuous(expand = expansion(mult = c(0, 0)))

    if(xAxisName == "Number_of_Participants")
    {print("!!!!!! WARNING!!!!! Number of participants should be on the y Axis(second argument)")}
    
    if(yAxisName == "Number_of_Participants")
    {
      print("Y axis is Number of Participants; ordering Participants")
      
      levels =  c("0", "1", "2","3","4","5","6","7","8","9",
                  "10", "11", "12","13","14","15","16","17","18","19",
                  "20", "21", "22","23","24","25","26","27","28","29",
                  "30", "31", "32","33","34","35","36","37","38","39",
                  "40", "41", "42","43","44","45","46","47","48","49",
                  "50", "51", "52","53","54","55","56","57","58","59",
                  "60", "61", "62","63","64","65","66","67","68","69",
                  "70")
      #remove all spaces
      tempTable$Number_of_Participants <- gsub('\\s+', '',  tempTable$Number_of_Participants)

      #order
      tempTable$Number_of_Participants <- factor(tempTable$Number_of_Participants, level = levels)
      tempTable <- tempTable[tempTable$Number_of_Participants != "N/A", ]
      tempTable <- na.omit(tempTable)
      which(is.na(tempTable$Number_of_Participants))
      level_order <- factor(tempTable$Number_of_Participants, level = levels);
      level_order;
      #extract numericals from the participants to calculate the mean later
      temp <- as.numeric(tempTable$Number_of_Participants) - 1
      temp = temp[temp!= 100]
      temp = temp[temp!= 101]
      
      scatter <- ggplot(data = tempTable, aes(x = tempTable[,xAxisName], level_order)) +
        geom_count(aes(colour = tempTable[,yAxisName], shape = Wearable), show.legend = FALSE, alpha = 0.5) +
        theme_solarized(light = FALSE) + 
        scale_size_area() + 
        ylab(yAxisName) +
        xlab(xAxisName) +
        scale_y_discrete(drop = FALSE) +
        theme(plot.margin = unit(c(0,0, 0, 0), "cm"), text=element_text(colour="white", size=14),
              axis.text.y = element_text(colour="black",size=8),
              axis.text.x = element_text(colour="black", angle = textAngle, vjust = textVjust, hjust = textHjust),
              panel.background = element_rect(fill="white", colour="white",size = 0.5, linetype = "solid"),
              panel.grid.major = element_line(size = 0.25, linetype = 'dashed',
                                              colour = "grey"), 
              panel.grid.minor = element_line(size = 0.25, linetype = 'dashed',
                                              colour = "grey"),
              plot.background = element_rect(fill = "white")) 
      
      #create the histogram  
      hist_right <- ggplot(data = tempTable, aes(x = tempTable[,yAxisName])) + 
        geom_bar(width = 0.5, aes(colour = tempTable[,yAxisName], fill = tempTable[,yAxisName]), show.legend = FALSE) +
        geom_vline(aes(xintercept = mean(as.numeric(temp))), colour = "orange", size = 1, linetype = 2 )+
        coord_flip() +
        stat_count(aes(y=..count.., label=..count..), geom="text", hjust= 1.2) + 
        theme_solarized(light = FALSE) + 
        scale_x_discrete(drop = FALSE) +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
              axis.title.y = element_blank(), plot.margin = unit(c(0, 0, 0, 0), "cm"),
              axis.line.y = element_line(colour = "black", size = 1, linetype = "solid"),
              text=element_text(colour="white", size = 14),axis.text.x = element_text(colour="black"),
              panel.background = element_rect(fill="white", colour="white",size = 0.5, linetype = "solid"),
              panel.grid.major = element_line(size = 0.25, linetype = 'dashed',
                                              colour = "grey"), 
              panel.grid.minor = element_line(size = 0.25, linetype = 'dashed',
                                              colour = "grey"),
              plot.background = element_rect(fill = "white"))+
         scale_y_continuous(expand = expansion(mult = c(0, 0)))
    }
    
    if(xAxisName == "Number_of_Actuators")
    {print("!!!!!! WARNING!!!!! Number of actuators should be on the y Axis(second argument)")}
    
    if(yAxisName == "Number_of_Actuators")
    {
      print("Y axis is Nr of Actuators; Ordering actuators!")
      levels =  c("0", "1", "2","3","4","5","6","7","8","9",
                  "10", "11", "12","13","14","15","16","17","18","19",
                  "20", "21", "22","23","24","...",
                  "60", "61", "62","63","64")
      
      #remove all spaces
      tempTable$Number_of_Actuators <- gsub('\\s+', '',  tempTable$Number_of_Actuators)
      #order
      tempTable$Number_of_Actuators <- factor(tempTable$Number_of_Actuators, level = levels)
      tempTable <- tempTable[tempTable$Number_of_Actuators != "N/A", ]
      tempTable <- na.omit(tempTable)
      which(is.na(tempTable$Number_of_Actuators))
      level_order <- factor(tempTable$Number_of_Actuators, level = levels);
      
      actTemp <- as.numeric(tempTable$Number_of_Actuators) - 1
      actTemp = actTemp[actTemp!= 100]
      actTemp = actTemp[actTemp!= 101]
      
      scatter <- ggplot(data = tempTable, aes(x = tempTable[,xAxisName], level_order)) +
        geom_count(aes(colour = tempTable[,yAxisName], shape = Wearable), show.legend = FALSE, alpha = 0.5) +
        theme_solarized(light = FALSE) + 
        scale_size_area() + 
        ylab(yAxisName) +
        xlab(xAxisName) +
        scale_y_discrete(drop = FALSE) +
        theme(plot.margin = unit(c(0,0, 0, 0), "cm"), text=element_text(colour="white", size=14),
              axis.text.y = element_text(colour="black"),
              axis.text.x = element_text(colour="black", angle = textAngle, vjust = textVjust, hjust = textHjust),
              panel.background = element_rect(fill="white", colour="white",size = 0.5, linetype = "solid"),
              panel.grid.major = element_line(size = 0.25, linetype = 'dashed',
                                              colour = "grey"), 
              panel.grid.minor = element_line(size = 0.25, linetype = 'dashed',
                                              colour = "grey"),
              plot.background = element_rect(fill = "white")) 
      
      #create the histogram  
      hist_right <- ggplot(data = tempTable, aes(x = tempTable[,yAxisName])) + 
        geom_bar(width = 0.5, aes(colour = tempTable[,yAxisName], fill = tempTable[,yAxisName]), show.legend = FALSE) +
        geom_vline(aes(xintercept = mean(as.numeric(actTemp))), colour = "orange", size = 1, linetype = 2 )+
        coord_flip() +
        scale_x_discrete(drop = FALSE) +
        stat_count(aes(y=..count.., label=..count..), geom="text", hjust= 1.2) + 
        theme_solarized(light = FALSE) + 
        theme(axis.text.y = element_blank(),
              axis.line.y = element_line(colour = "black", size = 1, linetype = "solid"),
              axis.ticks.y = element_blank(),
              axis.title.y = element_blank(), plot.margin = unit(c(0, 0, 0, 0), "cm"),
              text=element_text(colour="white", size = 14),axis.text.x = element_text(colour="black"),
              panel.background = element_rect(fill="white", colour="white",size = 0.5, linetype = "solid"),
              panel.grid.major = element_line(size = 0.25, linetype = 'dashed',
                                              colour = "grey"), 
              panel.grid.minor = element_line(size = 0.25, linetype = 'dashed',
                                              colour = "grey"),
              plot.background = element_rect(fill = "white")) +
        scale_y_continuous(expand = expansion(mult = c(0, 0)))
    }
   #Toggle histogram On/Off
     if(histogram == TRUE)
    {
      #dualGraph <- grid.arrange(scatter, hist_right, ncol = 2, nrow = 1, widths=c(3, 1))
       dualGraph <- ggarrange(scatter, hist_right, ncol = 2, nrow = 1, widths=c(3, 1))
      return(dualGraph)
    }
    else
    {
      #print("HIST IS FALSE")
      return(scatter)
    }
  }

# "Year", "Purpose", "Listening_Situation", "Number_of_Actuators", "Actuators_Type", "Signals_Used",
# "DSP", "Mapping_Scheme", "Body_Area_Actuated", "Wearable", "Evaluation_Measure", "Evaluation_Measure_Detailed",
# "Evaluation_Population", "Number_of_Participants", "Evaluate on normal hearing users with C/I Simulation", "Provides deffinitions"
# " noHist",

df <- drawGraphs(Table, "Mapping_Scheme", "Number_of_Actuators", "Mapping_Scheme", "Number_of_Actuators", TRUE)
ggsave(paste("Mapping_Scheme"," vs ","Number_of_Actuators",".png",sep=""), plot=df, height=700, width=1000, scale = 3, units=c("px"), dpi=300, type = 'cairo')
df
