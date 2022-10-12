restaurant_inspections <- read.csv("DOHMH_New_York_City_Restaurant_Inspection_Results.csv")
restaurant_inspections_filtered <- restaurant_inspections[!(is.na(restaurant_inspections$VIOLATION.DESCRIPTION) | restaurant_inspections$VIOLATION.DESCRIPTION==""), ]
dim(restaurant_inspections)
dim(restaurant_inspections_filtered)
dim(restaurant_inspections_filtered_1)
dim(restaurant_inspections_filtered_2)
restaurant_inspections_filtered_1 <- restaurant_inspections_filtered[!(is.na(restaurant_inspections_filtered$Latitude) | restaurant_inspections_filtered$Latitude=="" | 
                                                                         is.na(restaurant_inspections_filtered$Longitude) | restaurant_inspections_filtered$Longitude=="") , ]
restaurant_inspections_filtered_1$dateandtime <- as.POSIXct(restaurant_inspections_filtered_1$INSPECTION.DATE, format = "%m/%d/%Y")
restaurant_inspections_filtered_1$years <- format(restaurant_inspections_filtered_1$dateandtime, format = "%Y")
restaurant_inspections_filtered_2 <- restaurant_inspections_filtered_1[which(restaurant_inspections_filtered_1$years %in% c("2018", "2019", "2020",
                                                                                                                            "2021", "2022", "2017",
                                                                                                                            "2016")),]


restaurant_inspections_filtered_3 <- restaurant_inspections_filtered_2[order(restaurant_inspections_filtered_2$DBA, 
                                                                                     as.Date(restaurant_inspections_filtered_2$INSPECTION.DATE, format = "%m/%d/%Y")),]



restaurant_inspections_filtered_3$criticalviolations <- ifelse(restaurant_inspections_filtered_3$CRITICAL.FLAG == "Critical", 1, 0)
restaurant_inspections_filtered_3$noncriticalviolations <- ifelse(restaurant_inspections_filtered_3$CRITICAL.FLAG == "Not Critical", 1, 0)


                                                                    
df1 <- aggregate(cbind(restaurant_inspections_filtered_3$criticalviolations,restaurant_inspections_filtered_3$noncriticalviolations) ~ 
                   restaurant_inspections_filtered_3$DBA, data = restaurant_inspections_filtered_3, FUN = sum, na.rm = TRUE)

df1$criticalYN <- ifelse(df1$V1 > df1$V2, "Critical", "Non-Critical")
df1$DBA <- df1$`restaurant_inspections_filtered_3$DBA`
df1$critical <- df1$V1
df1$noncritical <- df1$V2
df1 <-subset(df1, select = -c(V1, V2, `restaurant_inspections_filtered_3$DBA`))


restaurant_inspections_filtered_3 <-subset(restaurant_inspections_filtered_3, select = -c(VIOLATION.CODE, CRITICAL.FLAG))
total <- merge(df1, restaurant_inspections_filtered_3,by="DBA")
total1 <- total %>% distinct(DBA, .keep_all = TRUE)
View(total1 )


