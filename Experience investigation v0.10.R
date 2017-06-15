library("data.table")
library("reshape")

# Define global grouping variables
# these are variables used to group the output of the experience investigation
GroupingVariables <- list(AGE =  1:120,
                          POL_DUR_M = 1:600,
                          SMOKER = 0:1,
                          SEX = c("M","F")
                          )

GroupingVariableBands <- list(AGE = c(0,18,35,70),
                              POL_DUR_M = c(1:3,6,12*(1:20))
                              )

SupportingInformation <- list(Exposure_Definitions =c("Central","Initial")
                              )

# Choose 1 for Central and 2 for Initial exposed to risk
Exposure_Definition <- SupportingInformation$Exposure_Definitions[1]
  
################# Supporting tables ##################

# Import mapping tables
ImportSupportingTables <- function()
{
  # define the folder name within the working directory
  FolderName <- "Supporting tables"
  Data_Mapping_Table              <- data.frame(fread(paste0("./",FolderName,"/Data_Mapping_Table.csv")             ))
  Decrement_Mapping_Table         <- data.frame(fread(paste0("./",FolderName,"/Decrement_Mapping_Table.csv")        ))
  Grouping_Elements_Mapping_Table <- data.frame(fread(paste0("./",FolderName,"/Grouping_Elements_Mapping_Table.csv")))
  Status_Codes_Mapping_Table      <- data.frame(fread(paste0("./",FolderName,"/Status_Codes_Mapping_Table.csv")     ))
  Investigation_Periods           <- data.frame(fread(paste0("./",FolderName,"/Investigation_Periods.csv")          ))
  User_defined_output             <- data.frame(fread(paste0("./",FolderName,"/User_defined_output.csv"),
                                                    colClasses = c(WEIGHT="character")))

  ImportMTListOutput <- list(Data_Mapping_Table              = Data_Mapping_Table,
                             Decrement_Mapping_Table         = Decrement_Mapping_Table,
                             Grouping_Elements_Mapping_Table = Grouping_Elements_Mapping_Table,
                             Status_Codes_Mapping_Table      = Status_Codes_Mapping_Table,
                             Investigation_Periods           = Investigation_Periods,
                             User_defined_output             = User_defined_output
                             )
  
  return(ImportMTListOutput)
}

################# Data input and formatting ##################

ImportPolicyData <- function(Product)
{
  # define the folder name within the working directory
  FolderName <- "Data"
  # keep this is for testing for now...
  # NrPols <- 100000
  
  Data_Mapping_Table <- ImportSupportingTables()$Data_Mapping_Table
  Data_Table_Name    <- Data_Mapping_Table$TABLE_NAME[which(Data_Mapping_Table$PRODUCT == Product)]

  # Read in the first NrPols lines of the data file for testing
  # PolicyData <- data.frame(fread(paste0("./",FolderName,"/",Data_Table_Name), header = TRUE, sep=",", nrows = NrPols)) # used for testing
  PolicyData <- data.frame(fread(paste0("./",FolderName,"/",Data_Table_Name), header = TRUE, sep=",")) # run this for all policies

  print("Data imported successfully")
  
  #more cleaning to be done e.g. cleaning of sum assured, premiums etc
  
  #convert dates in data to dates
  PolicyData$DOB   <- as.Date(PolicyData$DOB, format="%d/%m/%Y")
  PolicyData$ENTRY <- as.Date(PolicyData$ENTRY, format="%d/%m/%Y")
  PolicyData$EXIT  <- as.Date(PolicyData$EXIT, format="%d/%m/%Y")
  
  PolicyDataListOutput <- list(PolicyData = PolicyData)
  
  return(PolicyDataListOutput)
}

################# Input decrement tables ##################
  
ImportDecrementTables <- function(Product, Decrement)
{
  # define the folder name within the working directory
  FolderName <- "Decrement tables"

  Decrement_Mapping_Table          <- ImportSupportingTables()$Decrement_Mapping_Table
  Decrement_Mapping_Table_RowIndex <- which(Decrement_Mapping_Table$PRODUCT==Product & Decrement_Mapping_Table$DECREMENT==Decrement)
  Decrement_Table_Name             <- Decrement_Mapping_Table$TABLE_NAME[Decrement_Mapping_Table_RowIndex]
  AIDS_INDICATOR                   <- Decrement_Mapping_Table$AIDS_INDICATOR[Decrement_Mapping_Table_RowIndex]
  Decrement_Table                  <- fread(paste0("./",FolderName,"/",Decrement_Table_Name), header = TRUE, sep=",")
  Decrement_Table$DEC_RATE         <- Decrement_Table$DEC_RATE * Decrement_Mapping_Table$MULTIPLIER[Decrement_Mapping_Table_RowIndex]
  if(AIDS_INDICATOR == 1)
  {
    AIDS_Decrement_Table_Name      <- Decrement_Mapping_Table$AIDS_TABLE_NAME[Decrement_Mapping_Table_RowIndex]
    AIDS_Decrement_Table           <- fread(paste0("./",FolderName,"/",AIDS_Decrement_Table_Name), header = TRUE, sep=",")

    AIDS_Decrement_Table_Long<- melt(AIDS_Decrement_Table,id=c("AGE","SEX"))
    names(AIDS_Decrement_Table_Long)[-c(1,2)] <- c("CALENDAR_YEAR","DEC_RATE")
    AIDS_Decrement_Table_Long$DEC_RATE   <- AIDS_Decrement_Table_Long$DEC_RATE * Decrement_Mapping_Table$AIDS_MULTIPLIER[Decrement_Mapping_Table_RowIndex]
  }
  if(Exposure_Definition == "Central")
  {
    Decrement_Table$DEC_RATE           <- -log(1 - Decrement_Table$DEC_RATE)
    if(AIDS_INDICATOR == 1)
    {
      AIDS_Decrement_Table_Long$DEC_RATE <- -log(1 - AIDS_Decrement_Table_Long$DEC_RATE)
    }
    
    
  }
  
  DecrementsListOutput <- list(Decrement_Table = Decrement_Table,
                               AIDS_INDICATOR = AIDS_INDICATOR,
                               AIDS_Decrement_Table_Long = if(AIDS_INDICATOR == 1){AIDS_Decrement_Table_Long} else NULL)
  
  return(DecrementsListOutput)
}

################# Control information ##################

ControlInformation <- function(Product,RUN_NR)
{
  # Import the supporting tables list
  Supporting_Tables <- ImportSupportingTables()
  
  # Status codes to be used for decrement
  Active_Status_Codes <- Supporting_Tables$Status_Codes_Mapping_Table$STATUS[
    which(Supporting_Tables$Status_Codes_Mapping_Table$DECREMENT=="Active")]

  Death_Status_Codes <- Supporting_Tables$Status_Codes_Mapping_Table$STATUS[
    which(Supporting_Tables$Status_Codes_Mapping_Table$DECREMENT=="Death")]
  
  Disability_Status_Codes <- Supporting_Tables$Status_Codes_Mapping_Table$STATUS[
    which(Supporting_Tables$Status_Codes_Mapping_Table$DECREMENT=="Disability")]
  
  Lapse_Status_Codes <- Supporting_Tables$Status_Codes_Mapping_Table$STATUS[
    which(Supporting_Tables$Status_Codes_Mapping_Table$DECREMENT=="Lapse")]
  
  Other_Status_Codes <- Supporting_Tables$Status_Codes_Mapping_Table$STATUS[
    !(Supporting_Tables$Status_Codes_Mapping_Table$DECREMENT %in% c("Lapse","Disability","Death","Active"))]

  # Import policy data for product
  PolicyData <- ImportPolicyData(Product = Product)$PolicyData
  
  Investigation_Periods <- Supporting_Tables$Investigation_Periods
  Investigation_Periods <- Investigation_Periods[which(Investigation_Periods$RUN_NR == RUN_NR),]
  print(paste("Investigation periods imported"))
  if(nrow(Investigation_Periods) == 0) stop(paste("No investigation periods defined for run number",RUN_NR))
  
  for(j in 1:nrow(Investigation_Periods))
  {
    extractstart <- as.Date(Investigation_Periods$START_DATE[j], format="%d/%m/%Y")
    extractend   <- as.Date(Investigation_Periods$END_DATE[j]  , format="%d/%m/%Y")
    
    ##Set missing dates as the end date
    PolicyData$EXIT_date <- PolicyData$EXIT #need separate variable for loop to work
    PolicyData$EXIT_date[is.na(PolicyData$EXIT)] <- extractend
    
    #as.numeric to prevent integer error later on
    PolicyData$PREM <- as.numeric(PolicyData$PREM)
    
    # remove policies with exit date before the start date and entry date after the end date
    PolicyData_j    <- PolicyData[PolicyData$EXIT_date>=extractstart & PolicyData$ENTRY <= extractend,]
   
    ##Function to keep dates as dates in ifelse##
    safe.ifelse <- function(cond, yes, no){ class.y <- class(yes)
    X <- ifelse(cond,yes,no)
    class(X) <-class.y; return(X)}
    
    #set date at end date or exit, whichever is earlier. S
    PolicyData_j$FinalDate     <- safe.ifelse(PolicyData_j$EXIT_date<extractend,PolicyData_j$EXIT_date, extractend ) #find smaller of exit and end date
    PolicyData_j$FirstDate     <- safe.ifelse(PolicyData_j$ENTRY>extractstart,PolicyData_j$ENTRY, extractstart ) #find smaller of exit and end date
    
    PolicyData_j_Active_OP <- PolicyData[which((PolicyData$EXIT_date > (extractstart-1)) &
                                               (PolicyData$ENTRY <= (extractstart-1))),]
    
    PolicyData_j_NB <- PolicyData_j[which(PolicyData_j$ENTRY >= extractstart),]
    
    #PolicyData_j_Active <- PolicyData_j[which(PolicyData_j$EXIT_date == extractend & PolicyData_j$STATUS %in% Active_Status_Codes),]
    
    ###NR Policies###
    
    NR_POLS_IF_OP <- nrow(PolicyData_j_Active_OP)
    NR_POLS_NB    <- nrow(PolicyData_j_NB)
    NR_POLS_DEATH <- nrow(PolicyData_j[which(PolicyData_j$STATUS %in% Death_Status_Codes & PolicyData_j$EXIT_date <= extractend),])
    NR_POLS_DISAB <- nrow(PolicyData_j[which(PolicyData_j$STATUS %in% Disability_Status_Codes & PolicyData_j$EXIT_date <= extractend),])
    NR_POLS_LAPSE <- nrow(PolicyData_j[which(PolicyData_j$STATUS %in% Lapse_Status_Codes & PolicyData_j$EXIT_date <= extractend),])
    NR_POLS_OTHER <- nrow(PolicyData_j[which(! PolicyData_j$STATUS %in% c(Death_Status_Codes,Disability_Status_Codes,Lapse_Status_Codes,Active_Status_Codes) & PolicyData_j$EXIT_date <= extractend),])
    NR_OFFS       <- sum(NR_POLS_DEATH,NR_POLS_DISAB, NR_POLS_LAPSE, NR_POLS_OTHER)
    NR_POLS_IF    <- nrow(PolicyData_j[which(PolicyData_j$EXIT_date >= extractend & !(PolicyData_j$EXIT == extractend & !(PolicyData_j$STATUS %in% Active_Status_Codes))),])
    NR_POLS_CHECK <- NR_POLS_IF_OP+NR_POLS_NB- NR_OFFS -NR_POLS_IF
    
    NR_POLS <- c(NR_POLS_IF_OP,NR_POLS_NB,NR_POLS_DEATH,NR_POLS_DISAB,NR_POLS_LAPSE,NR_POLS_OTHER,NR_OFFS,NR_POLS_IF,NR_POLS_CHECK)
    
    ###Premium###
    PREM_IF_OP    <- sum(PolicyData_j_Active_OP$PREM)
    PREM_IF_NB    <- sum(PolicyData_j_NB$PREM)
    PREM_OF_DEATH <- sum(PolicyData_j[which(PolicyData_j$STATUS %in% Death_Status_Codes & PolicyData_j$EXIT_date <= extractend),]$PREM)
    PREM_OF_DISAB <- sum(PolicyData_j[which(PolicyData_j$STATUS %in% Disability_Status_Codes & PolicyData_j$EXIT_date <= extractend),]$PREM)
    PREM_OF_LAPSE <- sum(PolicyData_j[which(PolicyData_j$STATUS %in% Lapse_Status_Codes & PolicyData_j$EXIT_date <= extractend),]$PREM)
    PREM_OF_OTHER <- sum(PolicyData_j[which(! PolicyData_j$STATUS %in% c(Death_Status_Codes,Disability_Status_Codes,Lapse_Status_Codes,Active_Status_Codes) & PolicyData_j$EXIT_date <= extractend),]$PREM)
    PREM_OF_TOTAL <- sum(PREM_OF_DEATH,PREM_OF_DISAB,PREM_OF_LAPSE,PREM_OF_OTHER)
    PREM_IF       <- sum(PolicyData_j[which(PolicyData_j$EXIT_date >= extractend & !(PolicyData_j$EXIT == extractend & !(PolicyData_j$STATUS %in% Active_Status_Codes))),]$PREM)
    PREM_CHECK    <- PREM_IF_OP+PREM_IF_NB- PREM_OF_TOTAL -PREM_IF
    
    PREMS <- c(PREM_IF_OP,PREM_IF_NB,PREM_OF_DEATH,PREM_OF_DISAB, PREM_OF_LAPSE, PREM_OF_OTHER, PREM_OF_TOTAL,PREM_IF,PREM_CHECK)
    
    ###Sum Assured##
    
    SUM_ASS_IF_OP    <- sum(as.numeric(PolicyData_j_Active_OP$SUM_ASS))
    SUM_ASS_IF_NB    <- sum(PolicyData_j_NB$SUM_ASS)
    SUM_ASS_OF_DEATH <- sum(PolicyData_j[which(PolicyData_j$STATUS %in% Death_Status_Codes & PolicyData_j$EXIT_date <= extractend),]$SUM_ASS)
    SUM_ASS_OF_DISAB <- sum(PolicyData_j[which(PolicyData_j$STATUS %in% Disability_Status_Codes & PolicyData_j$EXIT_date <= extractend),]$SUM_ASS)
    SUM_ASS_OF_LAPSE <- sum(PolicyData_j[which(PolicyData_j$STATUS %in% Lapse_Status_Codes & PolicyData_j$EXIT_date <= extractend),]$SUM_ASS)
    SUM_ASS_OF_OTHER <- sum(PolicyData_j[which(! PolicyData_j$STATUS %in% c(Death_Status_Codes,Disability_Status_Codes,Lapse_Status_Codes,Active_Status_Codes) & PolicyData_j$EXIT_date <= extractend),]$SUM_ASS)
    SUM_ASS_OF_TOTAL <- sum(SUM_ASS_OF_DEATH,SUM_ASS_OF_DISAB,SUM_ASS_OF_LAPSE,SUM_ASS_OF_OTHER)
    SUM_ASS_IF       <- sum(PolicyData_j[which(PolicyData_j$EXIT_date >= extractend & !(PolicyData_j$EXIT == extractend & !(PolicyData_j$STATUS %in% Active_Status_Codes))),]$SUM_ASS)
    SUM_ASS_CHECK    <- SUM_ASS_IF_OP+SUM_ASS_IF_NB -SUM_ASS_OF_TOTAL-SUM_ASS_IF
    
    SUMASS <- c(SUM_ASS_IF_OP,SUM_ASS_IF_NB,SUM_ASS_OF_DEATH,SUM_ASS_OF_DISAB, SUM_ASS_OF_LAPSE, SUM_ASS_OF_OTHER, SUM_ASS_OF_TOTAL,SUM_ASS_IF,SUM_ASS_CHECK)
    
    ##Approximate premium recevied in period
    PREM_Control_Info <- sum((as.numeric(PolicyData_j$FinalDate - PolicyData_j$FirstDate)/365.25)*PolicyData_j$PREM/12)
    
    sum(PolicyData$ENTRY < extractstart-1)

    ControlOutput <- data.frame(Build_up = c("Opening","NB","Deaths","Disability","Lapses","Other","Total_OFFS","Closing", "Check"),
                                 Period = paste0(extractstart,"_to_",extractend),
                                 NR_POLS  = NR_POLS,
                                 PREM_IN_FORCE = PREMS,
                                 SUM_ASS = SUMASS,
                                 PREM_RECEIVED=NA)
    
    ControlOutput$PREM_RECEIVED[8] <- PREM_Control_Info   

    if(j == 1)
    {
      ControlOutputFinal <- ControlOutput
    } else
      ControlOutputFinal <- rbind(ControlOutputFinal,ControlOutput)
    
  }
    
  write.csv(ControlOutputFinal,file = paste0("./Output/",Product,"_Control_Info.csv"))  
}


################# Temporary tables for a single policyholder ##################

PerPolicyTables <- function(PerPolicyTables_ListInput)
{
  Grouping_elements               <- PerPolicyTables_ListInput$Grouping_elements
  PerPolicy_Key_Info              <- PerPolicyTables_ListInput$PerPolicy_Key_Info
  PerPolicy_Grouping_Elements     <- data.frame(PerPolicyTables_ListInput$PerPolicy_Grouping_Elements,row.names = NULL) # as this data.frame is used as a list to define new variables in tt1 below we have to remove the row names
  Weighting                       <- PerPolicyTables_ListInput$Weighting
  Decrement_Status_Codes          <- PerPolicyTables_ListInput$Decrement_Status_Codes
  Decrement_Table                 <- PerPolicyTables_ListInput$Decrement_Table
  AIDS_Decrement_Table_Long       <- PerPolicyTables_ListInput$AIDS_Decrement_Table_Long
  CALENDAR_YEAR_start             <- PerPolicyTables_ListInput$CALENDAR_YEAR_start
  AIDS_INDICATOR                  <- PerPolicyTables_ListInput$AIDS_INDICATOR  
  
  STATUS <- PerPolicy_Key_Info$STATUS
  
  if("AGE" %in% Grouping_elements)
  {
    AgeDur_Start <- PerPolicy_Key_Info$AgeStart
    AgeDur_End   <- PerPolicy_Key_Info$AgeEnd
    tt1          <- data.frame(AGE = floor(AgeDur_Start):floor(AgeDur_End),
                      CALENDAR_YEAR = CALENDAR_YEAR_start:(CALENDAR_YEAR_start + floor(AgeDur_End)-floor(AgeDur_Start)),
                      PerPolicy_Grouping_Elements,
                      ExposureUnweighted = 1,
                      Exposure = Weighting,
                      ActualDecrementsUnweighted = 0,
                      ActualDecrements = 0,
                      ExpectedDecrementsUnweighted = 0,
                      ExpectedDecrements = 0
                      )
  } else if("POL_DUR_M" %in% Grouping_elements)
  {
    AgeDur_Start <- PerPolicy_Key_Info$DurStart
    AgeDur_End   <- PerPolicy_Key_Info$DurEnd
    tt1          <- data.frame(POL_DUR_M = floor(AgeDur_Start):floor(AgeDur_End),
                      PerPolicy_Grouping_Elements,
                      ExposureUnweighted = 1,
                      Exposure = Weighting,
                      ActualDecrementsUnweighted = 0,
                      ActualDecrements = 0,
                      ExpectedDecrementsUnweighted = 0,
                      ExpectedDecrements = 0
                      )
  }
  
  ### Exposure, unweighted
  # in the first period, the exposure is the fraction of the period remaining, or if exit was in the same period, the difference between the end and start durations
  tt1$ExposureUnweighted[1]         <- min(ceiling(AgeDur_Start)-AgeDur_Start,AgeDur_End-AgeDur_Start)
  # If the reason for exit is for the current decrement being modelled, the exposure should be calculated as 1, else a probability in excess of 1 would be calculated
  # in the last period, the exposure is the fraction of duration, or if exit was in the same period, the difference between the end and start durations
  tt1$ExposureUnweighted[nrow(tt1)] <- if(Exposure_Definition == "Initial")
  {
    if(STATUS %in% Decrement_Status_Codes & floor(AgeDur_Start) == floor(AgeDur_End))
                                       {
                                         ceiling(AgeDur_End) - AgeDur_Start
                                       } else if(STATUS %in% Decrement_Status_Codes)
                                       {
                                          1
                                       } else
                                          min(AgeDur_End - floor(AgeDur_End),AgeDur_End - AgeDur_Start)
  }                               else if(Exposure_Definition == "Central")
                                      {
                                          min(AgeDur_End - floor(AgeDur_End),AgeDur_End - AgeDur_Start)
                                      }
  
  ### Exposure, weighted
  tt1$Exposure <- tt1$ExposureUnweighted * Weighting

  ### Actual Decrements
  tt1$ActualDecrementsUnweighted[nrow(tt1)]<-if(STATUS %in% Decrement_Status_Codes) {1} else 0
  tt1$ActualDecrements<- tt1$ActualDecrementsUnweighted * Weighting
  
  ### Expected decrements
  tt1_DEC_RATE <- merge(tt1,Decrement_Table,by=Grouping_elements)[["DEC_RATE"]]
  if(AIDS_INDICATOR == 1)
  {
    ttAIDS <- merge(tt1,AIDS_Decrement_Table_Long,by=c("AGE","SEX","CALENDAR_YEAR"))[["DEC_RATE"]]
    tt1_DEC_RATE <- tt1_DEC_RATE + ttAIDS
  }

  tt1$ExpectedDecrementsUnweighted <- tt1$ExposureUnweighted * tt1_DEC_RATE
  tt1$ExpectedDecrements           <- tt1$Exposure * tt1_DEC_RATE
  
  ListOutput <- list(tt1 = tt1)
  return(ListOutput)
}

################# Calculate cumulative tables for all policyholders ##################

CumulativeTables <- function(Product,Decrement,WeightingField,RUN_NR)
{
  print(paste("Starting with",Product,Decrement,"runs using",WeightingField,"as weighting field"))

  # Import the supporting tables list
  Supporting_Tables <- ImportSupportingTables()
  # Read in the Grouping elements for the given product and decrement
  Grouping_elements <- Supporting_Tables$Grouping_Elements_Mapping_Table$GROUPING_ELEMENTS[
    which(Supporting_Tables$Grouping_Elements_Mapping_Table$PRODUCT==Product &
            Supporting_Tables$Grouping_Elements_Mapping_Table$DECREMENT==Decrement)]
  print(paste("Grouping elements assigned for",Product,Decrement,"calculation"))
  
  # create temporary table to be used for output of variables
  tt_output_format <- expand.grid(GroupingVariables[names(GroupingVariables) %in% Grouping_elements])  
  
  # list of inputs required for the per policy calcs defined
  PerPolicyTables_ListInput <- list(Grouping_elements = Grouping_elements,
                                    tt_output_format = tt_output_format
                                    )
  
  # Status codes to be used for decrement
  Decrement_Status_Codes <- Supporting_Tables$Status_Codes_Mapping_Table$STATUS[
    which(Supporting_Tables$Status_Codes_Mapping_Table$DECREMENT==Decrement)]
  PerPolicyTables_ListInput$Decrement_Status_Codes <- Decrement_Status_Codes
  
  # Import policy data for product
  PolicyData <- ImportPolicyData(Product = Product)$PolicyData
  
  # Clean data for grouping elements
    if("AGE" %in% Grouping_elements)
    {
      ## clean DoB - cannot use policies without DoB in an age related investigation
      PolicyData <- PolicyData[PolicyData$DOB != "00/00/0000",]
    }
    
    if("SEX" %in% Grouping_elements)
    {
      # Set gender to Female if it's not specified
      PolicyData$SEX   <- ifelse(PolicyData$SEX != "M","F","M")
    }
  
  
  #stopifnot(NrPols>0)
  
  ### import the relevant decrement table
  DecrementTablesOutput <- ImportDecrementTables(Product = Product, Decrement = Decrement)
  Decrement_Table       <- DecrementTablesOutput$Decrement_Table
  AIDS_INDICATOR        <- DecrementTablesOutput$AIDS_INDICATOR
  PerPolicyTables_ListInput$Decrement_Table <- Decrement_Table
  PerPolicyTables_ListInput$AIDS_INDICATOR  <- AIDS_INDICATOR
  
  if(AIDS_INDICATOR == 1){PerPolicyTables_ListInput$AIDS_Decrement_Table_Long <- DecrementTablesOutput$AIDS_Decrement_Table_Long}
  print(paste(Decrement, "decrement table imported"))

  # investigation period...
  
  #Set Investigation Start and End dates (change date to limit analysis to relevant period exclude data or analyse by cal_year )
  
  Investigation_Periods <- Supporting_Tables$Investigation_Periods
  Investigation_Periods <- Investigation_Periods[which(Investigation_Periods$RUN_NR == RUN_NR),]
  print(paste("Investigation periods imported"))
  if(nrow(Investigation_Periods) == 0) stop(paste("No investigation periods defined for run number",RUN_NR))
  
  for(j in 1:nrow(Investigation_Periods))
  {
    extractstart <- as.Date(Investigation_Periods$START_DATE[j], format="%d/%m/%Y")
    extractend   <- as.Date(Investigation_Periods$END_DATE[j]  , format="%d/%m/%Y")
    
    ##Set missing dates as the end date
    PolicyData$EXIT_date <- PolicyData$EXIT #need separate variable for loop to work
    PolicyData$EXIT_date[is.na(PolicyData$EXIT)] <- extractend
    
    CALENDAR_YEAR_start <- year(extractstart)
    PerPolicyTables_ListInput$CALENDAR_YEAR_start <- CALENDAR_YEAR_start
    
    # remove policies with exit date before the start date and entry date after the start date
    PolicyData_j <- PolicyData[PolicyData$EXIT_date>=extractstart & PolicyData$ENTRY <= extractend,]

    ##Function to keep dates as dates in ifelse##
    safe.ifelse <- function(cond, yes, no){ class.y <- class(yes)
    X <- ifelse(cond,yes,no)
    class(X) <-class.y; return(X)}
    
    #set date at end date or exit, whichever is earlier 
    PolicyData_j$FinalDate     <- safe.ifelse(PolicyData_j$EXIT_date<extractend,PolicyData_j$EXIT_date, extractend ) #find smaller of exit and end date
    
    if("AGE" %in% Grouping_elements)
    {
      #set age at start - those who had exited by start date get an NA -refine if needed
      PolicyData_j$AgeStart <- as.numeric((extractstart - PolicyData_j$DOB)/365.25)
      
      ##set end/exit age
      PolicyData_j$AgeEnd   <- as.numeric((PolicyData_j$FinalDate-PolicyData_j$DOB)/365.25)
      
      # defines the per policy information required for the PerPolicyTables function
      PerPolicy_Key_Info_names <- c("AgeStart", "AgeEnd", "STATUS")
      
      print(paste("Age related items calculated"))
      
    } else if("POL_DUR_M" %in% Grouping_elements)
    {
      ##set duration at start
      PolicyData_j$DurStart <- pmax(ifelse(extractstart<PolicyData_j$EXIT_date & extractend > PolicyData_j$ENTRY,
                                           as.numeric((extractstart - PolicyData_j$ENTRY )*12/365.25),
                                           0),0)
      
      ##set duration at end
      PolicyData_j$DurEnd   <- ifelse(extractstart<PolicyData_j$EXIT_date & extractend > PolicyData_j$ENTRY, as.numeric((PolicyData_j$FinalDate-PolicyData_j$ENTRY)*12/365.25),0)
      
      # defines the per policy information required for the PerPolicyTables function
      PerPolicy_Key_Info_names <- c("DurStart", "DurEnd", "STATUS")
      
      print(paste("Duration related items calculated"))
    }
    
    # finds the grouping elements which are also in the Policy Data
    Grouping_elements_in_PolicyData <- names(PolicyData)[names(PolicyData) %in% Grouping_elements]
    print(paste(length(Grouping_elements_in_PolicyData),"grouping elements found in policy data"))
    
    # subsets the Policy data to only include the columns which are grouping elements. This is not done in for loop for model efficiency
    PolicyData_Grouping_Elements <- PolicyData_j[,Grouping_elements_in_PolicyData]
    # do the same for per policy key info
    PolicyData_Key_Info <- PolicyData_j[,PerPolicy_Key_Info_names]

    NrPols <- nrow(PolicyData_j)
    print(paste(NrPols,"policies imported"))
    print("Start with per policy calculations")
    for(i in 1:NrPols)
    {
      PerPolicyTables_ListInput$PerPolicy_Grouping_Elements <- PolicyData_Grouping_Elements[i,]
      PerPolicyTables_ListInput$PerPolicy_Key_Info          <- PolicyData_Key_Info[i,]
      PerPolicyTables_ListInput$Weighting                   <- if(WeightingField=="1"){1} else PolicyData[[WeightingField]][i]
      
      if("AGE" %in% Grouping_elements)
      {
        non_cumulative_count <- length(Grouping_elements) + 1 # need the +1 as CALENDAR YEAR is added as well
      } else if("POL_DUR_M" %in% Grouping_elements)
      {
        non_cumulative_count <- length(Grouping_elements)
      }

      tt1    <- PerPolicyTables(PerPolicyTables_ListInput)$tt1
      
      tt2    <- tt_output_format
      tt2$id <- 1:nrow(tt_output_format)
      tt2    <- merge(tt2,tt1,by=Grouping_elements,all.x = T)
      tt2[is.na(tt2)] <- 0
      tt2    <- tt2[order(tt2$id),]
      tt2    <- tt2[,-match("id",names(tt2))]
      
      if(i == 1)
      {
        tt2_cumulative  <- tt2[,-(1:non_cumulative_count)]
      } else
        tt2_cumulative  <- tt2[,-(1:non_cumulative_count)] + tt2_cumulative
        if(i %% 1000 == 0) print(paste("Policy",i,"of",NrPols,"for",Product,Decrement,"between",extractstart,"and",extractend))
    }
    
    # create the overall output for the given product & decrement 
    ActualDecrementsUnweighted <- sum(tt2_cumulative$ActualDecrementsUnweighted)
    ActualExposureUnweighted <- sum(tt2_cumulative$ActualExposureUnweighted)
    Aggregate_AvE <- sum(tt2_cumulative$ActualDecrements)/sum(tt2_cumulative$ExpectedDecrements)
    
    # create the output for the total of all grouping variables, except the duration based variable
      if("AGE" %in% Grouping_elements)
      {
        tt2_ALL <- aggregate(x=tt2_cumulative,by=list(AGE=tt_output_format$AGE),FUN = sum)
        Grouping_no_Duration <- GroupingVariables[names(GroupingVariables) %in% Grouping_elements & names(GroupingVariables) != "AGE"]
        Grouping_element_duration <- GroupingVariables$AGE
      } else if("POL_DUR_M" %in% Grouping_elements)
      {
        tt2_ALL <- aggregate(x=tt2_cumulative,by=list(POL_DUR_M=tt_output_format$POL_DUR_M),FUN = sum)
        Grouping_no_Duration      <- GroupingVariables[names(GroupingVariables) %in% Grouping_elements & names(GroupingVariables) != "POL_DUR_M"]
        Grouping_element_duration <- GroupingVariables$POL_DUR_M
      }
    
    if(length(Grouping_no_Duration) == 0) # if there are no additional grouping elements, no additional columns are needed for the tt2_ALL table
    {
      tt2_ALL_final <- tt2_ALL
      
    } else # if there are more grouping elements, we need to create an indicator (ALL, 9999) that they represent the totals
    {
      # function to be used in lapply below. For the aggregate output we want to call character variables "ALL" and numeric variables 9999
        repeat_ALL <- function(x) 
        {
          if(class(x) == "character")
          {
          return(rep("ALL",length(Grouping_element_duration)))
          } else
          # use 9999 to represent the aggregate variable
          return(rep(9999,length(Grouping_element_duration)))
        }
        tmp <- lapply(X=Grouping_no_Duration,FUN=repeat_ALL)
        names(tmp) <- names(Grouping_no_Duration)
      
        tt2_ALL_final <- cbind(tt2_ALL,tmp)
    }
    
    tt2_cumulative_final <- cbind(tt_output_format,
                                  tt2_cumulative)
    tt2_final <- rbind(tt2_ALL_final,tt2_cumulative_final)
    tt2_final <- tt2_final[,match(names(tt2_cumulative_final),names(tt2_ALL_final))]
    
    tt2_final$CrudeRate <- tt2_final$ActualDecrements / tt2_final$Exposure
    
    ### Group output as required
  
    # create summary output format
    if("AGE" %in% Grouping_elements)
    {
      # read in the Bands for age grouping elements
      Bandstemp <- GroupingVariableBands$AGE
      # cut the age variable into bands
      tt2_final$AGE_Band <- cut(tt2_final$AGE,Bandstemp,right = F)
      # unique grouping variables
      Aggregate_Grouping_List <- if(length(Grouping_elements) == 1){list(AGE_Band = tt2_final$AGE_Band)} else
      {tt2_final[,(names(tt2_final) %in% Grouping_elements | names(tt2_final) == "AGE_Band") & names(tt2_final) != "AGE"]}
    } else if("POL_DUR_M" %in% Grouping_elements)
    {
      Bandstemp <- GroupingVariableBands$POL_DUR_M
      tt2_final$POL_DUR_M_Band  <- cut(tt2_final$POL_DUR_M,Bandstemp,right = F)
      Aggregate_Grouping_List <- if(length(Grouping_elements) == 1){list(POL_DUR_M_Band = tt2_final$POL_DUR_M_Band)} else
        {tt2_final[,(names(tt2_final) %in% Grouping_elements | names(tt2_final) == "POL_DUR_M_Band") & names(tt2_final) != "POL_DUR_M"]}
    }

    print("Cumulative table being calculated")

    Aggregate_Variable_List <- tt2_final[,c("ExposureUnweighted","Exposure","ActualDecrements","ExpectedDecrements")]

    Sumtt <- aggregate(x   = Aggregate_Variable_List,
                       by  = Aggregate_Grouping_List,
                       FUN = sum)
    

    Sumtt <- within(Sumtt,
                    {
                      CrudeRate = ActualDecrements/Exposure
                      AvE = ActualDecrements / ExpectedDecrements
                    })
    
    if(Exposure_Definition == "Central") 
    {
      Standard_error <- sqrt(Sumtt$CrudeRate / Sumtt$ExposureUnweighted)
    } else if(Exposure_Definition == "Initial")
    {
      Standard_error <- sqrt(Sumtt$CrudeRate * (1 - Sumtt$CrudeRate) / Sumtt$ExposureUnweighted)
    }
    
    Sumtt$Confidence_Interval0.95 <- paste0("(",
                                            pmax(0,Sumtt$CrudeRate - 1.96 * Standard_error),
                                            ";",
                                            pmin(1,Sumtt$CrudeRate + 1.96 * Standard_error),
                                            ")"
                                            )

    Sumtt <- Sumtt[,-match(c(names(Aggregate_Variable_List)),names(Sumtt))]
    
    Sumtt$Investigation_Period     <- paste(extractstart,"to",extractend)
    tt2_final$Investigation_Period <- paste(extractstart,"to",extractend)
      
    if(j == 1)
    {
      Sumtt_output <- Sumtt
      tt2_final_output <- tt2_final
      Aggregate_AvE_output <- data.frame(PRODUCT = Product,
                                         DECREMENT = Decrement,
                                         WEIGHT = WeightingField)
      Aggregate_AvE_output <- cbind(Aggregate_AvE_output,Aggregate_AvE)
      names(Aggregate_AvE_output)[names(Aggregate_AvE_output)=="Aggregate_AvE"] <- paste0("AvE_",extractstart,"_to_",extractend)
    } else if(j > 1)
    {
      Sumtt_output         <- rbind(Sumtt_output,Sumtt)
      tt2_final_output     <- rbind(tt2_final_output,tt2_final)
      Aggregate_AvE_output <- cbind(Aggregate_AvE_output,Aggregate_AvE)
      names(Aggregate_AvE_output)[names(Aggregate_AvE_output)=="Aggregate_AvE"] <- paste0("AvE_",extractstart,"_to_",extractend)
    }
  }
    

  if(WeightingField == "1") {WeightingFieldName = "Unweighted"} else WeightingFieldName = WeightingField
  write.csv(tt2_final_output,file = paste0("./Output/",Product,"_",Decrement,"_",WeightingFieldName,"_detailed.csv"))
  write.csv(Sumtt_output    ,file = paste0("./Output/",Product,"_",Decrement,"_",WeightingFieldName,"_summary.csv"))

  CumulativeTablesListOutput <- list(Aggregate_AvE_output = Aggregate_AvE_output)
  PerPolicyTables_ListInput  <- PerPolicyTables_ListInput
  return(CumulativeTablesListOutput)
}

################# User defined output ##################

Run_User_defined_output <- function(RUN_NR)
{
  # define timing variable
  StartTime <- Sys.time()
  # Import the mapping tables list
  Supporting_Tables <- ImportSupportingTables()
  User_defined_output <- Supporting_Tables$User_defined_output
  User_defined_output <- User_defined_output[which(User_defined_output$RUN_NR == RUN_NR),]
  if(nrow(User_defined_output) == 0) stop(paste("Run number",RUN_NR,"not defined in User_defined_output"))
  print(paste(User_defined_output$RUN_NAME[1],"selected to run"))
  
  for(i in 1:nrow(User_defined_output))
  {
    RunStartTime <- Sys.time()
    print(paste("Start of run",i,"of",nrow(User_defined_output),"Time:",RunStartTime))
    Output <- CumulativeTables(Product        = User_defined_output$PRODUCT[i],
                               Decrement      = User_defined_output$DECREMENT[i], 
                               WeightingField = User_defined_output$WEIGHT[i],
                               RUN_NR         = RUN_NR
                               )
    if(i == 1)
    {
      AvE_Output <- Output$Aggregate_AvE_output
    } else
    {
      AvE_Output <- rbind(AvE_Output,Output$Aggregate_AvE_output)
    }
    print(paste("Run",i,"of",nrow(User_defined_output),"finished. Time taken (mins):",difftime(Sys.time(),RunStartTime,units = "mins")))
    print(paste("Run",i,"of",nrow(User_defined_output),"finished. Total run time (mins):",difftime(Sys.time(),StartTime,units = "mins")))
  }
  print(paste("All runs finished. Overall runtime (mins):",difftime(Sys.time(),StartTime,units = "mins")))
  write.csv(AvE_Output,file = paste0("./Output/Overall_Output_summary.csv"))
}

################# User runs ##################

ControlInformation(Product = "Funeral",RUN_NR = 200) # run control information

Run_User_defined_output(RUN_NR = 100) # overall experience investigation
Run_User_defined_output(RUN_NR = 500) # Testing
