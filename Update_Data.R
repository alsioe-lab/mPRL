####Extract data from k-limbic file and update the saved .csv file. Written for my (FD) experiments
####and will need to be adapted.

# Reading in data from mPRL task
# This code assumes that the number of columns in the data is 60
rm(list=ls())
library(tidyverse)

n_col <- 60

# Enter the link to the data folder here
# DIR <- "C:/Users/sy22091/OneDrive - University of Bristol/Documents - grp-Alsiö lab-Computational models of cognition and motivation/GRIN2A BJA001 BJA002/"

DIR <- file.path('C:/Users',
                 'gi23004',
                 'OneDrive - University of Bristol',
                 'Documents',
                 'mPRL',
                 'Raw Data')

# Check which file (currently just one)
filenames <- list.files(DIR)

# Pick a set of files to analyse
LIST_OF_FILES <- filenames[grep(pattern = '_Combined.csv',
                                x = filenames)]

# Specify which subjects to include ...
subjects <- c("fd_01_1",
              "fd_01_2",
              "fd_01_3",
              "fd_01_4",
              "fd_01_5",
              "fd_01_6",
              "fd_01_7",
              "fd_01_8",
              "fd_01_9",
              "fd_01_10",
              "fd_01_11",
              "fd_01_12")

# Troubleshooting

for (FILE in LIST_OF_FILES) {
  
  # Create an empty data frame on the first iteration of the FILE loop
  if (FILE == LIST_OF_FILES[1]) data <- data.frame()
  
  # Read in the data from FILE as lines
  as_lines <- read_lines(file.path(DIR, FILE))
  
  # Identify relevant rows (indices) for various bits of information 
  #fd: this finds which lines contain the phrase 'Subject Id'
  pos_Subject_Id <- grep(pattern = TRUE,
                         x = str_starts(string = as_lines,
                                        pattern = 'Subject Id')
  )
  
  # List the subjects
  #first create a blank vector the same length as the number of times
  #'subject id' appears
  subjects_in_session <- vector(mode = "logical",
                                length = length(pos_Subject_Id))
  
  # Writing this as a for loop to get the Subject IDs out
  #fd: changed from johans 'as.numeric' because my subjects have fd in their id
  for (i in 1:length(pos_Subject_Id))  {
    subjects_in_session[i] <- as.character(
      str_split(string = as_lines[pos_Subject_Id[i]],
                pattern = ',')[[1]][2]
    )
  }
  
  
  ###fd: Extracting information about session type and duration
  pos_session_prot <- grep(pattern = TRUE,
                           x = str_starts(string = as_lines,
                                          pattern = 'Session Title')
  )
  protocols_in_day <- vector(mode = "logical",
                             length = length(pos_session_prot))
  for (i in 1:length(pos_session_prot))  {
    protocols_in_day[i] <- as.character(
      str_split(string = as_lines[pos_session_prot[i]],
                pattern = ',')[[1]][2]
    )
  }
  
  #...same for incomplete sessions
  pos_session_dur <- grep(pattern = TRUE,
                          x = str_starts(string = as_lines,
                                         pattern = 'Duration')
  )
  session_durations <- vector(mode = "logical",
                              length = length(pos_session_dur))
  for (i in 1:length(pos_session_dur))  {
    session_durations[i] <- as.numeric(
      str_split(string = as_lines[pos_session_dur[i]],
                pattern = ',')[[1]][2]
    )
  }
  
  
  #only continue with this file if at least one subject of interest is present
  if (!sum(subjects %in% subjects_in_session)==0){
    
    ################
    #fd: issue here, some alternative types of protocol dont have activity logs
    #at end. need to work out a way to be consistent ie include all types of
    #protocol start & finish pavlovian need a better way to work out end
    
    #find the points where one subject begins
    pos_starts <- grep(pattern = TRUE,
                       x = str_starts(string = as_lines,
                                      pattern = 'Ref')
    ) + 1
    
    #find the points where it ends
    pos_ends <- grep(pattern = TRUE,
                     x = str_starts(string = as_lines,
                                    pattern = 'ACTIVITYLOG')
    ) - 2
    
    ##fd: ask johan the most efficient way to do this, want to remove
    #values from pos_starts based on protocols_in_day
    #troubleshootig check;
    if (!length(protocols_in_day)==length(session_durations)){
      warning("PROTOCOLS IN DAY DOESNT MATCH SESSION DURATIONS")
    }
    
    session_notes<-
      data.frame(
        protocols=protocols_in_day,
        duration=session_durations,
        pos_starts=pos_starts,
        subjects_in_session=subjects_in_session,
        pos_Subject_Id=pos_Subject_Id,
        include=TRUE
      )
    
    for (PROT in 1:dim(session_notes)[1]){
      if ((!session_notes$protocols[PROT]=="PRL")|(session_notes$duration[PROT]<30000)){
        session_notes$include[PROT]<-FALSE
      }
    }
    
    session_notes<-session_notes|>
      filter(include==TRUE)
    #returning it to a list, in future can pull data from df but cba now
    pos_starts<-session_notes$pos_starts
    subjects_in_session<-session_notes$subjects_in_session
    pos_Subject_Id<-session_notes$pos_Subject_Id
    
    #subject<-1
    #subject<-"fd_01_1"
    
    # This might need to be updated to cope with multiple runs/subject
    #fd: quick fix, check if aborted early and skip if so
    for (subject in subjects) {
      
      # # Error handling
      #   #think this is causing entire loop to stop
      # stopifnot(subject %in% subjects_in_session)
      
      #does the subject appear in this experiment?
      if (subject %in% subjects_in_session){
        
        index <- match(subject, subjects_in_session)
        
        # Extract the date
        date = str_split(string = as_lines[pos_Subject_Id - 16][index],
                         pattern = ',')[[1]][2]
        
        # Extract the time
        time = str_split(string = as_lines[pos_Subject_Id - 15][index],
                         pattern = ',')[[1]][2]
        
        # Extract the box
        box = str_split(string = as_lines[pos_Subject_Id - 14][index],
                        pattern = ',')[[1]][2]
        
        # Extract the protocol fd: edited to make +3, not +2
        day_table = str_split(string = as_lines[pos_Subject_Id + 3][index],
                              pattern = ',')[[1]][2]
        
        # ~~~~~~~~~~~~~~~~~~~Sorting out the trials~~~~~~~~~~~~~~~~~~~~~
        
        # Extract numeric values from the session's trials
        # This does not appear to work on the older sessions - check out why
        trials_as_list <-
          sapply(X = str_split(as_lines[pos_starts[index]:pos_ends[index]],
                               pattern = ','),
                 FUN = as.numeric)
        
        # Extract the numeric values for the list - note we ignore the start trial
        for (j in 2:length(trials_as_list)) {
          
          # Set up empty data frame
          if (j == 2) trials <- data.frame()
          
          # Add the line from the list to the data frame, if the format is correct
          if (length(trials_as_list[[j]]) == n_col) {
            trials <- rbind(trials, trials_as_list[[j]])
          }
          
          # Update the names of the columns
          if (j == length(trials_as_list)) {
            names(trials) <- paste0("c", 1:dim(trials)[2])
          }
          
        }
        
        # Update the trials data frame
        trials$subj_id <- subject
        trials$date <- date
        trials$time <- time
        trials$box <- box
        trials$day_table <- day_table
        
        # Reorder columns in data frame
        trials <- trials %>%
          dplyr::select(subj_id, date, time, box, day_table, everything())
        
        # Bind to main data
        data <- rbind(data, trials)
      }# end of subject appears in subject_in_session
    } # end of subject loop
  }#end of 'subject appears' FILE if
} # end of FILE loop

prl_n_8 <- data %>%
  # Use this filter if you want to narrow down your data set here
  # filter(subj_id %in% c(3, 4),
  #        date %in% c('23-Apr-2025', '24-Apr-2025')) %>%
  # Create and rename variables from the sessions
  mutate(reward = c47,
         chose_left = c38 + c43,
         trial = c1 + 1,
         reward_latency = (c46 - c45) * c47,
         response_latency = (c36 - c35) + (c41-c40),
         initiate_latency = c16 - c15,
         left_is_active = as.numeric(c35 > c40)
  ) %>%
  # Select the columns to carry forward - we drop all the columns
  # with raw data from K-Limbic at this point
  dplyr::select(subj_id, date, time, box, day_table,
                trial, chose_left, reward, left_is_active,
                reward_latency, response_latency, initiate_latency)

# Save the data with a useful name
write_csv(prl_n_8,
          file = paste0('Processed Data/','PRL_PRELIM.csv'))


