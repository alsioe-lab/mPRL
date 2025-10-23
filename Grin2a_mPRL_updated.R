# Reading in data from K-Limbic mPRL task
# JA Oct 2025
# 
# NOTE: This code assumes that the number of columns in the data is 60
# Validated on mouse PRL after March 2025.
# Check your version of the PRL task.
n_col <- 60

# Clean slate if required
# rm(list=ls())

library(tidyverse)
library(cowplot)
library(rstan)

# Point the script to your local K-Limbic files
DIR <- file.path('C:/Users',
                 'sy22091',
                 'OneDrive - University of Bristol',
                 'Documents - grp-Alsiö lab-Computational models of cognition and motivation',
                 'GRIN2A BJA001 BJA002')

# Output directory, if using the Stan scripts
JOB_DIR <- file.path('C:/Users',
                      'sy22091',
                      'OneDrive - University of Bristol',
                      'Documents',
                      'Experimental data',
                      'Grin2A',
                      'PRL')

# Check the content of the directory
head(list.files(DIR))

# Check which file (currently just one)
filenames <- list.files(DIR)

# Pick a set of files to analyse
# LIST_OF_FILES <- filenames[grep(pattern = '_Combined.csv',
#                                 x = filenames)]

# ... or manually hard code all files
list_of_sessions <- list(
    Mouse03_FILES = c("30-Apr-2025_Combined.csv",
                      "02-May-2025_Combined.csv",
                      "06-May-2025_Combined.csv",
                      "07-May-2025_Combined.csv"),
    
    Mouse04_FILES = c("30-Apr-2025_Combined.csv",
                      "02-May-2025_Combined.csv",
                      "06-May-2025_Combined.csv",
                      "07-May-2025_Combined.csv"),
    
    Mouse05_FILES = c("17-Apr-2025_Combined.csv",
                      "22-Apr-2025_Combined.csv",
                      "23-Apr-2025_Combined.csv",
                      "24-Apr-2025_Combined.csv"),
    
    Mouse06_FILES = c("17-Apr-2025_Combined.csv",
                      "22-Apr-2025_Combined.csv",
                      "23-Apr-2025_Combined.csv",
                      "24-Apr-2025_Combined.csv"),
    
    Mouse07_FILES = c("01-Apr-2025_Combined.csv",
                      "02-Apr-2025_Combined.csv",
                      "03-Apr-2025_Combined.csv",
                      "07-Apr-2025_Combined.csv"),
    
    Mouse08_FILES = c("23-May-2025_Combined.csv",
                      "28-May-2025_Combined.csv",
                      "29-May-2025_Combined.csv",
                      "30-May-2025_Combined.csv"
                       ),
    
    Mouse09_FILES = c("15-May-2025_Combined.csv",
                      "16-May-2025_Combined.csv",
                      "21-May-2025_Combined.csv",
                      "22-May-2025_Combined.csv"),
    
    Mouse12_FILES = c("16-May-2025_Combined.csv",
                      "21-May-2025_Combined.csv",
                      "22-May-2025_Combined.csv",
                      "23-May-2025_Combined.csv")
    )

names(list_of_sessions)

# Manually hard code the subject IDs
mice <- c(3, 4, 5, 6, 7, 8, 9, 12)

# rm(prl_data)
if (!exists('prl_data')) prl_data <- data.frame()

# LOOP THROUGH SPECIFIED SESSIONS FOR EACH MOUSE

for (mouse in mice) {
    
    # Specify which subject to include ... "Subjects" can theoretically be many,
    # but here we are doing them one by one ...
    subjects <- mouse
    
    # Extract VECTOR from list of sessions
    LIST_OF_FILES <- list_of_sessions[[paste0('Mouse',
                                              str_pad(mouse,
                                              width = 2,
                                              pad = '0',
                                              side = 'left'),
                                              '_FILES')]]
                                      
    
    # Trouble shooting
    # FILE = "08-Apr-2025_Combined.csv"
    
    for (FILE in LIST_OF_FILES) {
        
        # Create an empty data frame on the first iteration of the FILE loop
        if (FILE == LIST_OF_FILES[1]) df <- data.frame()
        
        # Read in the data from FILE as lines
        as_lines <- read_lines(file.path(DIR, FILE))
        
        # Identify relevant rows (indices) for various bits of information 
        pos_Subject_Id <- grep(pattern = TRUE,
                               x = str_starts(string = as_lines,
                                              pattern = 'Subject Id')
                               )
    
        # List the subjects
        subjects_in_session <- vector(mode = "logical",
                                     length = length(pos_Subject_Id))
        
        # TODO: improve this solution to get the Subject IDs out
        for (i in 1:length(pos_Subject_Id))  {
          subjects_in_session[i] <- as.numeric(
            str_split(string = as_lines[pos_Subject_Id[i]],
                      pattern = ',')[[1]][2]
                                     )
        }
        
        pos_starts <- grep(pattern = TRUE,
                           x = str_starts(string = as_lines,
                                          pattern = 'Ref')
                           ) + 1
        
        pos_ends <- grep(pattern = TRUE,
                         x = str_starts(string = as_lines,
                                        pattern = 'ACTIVITYLOG')
                         ) - 2
        
        # TODO: Update code to read multiple sessions/subjects
        for (subject in subjects) {
          
          # Create a data frame to keep track of current subject
          # TODO: confirm where the df is reset
          # df <- data.frame()
            
          # IF STATEMENT TO SKIP ANY SUBJECT NOT PRESENT IN CURRENT SESSION
          if (subject %in% subjects_in_session) {
    
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
            
            # Extract the protocol
            day_table = str_split(string = as_lines[pos_Subject_Id + 2][index],
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
            df <- rbind(df, trials)
          
          } # end of if statement to extract data ONLY if subject %in% subjects
            
      } # end of subject loop
        
    } # end of FILE loop
    
    # Check that you have got data from all mice and days
    cat('Mouse ', unique(df$subj_id), '-- unique dates: ', unique(df$date))
    
    df %>%
        group_by(subj_id, date) %>%
        summarise(n = n())
    
    prl_data <- bind_rows(prl_data, df)
    
    cat(unique(prl_data$subj_id))

} # end of for loop

prl_data %>%
    group_by(subj_id, date) %>%
    summarise(n())

# Filter sessions and prepare for further analysis

# Select relevant session(s)
prl_n_8 <- prl_data %>%
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



# # Save the data with a useful name
# write_csv(prl_n_8,
#           file = 'prl_n_8_sessions1-4.csv')


################################################################################
# DATA PREP BELOW
################################################################################

# Should be able to change this section depending on which subject
# or otherwise you are looking at
data <- prl_n_8

unique(data$subj_id)

# If there was no reward, there is no reward latency
data$reward_latency[data$reward_latency == 0] <- NA

# Set up subject and session index with tidyverse
data <- data %>%
    mutate(subject_index = match(subj_id, unique(subj_id))) %>%
    group_by(subj_id) %>%
    mutate(session_index = match(date, unique(date))) %>%
    ungroup() %>%
    mutate(session_index = as.factor(session_index))

# Add fixed factors

# Set up genotype as factor
data$genotype <- data$subj_id %in% c(1, 4, 5, 8, 10, 11)
data$genotype <- factor(data$genotype,
                        levels = c(FALSE, TRUE), labels = c('wt', 'het'))

# Set up sex as factor
data$sex <- data$subj_id %in% c(1, 2, 3, 4, 9, 10) %>%
    factor(levels = c(FALSE, TRUE), labels = c('male', 'female'))

################################################################################
# Sorting out accuracy and towards criterion
  
data$chose_correct <- as.integer(data$chose_left == data$left_is_active)
data$towards_criterion <- NA

# This will loop over each row in the entire data set
for (row in 1:dim(data)[1]) {
    
    # First, check that the row does NOT contains NA's
    if (!is.na(data$chose_correct[row])) {
        
        # Second, is this the first trial of the session?
        if (data$trial[row] == 1) {
            
            # If it is the first trial, start from 0 or 1? (Optimal or not optimal?)
            data$towards_criterion[row] <- data$chose_correct[row]
            
        } else if (data$trial[row] > 1) {
            
            # It's not the 1st trial of the session, but was previous trial criterion?
            if (data$towards_criterion[row - 1] == 6) { 
                
                # reversal has happened! restart from 1 or 0 (Optimal choice?)
                data$towards_criterion[row] <- data$chose_correct[row]
                
            } else if (data$towards_criterion[row - 1] != 6) {
                
                # Reversal has NOT happened, did the mouse get it right or wrong?
                if (data$chose_correct[row] == 1) {
                    
                    # The mouse got it right! Add to the counter (6 is criterion)
                    data$towards_criterion[row] <-
                        data$towards_criterion[row - 1] + 1
                    
                } else if (data$chose_correct[row] == 0){
                    
                    # The mouse got it wrong! Reset the counter to 0!
                    data$towards_criterion[row] <- 0
                    
                } # end of opt == 1
                
            } # end of crit == 6
            
        } # end of trial == 1
    } else {
        data$towards_criterion[row] <- NA
    } # end of !is.na
} # end of for loop

# We can check whether we got that right by seeing that no value is higher than
# the criterion, 6 (that would indicate an error in our code or in the task).
max(data$towards_criterion, na.rm = TRUE)

################################################################################
# Win-stay lose-shift
data$stay <- 0
data$win_stay <- 0
data$lose_shift <- 0

# Win stay lose shift
for (row in 2:dim(data)[1]) {
    if (data$trial[row] != 1) {
        data$stay[row] <- as.integer(
                            data$chose_left[row] == data$chose_left[row - 1])
        data$win_stay[row] <- data$reward[row - 1] * data$stay[row]
        data$lose_shift[row] <- (1 - data$reward[row - 1]) * (1 - data$stay[row])
    }
}

data_by_subj_session <-
    data %>%
    group_by(subj_id, session_index, genotype, sex) %>%
    summarise(mean_trials = n(),
              # Reversals per 100 trials
              mean_reversals = sum(towards_criterion == 6)/n()*100,
              mean_accuracy = mean(chose_correct),
              mean_win_stay = sum(win_stay)/sum(reward==1),
              mean_lose_shift = sum(lose_shift)/sum(reward==0),
              mean_stay = sum(stay)/n(),
              mean_reward_lat = median(log(reward_latency), na.rm = TRUE)*10,
              mean_response_lat = median(log(response_latency), na.rm = TRUE)*10,
              mean_initiate_lat = median(log(initiate_latency), na.rm = TRUE)*10)

# WITHIN-SUBJECT VARIABILITY OVER SESSIONS
data_by_subj_session_long <-
    data_by_subj_session %>%
           pivot_longer(cols = c(-subj_id, -session_index, -genotype, -sex),
                        names_to = 'variable',
                        values_to = 'value')

# PLOT ALL SUBJECTS ACROSS SESSIONS 1-4
data_by_subj_session_long %>%
    ggplot(aes(x = session_index,
               y = value,
               colour = as.factor(subj_id),
               group = subj_id)) +
    geom_line() +
    facet_wrap(~ variable,
               scales = 'free') +
    theme_bw()

data_by_subj_session_wide <-
    data_by_subj_session_long %>%
    pivot_wider(names_from = session_index,
                names_prefix = 'session_')

data_by_subj_session_wide %>%
    ggplot(aes(x = session_3, y = session_4)) +
    geom_point() +
    facet_wrap(~variable, scale = 'free')


# Continue with genotype analysis
data_by_subj <-
    data %>%
    group_by(subj_id, genotype, sex) %>%
    summarise(mean_trials = n(),
              # Reversals per 100 trials
              mean_reversals = sum(towards_criterion == 6)/n()*100,
              mean_accuracy = mean(chose_correct),
              mean_win_stay = sum(win_stay)/sum(reward==1),
              mean_lose_shift = sum(lose_shift)/sum(reward==0),
              mean_stay = sum(stay)/n(),
              mean_reward_lat = median(reward_latency, na.rm = TRUE)*10,
              mean_response_lat = median(response_latency, na.rm = TRUE)*10,
              mean_initiate_lat = median(initiate_latency, na.rm = TRUE)*10)

# Prepare data for ggplot
to_plot_by_subj <-
    data_by_subj %>%
                 pivot_longer(cols = c(mean_trials, mean_reversals,
                                       mean_win_stay, mean_lose_shift,
                                       mean_stay, mean_reward_lat,
                                       mean_response_lat, mean_initiate_lat
                                       ),
                              names_to = 'variable',
                              values_to = 'value') %>%
    mutate(variable = factor(variable,
                             levels = c('mean_trials', 'mean_reversals',
                                        'mean_win_stay', 'mean_lose_shift',
                                        'mean_stay', 'mean_reward_lat',
                                        'mean_response_lat', 'mean_initiate_lat'),
                             labels = c('Trials completed', 'Criteria//100 trials',
                                        'Win-stay', 'Lose-shift',
                                        'Stay', 'Reward latency (ms)',
                                        'Response latency (ms)', 'Initiation latency (ms)')
                             )
           )
    
# Prepare more data for ggplot
ggplot(data = to_plot_by_subj,
       aes(x = genotype,
           y = value,
           colour = genotype,
           shape = sex)) +
    facet_wrap(~ variable,
               scales = 'free_y',
               nrow = 2) +
    geom_point(position = position_jitter(width = 0.2)) +
    theme_bw() +
    labs(y = element_blank(),
         x = element_blank())


# Source modified Mike Lawrence script
source('r scripts/get_contrast_matrix.R')

STAN_DIR <- file.path('C:', 'users', 'sy22091', 'OneDrive - University of Bristol',
                      'Documents', 'R', 'multivariate_normal_hierarchical',
                      'stan scripts', 'prl')

# Between-subjects' structure
B <- get_contrast_matrix(data = data %>%
                             group_by(subj_id, genotype, sex) %>%
                             summarise(),
                         # NOTE ADDITIVE MODEL
                         formula = ~ genotype + sex)

head(B, n = 10)

# Within-subject structure
# W <- get_contrast_matrix(data = data,
#                          formula = ~ session_index)
W <- get_contrast_matrix(data = data,
                         formula = ~ 1)

head(W)

# Prepare for fit
data_for_stan <- list(n_y = dim(data)[1],
                      n_subj = length(unique(data$subject_index)),
                      n_w = dim(W)[2],
                      n_b = dim(B)[2],
                      subj_id = data$subject_index,
                      trial = data$trial,
                      W = W,
                      B = B,
                      outcome = data$reward,
                      chose_left = data$chose_left)

# Fit the data and compute the loo objects

list.files(STAN_DIR)

models_to_fit <- c('aabk', 'aabkp', 'abk', 'abkp')

for (model in models_to_fit) {
    
    # temporary fit object
    fit <- rstan::stan(
        seed = 1876,
        file = file.path(STAN_DIR,
                         paste0('mvn_prl_', model, '.stan')),
        model_name = model,
        data = data_for_stan,
        iter = 4000,
        warmup = 2000,
        chains = 4,
        cores = 4,
        control = list(adapt_delta = 0.95, 
                       max_treedepth = 10))
    
    assign(paste('fit_', model),
           fit)
    
    # Set up name for the loo object
    loo_name <- paste0('loo_', model)
    
    # Extract the log_lik from the current fit
    log_lik <- loo::extract_log_lik(fit,
                                    merge_chains = FALSE)
    
    # Relative effective sample size for each log_lik value
    r_eff_vals <- loo::relative_eff(exp(log_lik))
    
    # Create the loo object
    assign(paste0('loo_', model),
           value = loo::loo(x = log_lik,
                            r_eff = r_eff_vals, 
                            save_psis = TRUE)) 
    
    # Save the fit
    saveRDS(fit,
            file = file.path(JOB_DIR,
                             paste0('fit_', model, '.RDS')
            )
    )
    
    # Save the loo object
    saveRDS(object = get(paste0('loo_', model)),
            file = file.path(JOB_DIR,
                             paste0('loo_', model, '.RDS')
            )
    )
    
    # Clean up memory (not necessary on the cluster, but still!)
    rm(fit)
    gc()
    
}

# Check traceplot for the "hairy caterpillar" look
rstan::traceplot(fit_abk)

# Hairy caterpillar?
rstan::traceplot(fit_abkp)

# Hairy caterpillar?
rstan::traceplot(fit_aabk)

# Hairy caterpillar?
rstan::traceplot(fit_aabkp)


# Compare loo objects
# for (m in list_of_models) {
#     
#     if (m == list_of_models[1]) loo_list <- list()
#     
#     log_lik <- loo::extract_log_lik(stanfit = get(paste0('fit_', m)),
#                                     merge_chains = FALSE)
# 
#     # Relative effective sample size for each log_lik value
#     r_eff_vals <- loo::relative_eff(exp(log_lik))
#     
#     loo_list[[m]] <- loo::loo(x = log_lik,
#                               r_eff = r_eff_vals,
#                               save_psis = TRUE)
#     
# }

# Set up empty list
loo_list <- list()

for (m in models_to_fit) {
    
    # Set up loo name
    file_name <- file.path(JOB_DIR,
                           paste0('loo_', m, '.RDS'))
    
    # Read in the LOO object
    loo_list[[paste0('loo_', m)]] <- readRDS(file_name)
    
}

# Compare the loos
loo_result <- loo::loo_compare(loo_list)

# Convert to data frame and preserve model names
loo_df <- as.data.frame(loo_result) %>%
    tibble::rownames_to_column(var = "model") %>%  # adds rownames as column
    relocate(model, .before = everything())        # move model to first column

loo_df

loo_result

fit <- readRDS(file = file.path(JOB_DIR,
                                'fit_aabkp.RDS'))

# to_remove <- ls()[str_starts(string = ls(), pattern = 'fit_ ')]
# 
# for (fit_object in to_remove) {
#     do.call(rm,
#             list(fit_object))
# }

# Inspect the summary including r_hat
round(rstan::summary(fit, par = 'theta_mean')$summary, 2)

round(rstan::summary(fit, par = 'theta_by_subj')$summary, 2)

# saveRDS(object = fit,
#         file = 'fit_grin2a_gene_by_sex_additive_prl_8_aabkp.RDS')

# READ FIT
fit <- readRDS(file = 'fit_grin2a_gene_by_sex_additive_prl_8_aabkp.RDS')

par <- rstan::extract(fit, par = c('theta_mean', 'theta_by_subj'))

dim(par$theta_mean)

# Inspect the summary measures of the posteriors
quantiles <-
  apply(X = par$theta_mean,
    MARGIN = c(3,2),
    FUN = quantile,
    probs = c(0.025, 0.125, 0.5, 0.875, 0.975))

quantiles

dim(par$theta_mean)

# HDIs for defined groups
# 
# 1) Summary for the groups
groups <- data.frame(
  group = c('Male Wt', 'Male Het', 'Female Wt', 'Female Het'),
  genotype = factor(c('wt', 'het', 'wt', 'het'),
                    levels = c('wt', 'het')),
  sex = factor(c('male', 'male', 'female', 'female'),
               levels = c('male', 'female'))
)

# Set up contrast matrix for groups
groups_B <- get_contrast_matrix(groups,
                               formula = ~ genotype + sex)

groups_B

head(B)

# Names of parameters)
par_names <- c('alpha_win',
               'alpha_loss',
               'beta',
               'kappa',
               'psi')

n_par <- dim(par$theta_mean)[2]

length(par_names) == n_par

# Set up posterior by group
theta_by_group <- array(data = NA, dim = c(8000, n_par, 4))

# Populate posterior by group
for (p in 1:n_par) {
    theta_by_group[, p, ] <- par$theta_mean[, p, ] %*% t(groups_B)
}

# Slice the data and create plots
for (p in 1:length(par_names)) {
  
  # Extract quantiles for box plot
  quantiles_groups <- 
    apply(theta_by_group[, p, ],
          MARGIN = c(2),
          FUN = quantile,
          probs = c(0.01, 0.025, 0.125, 0.5, 0.875, 0.975, 0.99))
  
  quantiles_effects <- 
         apply(par$theta_mean[, p, ],
               MARGIN = c(2),
               FUN = quantile,
               probs = c(0.01, 0.025, 0.125, 0.5, 0.875, 0.975, 0.99))

  # Arrange the data in data frame
  box_groups <- data.frame(
    group = as.factor(c('Male Wt',
                         'Male Het',
                         'Female Wt',
                         'Female Het')),
    ylim_lower = quantiles_groups['1%', ],
    ymin = quantiles_groups['2.5%', ],
    lower = quantiles_groups['12.5%', ],
    middle = quantiles_groups['50%', ],
    upper = quantiles_groups['87.5%', ],
    ymax = quantiles_groups['97.5%', ],
    ylim_upper = quantiles_groups['99%']
  )
    
  # Arrange the data in data frame
  box_effects <- data.frame(
    effect = as.factor(c('intercept',
                         'effect_het',
                         'effect_sex')),
    ylim_lower = quantiles_effects['1%', ],
    ymin = quantiles_effects['2.5%', ],
    lower = quantiles_effects['12.5%', ],
    middle = quantiles_effects['50%', ],
    upper = quantiles_effects['87.5%', ],
    ymax = quantiles_effects['97.5%', ],
    ylim_upper = quantiles_effects['99%', ]
  )
  
  # Sort out fill for effects plot
  for (row in 1:dim(box_effects)[1]) {
    
    if (row == 1) {
      box_effects$HDI75 <- NA
      box_effects$HDI95 <- NA
      box_effects$HDI_fill <- -1
    }
    
    # 75% HDI
    box_effects$HDI75[row] <-
      (box_effects$lower[row] > 0 && box_effects$upper[row] > 0) ||
      (box_effects$lower[row] < 0 && box_effects$upper[row] < 0)
    
    # 95% HDI
    box_effects$HDI95[row] <-
      (box_effects$ymin[row] > 0 && box_effects$ymax[row] > 0) ||
      (box_effects$ymin[row] < 0 && box_effects$ymax[row] < 0)
    
    if (row > 1) {
      box_effects$HDI_fill[row] <- box_effects$HDI75[row] + box_effects$HDI95[row] 
    }

    box_effects$HDI_fill <- factor(box_effects$HDI_fill,
                                    levels = c(-1, 0, 1, 2))
    
    box_effects$effect <- factor(box_effects$effect,
                                  levels = c('intercept',
                                             'effect_het',
                                             'effect_sex')
                                 )
      
  }
    
  # Extract full posteriors (groups)
  assign(x = paste0('group_', par_names[p]),
         value = as.data.frame(theta_by_group[, p, ]) %>%
           rename(c('Male Wt' = V1,
                    'Male Het' = V2,
                    'Female Wt' = V3,
                    'Female Het' = V4)) %>%
           tibble::rownames_to_column(var = 'iter') %>%
           pivot_longer(cols = -iter,
                        names_to = 'group',
                        values_to = 'estimate') %>%
           mutate(group = factor(group,
                                  levels = c('Male Wt',
                                             'Male Het',
                                             'Female Wt',
                                             'Female Het')
                                 )
                  )
         )
  
  # Extract full posteriors (effects)
  assign(x = paste0('posterior_', par_names[p]),
         value = as.data.frame(par$theta_mean[, p, ]) %>%
           rename(c('intercept' = V1,
                    'effect_het' = V2,
                    'effect_sex' = V3)) %>%
           tibble::rownames_to_column(var = 'iter') %>%
           pivot_longer(cols = -iter,
                        names_to = 'effect',
                        values_to = 'estimate') %>%
           mutate(effect = factor(effect,
                                  levels = c('intercept',
                                             'effect_het',
                                             'effect_sex')
                                  )
           )
  )

  # Plot the grouped estimates!
  assign(x = paste0('plot_groups_', par_names[p]),
         
         # Initiate ggplot
         ggplot() +
           
           # Add violinplot
           geom_violin(data = get(paste0('group_', par_names[p])),
                       aes(x = group,
                           y = estimate),
                       fill = 'lightgrey',
                       colour = NA,
                       alpha = 0.5) +
           
           # Add boxplot
           geom_boxplot(data = box_groups,
                        aes(x = group,
                            ymin = ymin,
                            lower = lower,
                            middle = middle,
                            upper = upper,
                            ymax = ymax,
                            fill = group,
                            colour = group),
                        stat = 'identity',
                        width = 0.1,
                        alpha = 0.5,
                        show.legend = FALSE) +
           
           # Flair
           coord_cartesian(ylim = c(min(box_groups$ylim_lower),
                                    max(box_groups$ylim_upper))
                           ) +
           theme_bw() +
           labs(x = par_names[p]) +
           scale_fill_manual(values = c('Male Wt' = '#091540',
                                        'Male Het' = '#7692FF',
                                        'Female Wt' = '#ABD2FA',
                                        'Female Het' = '#3D518C')) +
           scale_colour_manual(values = c('Male Wt' = '#091540',
                                          'Male Het' = '#7692FF',
                                          'Female Wt' = '#ABD2FA',
                                          'Female Het' = '#3D518C')) +
           labs(x = element_blank(),
                y = par_names[p]) +
             
           # Added rotation on axis tick mark legends           
           theme(axis.text.x = element_text(angle = 15, hjust = 1))
         
         ) # end assign function
  
  # Plot the effects!
  assign(x = paste0('plot_effects_', par_names[p]),
         
         # Initiate ggplot
         ggplot() +
           
           # Add violinplot
           geom_violin(data = get(paste0('posterior_', par_names[p])) %>%
                         filter(effect != 'intercept'),
                       aes(x = effect,
                           y = estimate),
                       fill = 'lightgrey',
                       colour = NA,
                       alpha = 0.5) +

           # Add boxplot
           geom_boxplot(data = box_effects %>% filter(effect != 'intercept'),
                        aes(x = effect,
                            ymin = ymin,
                            lower = lower,
                            middle = middle,
                            upper = upper,
                            ymax = ymax,
                            fill = HDI_fill,
                            colour = HDI_fill),
                        stat = 'identity',
                        width = 0.1,
                        alpha = 0.5,
                        show.legend = FALSE) +
           
           # Flair
           coord_cartesian(ylim = c(min(box_effects$ylim_lower),
                                    max(box_effects$ylim_upper))
                           ) +
           geom_hline(yintercept = 0,
                      linetype = 'dotted') +
           theme_bw() +
           # theme(axis.text.y=element_blank()) +
           labs(x = par_names[p]) +
           scale_fill_manual(values = c('-1' = 'turquoise',
                                        '0' = 'darkgrey',
                                        '1' = 'orange',
                                        '2' = 'darkred')) +
           scale_colour_manual(values = c('-1' = 'turquoise',
                                          '0' = 'darkgrey',
                                          '1' = 'orange',
                                          '2' = 'darkred')) +
           labs(x = element_blank(),
                y = par_names[p]) +
         
           # Added rotation on axis tick mark legends       
           theme(axis.text.x = element_text(angle = 15, hjust = 1))
         
         ) # end assign function
  
} # end for loop

# Set up legend
# Plot the effects!
plot_legend <- 
       
       # Initiate ggplot
       ggplot() +
           
           # Add boxplot
           geom_boxplot(data = data.frame(
                               effect = c('hic', 'haec', 'hoc'),
                               ymin = c(-2, -2, -2),
                               lower = c(1, 0, -1),
                               middle = c(2, 2, 2),
                               upper = c(3, 3, 3),
                               ymax = c(4, 4, 4),
                               HDI_fill = as.factor(c(2, 1, 0))),
                        aes(x = effect,
                            ymin = ymin,
                            lower = lower,
                            middle = middle,
                            upper = upper,
                            ymax = ymax,
                            fill = HDI_fill,
                            colour = HDI_fill),
                        stat = 'identity',
                        width = 0.1,
                        alpha = 0.5) +

           scale_fill_manual(name = 'HDI',
                             values = c('-1' = 'turquoise',
                                        '0' = 'darkgrey',
                                        '1' = 'orange',
                                        '2' = 'darkred'),
                             labels = c('-1' = 'NA',
                                        '0'  = expression(0 %in% '75%'),
                                        '1'  = expression(0 %notin% '75%'),
                                        '2'  = expression(0 %notin% '95%')
                                        )
                             ) +
           scale_colour_manual(name = 'HDI',
                               values = c('-1' = 'turquoise',
                                          '0' = 'darkgrey',
                                          '1' = 'orange',
                                          '2' = 'darkred'),
                               labels = c('-1' = 'NA',
                                          '0'  = expression(0 %in% '75%'),
                                          '1'  = expression(0 %notin% '75%'),
                                          '2'  = expression(0 %notin% '95%')
                                          )
                               ) +
           theme_bw()


# Arrange with cowplot
cowplot::plot_grid(plotlist = list(plot_groups_alpha_win +
                                       labs(y = expression(atop(logit (alpha[win]),
                                                                '(by group)')
                                                           )
                                            ),
                                   plot_effects_alpha_win  +
                                       labs(y = expression(atop(logit (alpha[win]),
                                                                '(effect)')
                                                           )
                                       ),
                                   ggplot() + theme_void(),
                                   plot_groups_alpha_loss +
                                       labs(y = expression(atop(logit (alpha[loss]),
                                                                '(by group)')
                                                           )
                                       ),
                                   plot_effects_alpha_loss +
                                       labs(y = expression(atop(logit (alpha[loss]),
                                                                '(effect)')
                                                           )
                                       ),
                                   ggplot() + theme_void(),
                                   plot_groups_beta +
                                       labs(y = expression(atop(log (beta),
                                                                '(by group)')
                                                           )
                                       ),
                                   plot_effects_beta +
                                       labs(y = expression(atop(log (beta),
                                                                '(effect)')
                                                           )
                                       ),
                                   get_legend(plot_legend),
                                   plot_groups_kappa +
                                       labs(y = expression(atop(kappa,
                                                                '(by group)')
                                                           )
                                       ),
                                   plot_effects_kappa +
                                       labs(y = expression(atop(kappa,
                                                                '(effect)')
                                                           )
                                       ),
                                   ggplot() + theme_void(),
                                   plot_groups_psi +
                                       labs(y = expression(atop(psi,
                                                                '(by group)')
                                                           )
                                       ),
                                   plot_effects_psi +
                                       labs(y = expression(atop(psi,
                                                                '(effect)')
                                                           )
                                       ),
                                   ggplot() + theme_void()),
                   ncol = 3,
                   rel_widths = c(3, 2, 1))

