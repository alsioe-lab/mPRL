###Process for reversals, accuracy, win-stay & lose-shift
Experiment<-"MK-801"

if (!exists("Experiment")){
  Experiment<-"PRL"
  
}

data <- read_csv(paste0("Processed Data/",Experiment,"_PRELIM.csv"))

prl<-(data|>
         filter(
           day_table=="PRL - Constant Criteria")
       
)

prl$date<-dmy(prl$date)
prl<-prl[complete.cases(prl), ]
prl$opt<-0
prl$towards_criterion<-0
prl$win_stay<-0
prl$lose_shift<-0
prl$true_win_stay<-0
prl$perseverence_counter<-0
prl$perseverence<-0

for (trial in 1:dim(prl)[1]){
  #first check if optimum side was chosen
  if (prl$chose_left[trial]==prl$left_is_active[trial]){
    prl$opt[trial]<-1
  }
  
  #CRITERION CALCULATION:
  #if its the first trial then towards criterion is set to 0 if
  #false start or 1 if optimum choice was made
  
  if (prl$trial[trial]==1){
    prl$towards_criterion[trial]<-prl$opt[trial]
    
    #if it is not the first trial, check if previous trial was criteria
  } else if (prl$towards_criterion[trial-1]==6){
    #if it was restart as before 
    prl$towards_criterion[trial]<-prl$opt[trial]
    #if its not then we check whether the trial was correct
  } else if (prl$opt[trial]==1){
    #if so add to the counter
    prl$towards_criterion[trial]<-prl$towards_criterion[trial-1]+1
    #if not the counter resets to 0
  } else if (prl$opt[trial]==0){
    prl$towards_criterion[trial]<-0
  }
  
  #WINSTAY vs LOSESHIFT
  
  if (!prl$trial[trial]==1){
    #first check if they won the previous trial
    if (prl$reward[trial-1]==1){
      #yes they got a reward, a WIN. Did they stick this time?
      if (prl$chose_left[trial-1]==prl$chose_left[trial]){
        #yes they stuck!
        prl$win_stay[trial]<-1
      } #no need to do anything if they switched, automatic is false
    } else if (prl$reward[trial-1]==0) {
      #last time they lost, no reward ):, did they shift their strategy this time?
      if (!prl$chose_left[trial-1]==prl$chose_left[trial]){
        #yes, they chose differently this time!
        prl$lose_shift[trial]<-1
      } #end they shifted
    } #end no reward
    
    #TRUE WIN_STAY
    #did they chose the correct side last time?
    if (prl$opt[trial-1]==1){
      #yes! DId they stick (regardless of feedback)
      if (prl$chose_left[trial-1]==prl$chose_left[trial]){
        #yes!
        prl$true_win_stay[trial]<-1
      } #end of stuck
    } #end of chose optimum side
  } #end not the first trial, winstay loseshift
  
  #PERSEVERENCE
  #not the first trial
  if (!prl$trial[trial]==1){
    if (prl$towards_criterion[trial-1]==6){
      #reversal has just happened. do they chose a different side?
      if (prl$chose_left[trial-1]==prl$chose_left[trial]){
        #NO, add one to counter
        prl$perseverence_counter[trial]<-1
      }
    }
    #previously the mouse was persevering
    if (prl$perseverence_counter[trial-1]>0){
      #this time did they persevere?
      if (prl$chose_left[trial-1]==prl$chose_left[trial]){
        #yes, increase counter
        prl$perseverence_counter[trial]<-prl$perseverence_counter[trial-1]+1
        }
    }
    
    if (prl$perseverence_counter[trial]==0){
      if (prl$perseverence_counter[trial-1]>0){
        prl$perseverence[trial-1]<-prl$perseverence_counter[trial-1]
      }
    }
  }
}#end whole thing

prl$perseverence_true<-prl$perseverence>0

prl_sum<-prl|>
  group_by(subj_id,date)|>
  summarise(crits=sum(towards_criterion==6),
            trials=max(trial),
            crit_per_100=((sum(towards_criterion==6))/max(trial))*100,
            win_stay_prob=(sum(win_stay==1))/sum(reward==1),
            lose_shift_prob=sum(lose_shift==1)/sum(reward==0),
            percent_chose_left=(sum(chose_left==1)/max(trial))*100,
            accuracy=(sum(opt)/max(trial))*100,#
            n_trials=max(trial),
            true_win_stay_prob=(sum(true_win_stay==1))/sum(opt==1),
            m_perseverence=sum(perseverence)/sum(perseverence_true)
            )

m_prl_sum<-prl_sum|>
  group_by(date)|>
  summarise(m_crit_per_100=mean(crit_per_100),
            m_win_stay_prob=mean(win_stay_prob),
            m_lose_shift_prob=mean(lose_shift_prob),
            m_accuracy=mean(accuracy),
            m_crits=mean(crits),
            m_n_trials=mean(n_trials),
            m_true_win_stay_prob=mean(true_win_stay_prob),
            m_m_perseverence=mean(m_perseverence,na.rm=TRUE))

write_csv(prl,
          file = paste0('Processed Data/',Experiment,'_PROCESSED.csv'))

if (Experiment=="PRL"){
  
  write_csv(prl_sum,
          file = paste0('Processed Data/',Experiment,'_SUM.csv'))
  
  write_csv(m_prl_sum,
          file = paste0('Processed Data/',Experiment,'_SUM_MEANS.csv'))
} else {warning("(FREYA) Summaries & means not automatically saved for specific experiments. May need to assign doses.")}

