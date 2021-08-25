# Shiny additions are only to do with showing notifications namely:
# - In the main loop make a progress bar, and after each loop update the bar
# - Add a notifiaction to say when the juveniles are being added
# - Removing working directory redirect for obvious reasons
# - Redirecting to 'shinyversion' of analysis functions 



#setwd("C:/Users/se308/Google Drive/Work/Comparative/Projects/Exploring Kinship Dynamics/R Projects/Kinship Dynamics Model")

require(tidyverse)
require(gridExtra)
require(survival)
require(kinship2)
require(mgcv)

#rm(list = ls())

source('KinDym model functions v5.R') # remember to save this you fool
# source('KinDym analysis functions v5 shinyversion.R')



###THE MODEL
run_kinship_dynamics_model = function(AGE_AT_MATURITY, 
                                      MALE_DISPERSAL_RATE, 
                                      FEMALE_DISPERSAL_RATE,
                                      MALE_ADULT_EX,
                                      FEMALE_ADULT_EX,
                                      LOCAL_MATING_RATE,
                                      ADULT_GROUP_SIZE,
                                      JUVENILE_GROUP_SIZE = 0,
                                      INCLUDE_JUVENILES = TRUE,
                                      RETURN_JUVENILES = FALSE,
                                      FIXED_SEX_RATIO = "Y",
                                      TOTAL_INDIVIDUALS_AIM = 2000
) {
  #derived inputs
  MALE_ADULT_MORTALITY_RATE = 0.5 / (MALE_ADULT_EX - AGE_AT_MATURITY)
  FEMALE_ADULT_MORTALITY_RATE = 0.5 / (FEMALE_ADULT_EX - AGE_AT_MATURITY)
  if(INCLUDE_JUVENILES == TRUE) # to compensate (inaccuaraely) for loss of a couple of individuals when removing last few runs (See below)
  {TOTAL_INDIVIDUALS_AIM = TOTAL_INDIVIDUALS_AIM + AGE_AT_MATURITY} # means total will always be over the aim
  
  
  ##Seed table
  LIVING = list()
  
  seed = data.frame(
    id = seq(1,ADULT_GROUP_SIZE,1),
    age = sample(c(1:(max(c(FEMALE_ADULT_EX, MALE_ADULT_EX)))*2),ADULT_GROUP_SIZE , replace = TRUE),
    sex = c(rep.int("F", ceiling(ADULT_GROUP_SIZE/2)), rep.int("M", floor(ADULT_GROUP_SIZE/2))),
    mother = rep.int(NA, ADULT_GROUP_SIZE), # through mother id of NA reproents unknown
    father = rep.int(NA,ADULT_GROUP_SIZE)
  )
  
  for(i in 1:(AGE_AT_MATURITY+1)){
    seed$age = seed$age + (i-1)
    LIVING[[i]] = seed
  }
  
  
  
  # the life loop
  BURN_IN = 5 * (mean(c(MALE_ADULT_EX, FEMALE_ADULT_EX))*2.39) # 100 generations approx before end of burn in
  
  i = AGE_AT_MATURITY +1 # i is year
  j = 0 # j is the total number of individuals who have so far exisited in the database
  withProgress(message = "Model Progress",min = 0, max =TOTAL_INDIVIDUALS_AIM ,value = 0, { 
    
    while(j <= TOTAL_INDIVIDUALS_AIM){
      #if(i %% 50 == 0){print(paste("model.year: ", i, " ", "individuals.lived: ", j, sep = ""))}
      
      survivors = mortality(current.living.table = LIVING[[i-1]], 
                            M.adult.mortality = MALE_ADULT_MORTALITY_RATE,
                            F.adult.mortality = FEMALE_ADULT_MORTALITY_RATE,
                            M.adult.Ex = MALE_ADULT_EX,
                            F.adult.Ex = FEMALE_ADULT_EX 
      )
      replacments = replacement(living.list = LIVING,
                                post.mortality.population.df = survivors,
                                group.size = ADULT_GROUP_SIZE,
                                m.dispersal.prob = MALE_DISPERSAL_RATE,
                                f.dispersal.prob = FEMALE_DISPERSAL_RATE,
                                age.at.maturity = AGE_AT_MATURITY, 
                                local.mating.prob = LOCAL_MATING_RATE,
                                fixed.sex.ratio = FIXED_SEX_RATIO
      )
      
      #Ageing phase
      new.living = arrange(rbind(survivors,replacments), desc(age))
      new.living$age = new.living$age + 1
      
      
      LIVING[[i]] = new.living
      
      oldj = j
      if(i > BURN_IN) {j = j + nrow(replacments) } # only start counting individuals once the burn in period is past
      if(i %% 50 == 0){
        incProgress(amount = (j - oldj),
                    detail = paste("model.year: ", i, " ",
                                   "individuals.lived: ", j, " of ", TOTAL_INDIVIDUALS_AIM,
                                   sep = ""))
        #print(paste("model.year: ", i, " ", "individuals.lived: ", j, sep = "")) 
      } else {
        incProgress(amount = (j-oldj))
      }
      
      i = i+1
    }
    
  })
  
  if(INCLUDE_JUVENILES == TRUE){ # Juveniles are added after the fact using the Calvanistic Methodlogy
    showNotification("Applying Juveniles",
                     duration = NULL,
                     id = "juve.note"
                     )
    JUVENILES = create_juveniles(living.list = LIVING,
                                 juvenile.group.size = JUVENILE_GROUP_SIZE,
                                 age.at.maturity = AGE_AT_MATURITY,
                                 local.mating.rate = LOCAL_MATING_RATE,
                                 show.damned.age.plot =  FALSE
    )
    
    
    for(i in 1:length(LIVING)){
      adults = LIVING[[i]]
      adults$calvin_type = NA
      adults$life.stage = "adult"
      juves = JUVENILES[[i]]
      juves$life.stage = "juvenile"
      LIVING[[i]] = rbind(adults, juves)
    }
    removeNotification(id = "juve.note")
    
    
    showNotification("Calculating relatedness",
                     duration = NULL,
                     id = "relat.note")
    
    burn.period = LIVING[1:BURN_IN]
    LIVING = LIVING[(BURN_IN +1):length(LIVING)]
    
    run_checks_in_kindym_model(LIVING, LOCAL_MATING_RATE, INCLUDE_JUVENILES) # runs some checks, output is either to stop the funciton or to print a warning
    
    live.to.kin = lapply(LIVING, function(x){return(select(x, -(calvin_type), -(life.stage)))})
    burn.period.II = lapply(burn.period, function(x){return(select(x, -(calvin_type), -(life.stage)))})
    all.kinship = get_kinship_matrix(live.to.kin, burn.period.II)
    
    LIVING = lapply( LIVING, get_local_relatedness, all.kinship)
    
    removeNotification(id = "relat.note")
    
    if(RETURN_JUVENILES == FALSE){
      LIVING = lapply(LIVING, function(x){return(filter(x, life.stage == "adult"))})
      LIVING = lapply(LIVING, function(x){return(select(x, -(calvin_type), -(life.stage)))})
    }
    
  } else { # end include_juves
    
    showNotification("Calculating relatedness",
                     duration = NULL,
                     id = "relat.note")
    burn.period = LIVING[1:BURN_IN]
    LIVING = LIVING[(BURN_IN +1):length(LIVING)]
    
    run_checks_in_kindym_model(LIVING, LOCAL_MATING_RATE, INCLUDE_JUVENILES) # runs some checks, output is either to stop the funciton or to print a warning
    
    all.kinship = get_kinship_matrix(LIVING, burn.period)
    
    LIVING = lapply( LIVING, get_local_relatedness, all.kinship)
    
    removeNotification(id = "relat.note")

  } 
  
  return(LIVING)
}


# ##RUNNING THE MODEL

# output = run_kinship_dynamics_model(AGE_AT_MATURITY = 2,
#                                     MALE_DISPERSAL_RATE = 0.85,
#                                     FEMALE_DISPERSAL_RATE = 0.15,
#                                     MALE_ADULT_EX = 10,
#                                     FEMALE_ADULT_EX= 10,
#                                     LOCAL_MATING_RATE = 0.82,
#                                     ADULT_GROUP_SIZE = 10,
#                                     JUVENILE_GROUP_SIZE = 5,
#                                     INCLUDE_JUVENILES = TRUE,
#                                     RETURN_JUVENILES = TRUE,
#                                     FIXED_SEX_RATIO = "Y",
#                                     TOTAL_INDIVIDUALS_AIM = 2000)
# 
# sanity_check_plots(output, 2)
# all.living = do.call(rbind, output)
# all.living
# 
# 
# 
# # relatedness_GAM.plot(all.living, sex = "F") # as J and C 2010
# # relatedness_GAM.plot(all.living, sex = "M")
# relatedness_GAM.plot(all.living, sex = "both", return.graph = TRUE, return.data = FALSE)
# 

