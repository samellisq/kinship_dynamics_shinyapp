require(tidyverse)
require(gridExtra)
require(survival)
require(kinship2)
require(mgcv)

source('KinDym model functions v3.R') # remember to save this you fool
source('KinDym analysis functions v3 SmallShinyChanges.R')

server <- function(input, output, session) { # session only included to fiddle with plot height dynamically. not really necessarry
  
  model.output = eventReactive(input$go.button, { # within this reactive statement the MODEL run (KinDym model v3.R). Apart from the graphs which are rendered below
    
    #INPUTS
    
    AGE_AT_MATURITY = input$age.at.mat # NOTE Probably needs to be split into M and F ages at matuurity
    MALE_DISPERSAL_RATE = input$m.disp.rate
    FEMALE_DISPERSAL_RATE = input$f.disp.rate
    MALE_ADULT_EX = input$m.ex #this ex at adulthood
    FEMALE_ADULT_EX = input$f.ex
    
    LOCAL_MATING_RATE = input$local.mate.rate #1 all mating local, 0 no mating local. Takes proportions
    
    ADULT_GROUP_SIZE = input$adult.grp.size
    
    #Modelling inputs
    FIXED_SEX_RATIO = input$sex.ratio.fix # Tells the model whether to artifically mantain a 50 50 sex ratio or to let it vary. Vary is better but problamatic if group size < 10(ish)
    
    #derived inputs
    MALE_ADULT_MORTALITY_RATE = 0.5 / (MALE_ADULT_EX - AGE_AT_MATURITY)
    FEMALE_ADULT_MORTALITY_RATE = 0.5 / (FEMALE_ADULT_EX - AGE_AT_MATURITY)
    
    
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
    BURN_IN = 10 * AGE_AT_MATURITY # 10 is arbitary, just needs to be long enough to purge out the seed generation thoroughly
    TIME = 1000
    
    TOTAL_TIME = BURN_IN + TIME
    TOTAL_INDIVIDUALS_AIM = input$input.jaim
    
    i = AGE_AT_MATURITY +1 # i is year
    j = 0 # j is the total number of individuals who have so far exisited in the database
    
    withProgress(message = "Model Progress",min = 0, max =TOTAL_INDIVIDUALS_AIM ,value = 0, { # put the loop in a with progress to display progress bar
      while(j <= TOTAL_INDIVIDUALS_AIM){
        
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
    
    
    
    burn.period = LIVING[1:BURN_IN]
    LIVING = LIVING[(BURN_IN +1):length(LIVING)]
    
    if(is.null(LIVING[[length(LIVING)]])){print("Removing non null years for error checking");
      LIVING = Filter(Negate(is.null), LIVING)}
    
    
    if(LOCAL_MATING_RATE < 1){
      all.living1 = do.call(rbind, LIVING)
      all.ids = distinct(select(all.living1,id,father))
      duplicates = duplicated(filter(all.ids, father >= 1000000000)$father)
      if (sum(duplicates) > 1){
        stop("WARNING. Randomly generated father ids accidentally duplicated. Run model again") 
      }
      rm(all.living1)
      rm(all.ids)
      rm(duplicates)
    }
    
    
    all.kinship = get_kinship_matrix(LIVING, burn.period)
    
    LIVING = lapply( LIVING, get_local_relatedness, all.kinship)
    # all.living = do.call(rbind, LIVING)
    
  })
  

  output$check.plot <- renderPlot({
    sanity_check_plots(model.output(), input$age.at.mat)
  }, height = function() {
    session$clientData$output_check.plot_width*0.65 # this session stuff copied from online (shiny developer) to allow dynamic reporgramming of height. Width does it already
  }
  )
  
  output$relatedness.plot <- renderPlot({
    if(input$graphtype == "Females to: the group, females and males"){relatedness_GAM.plot(do.call(rbind, model.output()), sex = "F")
    } else{ 
      if(input$graphtype == "Males to: the group, females and males"){relatedness_GAM.plot(do.call(rbind, model.output()), sex = "M")
      } else {
        relatedness_GAM.plot(do.call(rbind, model.output()), sex = "both")
        }
        }
    
  }, height = function() {
    session$clientData$output_check.plot_width*0.65 # this session stuff copied from online (shiny developer) to allow dynamic reporgramming of height. Width does it already
  })
}
