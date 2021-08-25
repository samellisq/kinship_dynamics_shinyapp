require(tidyverse)
require(gridExtra)
require(survival)
require(kinship2)
require(mgcv)

source("KinDym model v5 shinyversion.R")
source("KinDym ShinyApp Functions v1.R")
# source('KinDym model functions v3.R') # remember to save this you fool
# source('KinDym analysis functions v3 SmallShinyChanges.R')

server <- function(input, output, session) { # session only included to fiddle with plot height dynamically. not really necessarry
  
  model.output = eventReactive(input$go.button, { # within this reactive statement the MODEL run (KinDym model v3.R). Apart from the graphs which are rendered below
    
    #removeNotification(id = "gam.note")
    removeNotification(id = "saneplot.note")
    removeNotification(id = "juve.note")
    
    #Modelling inputs
    living.list = run_kinship_dynamics_model(AGE_AT_MATURITY = input$age.at.mat,
                                             MALE_DISPERSAL_RATE = input$m.disp.rate,
                                             FEMALE_DISPERSAL_RATE = input$f.disp.rate,
                                             MALE_ADULT_EX = input$m.ex,
                                             FEMALE_ADULT_EX= input$f.ex,
                                             LOCAL_MATING_RATE = input$local.mate.rate,
                                             ADULT_GROUP_SIZE =input$adult.grp.size,
                                             JUVENILE_GROUP_SIZE = input$juv.grp.size,
                                             INCLUDE_JUVENILES = ifelse(input$juv.grp.size == 0, FALSE, TRUE),
                                             RETURN_JUVENILES = FALSE,
                                             FIXED_SEX_RATIO = TRUE,
                                             TOTAL_INDIVIDUALS_AIM = input$input.jaim)
    
  })
  
  which.plot = eventReactive(input$graphtype, { 
    which.plot = input$graphtype
  }
  )
  
  thin.int = eventReactive(input$graphtype, { #Sets the thinning interval but only change if graph type changes
    thin.in = input$thinning.int
  })
  


  output$relatedness.plot <- renderPlot({
    plotting.data = apply_thinning(model.output(), thin.int())
    if(which.plot() == "Females to: the group, females and males"){
      relatedness_mean_plot(bind_rows(model.output()), sex = "F", return.graph = FALSE, return.data = TRUE)
    } else{ 
      if(which.plot() == "Males to: the group, females and males"){
        relatedness_mean_plot(bind_rows(model.output()), sex = "F", return.graph = FALSE, return.data = TRUE)
      } else {
        if(which.plot() == "Females to the group vs. males to the group"){
          relatedness_mean_plot(bind_rows(model.output()), sex = "F", return.graph = FALSE, return.data = TRUE)    }
      }
    }
    
  }, height = function() {
    session$clientData$output_check.plot_width*0.65 # this session stuff copied from online (shiny developer) to allow dynamic reporgramming of height. Width does it already
  })
  
  output$check.plot <- renderPlot({
    sanity_check_plots(model.output(), input$age.at.mat)
  }, height = function() {
    session$clientData$output_check.plot_width*0.65 # this session stuff copied from online (shiny developer) to allow dynamic reporgramming of height. Width does it already
  }
  )
 
  

  
  
}
