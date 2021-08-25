require(tidyverse)
# require(gridExtra)
# require(survival)
require(kinship2)
# require(mgcv)
require(viridis)

source("KinDym model v5 shinyversion.R")
# source("KinDym ShinyApp Functions v1.R")

server <- function(input, output, session) { # session only included to fiddle with plot height dynamically. not really necessarry
  
  model.output = eventReactive(input$go.button, { # within this reactive statement the MODEL run (KinDym model v3.R). Apart from the graphs which are rendered below
    
    removeNotification(id = "plot.note")
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
                                             INCLUDE_JUVENILES = ifelse(input$juv.grp.size > 0 & !(is.null(input$includejuves)), TRUE, FALSE),
                                             RETURN_JUVENILES = ifelse(
                                               is.null(input$returnjuves) & is.null(input$includejuves & input$juv.grp.size > 0), FALSE, TRUE),
                                             FIXED_SEX_RATIO = TRUE,
                                             TOTAL_INDIVIDUALS_AIM = input$input.jaim)
    
  })
  
  INDICATOR = eventReactive(input$go.button, {
    INDICATOR = FALSE
  })
  
  
  output$relatedness.plot <- renderPlot({
    all.living.df = bind_rows(model.output())
    all.living.df$sex = ifelse(all.living.df$sex == "F", "Female", "Male")
    showNotification(
      ui = "Rendering Plot", 
      id = "plot.note", type = "default", duration = 15)
    print(min(all.living.df$age))
    ggplot(all.living.df, aes(age, local.relatedness, colour = sex, fill = sex))+
      # geom_smooth(method = "loess", span = 2) +
      geom_smooth(method = "gam") +
      scale_colour_viridis(discrete = TRUE)+
      scale_fill_viridis(discrete = TRUE)+
      xlab("Age") +
      ylab("Local Relatedness")+
      theme_bw()+
      theme(
        axis.title = element_text(size = 30),
        axis.text = element_text(size = 24),
        legend.text = element_text(size = 24),
        legend.title = element_blank()
      )
  }, height = function() {
    session$clientData$output_relatedness.plot_width*0.65 # this session stuff copied from online (shiny developer) to allow dynamic reporgramming of height. Width does it already
    
  }
  
  )
  
  
  
}
