ui <- fluidPage(
  
  # Application title
  titlePanel("Kinship Dynamics Simulator"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      div(style="display: inline-block;vertical-align:top; width: 150px;", # annoting fiddling with div allows them to be displayed next to eeach other
          numericInput(inputId = "f.ex",
                   label = "Female Expected Lifespan at Adulthood",
                   min = 1,
                   max = 15,
                   value = 10,
                   width = "100%") 
          ),
      div(style="display: inline-block;vertical-align:top; width: 150px;",
          numericInput(inputId = "m.ex",
                   label = "Male Expected Lifespan at Adulthood",
                   min = 1,
                   max = 15,
                   value = 10,
                   width = "100%")
          ),
      sliderInput(inputId = "f.disp.rate",
                  label = "Female Dispersal Rate (1 = all females disperse)",
                  min = 0,
                  max = 1,
                  value = 0.15),
      sliderInput(inputId = "m.disp.rate",
                  label = "Male Dispersal Rate (1 = all males disperse)",
                  min = 0,
                  max = 1,
                  value = 0.75),
      numericInput(inputId = "age.at.mat",
                     label = "Age at Maturity:",
                     min = 1,
                     max = 20,
                     value = 2,
                     width = "100%"),
      sliderInput(inputId = "local.mate.rate",
                  label = "Rate of Local Mating (0 = no local mating)",
                  min = 0,
                  max = 1,
                  value = 0.82),
      numericInput(inputId = "adult.grp.size",
                   label = "N Adults in Group",
                   min = 1,
                   max = 20,
                   value = 10,
                   width = "100%"),
      wellPanel(
      div(style="display: inline-block;vertical-align:top; width: 150px;", # annoting fiddling with div allows them to be displayed next to eeach other
          checkboxGroupInput(inputId  = 'includejuves', 
                             label    = "Include Juveniles", 
                             choices  = "",
                             # selected = "",
                             inline   = FALSE)
      ),
      div(style="display: inline-block;vertical-align:top; width: 150px;", # annoting fiddling with div allows them to be displayed next to eeach other
          checkboxGroupInput(inputId  = 'returnjuves', 
                             label    = "Return Juveniles", 
                             choices  = "",
                             # selected = "",
                             inline   = FALSE)
      ),
      div(style="display: inline-block;vertical-align:top; width: 150px;",
          numericInput(inputId = "juv.grp.size",
                       label = "N Juveniles in Group",
                       min = 0,
                       max = 40,
                       value = 0,
                       width = "100%")
      ),
      # style = "outline-color: blue"
      ),
 
      actionButton("go.button", "Run Model", 
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      br(),br(),
      wellPanel(
                numericInput(inputId = "input.jaim",
                             label = "Model n individual lifespans",
                             min = 100,
                             max = 10000,
                             value = 500,
                             width = "100%"),
                style = "background: lightgrey"
                )
    ), # end of sidebar Panel

    # Show a plot of the generated distribution
   mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Kinship Dynamics Plot", plotOutput("relatedness.plot")),
                  tabPanel("Read Me", 
                           tags$iframe(style="height:800px; width:100%; scrolling=yes", 
                                       src="Kinship-Dynamics-Simulator-Read-Me.pdf"))
                  )

  )
 )#  end of sidebar layout
)
