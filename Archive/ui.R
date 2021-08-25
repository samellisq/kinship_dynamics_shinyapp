ui <- fluidPage(
  
  # Application title
  titlePanel("Kinship Dynamics Simulator"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      div(style="display: inline-block;vertical-align:top; width: 150px;", # annoting fiddling with div allows them to be displayed next to eeach other
          numericInput(inputId = "f.ex",
                   label = "Female Expected Lifespan at Adulthood (years)",
                   min = 1,
                   max = 100,
                   value = 10,
                   width = "100%") 
          ),
      div(style="display: inline-block;vertical-align:top; width: 150px;",
          numericInput(inputId = "m.ex",
                   label = "Male Expected Lifespan at Adulthood (years)",
                   min = 1,
                   max = 100,
                   value = 10,
                   width = "100%")
          ),
      sliderInput(inputId = "f.disp.rate",
                  label = "Female Dispersal Rate (1 = all females disperse)",
                  min = 0,
                  max = 1,
                  value = 0.5),
      sliderInput(inputId = "m.disp.rate",
                  label = "Male Dispersal Rate (1 = all males disperse)",
                  min = 0,
                  max = 1,
                  value = 0.5),
      numericInput(inputId = "age.at.mat",
                     label = "Age at Maturity:",
                     min = 1,
                     max = 100,
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
                   max = 100,
                   value = 10,
                   width = "100%"),
      numericInput(inputId = "juv.grp.size",
                   label = "N Juveniles in Group (0 for only adults model)",
                   min = 0,
                   max = 100,
                   value = 4,
                   width = "100%"),
      actionButton("go.button", "Run Model", 
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      br(),br(),
      selectInput(inputId = "graphtype",
                  label = "Plot kin dynamics of",
                  choices = c("Females to the group vs. males to the group",
                              "Females to: the group, females and males",
                              "Males to: the group, females and males"
                  )
      ),
      br(),
      wellPanel("Modelling Inputs (default recommended):",
                br(),
                numericInput(inputId = "thinning.int",
                             label = "Thinning Interval (1 = no thinning)",
                             min = 1,
                             max = 100000,
                             value = 1,
                             width = "100%"),
                numericInput(inputId = "input.jaim",
                             label = "Model n individual lifespans",
                             min = 100,
                             max = 1000000,
                             value = 2000,
                             width = "100%"),
                style = "background: lightgrey"
                )
    ), # end of sidebar Panel

    # Show a plot of the generated distribution
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Model Population Traits", plotOutput("check.plot", height = "auto")),
                  tabPanel("Kinship Dynamics Plot", plotOutput("relatedness.plot"))
                  )

    )
  ) # end of sidebar layout
)
