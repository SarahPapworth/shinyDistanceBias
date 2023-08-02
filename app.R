#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

rm(list=ls())

# make sure needed packages and dependencies are installed
# copied this code from: https://stackoverflow.com/questions/45346367/installing-required-packages-in-shiny-app
# in the end tried to copy code from https://github.com/SandraKla/Zlog_AdRI

if("circular" %in% rownames(installed.packages())){
  library(circular)} else{
    install.packages("circular")
    library(circular)}

# load in needed packages
library(shiny)
library(shinybusy)
library(shinyWidgets)
library(Distance)

OnlineVersion = T

##  YOU MIGHT NEED TO UNHASH THIS NEXT LINE FOR RUNNING ON YOUR LAPTOP

#source (file.path ( getwd() , "source" , "quickLibrary.r"))






# Define UI
ui <- fluidPage(
  navbarPage("Avoiding biased estimates", id = "nav",
             tabPanel("Introduction",
                      h3("Correcting for observer avoidance in distance sampling"),
                      h4("This ShinyApp is designed to allow you to estimate the effect of observer avoidance on density estimates. Do you get the 'Avoiding biased estimates' pun yet?"),
                      p("This because when animals flee before being detected, they become more easily missed by the observer. This can lead to an underestimation of the density of human avoidant animals, and an overall reduction in density estimates relative to actual densities. This app allows you to estimate the degree of bias using computer simulations. On this tab we explain what the simulations are doing, on the next one we explain the input parameters, and then on the final one you can run your own simulations."),
                      fluidRow(
                        column(6,
                               
                               h4("What is the computer simulation doing?"),
                               h5("The computer simulation works by extracting parameters from a model like the one shown to the right. "),
                               h5("The ", tags$span(style = "color:red; font-weight:bold;", "red"), "  spot represents the observer who is following a transect outlined by a red line within a two-dimensional environment."),
                               h5("This environment is teeming with animals depicted as ", tags$span(style = "color:black; font-weight:bold;", "black"), " spots."),
                               h5("In each time step, both the ", tags$span(style = "color:red; font-weight:bold;", "red"), " observer and the ", tags$span(style = "color:black; font-weight:bold;", "black"), " animals scan their surroundings."),
                               h5("If the animals detect the observer's presence, they may flee based on various parameters, that you can edit yourself in the next tab."),
                               h5("if the observer detects any animals, it records their presence, resulting in the deletion of those animals from the model."),
                               h5("Can you see this deletion occurring?"),
                               h5("In the computer simulations you can change key parameters, such as observer speed and fleeing distance, to fit the circumstances of your own data collection."),
                        ),
                        column(6,
                               tags$video(src =   "exampleTransect.mp4", type = "video/mp4", controls = NA, autoplay = TRUE, loop = TRUE)
                        )
                      )
             ),
             
             #### TAB 2 -- videos
             tabPanel("Input parameters",
                      
                      h3("Input parameters for simulations"),
                      h5(tags$u("As the outputs of the model, and the estimated bias, depend on the inputs used, let's explore these inputs.")),
                      h5("If you have measured avoidance behaviours such as the flight initiation distances or fleeing distances of animals in your population you can input them below. You could also use values extracted from the literature. Entered below are default values extracted from the manuscript which accompanies this ShinyApp (Excluding distance to border which is larger in the ShinyApp for clarity of visualisation)."),
                      
                      fluidRow(
                        column(4,
                               numericInput ( inputId = "monk.scale.param"          , label = "Animal detection distance (m)"               , min = 1 , max = 10000 , value = 60),
                               h5( em ( "How far away can animals detect the human observer?")),
                               numericInput ( inputId = "scale.param"               , label = "Observer detection distance (m)"             , min = 10, max = 10000, value = 30),
                               h5( em ( "How far away can human observers dectect the animals?")),
                               textInput    ( inputId = "fid.param"                 , label = 'Flight initiation distance(FID) in metres', "43,44,57"),
                               h5( em ( "Enter multiple FIDs separated with commas, or just one value. Default are three of the FIDs we found in our Monkey popuation")),
                               numericInput ( inputId = "monk_density_grp_km_2"     , label = "Density of animals (or animal groups) per sq. km"          , min = 0.1, max = 50, value = 2.5 ),
                               h5( em ( "What animal density have you measured in your survey? Otherwise choose appropriate proxy"))),
                        
                        
                        column(4,
                               numericInput ( inputId = "speed.dist.rate.decay"     , label = "Animal speed when not fleeing (m/ time step)"           , min = 0, max = 1, value = 0.15),
                               h5( em ( "How fast do animals move when observers are absent? Taken from an exponential curve where lower values equate to faster speeds")),
                               numericInput ( inputId = "norm.mean"                 , label = "Mean animal speed when fleeing (m/ time step)", min = 0, max = 100, value = 60),
                               h5( em ( "How fast do animals move when fleeing?")),
                               numericInput ( inputId = "norm.sd"                   , label = "Standard deviation of animal speed when fleeing"                         , min = 0, max = 50, value = 15),
                               h5( em ( "Speed when fleeing can vary. The degree to which it varies is controlled by this parameter. Consider mean speed divided by four as we chose in the manuscript")),
                               numericInput ( inputId = "kappa."                    , label = "Precision in direction of animal fleeing (kappa)"          , min = 0, max = 100, value = 50),
                               h5( em ( "Movements away from the observer are likely to be directed, but how much so? Exactly in the opposite direction (high kappa), or with some variance (low kappa)"))),
                        column(4,
                               numericInput ( inputId = "shape.param"               , label = "Observer shape parameter"                                  , min = 2, max = 4 , value =3 ),
                               h5( em ( "Parameter controlling the shape of the detection curve. See Hazard rate equations in Buckland et al. 2001 ")),
                               numericInput ( inputId = "monk.shape.param"          , label = "Animal shape parameter"                                    , min = 2, max = 4, value = 3),
                               h5( em ( "As above")),
                               numericInput ( inputId = "step.length"               , label = "Observer step length (distance between observations)"      , min = 10   , max = 100   , value = 25  ),
                               h5( em ( "How far does the observer move per time step")),
                               numericInput ( inputId = "di"                        , label = "Distance between transects and borders (m)"                    , min = 500  , max = 2000  , value = 1000),
                               h5( em ( "shortest distance between the transect and the borders in the model")),
                               numericInput ( inputId = "grid.size.y"               , label = "Transect length (m)"                                        , min = 2000 , max = 10000 , value = 5000),
                               h5( em ( "Length of the transect"))),
                        
                      ),
                      h5("Once you have changed the parameters above to suit your study:"),
                      h5("1. Click the 'Generate landscape' button below to create the landscape and populate it with animals."),
                      h5("2. Then click the arrow below the slider to the right and the simularion will run, showing you how the observer and individuals move through the landscape."),
                      
                      fluidRow(
                        column(5,
                               h5(""),
                               actionButton("vidrun"   , "Generate landscape",style='background-color: #137ab3; color: #fff; padding:20px; font-size:200%')),
                        column(5,
                               sliderInput(inputId = "time",
                                           label = "",
                                           min = 1,
                                           max = 200,
                                           width = NULL,
                                           value = 1,
                                           step = 1,
                                           animate = animationOptions(interval = 40))
                        ),
                        column(2,add_busy_spinner(spin = "fading-circle")
                        ))
                      ,
                      fluidRow(
                        column(5,
                               plotOutput("vidPlot",width = "200px", height= "500px"),
                        ),
                        column(5,
                               h4("Once you've run the simulation once, why not change some of the inputs and rerun the simulation to see what happens?"),
                               h5(HTML("1. Increase animal flight initiation distance (FID) and animal detection distance. Try 500 meters for each category and watch how the animals flee ahead of the observer without being recorded and disappearing. ")),
                               h5(HTML("2. Decrease animal FID and animal detection distance to 50m, then turn up observer detection distance to 200m. What happens now?")),
                        ) 
                      ),
                      tags$head(tags$style(type='text/css', ".slider-animate-button { font-size: 30pt !important; }"))
                      ,),
             
             
             ### TAB 3. SIMULATIONS!!
             tabPanel("Run simulations",
                      h3("Running your own simulations."),
                      h5("The previous tab showed you a single transect with the parameters you input, but to estimate bias we need to run multiple transects. This tab will allow you to run simulations with the inputs chosen on Tab 2: Input parameters"),
                      h5("Before running the simulations, you need to give 2 more parameters:"),
                      h5("The number of transects to use to generate a density estimate."),
                      h5("The number of simulations to run - how many density estimates would you like to use to estimate the bias."),
                      
                      fluidRow(
                        column(5,
                               numericInput ( inputId = "transects.per.sim"         , label = "N transects per simulation"                                , min = 20, max = 100, value = 2)),
                        column(5,
                               numericInput ( inputId = "n.sim"                     , label = "N simulations for density estimate"                        , min = 20, max = 10000, value = 2),
                        )),
                      h5("We suggest running a lot of simulations (e.g. 1000), but be aware this will take some time to run! After you click 'Run simulations' below, wait until you see the simulations appear, then your estimates are ready to download."),
                      
                      
                      # Sidebar with a slider input for number of bins
                      fluidRow(
                        column(6,
                               actionButton("calculate", "Run simulations",style='background-color: #137ab3; color: #fff; padding:20px; font-size:200%')),
                        
                        column(6,
                               downloadButton("downloadData", label = "Download data", style='background-color: yellow; color: black; padding:20px; font-size:200%')
                        ),
                        
                        
                      ),
                      fluidRow(
                        column(width = 6,plotOutput("histPlot")),
                        column(width = 6,textOutput("textRender")),
                      )
             )
  )
)







# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  # rm(list=ls())
  #    input= list (
  # "n.sim"                      = 10,
  # "transects.per.sim"          = 20,
  #        "time"                = 1,
  # "grid.size.x"                = 20000,
  # "grid.size.y"                = 5000,
  # "monk_density_grp_km_2"      = 2.5 ,
  # "step.length"                = 25  ,
  # "scale.param"                = 30,
  # "shape.param"                = 3 ,
  # "fid.param"                  = "4000",
  # "monk.scale.param"           = 10000,
  # "monk.shape.param"           = 3,
  # "kappa."                     = 50,
  # "speed.dist.rate.decay"      = 0.1,
  # "norm.mean"                  = 60,
  # "norm.sd"                    = 15,
  # "max.detect.dist"            = 300,
  # "di"                         = 1000
  # )
  # source (file.path ( getwd() , "source" , "quickLibrary.r"))
  
  sam = eventReactive( input$vidrun , {
    
    # # # # # # # #
    #             #
    # START HERE! # 
    #             #
    # # # # # # # #
    
    fid.param <- as.numeric(unlist(strsplit(input$fid.param,",")))
    d = model ( 
      
      grid.size.y           = input$grid.size.y,
      monk_density_grp_km_2 = input$monk_density_grp_km_2,
      step.length           = input$step.length , #how many steps from start to finish (number of steps)
      scale.param           = input$scale.param, #human scale param
      shape.param           = input$shape.param, #affects the shape of the detection function
      fid.param             = fid.param, #distance at which monkeys run away (from my data). Can use vector of numbers or single value!
      monk.scale.param      = input$monk.scale.param, #affects how quickly probability of detection falls
      monk.shape.param      = input$monk.shape.param,  #affects the shape of the detection function
      kappa.                = input$kappa.,#how narrow is the von mises distribution (higher kappa, narrower the distribution)
      speed.dist.rate.decay = input$speed.dist.rate.decay,  #monkey speed: param which gave monkeys a max speed of 100 m/timestep
      norm.mean             = input$norm.mean,  # monkey speed when detected
      norm.sd               = input$norm.sd,  # sd of speed above
      di                    = input$di,
      plot_monk             =  T
    )
    d$plot.return
  })
  
  staT = eventReactive( input$calculate , {
    
    if( ! OnlineVersion){
      dens = c()
      
      fid.param <- as.numeric(unlist(strsplit(input$fid.param,",")))
      i=2
      for( j in 1:input$n.sim){
        withProgress(message = 'Running simulations', value = j/input$n.sim, {
          d1s = NULL
          d2s= NULL
          for ( i in 1:input$transects.per.sim){
            
            d = model ( 
              
              grid.size.y           = input$grid.size.y,
              monk_density_grp_km_2 = input$monk_density_grp_km_2,
              step.length           = input$step.length , #how many steps from start to finish (number of steps)
              scale.param           = input$scale.param, #human scale param
              shape.param           = input$shape.param, #affects the shape of the detection function
              fid.param             = fid.param, #distance at which monkeys run away (from my data). Can use vector of numbers or single value!
              monk.scale.param      = input$monk.scale.param, #affects how quickly probability of detection falls
              monk.shape.param      = input$monk.shape.param, #affects the shape of the detection function
              kappa.                = input$kappa.,#how narrow is the von mises distribution (higher kappa, narrower the distribution)
              speed.dist.rate.decay = input$speed.dist.rate.decay,  #monkey speed: param which gave monkeys a max speed of 100 m/timestep
              norm.mean             = input$norm.mean,  # monkey speed when detected
              norm.sd               = input$norm.sd,  # sd of speed above
              di                    = input$di,
              plot_monk             = F
            )
            d1s = rbind(d1s,cbind(d , i))
            d2= model ( 
              grid.size.y           = input$grid.size.y,
              monk_density_grp_km_2 = input$monk_density_grp_km_2,
              step.length           = input$step.length , #how many steps from start to finish (number of steps)
              scale.param           = input$scale.param, #human scale param
              shape.param           = input$shape.param, #affects the shape of the detection function
              fid.param             = 0, #distance at which monkeys run away (from my data). Can use vector of numbers or single value!
              monk.scale.param      = input$monk.scale.param, #affects how quickly probability of detection falls
              monk.shape.param      = input$monk.shape.param, #affects the shape of the detection function
              kappa.                = input$kappa.,#how narrow is the von mises distribution (higher kappa, narrower the distribution)
              speed.dist.rate.decay = input$speed.dist.rate.decay,  #monkey speed: param which gave monkeys a max speed of 100 m/timestep
              norm.mean             = input$norm.mean,  # monkey speed when detected
              norm.sd               = input$norm.sd,  # sd of speed above
              di                    = input$di,
              plot_monk             =  F
            )
            d2s = rbind(d2s,cbind( d2,i) )
            
          }
          effort = (input$grid.size.y-input$step.length)/1000
          d1s=as.data.frame(d1s)  
          d2s=as.data.frame(d2s)
          dat = data.frame ( distance = d1s[,"d"]/1000,  # converting distances to km
                             Area = ( (input$grid.size.y+input$di*2)/1000) * (input$di*2/1000)*length(unique(d1s[,"i"])),  # size of the region of interest (for density estimate) in km
                             Region.Label = "baseline",  # Stratum containing the transect (in this case all in the same region)
                             Effort =  effort, #Lenght of transects (depends on size of grid)
                             Sample.Label = d1s[,"i"] )  #  ID of the
          dat2 = data.frame ( distance = d2s[,"d2"]/1000,  # converting distances to km
                              Area = ( (input$grid.size.y+input$di*2)/1000) * (input$di*2/1000)*length(unique(d2s[,"i"])),  # size of the region of interest (for density estimate) in km
                              Region.Label = "baseline",  # Stratum containing the transect (in this case all in the same region)
                              Effort =  effort, #Lenght of transects (depend2s on size of grid)
                              Sample.Label = d2s[,"i"] )  #  ID of the
          
          if ( any( !is.na(dat$distance))){
            den= R.utils::withTimeout(Density.estimate(dat) , timeout = 400, onTimeout = "error")
            den1 = den$est
          } else {
            den1=0
          }
          if ( any( !is.na(dat2$distance))){
            den= R.utils::withTimeout(Density.estimate(dat2) , timeout = 400, onTimeout = "error")
            den2 = den$est
          } else {
            den2=0
          }
          
          dens = rbind(dens, c(den1,den2))
          
        })
        
        # Increment the progress bar, and update the detail text.
        #incProgress(1/input$n.sim, detail = paste("Doing part", i, "/", input$n.sim ))
      }
      dens = as.data.frame(dens)
      names ( dens ) = c( "avoidance" , "noAvoidance")
      dens
    } else {
      NA
    }
  })
  
  
  ###### VIDEO PLOT
  di = reactive ( input$di)
  output$vidPlot <- renderPlot({
    
    if ( !is.na ( sam()$monkeys [1,1,input$time+1])){ 
      # sam = function ( )print ( d$plot.return)
      #di = function()print(input$di)
      par ( mar = c(10,4,4,4))
      plot ( sam()$monkeys [,1,input$time+1] , 
             sam()$monkeys [,2,input$time+1]  ,xlim =c(0, di() *2),xlab="x axis (metres)",ylab="y axis (metres)",
             main = "One transect" , 
             ylim = c(0,input$grid.size.y+input$di*2 )
             #,sub = "(or first 60 steps of \n transect if you change \n y axis or step length)"
      )
      points ( sam()$camwalk[input$time+1,1] , 
               sam()$camwalk[input$time+1,2],col = "red",pch=19)
      lines ( sam()$camwalk[2:(nrow(sam()$camwalk)),1] , 
              sam()$camwalk[2:(nrow(sam()$camwalk)),2],col = "red")
      
    } 
    # else {
    #   plot.new()
    #   legend("center",legend="fin")
    # }
  })
  
  
  ####### DENSITY OUTPUT PLOT
  output$histPlot = renderPlot({
    if ( !OnlineVersion){
      par(mfrow=c(3,1))
      hist(staT()[,1], main = paste0 ( input$n.sim," density estimates with observer avoidance"),xlab=""   ,xlim = c(0,max(staT(),na.rm=T)+1), sub = paste ( "median density estimate =", round ( median( na.omit(staT()[,1])),4)))
      abline(v =median( na.omit(staT()[,1])),col = "red")
      hist(staT()[,2], main = paste0 ( input$n.sim," density estimates with no observer avoidance"),xlab="",xlim = c(0,max(staT(),na.rm=T)+1),  sub = paste ( "median density estimate =", round ( median( na.omit(staT()[,2])),4)))
      abline(v =median( na.omit(staT()[,2])),col = "red")
    } else { 
      u = staT()
      par ( mar = rep(0,4))
      plot(1:10,type = "n")
      legend("center", legend= "No simulations on online version")
    }
  })
  
  output$textRender = renderText({
    if ( ! OnlineVersion){
      med.av   = median( na.omit(staT()[,1]))
      med.n.av = median( na.omit(staT()[,2]))
      div = med.av / med.n.av
      if ( div < 1){
        paste0( "Your avoidance behaviours are causing an underestimate of your population density, with an estimated bias of " , round( 100-(div)*100 ,2),  "%. To correct for this bias, multiply your density estimate by ", round(med.n.av/med.av,4),".")
      } else {
        paste0 ( "Your simulations suggest avoidance behaviours positively impact detectability. This is likely due to sampling error, try running with more replicates")
      }
    } else { 
      u=staT()
      "No simulations on online version. To run simulations, please download shiny app onto your computer from: https://github.com/sankeydan/shinyDistanceBias/ "
    }
  })
  
  ######### DOWNLOAD DATA
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(staT(), file, row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
