#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

rm(list=ls())
library(shiny)
library(circular)
library(shinybusy)
library(Distance)


## HI SARAH. YOU MIGHT NEED TO UNHASH THIS NEXT LINE FOR RUNNING ON YOUR LAPTOP

# source (file.path ( getwd() , "source" , "quickLibrary.r"))


# function which checks the data; returns TRUE or FALSE
checkData <- function(dat){
  TRUE
}

# function which transforms the data; returns NULL if check not TRUE
processData <- function(dat){
  if(checkData(dat)){
    # do something with dat
    names(dat) <- toupper(names(dat)) # for our example
    return(dat)
  }else{
    return(NULL)
  }
}



# Define UI
ui <- fluidPage(
  navbarPage("Avoiding biased estimates:", id = "nav",
             tabPanel("Introduction",
                      fluidRow(
                        column(6,
                               h3("Can you see this dynamic scenario?"),
                               h4( em ( "----------------------------------------------------------------")),
                               h5("Let's pause for a moment to clarify what's happening here. "),
                               h5("The ", tags$span(style = "color:red; font-weight:bold;", "red"), "  spot represents the observer who is following a transect outlined by a red line within a two-dimensional environment."),
                               h5("This environment is teeming with animals depicted as ", tags$span(style = "color:black; font-weight:bold;", "black"), " spots."),
                               h5("In each time step, both the ", tags$span(style = "color:red; font-weight:bold;", "red"), " observer and the ", tags$span(style = "color:black; font-weight:bold;", "black"), " animals scan their surroundings."),
                               h5("If the animals detect the observer's presence, they may flee based on various parameters, that you can edit yourself in the next slide "),
                               h5("if the observer detects any animals, it records their presence, resulting in the deletion of those animals from the model."),
                               h5("Can you see this deletion occurring?"),
                               h5("-------------------------------------"),
                               h4(em("What's the point of all this?")),
                               p("The purpose is to avoid biased estimates. When the animals flee before being detected, they become more easily missed by the observer. This is crucial because it can lead to an underestimation of the density of human avoidant animals."),
                               p("In order to achieve unbiased estimates, we program the avoidance behaviors into our ", tags$span(style = "color:black; font-weight:bold;", "black"), " animals within the model. This allows us to observe the impact on the final density estimate."),
                               p("Before running computationally expensive models to estimate this bias (Tab 3: Run Simulations), we will first explore Tab 2: Input parameters, to gain a better understanding of how the model functions.")
                        ),
                        column(6,
                               tags$video(src =   "exampleTransect.mp4", type = "video/mp4", controls = NA, autoplay = TRUE, loop = TRUE)
                        )
                      )
             ),
             
             #### TAB 2 -- videos
             tabPanel("Input parameters",
                      
                      titlePanel(em(strong("See your model in action"))),
                      h5(  HTML("<u><em><span style='font-size: 18px;'>Here's where we get to change what goes into the model</span></em></u>")),
                      h5("Let's say we measured the flight initiation distances or fleeing distances of animals in a population."),
                      h5("This tab provides a place to input these data"),
                      h5("as well as providing visual representation of what the model is doing with those measurements"),

                      fluidRow(
                        column(3,
                               h5(""),
                               actionButton("vidrun"   , "Run video",style='background-color: #137ab3; color: #fff; padding:20px; font-size:200%')),
                        column(4, 
                               h3("<--- Click 'Run video'"),
                               h3("then click play --->")),
                        column(3,
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
                        )),
                      fluidRow(sidebarLayout(sidebarPanel(
                        textInput    ( inputId = "fid.param"                 , label = 'FID - Flight initiation distance (m)', "40,45,42"),
                        h5( em ( "Enter multiple FIDs separated with commas, or just one value")),
                        numericInput ( inputId = "monk.scale.param"          , label = "Animal detection distance (m)"               , min = 1 , max = 10000 , value = 100),
                        numericInput ( inputId = "scale.param"               , label = "Observer detection distance (m)"             , min = 10, max = 10000, value = 100),
                        numericInput ( inputId = "grid.size.y"               , label = "Transect length"                                        , min = 2000 , max = 10000 , value = 5000),
                        numericInput ( inputId = "monk_density_grp_km_2"     , label = "Density of animals (or animal groups) per sq. km"          , min = 0.1, max = 50, value = 4 ),
                        numericInput ( inputId = "step.length"               , label = "Observer step length (distance between observations)"      , min = 10   , max = 100   , value = 25  ),
                        numericInput ( inputId = "shape.param"               , label = "Observer shape parameter"                                  , min = 2, max = 4 , value =3 ),
                        numericInput ( inputId = "monk.shape.param"          , label = "Animal shape parameter"                                    , min = 2, max = 4, value = 3),
                        numericInput ( inputId = "kappa."                    , label = "Precision in direction of animal fleeing (kappa)"          , min = 0, max = 100, value = 50),
                        numericInput ( inputId = "speed.dist.rate.decay"     , label = "Animal movements when not fleeing (less is more)"           , min = 0, max = 1, value = 0.1),
                        numericInput ( inputId = "norm.mean"                 , label = "Animal speed when fleeing (mean of normal distr.)"                         , min = 0, max = 100, value = 60),
                        numericInput ( inputId = "norm.sd"                   , label = "Animal speed when fleeing (S.D. of normal distr.)"                         , min = 0, max = 50, value = 15),
                        numericInput ( inputId = "di"                        , label = "Distance between transects and borders"                    , min = 500  , max = 2000  , value = 1000)
                      ),
                      mainPanel(
                        plotOutput("vidPlot",width = "200px", height= "500px"),
                        h4("Now let's try changing some things:"),
                        h5(HTML("1. Turn up animal flight initiation distance (FID) and animal detection distance. Try 3000 meters for each category. Click 'Run Video' again and press Play. Watch them go unnoticed.")),
                        h5(HTML("2. Turn back down animal FID and detection distance to roughly 50, then turn up observer detection distance (try 500 meters). Click 'Run Video' again and press play. Watch the animals disappear as they are recorded by the observer and removed from the model.")),
                        h5("Note: The next tab will provide a robust estimate of how our measurements affect estimates.")
                      ) )))
             ,
             tags$head(tags$style(type='text/css', ".slider-animate-button { font-size: 30pt !important; }"))
             ,
             
             
             ### TAB 3. SIMULATIONS!!
             tabPanel("Run simulations",
                      h3(strong(HTML("You are ready for this. We believe in you."))),
                      h4( "Now running simulations with your chosen Parameters from Tab 2: Input parameters"),
                      h4( "----------------------------------------------------------"),
                      h5(  HTML("<u><em><span style='font-size: 18px;'>Introducing Two New Inputs</span></em></u>")),
                      h4(( "N Transects: Run multiple transect replicates to obtain a density estimate")),
                      h4(("N Simulations: Repeat the entire process multiple times")),
                      h5(( "Then download your data using the 'Download Data' button below")),
                      h4( "----------------------------------------------------------"),
                      # Sidebar with a slider input for number of bins
                      fluidRow(
                        column(4,
                               actionButton("calculate", "Run simulations",style='background-color: #137ab3; color: #fff; padding:20px; font-size:200%')),
                        column(4,
                               h4("<--- Click 'Run simulations' "),
                               h4("...wait until you see histograms... "),
                               h4("then 'Download data' --->"),
                        ),
                        column(4,
                               downloadButton("downloadData", label = "Download data", style='background-color: yellow; color: black; padding:20px; font-size:200%')
                        ),
                        
                        
                      ),
                      fluidRow(sidebarLayout(sidebarPanel(
                        numericInput ( inputId = "transects.per.sim"         , label = "N transects per simulation"                                , min = 20, max = 100, value = 2),
                        numericInput ( inputId = "n.sim"                     , label = "N simulations for density estimate"                        , min = 20, max = 10000, value = 2),
                      ),
                      mainPanel(
                        h5(" Run a lot of simulations (e.g. 1000) with a decent number of transects (e.g. 100) in each simulation. The disparity between the density estimates obtained represents the estimation of bias in the distance method caused by the human avoidant behaviors you have inputted"),
                        fluidRow(
                          column(width = 6,plotOutput("histPlot")),
                          column(width = 6,textOutput("textRender")),
                        )
                      )
                      )
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
    par(mfrow=c(3,1))
    hist(staT()[,1], main = paste0 ( input$n.sim," density estimates with observer avoidance"),xlab=""   ,xlim = c(0,max(staT(),na.rm=T)+1), sub = paste ( "median density estimate =", round ( median( na.omit(staT()[,1])),4)))
    abline(v =median( na.omit(staT()[,1])),col = "red")
    hist(staT()[,2], main = paste0 ( input$n.sim," density estimates with no observer avoidance"),xlab="",xlim = c(0,max(staT(),na.rm=T)+1),  sub = paste ( "median density estimate =", round ( median( na.omit(staT()[,2])),4)))
    abline(v =median( na.omit(staT()[,2])),col = "red")
  })
  
  output$textRender = renderText({
    med.av   = median( na.omit(staT()[,1]))
    med.n.av = median( na.omit(staT()[,2]))
    div = med.av / med.n.av
    if ( div < 1){
    paste0( "Your avoidance behaviours are causing an underestimate of your population density, with bias equal to " , round( 100-(div)*100 ,3),  "% -- To correct for this bias, multiply your density estimate by ", round(med.n.av/med.av,4))
    } else {
      paste0 ( "Your simulations suggest avoidance behaviours positively impact detectability. This is likely due to sampling error, try running with more replicates")
    }
  })
  
  ######### DOWNLOAD DATA
  
  
  
  # Download data
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
