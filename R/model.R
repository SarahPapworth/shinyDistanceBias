
# final model function

# define model parameters
model = function (
  
  grid.size.y = 5000,
  monk_density_grp_km_2 = NULL, #set number of monkeys
  step.length = NULL, #how many steps from start to finish (number of steps)
  plot_monk = F,  #plot at the end: yes or no
  video.name = NULL, #name of ouput file
  scale.param = 30 ,#human scale param
  shape.param = 3,#affects the shape of the detection function
  fid.param = NULL ,#distance at which monkeys run away (from my data). Can use vector of numbers or single value!
  monk.scale.param = NULL, #affects how quickly probability of detection falls
  monk.shape.param = 3, #affects the shape of the detection function
  kappa. = 50, #how narrow is the von mises distribution (higher kappa, narrower the distribution)
  speed.dist.rate.decay = 0.10, #monkey speed: param which gave monkeys a max speed of 100 m/timestep
  norm.mean = 60, # monkey speed when detected
  norm.sd = 15, # sd of speed above
  di = 300
  
)
{
  
  ## Variables (Running from a RunModel.r file)
  
  
  # monk_density_grp_km_2 =  0.01#monk_density_grp_km_2  # change only these variables
  # grid.size.y = 5000
  # scale.param = 28
  # shape.param = 3
  # speed.dist.rate.decay = 0.15
  # fid.param = fid.param
  # step.length = step.length
  # monk.scale.param = monk.scale.param
  # monk.shape.param = monk.shape.param
  # norm.mean = 60
  # kappa. = 50
  # norm.sd = 15
  # di = 300
  # plot_monk=F
  
  
  ## Variables (Running from an app.r file)
  
  
  # grid.size.y= input$grid.size.y
  # monk_density_grp_km_2 = input$monk_density_grp_km_2
  # step.length = input$step.length  #how many steps from start to finish (number of steps)
  # scale.param = 3000#input$scale.param #human scale param
  # shape.param  = input$shape.param #affects the shape of the detection function
  # fid.param   = fid.param #distance at which monkeys run away (from my data). Can use vector of numbers or single value!
  # monk.scale.param = input$monk.scale.param #affects how quickly probability of detection falls
  # monk.shape.param = input$monk.shape.param  #affects the shape of the detection function
  # kappa. = input$kappa.#how narrow is the von mises distribution (higher kappa, narrower the distribution)
  # speed.dist.rate.decay = input$speed.dist.rate.decay  #monkey speed: param which gave monkeys a max speed of 100 m/timestep
  # norm.mean   = input$norm.mean  # monkey speed when detected
  # norm.sd = input$norm.sd  # sd of speed above
  # di         = input$di
  # plot_monk =  T
  
  ## Variables (Running from scratch)
  
  # rm(list= ls())
  # grid.size.y= 5000
  # monk_density_grp_km_2 = 6
  # step.length = 40 #how many steps from start to finish (number of steps)
  # scale.param = 100 #human scale param
  # shape.param  = 3 #affects the shape of the detection function
  # fid.param   = 6000 #distance at which monkeys run away (from my data). Can use vector of numbers or single value!
  # monk.scale.param = 6000 #affects how quickly probability of detection falls
  # monk.shape.param = 3  #affects the shape of the detection function
  # kappa. = 50 #how narrow is the von mises distribution (higher kappa, narrower the distribution)
  # speed.dist.rate.decay =0.15 #monkey speed: param which gave monkeys a max speed of 100 m/timestep
  # norm.mean   =600  # monkey speed when detected
  # norm.sd = 15  # sd of speed above
  # di         = 300
  # plot_monk =  T
  # source ( file.path (getwd(), "source", "quickLibrary.r"))
  
  
  
  #######
  
  # SCRIPT
  
  #######
  {
    # Density = num monkey groups * area in km
    nmg1 = monk_density_grp_km_2 * ((di*2/1000) * ( (grid.size.y+di*2) /1000))
    nmg2 = floor(nmg1)
    nmg3 = nmg2 + rbinom (1,1, nmg1 - nmg2)
    num.monkey.grps = nmg3
    
    # Cami's trajectory
    camwalk = cbind( di,seq(di,di+grid.size.y,step.length))
    # numsteps = nrow(camwalk)
    
    
    #Starting position of monkeys; x and y = sample from a uniform distribution (runif) from 0 (min) to grid.size (max)
    monkeys.pos = cbind( x = runif(num.monkey.grps, 0,di*2),
                         y = runif(num.monkey.grps, 0,(grid.size.y+di*2)))
    
    
    # Plot stuff
    if ( plot_monk){
      monkeys.poss= array ( Inf, c( dim(monkeys.pos),nrow(camwalk)))
      monkeys.poss[,,1] = monkeys.pos
    }
    
    # Empty objects
    perpendicular = vector()
    
  }
  
  
  ######## LOOP
  
  for ( i in 1: nrow ( camwalk ) ){
    # for ( i in 1:6 ){
    #i=3
    
    # cami's position
    cam.pos = camwalk[i,]
    
    # distance to each monkey
    mcd = mon.cam.dist( cam.pos,monkeys.pos)
    #mcd[mcd>max.detect.dist ] = NA
    
    # does monkey detect cam
    monkdetects = monk.detect.funct(mcd, monk.scale.param = monk.scale.param,monk.shape.param = monk.shape.param)
    
    # is monkey detected by cam
    if ( i == 1){ # not on first step
      detected = rep(0,nrow(monkeys.pos))
    } else {
      detected =  detect.funct(mcd,scale.param = scale.param,shape.param = shape.param)
    }
    
    #  perpendicular distance to transect. the ones in matrix detected that are = 1,
    #  calculate perpendicular distance and store in perpendicular matrix
    whi = which( detected == 1)
    if ( length (whi) > 0   ){ # not on first step of new transect. Monkeys haven't had a chance to respond
      perp = perp.funct( camwalk[i,] , monkeys.pos[whi,] )
      perpendicular = c( perpendicular , perp)
      monkeys.pos[whi,]=Inf # Kill monkeys! ( Only joking, remove them once spotted)
    }
    
    # move the monkeys!
    monkeys.pos.list = get_monkeys_pos( monkeys.pos = monkeys.pos,
                                        cam.pos = cam.pos,
                                        speed.dist.rate.decay = speed.dist.rate.decay,
                                        grid.size.y = grid.size.y,
                                        kappa = kappa.,
                                        monkdetects = monkdetects,
                                        mcd = mcd,
                                        fid.param = fid.param,
                                        norm.mean=norm.mean,
                                        norm.sd = norm.sd)
    monkeys.pos = monkeys.pos.list[[1]]
    
    # Plot stuff
    if ( plot_monk){
      monkeys.poss[,,i] = monkeys.pos
    }
    
    # print ( i )
    
  }
  
  # PLOTcheck
  #hist(perpendicular)
  
  # If no observations
  if ( length(perpendicular)==0){
    perpendicular = NA
  }
  
  
  # return
  if ( plot_monk){
    plot.return = list ( camwalk = camwalk , monkeys = monkeys.poss)
    return ( list (
      plot.return = plot.return,
      dat = perpendicular
    ))
  } else {
    return(perpendicular)
  }
}
