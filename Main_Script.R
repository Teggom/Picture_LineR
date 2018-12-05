if(!(dir.exists("DiagPictures"))){dir.create("DiagPictures")}
if(!("png" %in% installed.packages())){install.packages("png");library("png")}else{library("png")}

Pictures_In_Dir <- as.list(dir()[grepl("\\.png$", dir())])


sample_Vectors <- list(
  c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9),
  c(0, .5, .1, .6, .2, .7, .3, .8, .4, .9),
  c(-1, .1, -1, .6, -1, .3, -1, .8, -1, .4)
) # to play with 



# Here is the default picture Creator
## VERTICAL
# Specify a length of the horizontal sweeper, default is full length
#   Going over shortens to max length
# You can also specify a specific vector to run over
# You can also specify black or white (T as default)
#  if not bw you can specify random color Vectors 
#   Default = F
# You can also specify a list of pictures to go over, The default is all in dir
Line_Picture <- function(length = NA, Vector = NA, RCV = F, BW = T, Pictures = Pictures_In_Dir){
  if(!(dir.exists("DiagPictures/Simple Vertical"))){
    dir.create("DiagPictures/Simple Vertical")
  }
  if(class(Pictures)=="list"){ # multiple pictures
    for(pic_path in Pictures){
      Pic <- readPNG(pic_path) # read picture
      if(is.na(length) || length > dim(Pic)[2]){ # set length
        v_length <- dim(Pic)[2]
      } else {
        v_length <- length
      }
      
      if(is.na(Vector)[1]){ # check supplied vector
        Diags <- runif(v_length)
      } else {
        if(length(Vector)>dim(Pic)[2]){
          warning(paste("Supplied vector for ", pic_path, " is ", length(Vector), ">Pic Length(", dim(Pic)[2], "): Cropping vector", sep = ""))
          Diags <- Vector[1:dim(Pic)[2]]
        } else {
          Diags <- Vector
        }
      }
      
      if(length(dim(Pic))>2){ # has RGB
        Pic <- Pic[,,1:3] # remove any possible transparency if applicable
        if(BW){
          Pic <- (Pic[,,1]+Pic[,,2]+Pic[,,3])/3 # turns pic bw if bw=T
        }
      }
      
      # Runs BW example
      if(BW){
        if(length(Diags)==dim(Pic)[2]){ # if no slide
          for(row in 1:dim(Pic)[1]){
            Pic[row, ] <- abs(as.numeric(Pic[row,]>Diags))
          }
        } else { # has slide
          while(length(Diags)<dim(Pic)[2]){ # expand diag
            Diags <- c(Diags, Diags)
          } 
          Diags <- Diags[1:dim(Pic)[2]] # crop new long diag
          
          for(row in 1:dim(Pic)[1]){
            Pic[row, ] <- abs(as.numeric(Pic[row,]>Diags))
          }
        }
      } else { # color
        if(!RCV){
          if(length(Diags)==dim(Pic)[2]){ #if no slide
            for(row in 1:dim(Pic)[1]){
              for(Color in 1:3){
                Pic[row,,Color] <- abs(as.numeric(Pic[row,,Color]>Diags))
              }
            }
          } else { # has slide
            while(length(Diags)<dim(Pic)[2]){ # expand diag
              Diags <- c(Diags, Diags)
            }
            Diags <- Diags[1:dim(Pic)[2]] # crop new long diag
              
            for(row in 1:dim(Pic)[1]){
              Pic[row,,Color] <- abs(as.numeric(Pic[row,,Color]>Diags))  
            
            }
          }
        } else { # random color vectors
          for(Color in 1:3){
            Diags <- runif(length(Diags))
            while(length(Diags)<dim(Pic)[2]){
              Diags <- c(Diags, Diags)
            }
            Diags <- Diags[1:dim(Pic)[2]]
            
            for(row in 1:dim(Pic)[1]){
              Pic[row,,Color] <- abs(as.numeric(Pic[row,,Color]>Diags))  
            }
          }
        }
      }
      # Make save name 
      Name <- gsub("/$", "", pic_path)
      Name <- gsub(".*/|\\.png$", "", Name)
      writePNG(image = Pic, target = paste(getwd(), "/DiagPictures/Simple Vertical/", Name,"_Verticalized.png", sep = ""))
    }
  } else { # just 1 picture
    Pic <- readPNG(Pictures) # read picture
    if(is.na(length) || length > dim(Pic)[2]){ # set length
      v_length <- dim(Pic)[2]
    } else {
      v_length <- length
    }
    
    if(is.na(Vector)[1]){ # check supplied vector
      Diags <- runif(v_length)
    } else {
      if(length(Vector)>dim(Pic)[2]){
        warning(paste("Supplied vector for ", pic_path, " is ", length(Vector), ">Pic Length(", dim(Pic)[2], "): Cropping vector", sep = ""))
        Diags <- Vector[1:dim(Pic)[2]]
      } else {
        Diags <- Vector
      }
    }
    
    if(length(dim(Pic))>2){ # has RGB
      Pic <- Pic[,,1:3] # remove any possible transparency if applicable
      if(BW){
        Pic <- (Pic[,,1]+Pic[,,2]+Pic[,,3])/3 # turns pic bw if bw=T
      }
    }
    
    # Runs BW example
    if(BW){
      if(length(Diags)==dim(Pic)[2]){ # if no slide
        for(row in 1:dim(Pic)[1]){
          Pic[row, ] <- abs(as.numeric(Pic[row,]>Diags))
        }
      } else { # has slide
        while(length(Diags)<dim(Pic)[2]){ # expand diag
          Diags <- c(Diags, Diags)
        } 
        Diags <- Diags[1:dim(Pic)[2]] # crop new long diag
        
        for(row in 1:dim(Pic)[1]){
          Pic[row, ] <- abs(as.numeric(Pic[row,]>Diags))
        }
      }
    } else { # color
      if(!RCV){
        if(length(Diags)==dim(Pic)[2]){ #if no slide
          for(row in 1:dim(Pic)[1]){
            for(Color in 1:3){
              Pic[row,,Color] <- abs(as.numeric(Pic[row,,Color]>Diags))
            }
          }
        } else { # has slide
          while(length(Diags)<dim(Pic)[2]){ # expand diag
            Diags <- c(Diags, Diags)
          }
          Diags <- Diags[1:dim(Pic)[2]] # crop new long diag
          
          for(row in 1:dim(Pic)[1]){
            Pic[row,,Color] <- abs(as.numeric(Pic[row,,Color]>Diags))  
            
          }
        }
      } else { # random color vectors
        for(Color in 1:3){
          Diags <- runif(length(Diags))
          while(length(Diag)<dim(Pic)[2]){
            Diags <- c(Diags, Diags)
          }
          Diags <- Diags[1:dim(Pic)[2]]
          
          for(row in 1:dim(Pic)[1]){
            Pic[row,,Color] <- abs(as.numeric(Pic[row,]>Diags))  
          }
        }
      }
    }
    # Make save name 
    Name <- gsub("/$", "", Pictures)
    Name <- gsub(".*/|\\.png$", "", Name)
    writePNG(image = Pic, target = paste(getwd(), "/DiagPictures/Simple Vertical/", Name,"_Verticalized.png", sep = "")) 
  }
}












