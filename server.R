library(shiny)
library(bnlearn)
library(plyr)
library(lpSolve)
#library(ggplot2)

#path <- "C:/Users/fteschner/Desktop/"
path <- ""
out<- read.csv(paste(path, "com.csv", sep=""), sep="|") 

club <- read.csv(paste(path, "club.csv", sep=""), sep=";")
comRang <- read.csv(paste(path, "comRang2.csv", sep=""), sep=";")
pred<- read.csv(paste(path, "com.csv", sep=""), sep="|") 

out$Tore <- as.numeric(out$Tore)
out$Punkte <- as.numeric(out$Punkte)
out$MW <- as.numeric(out$MW)
out$spiele <- as.numeric(out$spiele)



out$pps <- out$Punkte/out$spiele
out$pps <- ifelse(out$pps >1000, 0, out$pps)

out$mwps <- out$MW/out$spiele
out$mwps <- ifelse(out$spiele ==0, 0, out$mwps)



########## generate prediction for 2014:

pred$Club <- as.character(pred$Club)
pred$MW <- as.numeric(pred$MW)

foo3 <- aggregate(pred$MW, by=list(pred$Club), FUN=sum)
foo3 <- foo3[order(-foo3$x),]
foo3 <- foo3[foo3$x > 17380000,]

reduceSet2 <-function(df, club, n){
  
  df <- df[df$Club == club, ]
  
  df <- df[order(-df$MW),]
  
  return(df[1:n,] )
  
}



clubRanking <- reduceSet2(pred, "Borussia Dortmund", 5)

for(i in 2:nrow(foo3)){
  
  
  clubRanking <- rbind(clubRanking, reduceSet2(pred, foo3[i,]$Group.1, 5))
  
}

finalPrediction <- aggregate(clubRanking$MW, by=list(clubRanking$Club), FUN=sum)


finalPrediction <- finalPrediction[order(-finalPrediction$x),]
finalPrediction$Rang <- seq(1,18)

colnames(finalPrediction) <- c("Team" , "MarktWert Top5 Spieler" , "Prognostizierter Rang")




############# get data!

colnames(club) <- c("spieler", "club" , "MW", "datestr")
club$date <- as.Date(club$datestr)
club$MW2012 <- as.numeric(club$MW)


subClub <- club[club$datestr == '2012-07-15', ]
subClub$count <- 1
foo <- aggregate(subClub$count, by=list(subClub$club), FUN=sum)
foo <- foo[order(-foo$x),]



colnames(comRang) <- c("club", "rang2012")


############ comunio to predict bundesliga ranking!

reduceSet <-function(df, club, n){
  
  df <- df[df$club == club, ]
  
  df <- df[order(-df$MW2012),]
  
  return(df[1:n,] )
  
}



calculate <- function(subClub, n){
  
  clubRanking <- reduceSet(subClub, "SV Werder Bremen", n)
  
  for(i in 2:nrow(foo)){
    
    clubRanking <- rbind(clubRanking, reduceSet(subClub,foo[i,]$Group.1, n))
    
  }
  
  foo2 <- aggregate(clubRanking$MW2012, by=list(clubRanking$club), FUN=mean)
  foo2 <- foo2[order(-foo2$x),]
  foo2$rang <- seq(1,18)
  
  colnames(foo2) <- c("club", "MW", "rangPred")
  
  tmp <- merge(comRang, foo2, by=c("club"))
  
  tmp$delta <- abs(tmp$rang2012 - tmp$rangPred)
  
  return (sum(tmp$delta) /18)
}


trigger <- function(){
      result <- as.data.frame(seq(1:22))
      
      result$error <- 0
      result$crap <- 0
      colnames(result) <-c ("NSpieler", "Date",  "Fehler")
      
      dates <- club[10:100,]$datestr
      
      
      for(j in 1:length(dates)){
        
        
        
        subClub <- club[club$datestr == dates[j], ]  
        
        
        for(i in 1:22){
          
          #print(i)
          result[(j-1)*22+i,]$Date <-as.character(dates[j])
          result[(j-1)*22+i,]$NSpieler <- i
          result[(j-1)*22+i,]$Fehler <- (calculate(subClub, i))
          
          
        }
      }

}


###########


# Define server logic for slider examples
shinyServer(function(input, output) {
  
  # Reactive expression to compose a data frame containing all of the values
  sliderValues <- reactive({
    
    # Compose data frame
    
  #  data.frame(
  #    Name = c("Integer", 
  #             "Decimal",
  #             "Range",
  #             "Custom Format",
  #             "Animation"),
  #    Value = as.character(c(input$integer, 
  #                           input$decimal,
  #                           paste(input$range, collapse=' '),
  #                           input$format,
  #                           input$animation)), 
  #    stringsAsFactors=FALSE)
  }) 
  
  # Show the values using an HTML table
 # output$values <- renderTable({
 #  sliderValues()
 # })
  # Show the first "n" observations
  
  
  
selection <- function(dataIn, input1, input2){
   if(input1 == "Alle"){
	  
		 if(input2 == "Alle"){		  
		  return (dataIn)
		}
		else{
		   return(dataIn[dataIn$Club == input2,] )
		  
		}
    }
    else{
	
		if(input$teams == "Alle"){
		 
		   return(dataIn[dataIn$Position == input1,])
		}
		else{
		 # summary(out[which(out$Club == input$teams),]$MW)
		  return(dataIn[dataIn$Position == input1 & dataIn$Club == input2,])
		}	

    }

  }

  
  output$Einsaetze <- renderTable({
    #summary(out)
    
      summary(out$spiele)
   
  })
             
  output$teams <- renderTable({

	 ddply(out, ~Club, summarise, MittelWert_Punkte=mean(Punkte), MittelWert_MarktWert=mean(MW))
	})  
  
  output$regression <- renderTable({
    #summary(out)
 
	 if(input$n_breaks == "Alle"){
	
      if(input$teams == "Alle"){
		summary(lm(MW~Tore+Punkte+spiele+factor(Position) + factor(Club), data=out))
	  }
	  else{
	     temp <- selection(out,input$n_breaks, input$teams)
	     summary(lm(MW~Tore+Punkte+spiele +factor(Position), data=temp))
	  }
        
    }
    else{
	  temp <- selection(out,input$n_breaks, input$teams)
      
	  if(input$teams == "Alle"){
		summary(lm(MW~Tore+Punkte+spiele+ factor(Club), data=temp))
	  }
	  else{
	     summary(lm(MW~Tore+Punkte+spiele, data=temp))
	  }
	  
    }
	
  })
  
  output$regression2 <- renderTable({
    #summary(out)
    if(input$n_breaks == "Alle"){
	
      if(input$teams == "Alle"){
		summary(lm(Punkte~Tore+MW+spiele+factor(Position) + factor(Club), data=out))
	  }
	  else{
	     temp <- selection(out,input$n_breaks, input$teams)
	     summary(lm(Punkte~Tore+MW+spiele +factor(Position), data=temp))
	  }
        
    }
    else{
	  temp <- selection(out,input$n_breaks, input$teams)
      
	  if(input$teams == "Alle"){
		summary(lm(Punkte~Tore+MW+spiele+ factor(Club), data=temp))
	  }
	  else{
	     summary(lm(Punkte~Tore+MW+spiele, data=temp))
	  }
	  
    }
  })
  
  output$plot <- renderPlot({
    
	temp <- selection(out,input$n_breaks, input$teams)
	hist(temp$Punkte, main = "Punkte",xlab="Punkte")
    #summary(out)
   # if(input$n_breaks == "Alle"){
      
    #  hist(out$Punkte, main = "Punkte",xlab="Punkte")
    #}
    #else{
    #  hist(out[which(out$Position == input$n_breaks),]$Punkte, main = "Punkte",xlab="Punkte")
   # }
    
    
  })
  
  output$plot2 <- renderPlot({
    temp <- selection(out,input$n_breaks, input$teams)
	hist(temp$MW, main="Marktwert",xlab="Marktwert")
    
  })
  
  
  output$ratio <- renderPlot({    
   
      temp <- selection(out,input$n_breaks, input$teams)
      hist(temp$pps, main="Punkte pro Spiel", xlab="Punkte pro Spiel")
    
    
    
  })
  
  output$ratio2 <- renderPlot({
     temp <- selection(out,input$n_breaks, input$teams)
      hist(temp$mwps, main ="Marktwert pro Spiel", xlab='Marktwert pro Spiel')
    
  })
  
  
  
  output$bayes <- renderPlot({
      temp <- selection(out,input$n_breaks, input$teams)   
      reduced <- temp[c("Punkte", "Tore", "MW", "spiele")]    
      g<-gs(reduced)    
      plot(g) 
    
      
  })
  
  
  output$kMeans <- renderPlot({
    
    temp <- selection(out,input$n_breaks, input$teams)   
    
    
      
     # cl <- kmeans(out[c("Punkte", "Tore", "MW", "spiele")] , as.numeric(input$n_cluster), nstart = 25)
    #  plot(out[c("Punkte", "Tore", "MW", "spiele")], col = cl$cluster)
      
      #plot(gs(out[c("Punkte", "Tore", "MW", "spiele")] ))
      
  
      reduced <- temp[c("Punkte", "Tore", "MW", "spiele")]    
 
      cl <- kmeans(reduced , as.numeric(input$n_cluster), nstart = 25)
      plot(reduced, col = cl$cluster)
    
    
  }) 
  
  
  output$lp1 <- renderTable({
		
		player <- rep(1, nrow(out))

		f.obj <- out$Punkte  ###objective max punkte
		f.con <- t(out$MW)  ### constraints max MW <= Bud
		f.con <- rbind(f.con, player)


		A <- as.data.frame(model.matrix(MW ~ Position, out) )
		A$sum <- A$PositionMittelfeld + A$PositionSturm + A$PositionTorwart
		A$PositionAbwehr <- ifelse(A$sum ==0, 1,0)
		B <- A[ ,c("PositionTorwart", "PositionAbwehr","PositionMittelfeld","PositionSturm") ]
		## add constraints
		f.con <- rbind(f.con, t(as.matrix(B)))

		f.dir <- c("<=", "<=", "=", "<=", "<=" ,"<=")
		f.rhs <- c(input$decimal, 13, 1, 5, 5, 3)  ## right hand side .. not more than. no more than 13.


		solved<- lp("max", f.obj, f.con, f.dir, f.rhs, all.bin=TRUE)  ## just binary variables!

		###################output!
		out$buy <<- solved$solution

		output <- as.data.frame(matrix(nrow=1, ncol=0))
		## sanity check
		output$Marktwert <- sum(out[out$buy == 1,]$MW)


		## how many players

		output$AnzahlSpieler <-sum(out[out$buy == 1,]$buy)
		## only seven .. not good!

		## how many 
		output$Punkte <-sum(out[out$buy == 1,]$Punkte)

    output
	})
	
    output$lp2 <- renderTable({
	
	        crap <- input$decimal
			
			output2 <- as.data.frame(matrix(ncol=0, nrow=length(out[out$buy == 1,]$Name)))
		   
			output2$Spieler <- out[out$buy == 1,]$Name
			output2$Punkte <- out[out$buy == 1,]$Punkte
			output2$Marktwert <- out[out$buy == 1,]$MW

			#output <- cbind(output, output2)
			
		  output2
	})
  
  
  output$information <- renderPlot({
    
    
    
    #just hard-coded!
    
    
   # trigger()
    
   # plotData <- result
    
   # plotData$Date <- as.Date(plotData$Date)
    
   # plotDataFiltered <- plotData[plotData$NSpieler %in% c(3,4,5,6,10,15,20,22),]
    
  #  p<- ggplot(data=plotDataFiltered, aes(x=Date,y=Fehler))
  #  p <- p + geom_line(aes(color=as.factor(NSpieler)))
    #p <- p + scale_color_grey()
  #  print(p)
    
    
    
    
   # p<- ggplot(data=plotData, aes(x=as.factor(NSpieler),y=Fehler))
   # p <- p + geom_boxplot()
    #p <- facet_wrap(~Date)
   # print(p)
    
    
  })
  
  output$prognose <- renderTable({
    finalPrediction
    
  })
                                 
  

  
})
