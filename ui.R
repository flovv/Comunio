library(shiny)

# Define UI for slider demo application
shinyUI(pageWithSidebar(


  #  Application title
  headerPanel("Comunio Stats!"),
  
  # Sidebar with sliders that demonstrate various available options
  sidebarPanel(
    # Simple integer interval
   # sliderInput("integer", "Integer:", 
    #            min=0, max=1000, value=500),
    
    # Decimal interval with step value
    #sliderInput("decimal", "Decimal:", 
    #            min = 0, max = 1, value = 0.5, step= 0.1),
   
    # Specification of range within an interval
    #sliderInput("range", "Range:",
    #            min = 1, max = 1000, value = c(200,500)),
    
    # Provide a custom currency format for value display, with basic animation
    #sliderInput("format", "Custom Format:", 
    #            min = 0, max = 10000, value = 0, step = 2500,
    #            format="$#,##0", locale="us", animate=TRUE),
    
    # Animation with custom interval (in ms) to control speed, plus looping
    #sliderInput("animation", "Looping Animation:", 1, 2000, 1, step = 10, 
    #            animate=animationOptions(interval=300, loop=TRUE))
    
	 selectInput(inputId = "teams",
                label = "nach Team",
                choices =  c("Alle"        ,               "Hannover 96"     ,       "1899 Hoffenheim"    ,    "Eintracht Frankfurt"    ,"SV Werder Bremen"      ,"VfB Stuttgart"    ,      "Eintracht Braunschweig", "FC Bayern München"    , "SC Freiburg" ,           "Hertha BSC"   ,         "Borussia Dortmund"   ,   "FC Augsburg"        ,    "1. FC Nürnberg"      ,  "FC Schalke 04"  ,        "Hamburger SV"    ,       "Borussia M'gladbach"   , "VfL Wolfsburg"       ,   "1. FSV Mainz 05"       , "Bayer 04 Leverkusen" ),
	selected = "Alle" ),
    
    selectInput(inputId = "n_breaks",
                label = "nach Position",
                choices = c("Alle", "Abwehr", "Mittelfeld"  ,    "Sturm",    "Torwart" ),
                selected = "Alle" )
    
  ),
  
  # Show a table summarizing the values entered
  mainPanel(
      tabsetPanel(
	
      tabPanel("Basic Stats",
    
              
               h3("Punkte."),
               plotOutput("plot"),
               
               h3("Marktwert."),
               plotOutput("plot2")
      #  tableOutput("MW")
    
       # tableOutput("Punkte"),
               
      #  tableOutput("Einsaetze")
               
      ),
      
      tabPanel("Ratios",
               
               
               h3("Punkte pro Spiel."),
               plotOutput("ratio"),
               
               h3("Marktwert pro Spiel"),
               plotOutput("ratio2")
               #  tableOutput("MW")
               
               # tableOutput("Punkte"),
               
               #  tableOutput("Einsaetze")
               
      ),
      
      
      tabPanel("Regressionen",
         h4("Regression auf den Marktwert."),
         tableOutput("regression"),
        
          h4("Regression auf die Punkte."),
          tableOutput("regression2")
               
      ),           
     
      tabPanel("BayesNetz",
    

    
      h4("Mr. Bayes"),
       plotOutput("bayes")
        #tableOutput("regression")
    
      ),
      
      
      tabPanel("kmeans",
               
                
               selectInput(inputId = "n_cluster",
                      label = "Anzahl Cluster",
                       choices = c(2,3,4,5,6,7),
                      selected = 3 ),
               
           h4("K-Means-clustering"),
           plotOutput("kMeans")
               
      ),
	      
	
	 tabPanel("Teams",
         h4("Team Statistik"),
         tableOutput("teams")
        
         
               
      ),
	  
	  
	  tabPanel("Optimization",
	  
	    tags$head(
		  tags$style(type="text/css", ".jslider { max-width: 600px; }")
		),
		  
	  	HTML("<h3>Optimiert die Gesamtpunkte eines Teams unter Nebenbedingungen:</h3> 
		 (i) Gesamtbudget <br>
		 (ii) maximal 13 Spieler <br>
		 (iii) maximal ein Torwart, 5 Abwehr-, 5 Mittelfeldspieler und 3 Stürmer<br><br>
		 "),
	  
	     sliderInput("decimal", "Budget:",  min = 3000000, max = 30000000, value = 20000000, step= 100000 ),
	  

	  
         h4("Optimales Ergebnis"),
         tableOutput("lp1"),
		 h4("Ausgewählte Spieler"),
		 tableOutput("lp2")
        
         
               
      ),
      tabPanel("Comunio-Prognose",
               
               
               
          h4("Prognostiziert der Comunio Marktwert den Ausgang der Bundesliga?"),
               
       # plotOutput("information"),
            includeHTML('www/plot.html'),      
               
               HTML("<h3>So ist der Chart zu lesen:</h3> 
		 (i) als erstes erstellen wir aus dem Marktwerte der Mannschaften eine Rangliste (Sommer 2012) <br>
		 (ii)  die entstandene Rangliste vergleichen wir mit der Tabelle am Ende der Saison (Sommer 2013) <br>
		 (iii) der Fehler entsteht aus der Summe der Ranglisten-Differenzen / 18 <br>
     (iv) dabei ist wichtig wieviele Spieler aus jeder Mannschaft gewählt (Teuerste N Spieler) werden <br>
     (v) wie zu erwarten nimmt der Fehler zum Start der Saison ab<br> <br>
     <h3>Prognosegenauigkeit:</h3> 
      Ab Oktober ist der Tabellenschlussstand pro Team mit einer Ungenauigkeit von +/- 1.5 Tabellenplätze ablesbar.<br><br>
		 "), 
               
       h2("Aktuelle Comunio Prognose "),      
      tableOutput("prognose")
               
      )
      
      
    )
  )
))