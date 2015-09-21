library(shiny)
##library(shinyIncubator)
library(shinyjs)
# ui.R

#require(rCharts)
#options(RCHART_LIB = 'polycharts')

shinyUI(fluidPage(      tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "https://googledrive.com/host/0B_fdodB_XjlmMlRibVNtS0xuak0/")
),
	#https://drive.google.com/open?id=0B_fdodB_XjlmMlRibVNtS0xuak0
#https://googledrive.com/host/0B_fdodB_XjlmdUNReTBtY1Rab1k/
#https://drive.google.com/file/d/0B_fdodB_XjlmdUNReTBtY1Rab1k/view?usp=sharing

  headerPanel(
  HTML('<div style="background-color: #404242;

.shiny-progress .bar {
background-color: BLUE;
.opacity = 0.8;
}
.shiny-progress .progress {
height:7px;
}  ">
<H3> 
<div> <img src="http://cdn3.yourstory.com/wp-content/uploads/2014/01/LatentView_Analytics_Logo.jpg" style="width:100px;height:60px;display:inline-block;white-space:nowrap;border:1px solid #404242" class="ImageBorder"	align="left;top;" float="left">
<div style="width:400px;height:60px;border:0px solid #404242;text-align:center; background-color: #404242;display:inline; white-space:nowrap;">
<font color="white">
&nbsp&nbspFORECASTING TOOL 1.12 (BETA)&nbsp&nbsp
</font>
</div>
</div>
</H3>
</div>')
, windowTitle = 'AFT - LATENTVIEW'
  
  ),

hr(),

	#  headerPanel(  HTML('<H4>AUTOMATED FORECASTING TOOL<H4>')),    
	  
	  sidebarLayout(
		sidebarPanel(
		# tags$head(
        # tags$style(type="text/css", "label.radio { display: inline-block; }", ".radio input[type=\"radio\"] { float: none; }"),
        # tags$style(type="text/css", "select { max-width: 200px; }"),
        # tags$style(type="text/css", "textarea { max-width: 185px; }"),
        # tags$style(type="text/css", ".jslider { max-width: 200px; }"),
        # tags$style(type='text/css', ".well { padding: 2px; margin-bottom: 5px; max-width: 280px; }"),
        # tags$style(type='text/css', ".span4 { max-width: 280px; }")),


    # fluidRow(
# column(8,
 # fileInput('file1', 'Choose CSV File',
           # accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')))

    # # textInput("fpath", label = h4("Enter file path"), 
     # #   value = "Enter path..."))
    # ),
	
	    fluidRow(
column(8,	
	shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),

    actionButton("refresh",label=HTML('<H6  style="font-family:arial; color:white">&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbspPRESS TO REFRESH THE APP&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp</H6>')))
	   ),
	

fluidRow(
    column(8,
      sliderInput("slider1", label =  HTML('<H6  style="font-family:arial"><B>FORECAST HORIZON</B></H6>'), min = 0, 
        max = 30, value = 6)
    )),
	
	
	
	
	
	  fluidRow(	
	   column(8,
		  selectInput("Freq", label =  HTML('<H6  style="font-family:arial"><B>FREQUENCY</B></H6>'), 
			choices = list("Daily" = 7, "Weekly" = 52, "Monthly" = 12,"Quaterly"=4), selected = 12))
							),
  # fluidRow(	
   # column(8,
      # selectInput("controlreq1", label = HTML('<H6  style="font-family:arial"><B>CONTROL SAMPLE</B></H6>'), 
        # choices = list("Yes" = 1, "No" = 0), selected = 1))
					    # ),						
						
	fluidRow(	
   column(8,
      selectInput("Hosize", label = HTML('<H6  style="font-family:arial"><B>TEST SIZE (% of Total)</B></H6>'), 
        choices = list("5%" = 0.05, "10%" = 0.10, "20%" = 0.20,  "last 3 dpts" = 3, "last 6 dpts" = 6), selected = 0.10))
					    ),		

#c93d2e
#<a href="http://tinypic.com?ref=uv4fr" target="_blank"><img src="http://i59.tinypic.com/uv4fr.png" border="0" alt="Image and video hosting by TinyPic"></a>
fluidRow(	
   column(8,
     # selectInput("ModelSelection", label =HTML('<H6  style="font-family:arial; color:darkred"><I>MODEL SELECTION</I></H6>'), 
     #   choices = list("-------ALL-------"), selected = 0)
	 # <input id="outliers" type="checkbox"/>
		checkboxInput("SplineSmoothing", label =HTML('<H6  style="font-family:arial; color:#404242"><B>SPLINE SMOOTHING &nbsp </B></H6>'), value = FALSE)
		)
						),
						

						#fluidRow(	
   # column(8,
      # selectInput("Mvariate", label =HTML('<H6  style="font-family:arial; color:darkred"><I>MULTIVARIATE</I></H6>'), 
        # choices = list("Currently Not Available"), selected = 0))
						# ),
	
		    fluidRow(
column(8,
 fileInput('file1', HTML('<H6  style="font-family:arial; color: #404242"><B>CHOOSE THE CSV FILE</B></H6>') ,
           accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')))
     ),					
						
						
	
			fluidRow(    
    column(8,
       actionButton("Pressed","SUBMIT"))),
	  
	  

	  
	  	# fluidRow(
    # column(8,
	        # helpText("Press download once the forecast completes."))
  # ),
 column(8,
 br(),
  downloadButton('downloadData', 'Download'))
  

 ),
	   position = "left",
	  	
  mainPanel( 
# progressInit(),
#<a href="http://tinypic.com?ref=2m4vw2u" target="_blank"><img src="http://i57.tinypic.com/2m4vw2u.png" border="0" alt="Image and video hosting by TinyPic"></a>
 tabsetPanel(
         tabPanel("Readme",
		#<a href="http://tinypic.com?ref=2a8i4jr" target="_blank"><img src="http://i58.tinypic.com/2a8i4jr.png" border="0" alt="Image and video hosting by TinyPic"></a>
		fluidRow(
#<a href="http://tinypic.com?ref=2a8i4jr" target="_blank"><img src="http://i58.tinypic.com/2a8i4jr.png" border="0" alt="Image and video hosting by TinyPic"></a>
HTML('<center><a target="_blank"><img src="https://googledrive.com/host/0B_fdodB_XjlmdUNReTBtY1Rab1k/" border="0" alt="SDT"></a></center>')
		#style="width:900px;height:500px;display:inline-block;white-space:nowrap;border:0px solid #0080FF" class="ImageBorder">
		)) ,
		
		 tabPanel("Xcel Prep",
		#<a href="http://tinypic.com?ref=2a8i4jr" target="_blank"><img src="http://i58.tinypic.com/2a8i4jr.png" border="0" alt="Image and video hosting by TinyPic"></a>
		fluidRow(
		#https://drive.google.com/open?id=0B_fdodB_XjlmUFFlb3dsWS1QUDg
#<a href="http://tinypic.com?ref=2a8i4jr" target="_blank"><img src="http://i58.tinypic.com/2a8i4jr.png" border="0" alt="Image and video hosting by TinyPic"></a>
HTML('<center><a target="_blank" ><img src="https://googledrive.com/host/0B_fdodB_XjlmUFFlb3dsWS1QUDg/" border="0" alt="SDT"></a></center>')
		#style="width:900px;height:500px;display:inline-block;white-space:nowrap;border:0px solid #0080FF" class="ImageBorder">
		)) 
		
		, 
	tabPanel("Output",
	 fluidRow(
     uiOutput("inSelect")
	   )
		#,showOutput("chart2", "polycharts")
		
		,plotOutput("plot",height="400px")
		)
		
 	  )



 )
 #,
 # absolutePanel(
    # bottom = 20, right = 20, width = 300,
    # bottom = 20, right = 20, width = 300,
    # draggable = TRUE)
  ),
   hr(),
HTML('<H14  style="color:#404242; display:inline; white-space:nowrap;"><I>For suggestions,bugs or enquiring about the code, mail me at : somendra.tripathi@latentview.com
</I></H14>')
))