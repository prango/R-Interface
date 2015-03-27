
shinyUI(fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
  ),
  fluidRow(
    column(9,
           img(src="http://www.splitstrategy.com/wp-content/uploads/2013/02/twitter.jpg", height = 150, width = 175)),
    
    column(3,
           img(src="http://utahavalanchecenter.org/sites/default/files/images/staff/Paul/twitterimg.jpg", height = 150, width = 320))
    
  ),
  
  fluidRow(
    column(12,
           fluidRow(    
             column(3,textInput("text", label = h1("Text input"), 
                                value = "kejriwal")),
             column(3,textInput("text2", label = h1("category"), 
                                value = "politician"))
             ,
             column(3,selectInput("graph", label = h1("Graph Type"), 
                                  choices = list("score graph" = 1, "emotion graph" = 2, "pos-neg graph" = 3), selected = 1))),
             fluidRow(
             column(3,textInput("tweets", label = h1("No. Of Tweets"), 
                                value = "50")),
             column(3, 
                    dateInput("date", 
                              label = h1("Date input"), 
                              value = "1980-01-01")) 
           )
           
           
    ),
    column(12,
           
           #textOutput("text3"),
           fluidRow(
             column(6,
                    plotOutput("plot1")
             ),
             column(6,
                    tableOutput("tb"),
                    tags$head(tags$style("#tb table {background-color:white; }", media="screen", type="text/css"))
             )
             
             
             
           ))
    
    
    
  )))