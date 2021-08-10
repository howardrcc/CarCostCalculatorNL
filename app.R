#shiny app voor berekeningen bijtelling auto (2021)



# https://www.rijksoverheid.nl/onderwerpen/belastingen-op-auto-en-motor/bijtelling-2018---2020

# Vergelijking young-timer vs Elektrische Auto zakelijk

#Volledig elektrische auto's                      	Nieuwwaarde < €40.000 	Vanaf €40.000
#DET > 1 januari 2021 	                                            12% 	        22%
#DET > 1 januari 2022 	                                            16% 	        22%

#slide voor kostprijs Auto

# Model 3 48,980 (448km), Long Range: 56.980 (614km, 330-660km, 73.5-79kWh)

# Ioniq 5: 43.500 (58 kWh,384KM), Long Range (73 (72.6-77.4kWh) , 46.500 (481KM, 275KM-565KM))

# https://ev-database.nl/auto/1280/Skoda-Enyaq-iV-80
# Kia EV6

#Thuis laden: +/- €0.24 incl btw
#benzine prijs (€1.829)


#young timer
#V70 €15.000 (tussen 10 en 15k), 35% bijtelling
# vaste afschrijving
# verzekering
# mrb
# Onderhoud
# verbruik (1/10)



#of hybride (nieuw op de zaak)


#voorbeeld1
#15.000km per jaar/5 jaar (250 per maand afschrijving/125 per maand netto)
# 1.83 * 1500 = 2745 euro bruto per jaar =1850 netto
# bijtelling 35% * 10000 = 3500 = 1750 netto
# 3600 netto per jaar

# Ioniq 5: 43.500 (58 kWh,384KM), Long Range (73 (72.6-77.4kWh) , 46.500 (481KM, 275KM-565KM))
# Brandstof
# 
# Bijtelling 16% x 40.000 = 6400/12 = 533
# 22%  6.500 = 1430/12 = 119
# = 652 bruto = 350 netto = 4200 per jaar netto


# 20kWh/100km
# 3000 kwH per jaar = 720euro bruto = 360 netto
 
#bijtelling + brandstof = 4200+360 = 4560 per jaar

#lease bedrag/ 675/775 per maand elektrisch = 300-400 per maand netto 3600-4800 per jaar




library(shiny)
library(dwhr)
library(data.table)
library(highcharter)




# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Kosten calculator auto: elektrisch-youngtimer/zakelijk-prive 2022"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            conditionalPanel(
                condition = "output.browser",
                actionButton(inputId = 'browser','browser/debug()')),
            sliderInput("kmpj",
                        "Kilometer per jaar:",
                        min = 10000,
                        max = 50000,
                        value = 15000),
            sliderInput("prijs",
                        "Prijs auto:",
                        min = 10000,
                        max = 100000,
                        value = 46500),
            sliderInput("ink",
                        "inkomen per jaar:",
                        min = 20000,
                        max = 100000,
                        value = 70000),
            sliderInput("prijskwh",
                        "kWh prijs",
                        min = 0,
                        max = 1,
                        value = 0.24),
            sliderInput("prijsbenz",
                        "Benzine Prijs",
                        min = 1.50,
                        max = 2.50,
                        value = 1.83),
            sliderInput("verbruikElec",
                        "Verbruik Elektrisch in kWh/100Km",
                        min = 5,
                        max = 35,
                        value = 20),
            sliderInput("verbruikBenz",
                        "Verbruik Benzine in L/100Km",
                        min = 1,
                        max = 15,
                        value = 10),
            
            
            
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            htmlOutput("distPlot")
            ,HTML('<br><br><br>')
            ,htmlOutput("bijtelling")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    output$distPlot <- renderText({
        # generate bins based on input$bins from ui.R
        x    <- input$kmpj
        #bins <- seq(input$ink, input$kmpj, length.out = input$bins + 1)
        
        kwh <- (x/100)*20
        kosten <- kwh*.24
        
        text <- paste('<b>Elektrisch</b><br><br>'
                        ,'kosten elektrisch bij verbruik van 20kWh/100km <br>'
                        , x, 'kilometer per jaar <br>'
                        , kwh, 'kWh <br>'
                        , '€0,24 per kWh : €'
                        , kosten)
        
        
        
      
        text
        # draw the histogram with the specified number of bins
        #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    
    
    output$bijtelling <- renderText({
        # generate bins based on input$bins from ui.R
        x    <- input$prijs
        #bins <- seq(input$ink, input$kmpj, length.out = input$bins + 1)
        
        if (x <= 40000){
            y1 <- x*.16
            y2 <- 0
        } else {
            y1 <- 40000*.16
            y2 <- (x-40000)*.22
        }
  
        
        text <- paste0('<b>Bijtelling Elektrisch Bruto (16% in 2022)</b><br><br>'
                      ,'Bijtelling tot 40k: €', y1
                      ,'<br>Bijtelling hoog: €', y2
                      , '<br>Bijtelling totaal: €',y1+y2, ' <br>'
        )
                      
        
        
        
        
        text
        # draw the histogram with the specified number of bins
        #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$browser <- reactive({if (!(Sys.info()[4] %in% c("umcbiadasht01.umcn.nl","umcbiadashp01.umcn.nl"))) {TRUE} else {FALSE}})
    
    outputOptions(output, "browser", suspendWhenHidden = FALSE)  
    
    observeEvent(input[['browser']],{
        if (!(Sys.info()[4] %in% c("umcbiadasht01.umcn.nl","umcbiadashp01.umcn.nl")))
        {browser()} else {return()}
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
