#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)  #Paket laden
library(DT)
library(shinythemes)
library(png)

titanic <- read.csv("../titanic_data.csv")

plotTypeTicketPrice <- function(x, type) {
  ticketpricesSurvive <- c(titanic$Fare[titanic$Survived == 1])
  avg_price_survived <- round(mean(ticketpricesSurvive),1)
  ticketpricesDead <- c(titanic$Fare[titanic$Survived == 0])
  avg_price_dead <- round(mean(ticketpricesDead),1)
  switch(type,
         "1" =   barplot(c(avg_price_dead,avg_price_survived), names = c("dead","survived"), main = "Gegenüberstellung der durchschn. Ticketpreise", ylab = "Ticketprice", beside = T)
         ,
         "2" = boxplot(ticketpricesDead,ticketpricesSurvive, outline = F, main = "Ticketpreise Tote & Überlebende", names = c("dead","survived"), ylab = "Ticketprice"))
  
}


ui <- fluidPage(
  theme = shinytheme("sandstone"),
  #andere mÃ¶gliche themes: "cerulean", "darkly", "cosmo", "flatly"
  navbarPage(
    title = "Projektarbeit Shiny-App",
    
    
    
    tabPanel(
      "Info",
      
      sidebarPanel(
        img(src = 'R.png',
            height = "30px"),
        h3("Willkommen!"),
        h4(
          "Diese App ist eine Projektarbeit im Rahmen des Moduls Statistik des Studiengangs Wirtschaftsinformatik 
          der HTW-Berlin im Sommersemester 2022."
        ),
        h4(
          "Finaler Abgabetermin der Projektarbeit ist der 3.7.2022 um 23:59."
        ),
        
        img(src = 'Hochschule-fuer-Technik-und-Wirtschaft-Berlin-Logo.png',
            height = "110px"),
        h5(""),
        img(src = 'note.png',
            height = "30px"),
        h4(
          "Wir empfehlen Ihnen ausdrücklich, sich diese Shiny-App im Browser anzusehen,
          um in den optimalen Genuss unserer Visualisierungen zu kommen."
        ),
        h5("Gruppe02 - Mitglieder:"),
        h5("Pauline Trunte, Leon Korth und Lucas Leiva")

      ),#sidebarPanel - Info
      mainPanel(
        h4(
          "Die Aufgabe befasst sich mit dem Untergang des Kreuzfahrtschiffes Titanic, das 1912 auf seiner Jungfernfahrt gesunken ist.
                           Auf der Basis eines Datensatzes soll herausgefunden, welche Werte welcher Merkmale der Passagiere einen Einfluss auf die Überlebenschancen hatten.
                           Hat zum Beispiel das Alter etwas mit der Überlebenschance zu tun und wenn ja, welche Altersstufen hatten höhere und welche niedrigere Überlebenschancen?
                           Spielen Kombinationen von Merkmalen wie Alter, Kabinenklasse und Geschlecht eine Rolle? Wenn ja, welche?
                           Insbesondere sollen Gruppen von Passagieren gefunden, die besonders hohe oder niedrige Überlebenschancen hatten."
        ),
        
        h4(
          "Hierzu soll eine interaktive Visualisierung mit Shiny in R gebaut werden, mit deren Hilfe man die Überlebenschancen von Passagiergruppen untersuchen kann."
        ),
        
        h4(
          "Es werden mehrere Visualisierungen der Daten, welche z.B. durch Drop-Down-Menüs, Schieberegler, Knöpfe oder direkte Drill-Downs verändert werden können, um die Daten zu explorieren oder zu analysieren, erwartet.
                           Dabei sollen auch mehrere Merkmale in interaktiven Diagrammen verknüpfen, so dass man Gruppen mit besonders hohen oder niedrigen Überlebenschancen finden kann."
        ),
        img(src = 'titanic.jpg',
            height = 500, width = 850)
      )#mainPanel - Info
    ),
    #tabPanel - Info
    
    
    
    tabPanel(
      "Übersicht",
      h3("Titanic Daten - Übersicht"),
      
      mainPanel(
        "Diese Seite zeigt die Inhalte des Titanic-Datensatzes, der die Grundlage unserer Analyse ist.
         Sie können sich einen Überblick über den Datensatz verschaffen und nach beliebigen Werte suchen."
        ),#mainPanel
      
      DT::dataTableOutput("titanic")
    ),
    #tabPanel - Ãbersicht
    
    
    
    tabPanel(
      "Visualisierung 1",
      h3("Visualisierungen unter der Betrachtung eines Merkmals:"),
      sidebarPanel(
        tags$h3("Mosaikplot Überlebt nach Passagierklasse"),
      ),
      mainPanel(h1("Mosaikplot"),
                
                
                plotOutput(outputId = "mosaicPClassSurvived")),
      
      sidebarPanel(
        tags$h3("Legen Sie hier die Anzahl der Gruppen fest:"),
        sliderInput(
          inputId = "breaksAge",
          label = "Anzahl Gruppen:",
          min = 3,
          max = 16,
          value = 7
        )
        
        
      ),
      
      mainPanel(h1("Altersstruktur"),
                
                
                plotOutput(outputId = "histAge")),
      #mainPanel - V1
      sidebarPanel(
        tags$h3("Legen Sie hier das Alter fest:"),
        sliderInput(
          inputId = "age",
          label = "Alter:",
          min = 15,
          max = 80,
          value = 40
        )
      ),
      #sidebarPanel - V2
      
      
      mainPanel(
        h1("Überlebenschance nach Altersgruppen"),
        
        
        plotOutput(outputId = "mosaicAge"),
        
      ),#mainPanel - V2
      sidebarPanel(
        tags$h3("Was wollen Sie sehen?"),
        radioButtons(
          inputId = "boxOrBar",
          label = "Treffen Sie eine Auswahl:",
          choices = c("Balkendiagramm" = 1, "Boxplot" = 2),
          
        )
        
      ),
      
      mainPanel(h1("Ticketpreise"),
                plotOutput(outputId = "ticketPrices"),
      )#mainPanel - V1
    ),
    #tabPanel - V1
    
    
    
    tabPanel(
      "Visualisierung 2",
      h3("Visualisierungen unter der Betrachtung von zwei Merkmalen:"),
      
      
      sidebarPanel(
        tags$h3("Legen Sie hier das Geschlecht fest:"),
        radioButtons(
          inputId = "maleOrFemale",
          label = "Treffen Sie eine Auswahl:",
          choices = c("männlich" = 'male', "weiblich" = 'female'),
        ),
        tags$h3("Legen Sie hier die Passagierklasse fest:"),
        radioButtons(
          inputId = "pClassChoice",
          label = "Treffen Sie eine Auswahl:",
          choices = c("1" = 1, "2" = 2, "3" = 3),
        ),
        
      ),
      #sidebarPanel - V2
      
      
      mainPanel(
        h1("Vergleich Überlebenschance Geschlecht/Passasgierklasse"),
        
        
        plotOutput(outputId = "sexPClassBarPlot"),
        
      ),#mainPanel - V2
      sidebarPanel(
        tags$h3("Legen Sie hier das Geschlecht fest:"),
        radioButtons(
          inputId = "maleOrFemaleAge",
          label = "Treffen Sie eine Auswahl:",
          choices = c("männlich" = 'male', "weiblich" = 'female'),
        ),
        tags$h3("Legen Sie hier die Altersgrenzen fest:"),
        sliderInput("ageMinMax", label = "Treffen Sie eine Auswahl:", min = 0, 
                    max = 80, value = c(40, 60)),
        
      ),#sidebarPanel - V2
      mainPanel(
        h1("Vergleich Überlebenschance Geschlecht/Alter"),
        
        
        plotOutput(outputId = "agePClassBarPlot"),
        
      ),#mainPanel - V2
      sidebarPanel(
        tags$h3("Legen Sie hier das Geschlecht fest:"),
        radioButtons(
          inputId = "maleOrFemaleAgeForHist",
          label = "Treffen Sie eine Auswahl:",
          choices = c("männlich" = 'male', "weiblich" = 'female')
        )
        
      ),
      #sidebarPanel - V2
      
      mainPanel(
        h1("Vergleich Überlebenschance Geschlecht/Alter"),
        fluidRow(
          splitLayout(cellWidths = c("50%", "50%"), plotOutput("barplotWithSex"), plotOutput("barplotWithoutSex"))
        )
        
      )#mainPanel - V2
    ),
    #tabPanel - V2
    
    
    tabPanel(
      "Quellen",
      h3(
        "Für die Erstellung unserer Shiny-App haben wir folgende Quellen verwendet:"
      ),
      
      mainPanel(
        HTML(paste0(h4(
          "Shiny-Webseite:" ,
          a(href = "https://shiny.rstudio.com/",
            "Shiny", target =
              "_blank")
        ))),
        HTML(paste0(h4(
          "Shiny-Tutorial:" ,
          a(href = "https://shiny.rstudio.com/tutorial/",
            "Shiny-Tutorial", target =
              "_blank")
        ))),
        HTML(paste0(h4(
          "Shiny-Gallery:" ,
          a(href = "https://shiny.rstudio.com/gallery/",
            "Shiny-Gallery", target = "_blank")
        ))),
        HTML(paste0(h4(
          "Titanic-Datensatz:" ,
          a(href = "https://moodle.htw-berlin.de/mod/assign/view.php?id=1170831",
            "titanic_data.csv", target = "_blank")
        ))),
        HTML(paste0(h4(
          "elab2go:" ,
          a(
            href = "https://elab2go.de/demo4/",
            "Demo 4: Predictive Maintenance mit R",
            target = "_blank"
          )
        ))),
        HTML(paste0(
          h4(
            "YouTube - Machine Learning WebApp in R mit Shiny - Tutorial/Crashkurs für die Shiny Library:" ,
            a(
              href = "https://www.youtube.com/watch?v=tNiobiLRseI&t=578s",
              "Tutorial/Crashkurs für die Shiny Library",
              target = "_blank")
          ))),
        HTML(paste0(
          h4(
            "YouTube - R Shiny for Data Science Tutorial:" ,
            a(
              href = "https://www.youtube.com/watch?v=9uFQECk30kA&t=5s",
              "Build Interactive Data-Driven Web Apps",
              target = "_blank"
            )
          )
        )),
        HTML(paste0(
          h4(
            "Mastering Shiny - Your first Shiny app:" ,
            a(href = "https://mastering-shiny.org/basic-app.html#:~:text=There%20are%20several%20ways%20to%20create%20a%20Shiny,app%20should%20look%2C%20and%20how%20it%20should%20behave.",
              "Mastering Shiny", target = "_blank")
          )
        )),
        HTML(paste0(
          h4(
            "Best Practice - Entwicklung robuster Shiny Dashboards als R-Pakete:" ,
            a(href = "https://www.inwt-statistics.de/blog-artikel-lesen/best-practice-entwicklung-robuster-shiny-dashboards-als-r-pakete.html",
              "inwt-statistics", target = "_blank")
          )
        )),
        HTML(paste0(h4(
          "RDocumentation:",
          a(
            href = "https://www.rdocumentation.org/",
            "https://www.rdocumentation.org",
            target = "_blank"
          )
        ))),
        HTML(paste0(h4(
          "Implementierung Radio Buttons Boxplot/Barplot:",
          a(
            href = "https://stackoverflow.com/questions/25633662/create-plots-based-on-radio-button-selection-r-shiny", "stackoverflow",
            target = "_blank"
          )
        ))),
        HTML(paste0(h4(
          "Implementierung png-Files:",
          a(
            href = "https://stackoverflow.com/questions/21996887/embedding-image-in-shiny-app", "stackoverflow",
            target = "_blank"
          )
        ))),
        HTML(paste0(h4(
          "Implementierung Stacked Barplot Sex/PClass:",
          a(
            href = "https://statisticsglobe.com/draw-stacked-bars-within-grouped-barplot-r", "statistics globe",
            target = "_blank"
          )
        ))),
        HTML(paste0(h4(
          "Info-Icon Startseite:",
          a(
            href= "https://openclipart.org/detail/274087/info-icon", "Info-Icon",
            target = "_blank"
          )
        ))),
        HTML(paste0(h4(
          "HTW-Logo Startseite:",
          a(
            href = "https://www.ambrosia-fm.de/wp-content/uploads/2017/03/Logo_HTW_Berlin.svg_-1920x1156.png", "HTW-Logo",
            target = "_blank"
          )
        ))),
        HTML(paste0(h4(
          "Titanic Picture Startseite:",
          a(
            href = "https://s1.ibtimes.com/sites/www.ibtimes.com/files/styles/embed/public/2018/04/13/titanic.jpg", "Titanic-Pic",
            target = "_blank"
          )
        ))),
        HTML(paste0(h4(
          "Note-Icon Startseite:",
          a(
            href = "https://secure.webtoolhub.com/static/resources/icons/set112/e73ed3ae.png", "Note-Icon",
            target = "_blank"
          )
        )))
      ), #mainPanel - Quellen
    ), #tabPanel - Quellen
    
    tabPanel(
      "Zwischenpräsentation - Entwurf",
      h4(
        "Auf dieser Seite ist der erste Entwurf unserer App als Shiny-Dashboard abgebildet:"
      ),
      mainPanel(
        img(src = 'AppEntwurf.PNG',
            height = "800px", width = "1200px")
      )
    ), #tabPanel - Entwurf
    
    
    
    tabPanel(
      "Zwischenpräsentation - Analyse",
      h4("
         Auf dieser Seite sehen Sie die Analysen und den Code, der Grundlage unserer Shiny-App bzw. deren Visualisierungen sind."
      ),
      mainPanel(
          img(src = 'Zwischenpraes.PNG',
              )
      )#mainPanel Zwischenpräsentation - Analyse und Code
    ), #tabPanel - Zwischenpräsentation - Analyse und Code
    
    
    tabPanel(
      "Dokumentation",
      h4(
        "Abschließend unsere Auswertung der Analyse in Form eines Berichts: "
      ),
      mainPanel(
        img(src = 'Doku_Shiny.png',
            )
      )#mainPanel -Bericht
    )#tabPanel - Bericht
  )#navBar
)#ui - fluidPage


# Funktion definiert die BenutzeroberflÃ¤che
server <- function(input, output) {
  
  output$histAltersstruktur <- renderPlot({
    x    <- titanic$Age[!is.na(titanic$Age)]
    y    <- na.omit(x)
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
  })
  
  output$mosaicPClassSurvived <- renderPlot({
    abs_survived_pc1 <- length(titanic$Survived[titanic$Survived == 1 & titanic$Pclass == 1])
    abs_survived_pc2 <- length(titanic$Survived[titanic$Survived == 1 & titanic$Pclass == 2])
    abs_survived_pc3 <- length(titanic$Survived[titanic$Survived == 1 & titanic$Pclass == 3])
    
    abs_dead_pc1 <- length(titanic$Survived[titanic$Survived == 0 & titanic$Pclass == 1])
    abs_dead_pc2 <- length(titanic$Survived[titanic$Survived == 0 & titanic$Pclass == 2])
    abs_dead_pc3 <- length(titanic$Survived[titanic$Survived == 0 & titanic$Pclass == 3])
    
    table_survived_pclass <- cbind(survived = c(abs_survived_pc1,abs_survived_pc2,abs_survived_pc3), dead = c(abs_dead_pc1,abs_dead_pc2, abs_dead_pc3))
    row.names(table_survived_pclass) <- c("PClass1","PClass2","PClass3")
    
    #Mosaikplott
    mosaicplot(table_survived_pclass, main = "Mosaikplot Überlebt/Gestorben nach Passagierklasse", ylab = "Status of Survival", xlab = "Passenger Class")
    
  })
  
  output$mosaicAge <- renderPlot({
    age <- input$age
    age_survived_over_equals_age <-
      length(c(titanic$Age[titanic$Survived == 1 &
                             !is.na(titanic$Age)  & titanic$Age >= age]))
    age_survived_under_age <-
      length(c(titanic$Age[titanic$Survived == 1 &
                             !is.na(titanic$Age)  & titanic$Age < age]))
    age_dead_over_equals_age <-
      length(c(titanic$Age[titanic$Survived == 0 &
                             !is.na(titanic$Age  & titanic$Age >= age)]))
    age_dead_under_age <-
      length(c(titanic$Age[titanic$Survived == 1 &
                             !is.na(titanic$Age) & titanic$Age < age]))
    
    
    #Absolute HÃ¤ufigkeiten
    table_survived_age <-
      cbind(
        survived = c(age_survived_over_equals_age, age_survived_under_age),
        dead = c(age_dead_over_equals_age, age_dead_under_age)
      )
    row.names(table_survived_age) <-
      c(paste("Over", age), paste("Under", age))
    table_survived_age
    #relative HÃ¤ufigkeiten
    
    #Mosaikplot
    mosaicplot(
      table_survived_age,
      sort = c(1, 2),
      dir = c("h", "v"),
      "Mosaikplot nach Alter"
    )
  })
  
  output$histAge <- renderPlot({
    #Zuweisung
    age_survived <-
      round(c(titanic$Age[titanic$Survived == 1 &
                            !is.na(titanic$Age)]), 0)
    age_survived
    age_dead <-
      round(c(titanic$Age[titanic$Survived == 0 &
                            !is.na(titanic$Age)]), 0)
    age_dead
    
    #Histogramm
    par(mfrow = c(1, 2))
    breaks_sur <- input$breaksAge
    hist(
      age_survived,
      breaks = breaks_sur,
      main = "Alter der Überlebenden",
      ylab = "Anzahl",
      xlab = "Alter",
      freq = T
    )
    hist(
      age_dead,
      breaks = breaks_sur,
      main = "Alter der Toten",
      ylab = "Anzahl",
      xlab = "Alter"
    )
    
  })
  
  output$ticketPrices <- renderPlot({ 
    plotTypeTicketPrice(myData, input$boxOrBar)
  })
  
  output$sexPClassBarPlot <- renderPlot({ 
    pclassnr <- input$pClassChoice
    sex <- input$maleOrFemale
    
    text1 <- paste('Gruppe [PClass=',pclassnr,', Sex=', sex ,']')
    
    abs_survived_total <- length(titanic$Survived[titanic$Survived == 1])
    abs_dead_total <- length(titanic$Survived[titanic$Survived == 0])
    
    abs_survived_filtered <- length(titanic$Survived[titanic$Survived == 1 & titanic$Pclass == pclassnr & titanic$Sex == sex])
    abs_dead_filtered <- length(titanic$Survived[titanic$Survived == 0 & titanic$Pclass == pclassnr & titanic$Sex == sex])
    
    sum_total <- abs_dead_total + abs_survived_total
    sum_filtered <- abs_survived_filtered + abs_dead_filtered
    
    rel_survived_total <- round(abs_survived_total / sum_total,2)
    rel_dead_total <- abs_dead_total / sum_total
    
    rel_survived_filtered <- round(abs_survived_filtered / sum_filtered,2)
    rel_dead_filtered <- abs_dead_filtered / sum_filtered
    
    
    
    
    # Create example data frame
    data <- data.frame(
      vergleich = c("Grundgesamtheit","Grundgesamtheit", text1, text1),
      stack = c("überlebt", "gestorben"),
      relativerAnteil = c(rel_survived_total,rel_dead_total,rel_survived_filtered,rel_dead_filtered))
    # Print example data frame
    
    
    
    library("ggplot2")
    ggplot(data,                         # Draw barplot with grouping & stacking
           aes(x = vergleich,
               y = relativerAnteil,
               fill = stack)) + 
      geom_bar(stat = "identity",
               position = "stack") + labs(y= "Realtiver Anteil", x = "", title = paste('Vergleich der Überlebenschancen zwischen der Grundgesamtheit und der', text1 ), subtitle = paste('Die allgemeine Überlebenschance liegt bei:' ,rel_survived_total, '.Die Überlebenschance der', text1, 'liegt bei:', rel_survived_filtered ))
    
  })
  
  output$agePClassBarPlot <- renderPlot({ 
    
    sex <- input$maleOrFemaleAge 
    ageMin <- input$ageMinMax[1]
    ageMax <- input$ageMinMax[2]
    
    text1 <- paste('Gruppe [Sex=',sex,',',ageMin, '<= Age <=',ageMax,']')
    
    abs_survived_total <- length(titanic$Survived[titanic$Survived == 1])
    abs_dead_total <- length(titanic$Survived[titanic$Survived == 0])
    
    abs_survived_filtered <- length(titanic$Survived[titanic$Survived == 1 & titanic$Sex == sex & !is.na(titanic$Age) & titanic$Age >= ageMin & titanic$Age <= ageMax])
    abs_dead_filtered <- length(titanic$Survived[titanic$Survived == 0 & titanic$Sex == sex & !is.na(titanic$Age) & titanic$Age >= ageMin & titanic$Age <= ageMax])
    
    sum_total <- abs_dead_total + abs_survived_total
    sum_filtered <- abs_survived_filtered + abs_dead_filtered
    
    rel_survived_total <- round(abs_survived_total / sum_total,2)
    rel_dead_total <- abs_dead_total / sum_total
    
    rel_survived_filtered <- round(abs_survived_filtered / sum_filtered,2)
    rel_dead_filtered <- abs_dead_filtered / sum_filtered
    
    output$barplotWithSex <- renderPlot ({
      sex <- input$maleOrFemaleAgeForHist
      
      
      ######0/10
      total_survived_over_0_under_10 <- length(c(titanic$Age[titanic$Sex == sex & titanic$Survived == 1 &  !is.na(titanic$Age)  & titanic$Age >= 0 & titanic$Age < 10]));
      total_dead_over_0_under_10 <- length(c(titanic$Age[titanic$Sex == sex & titanic$Survived == 0 &  !is.na(titanic$Age)  & titanic$Age >= 0 & titanic$Age< 10]));
      
      rel_survived_over_0_under_10 <- round(total_survived_over_0_under_10/(total_survived_over_0_under_10 + total_dead_over_0_under_10),3)
      rel_survived_over_0_under_10
      
      
      ######10/20
      total_survived_over_10_under_20 <- length(c(titanic$Age[titanic$Sex == sex &titanic$Survived == 1 &  !is.na(titanic$Age)  & titanic$Age >= 10 & titanic$Age < 20]));
      total_dead_over_10_under_20 <- length(c(titanic$Age[titanic$Sex == sex &titanic$Survived == 0 &  !is.na(titanic$Age)  & titanic$Age >= 10 & titanic$Age< 20]));
      
      rel_survived_over_10_under_20 <- round(total_dead_over_10_under_20/(total_dead_over_10_under_20 + total_dead_over_10_under_20),3)
      
      
      #####20/30
      total_survived_over_20_under_30 <- length(c(titanic$Age[titanic$Sex == sex &titanic$Survived == 1 &  !is.na(titanic$Age)  & titanic$Age >= 20 & titanic$Age < 30]));
      total_dead_over_20_under_30 <- length(c(titanic$Age[titanic$Sex == sex &titanic$Survived == 0 &  !is.na(titanic$Age)  & titanic$Age >= 20 & titanic$Age< 30]));
      
      rel_survived_over_20_under_30 <- round(total_survived_over_20_under_30/(total_survived_over_20_under_30 + total_dead_over_20_under_30),3)
      rel_survived_over_20_under_30
      
      
      #####30/40
      total_survived_over_30_under_40 <- length(c(titanic$Age[titanic$Sex == sex &titanic$Survived == 1 &  !is.na(titanic$Age)  & titanic$Age >= 30 & titanic$Age < 40]));
      total_dead_over_30_under_40 <- length(c(titanic$Age[titanic$Sex == sex &titanic$Survived == 0 &  !is.na(titanic$Age)  & titanic$Age >= 30 & titanic$Age < 40]));
      
      rel_survived_over_30_under_40 <- round(total_survived_over_30_under_40/(total_survived_over_30_under_40 + total_dead_over_30_under_40),3)
      rel_survived_over_30_under_40
      
      
      #####40/50
      total_survived_over_40_under_50 <- length(c(titanic$Age[titanic$Sex == sex &titanic$Survived == 1 &  !is.na(titanic$Age)  & titanic$Age >= 40 & titanic$Age < 50]));
      total_dead_over_40_under_50 <- length(c(titanic$Age[titanic$Sex == sex &titanic$Survived == 0 &  !is.na(titanic$Age)  & titanic$Age >= 40 & titanic$Age < 50]));
      
      rel_survived_over_40_under_50 <- round(total_survived_over_40_under_50/(total_survived_over_40_under_50 + total_dead_over_40_under_50),3)
      rel_survived_over_40_under_50
      
      
      #####50/60
      total_survived_over_50_under_60 <- length(c(titanic$Age[titanic$Sex == sex &titanic$Survived == 1 &  !is.na(titanic$Age)  & titanic$Age >= 50 & titanic$Age < 60]));
      total_dead_over_50_under_60 <- length(c(titanic$Age[titanic$Sex == sex &titanic$Survived == 0 &  !is.na(titanic$Age)  & titanic$Age >= 50 & titanic$Age < 60]));
      
      rel_survived_over_50_under_60 <- round(total_survived_over_50_under_60/(total_survived_over_50_under_60 + total_dead_over_50_under_60),3)
      rel_survived_over_50_under_60
      
      
      #####60/70
      total_survived_over_60_under_70 <- length(c(titanic$Age[titanic$Sex == sex &titanic$Survived == 1 &  !is.na(titanic$Age)  & titanic$Age >= 60 & titanic$Age < 70]));
      total_dead_over_60_under_70 <- length(c(titanic$Age[titanic$Sex == sex & titanic$Survived == 0 &  !is.na(titanic$Age)  & titanic$Age >= 60 & titanic$Age < 70]));
      
      rel_survived_over_60_under_70 <- round(total_survived_over_60_under_70/(total_survived_over_60_under_70 + total_dead_over_60_under_70),3)
      rel_survived_over_60_under_70
      
      
      
      #####70/80
      total_survived_over_70_under_80 <- length(c(titanic$Age[titanic$Sex == sex &titanic$Survived == 1 &  !is.na(titanic$Age)  & titanic$Age >= 70 & titanic$Age < 80]));
      total_dead_over_70_under_80 <- length(c(titanic$Age[titanic$Sex == sex &titanic$Survived == 0 &   !is.na(titanic$Age)  & titanic$Age >= 70 & titanic$Age < 80]));
      
      rel_survived_over_70_under_80 <- round(total_survived_over_70_under_80/(total_survived_over_70_under_80 + total_dead_over_60_under_70),3)
      rel_survived_over_70_under_80
      if(is.na(rel_survived_over_70_under_80)) {rel_survived_over_70_under_80 <- 0}
      
      
      ####
      all_rel <- c(rel_survived_over_0_under_10,rel_survived_over_10_under_20,rel_survived_over_20_under_30,rel_survived_over_30_under_40,rel_survived_over_40_under_50,rel_survived_over_50_under_60,rel_survived_over_60_under_70,rel_survived_over_70_under_80)
      all_rel_name <- c('0-9','10-19','20-29','30-39','40-49','50-59','60-69','70-79')
      
      barplot(all_rel, names = all_rel_name, xlab = 'Altersgruppen', ylab = 'rel. Überlebenschance', main= ('Überlebenschance der verschiedenen Altersgruppen mit Geschlecht ') )
      
    })
    
    
    ###################
    
    
    
    
    output$barplotWithoutSex <- renderPlot ({
      
      
      ######0/10
      total_survived_over_0_under_10 <- length(c(titanic$Age[titanic$Survived == 1 &  !is.na(titanic$Age)  & titanic$Age >= 0 & titanic$Age < 10]));
      total_dead_over_0_under_10 <- length(c(titanic$Age[titanic$Survived == 0 &  !is.na(titanic$Age)  & titanic$Age >= 0 & titanic$Age< 10]));
      
      rel_survived_over_0_under_10 <- round(total_survived_over_0_under_10/(total_survived_over_0_under_10 + total_dead_over_0_under_10),3)
      rel_survived_over_0_under_10
      
      
      ######10/20
      total_survived_over_10_under_20 <- length(c(titanic$Age[titanic$Survived == 1 &  !is.na(titanic$Age)  & titanic$Age >= 10 & titanic$Age < 20]));
      total_dead_over_10_under_20 <- length(c(titanic$Age[titanic$Survived == 0 &  !is.na(titanic$Age)  & titanic$Age >= 10 & titanic$Age< 20]));
      
      rel_survived_over_10_under_20 <- round(total_dead_over_10_under_20/(total_dead_over_10_under_20 + total_dead_over_10_under_20),3)
      
      
      #####20/30
      total_survived_over_20_under_30 <- length(c(titanic$Age[titanic$Survived == 1 &  !is.na(titanic$Age)  & titanic$Age >= 20 & titanic$Age < 30]));
      total_dead_over_20_under_30 <- length(c(titanic$Age[titanic$Survived == 0 &  !is.na(titanic$Age)  & titanic$Age >= 20 & titanic$Age< 30]));
      
      rel_survived_over_20_under_30 <- round(total_survived_over_20_under_30/(total_survived_over_20_under_30 + total_dead_over_20_under_30),3)
      rel_survived_over_20_under_30
      
      
      #####30/40
      total_survived_over_30_under_40 <- length(c(titanic$Age[titanic$Survived == 1 &  !is.na(titanic$Age)  & titanic$Age >= 30 & titanic$Age < 40]));
      total_dead_over_30_under_40 <- length(c(titanic$Age[titanic$Survived == 0 &  !is.na(titanic$Age)  & titanic$Age >= 30 & titanic$Age < 40]));
      
      rel_survived_over_30_under_40 <- round(total_survived_over_30_under_40/(total_survived_over_30_under_40 + total_dead_over_30_under_40),3)
      rel_survived_over_30_under_40
      
      
      #####40/50
      total_survived_over_40_under_50 <- length(c(titanic$Age[titanic$Survived == 1 &  !is.na(titanic$Age)  & titanic$Age >= 40 & titanic$Age < 50]));
      total_dead_over_40_under_50 <- length(c(titanic$Age[titanic$Survived == 0 &  !is.na(titanic$Age)  & titanic$Age >= 40 & titanic$Age < 50]));
      
      rel_survived_over_40_under_50 <- round(total_survived_over_40_under_50/(total_survived_over_40_under_50 + total_dead_over_40_under_50),3)
      rel_survived_over_40_under_50
      
      
      #####50/60
      total_survived_over_50_under_60 <- length(c(titanic$Age[titanic$Survived == 1 &  !is.na(titanic$Age)  & titanic$Age >= 50 & titanic$Age < 60]));
      total_dead_over_50_under_60 <- length(c(titanic$Age[titanic$Survived == 0 &  !is.na(titanic$Age)  & titanic$Age >= 50 & titanic$Age < 60]));
      
      rel_survived_over_50_under_60 <- round(total_survived_over_50_under_60/(total_survived_over_50_under_60 + total_dead_over_50_under_60),3)
      rel_survived_over_50_under_60
      
      
      #####60/70
      total_survived_over_60_under_70 <- length(c(titanic$Age[titanic$Survived == 1 &  !is.na(titanic$Age)  & titanic$Age >= 60 & titanic$Age < 70]));
      total_dead_over_60_under_70 <- length(c(titanic$Age[titanic$Survived == 0 &  !is.na(titanic$Age)  & titanic$Age >= 60 & titanic$Age < 70]));
      
      rel_survived_over_60_under_70 <- round(total_survived_over_60_under_70/(total_survived_over_60_under_70 + total_dead_over_60_under_70),3)
      rel_survived_over_60_under_70
      
      
      
      #####70/80
      total_survived_over_70_under_80 <- length(c(titanic$Age[titanic$Survived == 1 &  !is.na(titanic$Age)  & titanic$Age >= 70 & titanic$Age < 80]));
      total_dead_over_70_under_80 <- length(c(titanic$Age[titanic$Survived == 0 &   !is.na(titanic$Age)  & titanic$Age >= 70 & titanic$Age < 80]));
      
      total_survived_over_70_under_80
      total_dead_over_70_under_80
      
      rel_survived_over_70_under_80 <- round(total_survived_over_70_under_80/(total_survived_over_70_under_80 + total_dead_over_60_under_70),3)
      rel_survived_over_70_under_80
      if(is.na(rel_survived_over_70_under_80)) {rel_survived_over_70_under_80 <- 0}
      
      
      ####
      all_rel <- c(rel_survived_over_0_under_10,rel_survived_over_10_under_20,rel_survived_over_20_under_30,rel_survived_over_30_under_40,rel_survived_over_40_under_50,rel_survived_over_50_under_60,rel_survived_over_60_under_70,rel_survived_over_70_under_80)
      all_rel_name <- c('0-9','10-19','20-29','30-39','40-49','50-59','60-69','70-79')
      
      barplot(all_rel, names = all_rel_name, xlab = 'Altersgruppen', ylab = 'rel. Überlebenschance', main= ('Überlebenschance der verschiedenen Altersgruppen ohne Geschlecht ') )
      
      
    })
    
    output$barplotWithoutSex <- renderPlot ({
      
      
      
      ######0/10
      total_survived_over_0_under_10 <- length(c(titanic$Age[titanic$Survived == 1 &  !is.na(titanic$Age)  & titanic$Age >= 0 & titanic$Age < 10]));
      total_dead_over_0_under_10 <- length(c(titanic$Age[titanic$Survived == 0 &  !is.na(titanic$Age)  & titanic$Age >= 0 & titanic$Age< 10]));
      
      rel_survived_over_0_under_10 <- round(total_survived_over_0_under_10/(total_survived_over_0_under_10 + total_dead_over_0_under_10),3)
      rel_dead_over_0_under_10 <- (1 - rel_survived_over_0_under_10)
      
      
      
      ######10/20
      total_survived_over_10_under_20 <- length(c(titanic$Age[titanic$Survived == 1 &  !is.na(titanic$Age)  & titanic$Age >= 10 & titanic$Age < 20]));
      total_dead_over_10_under_20 <- length(c(titanic$Age[titanic$Survived == 0 &  !is.na(titanic$Age)  & titanic$Age >= 10 & titanic$Age< 20]));
      
      rel_survived_over_10_under_20 <- round(total_dead_over_10_under_20/(total_dead_over_10_under_20 + total_dead_over_10_under_20),3)
      rel_dead_over_10_under_20 <- (1 - rel_survived_over_10_under_20)
      
      
      #####20/30
      total_survived_over_20_under_30 <- length(c(titanic$Age[titanic$Survived == 1 &  !is.na(titanic$Age)  & titanic$Age >= 20 & titanic$Age < 30]));
      total_dead_over_20_under_30 <- length(c(titanic$Age[titanic$Survived == 0 &  !is.na(titanic$Age)  & titanic$Age >= 20 & titanic$Age< 30]));
      
      rel_survived_over_20_under_30 <- round(total_survived_over_20_under_30/(total_survived_over_20_under_30 + total_dead_over_20_under_30),3)
      rel_dead_over_20_under_30 <- (1 - rel_survived_over_20_under_30)
      
      
      #####30/40
      total_survived_over_30_under_40 <- length(c(titanic$Age[titanic$Survived == 1 &  !is.na(titanic$Age)  & titanic$Age >= 30 & titanic$Age < 40]));
      total_dead_over_30_under_40 <- length(c(titanic$Age[titanic$Survived == 0 &  !is.na(titanic$Age)  & titanic$Age >= 30 & titanic$Age < 40]));
      
      rel_survived_over_30_under_40 <- round(total_survived_over_30_under_40/(total_survived_over_30_under_40 + total_dead_over_30_under_40),3)
      rel_dead_over_30_under_40 <- (1 - rel_survived_over_30_under_40)
      
      
      
      #####40/50
      total_survived_over_40_under_50 <- length(c(titanic$Age[titanic$Survived == 1 &  !is.na(titanic$Age)  & titanic$Age >= 40 & titanic$Age < 50]));
      total_dead_over_40_under_50 <- length(c(titanic$Age[titanic$Survived == 0 &  !is.na(titanic$Age)  & titanic$Age >= 40 & titanic$Age < 50]));
      
      rel_survived_over_40_under_50 <- round(total_survived_over_40_under_50/(total_survived_over_40_under_50 + total_dead_over_40_under_50),3)
      rel_dead_over_40_under_50 <- (1 - rel_survived_over_40_under_50)
      
      
      
      #####50/60
      total_survived_over_50_under_60 <- length(c(titanic$Age[titanic$Survived == 1 &  !is.na(titanic$Age)  & titanic$Age >= 50 & titanic$Age < 60]));
      total_dead_over_50_under_60 <- length(c(titanic$Age[titanic$Survived == 0 &  !is.na(titanic$Age)  & titanic$Age >= 50 & titanic$Age < 60]));
      
      rel_survived_over_50_under_60 <- round(total_survived_over_50_under_60/(total_survived_over_50_under_60 + total_dead_over_50_under_60),3)
      rel_dead_over_50_under_60 <- (1 - rel_survived_over_50_under_60)
      
      
      
      #####60/70
      total_survived_over_60_under_70 <- length(c(titanic$Age[titanic$Survived == 1 &  !is.na(titanic$Age)  & titanic$Age >= 60 & titanic$Age < 70]));
      total_dead_over_60_under_70 <- length(c(titanic$Age[titanic$Survived == 0 &  !is.na(titanic$Age)  & titanic$Age >= 60 & titanic$Age < 70]));
      
      rel_survived_over_60_under_70 <- round(total_survived_over_60_under_70/(total_survived_over_60_under_70 + total_dead_over_60_under_70),3)
      rel_dead_over_60_under_70 <- (1 - rel_survived_over_60_under_70)
      
      
      
      
      #####70/80
      total_survived_over_70_under_80 <- length(c(titanic$Age[titanic$Survived == 1 &  !is.na(titanic$Age)  & titanic$Age >= 70 & titanic$Age < 80]));
      total_dead_over_70_under_80 <- length(c(titanic$Age[titanic$Survived == 0 &   !is.na(titanic$Age)  & titanic$Age >= 70 & titanic$Age < 80]));
      
      total_survived_over_70_under_80
      
      rel_survived_over_70_under_80 <- round(total_survived_over_70_under_80/(total_survived_over_70_under_80 + total_dead_over_60_under_70),3)
      rel_dead_over_70_under_80 <- (1 - rel_survived_over_70_under_80)
      
      
      ####
      all_rel_new <- c(rel_survived_over_0_under_10,rel_dead_over_0_under_10,rel_survived_over_10_under_20,rel_dead_over_10_under_20,rel_survived_over_20_under_30,rel_dead_over_20_under_30,rel_survived_over_30_under_40,rel_dead_over_30_under_40,rel_survived_over_40_under_50,rel_dead_over_40_under_50,rel_survived_over_50_under_60,rel_dead_over_50_under_60,rel_survived_over_60_under_70,rel_dead_over_60_under_70,rel_survived_over_70_under_80,rel_dead_over_70_under_80)
      
      data <- data.frame(
        vergleich = c('0-9','0-9','10-19','10-19','20-29','20-29','30-39','30-39','40-49','40-49','50-59','50-59','60-69','60-69','70-79','70-79'),
        stack = c("überlebt", "gestorben"),
        relativerAnteil = all_rel_new)
      data                                 # Print example data frame
      
      
      
      library("ggplot2")
      ggplot(data,                         # Draw barplot with grouping & stacking
             aes(x = vergleich,
                 y = relativerAnteil,
                 fill = stack)) + 
        geom_bar(stat = "identity",
                 position = "stack") + labs(y= "Realtive Überlebenschancen", x = "Altersgruppen", title = paste("Überlebenschance der verschiedenen Altersgruppen ohne Geschlecht:"))
      
      
    })
    
    output$barplotWithSex <- renderPlot ({
      sex <- input$maleOrFemaleAgeForHist
      
      ######0/10
      total_survived_over_0_under_10 <- length(c(titanic$Age[titanic$Sex == sex & titanic$Survived == 1 &  !is.na(titanic$Age)  & titanic$Age >= 0 & titanic$Age < 10]));
      total_dead_over_0_under_10 <- length(c(titanic$Age[titanic$Sex == sex & titanic$Survived == 0 &  !is.na(titanic$Age)  & titanic$Age >= 0 & titanic$Age< 10]));
      rel_survived_over_0_under_10 <- round(total_survived_over_0_under_10/(total_survived_over_0_under_10 + total_dead_over_0_under_10),3)
      rel_dead_over_0_under_10 <- (1 - rel_survived_over_0_under_10)
      
      ######10/20
      total_survived_over_10_under_20 <- length(c(titanic$Age[titanic$Sex == sex & titanic$Survived == 1 &  !is.na(titanic$Age)  & titanic$Age >= 10 & titanic$Age < 20]));
      total_dead_over_10_under_20 <- length(c(titanic$Age[titanic$Sex == sex & titanic$Survived == 0 &  !is.na(titanic$Age)  & titanic$Age >= 10 & titanic$Age< 20]));
      rel_survived_over_10_under_20 <- round(total_dead_over_10_under_20/(total_dead_over_10_under_20 + total_dead_over_10_under_20),3)
      rel_dead_over_10_under_20 <- (1 - rel_survived_over_10_under_20)
      
      
      #####20/30
      total_survived_over_20_under_30 <- length(c(titanic$Age[titanic$Sex == sex & titanic$Survived == 1 &  !is.na(titanic$Age)  & titanic$Age >= 20 & titanic$Age < 30]));
      total_dead_over_20_under_30 <- length(c(titanic$Age[titanic$Sex == sex & titanic$Survived == 0 &  !is.na(titanic$Age)  & titanic$Age >= 20 & titanic$Age< 30]));
      rel_survived_over_20_under_30 <- round(total_survived_over_20_under_30/(total_survived_over_20_under_30 + total_dead_over_20_under_30),3)
      rel_dead_over_20_under_30 <- (1 - rel_survived_over_20_under_30)
      
      
      
      #####30/40
      total_survived_over_30_under_40 <- length(c(titanic$Age[titanic$Sex == sex & titanic$Survived == 1 &  !is.na(titanic$Age)  & titanic$Age >= 30 & titanic$Age < 40]));
      total_dead_over_30_under_40 <- length(c(titanic$Age[titanic$Sex == sex & titanic$Survived == 0 &  !is.na(titanic$Age)  & titanic$Age >= 30 & titanic$Age < 40]));
      rel_survived_over_30_under_40 <- round(total_survived_over_30_under_40/(total_survived_over_30_under_40 + total_dead_over_30_under_40),3)
      rel_dead_over_30_under_40 <- (1 - rel_survived_over_30_under_40)
      
      
      
      #####40/50
      total_survived_over_40_under_50 <- length(c(titanic$Age[titanic$Sex == sex & titanic$Survived == 1 &  !is.na(titanic$Age)  & titanic$Age >= 40 & titanic$Age < 50]));
      total_dead_over_40_under_50 <- length(c(titanic$Age[titanic$Sex == sex & titanic$Survived == 0 &  !is.na(titanic$Age)  & titanic$Age >= 40 & titanic$Age < 50]));
      
      rel_survived_over_40_under_50 <- round(total_survived_over_40_under_50/(total_survived_over_40_under_50 + total_dead_over_40_under_50),3)
      rel_dead_over_40_under_50 <- (1 - rel_survived_over_40_under_50)
      
      
      
      #####50/60
      total_survived_over_50_under_60 <- length(c(titanic$Age[titanic$Sex == sex & titanic$Survived == 1 &  !is.na(titanic$Age)  & titanic$Age >= 50 & titanic$Age < 60]));
      total_dead_over_50_under_60 <- length(c(titanic$Age[titanic$Sex == sex & titanic$Survived == 0 &  !is.na(titanic$Age)  & titanic$Age >= 50 & titanic$Age < 60]));
      rel_survived_over_50_under_60 <- round(total_survived_over_50_under_60/(total_survived_over_50_under_60 + total_dead_over_50_under_60),3)
      rel_dead_over_50_under_60 <- (1 - rel_survived_over_50_under_60)
      
      
      
      #####60/70
      total_survived_over_60_under_70 <- length(c(titanic$Age[titanic$Sex == sex & titanic$Survived == 1 &  !is.na(titanic$Age)  & titanic$Age >= 60 & titanic$Age < 70]));
      total_dead_over_60_under_70 <- length(c(titanic$Age[titanic$Sex == sex & titanic$Survived == 0 &  !is.na(titanic$Age)  & titanic$Age >= 60 & titanic$Age < 70]));
      
      rel_survived_over_60_under_70 <- round(total_survived_over_60_under_70/(total_survived_over_60_under_70 + total_dead_over_60_under_70),3)
      rel_dead_over_60_under_70 <- (1 - rel_survived_over_60_under_70)
      
      
      
      
      #####70/80
      total_survived_over_70_under_80 <- length(c(titanic$Age[titanic$Sex == sex & titanic$Survived == 1 &  !is.na(titanic$Age)  & titanic$Age >= 70 & titanic$Age < 80]));
      total_dead_over_70_under_80 <- length(c(titanic$Age[titanic$Sex == sex & titanic$Survived == 0 &   !is.na(titanic$Age)  & titanic$Age >= 70 & titanic$Age < 80]));
      
      total_survived_over_70_under_80
      total_dead_over_70_under_80
      
      rel_survived_over_70_under_80 <- round(total_survived_over_70_under_80/(total_survived_over_70_under_80 + total_dead_over_60_under_70),3)
      rel_dead_over_70_under_80 <- (1 - rel_survived_over_70_under_80)
      
      
      ####
      all_rel <- c(rel_survived_over_0_under_10,rel_survived_over_10_under_20,rel_survived_over_20_under_30,rel_survived_over_30_under_40,rel_survived_over_40_under_50,rel_survived_over_50_under_60,rel_survived_over_60_under_70,rel_survived_over_70_under_80)
      
      
      all_rel_new <- c(rel_survived_over_0_under_10,rel_dead_over_0_under_10,rel_survived_over_10_under_20,rel_dead_over_10_under_20,rel_survived_over_20_under_30,rel_dead_over_20_under_30,rel_survived_over_30_under_40,rel_dead_over_30_under_40,rel_survived_over_40_under_50,rel_dead_over_40_under_50,rel_survived_over_50_under_60,rel_dead_over_50_under_60,rel_survived_over_60_under_70,rel_dead_over_60_under_70,rel_survived_over_70_under_80,rel_dead_over_70_under_80)
      
      
      data <- data.frame(
        vergleich = c('0-9','0-9','10-19','10-19','20-29','20-29','30-39','30-39','40-49','40-49','50-59','50-59','60-69','60-69','70-79','70-79'),
        stack = c("überlebt", "gestorben"),
        relativerAnteil = all_rel_new)
      data                                 # Print example data frame
      
      
      
      library("ggplot2")
      ggplot(data,                         # Draw barplot with grouping & stacking
             aes(x = vergleich,
                 y = relativerAnteil,
                 fill = stack)) + 
        geom_bar(stat = "identity",
                 position = "stack") + labs(y= "Realtive Überlebenschancen", x = "Altersgruppen", title = paste("Überlebenschance der verschiedenen Altersgruppen mit Geschlecht:", sex))
      
      #############################
      
    })
    
    
    
    # Create example data frame
    data <- data.frame(
      vergleich = c("Grundgesamtheit","Grundgesamtheit", text1, text1),
      stack = c("überlebt", "gestorben"),
      relativerAnteil = c(rel_survived_total,rel_dead_total,rel_survived_filtered,rel_dead_filtered))
    # Print example data frame
    
    
    
    library("ggplot2")
    ggplot(data,                         # Draw barplot with grouping & stacking
           aes(x = vergleich,
               y = relativerAnteil,
               fill = stack)) + 
      geom_bar(stat = "identity",
               position = "stack") + labs(y= "Realtiver Anteil", x = "", title = paste('Vergleich der Überlebenschancen zwischen der Grundgesamtheit und der', text1 ), subtitle = paste('Die allgemeine Überlebenschance liegt bei:' ,rel_survived_total, '.Die Überlebenschance der', text1, 'liegt bei:', rel_survived_filtered ))
    
  })
  
  output$titanic = DT::renderDataTable({titanic})
  
  
}# Funktion mit R-Anweisungen und -Objekten

# Funktion, die ui und server zu einer Shiny-Anwendung verknÃ¼pft
shinyApp(ui = ui, server = server)