dane <- openpowerlifting_2024_01_06_4c732975
dane


library(tidyr)
library(dplyr)
dane <- filter(dane,Event=='SBD')
dane <- subset(dane, select = c("Name", "Sex", "Event", "Equipment", "Age", "AgeClass", "Division", 
                                "BodyweightKg", "WeightClassKg", "Best3SquatKg", "Best3BenchKg",
                                "Best3DeadliftKg", "TotalKg", "Place", "Date", "Tested", "MeetName","Country"))
dane <- na.omit(dane)
dane <- dane[!(dane$AgeClass==""),]
dane <-dane[dane$Age>=10,]
dane <- dane %>%
  mutate(Tested = ifelse(Tested == "", "Brak Testu",
                         ifelse(Tested == "Yes", "Testowany", Tested)))
dane <- dane %>%
  mutate(Age = floor(as.numeric(Age)))

daneName <- dane %>%
  group_by(Name) %>%
  slice_max(order_by = TotalKg) %>%
  ungroup() %>%
  distinct(Name, .keep_all = TRUE)

dane <- na.omit(dane)
dane




eqtotlkg <- subset(dane, select = c("Equipment","TotalKg"))
eqtotlkg <- subset(eqtotlkg, !(Equipment %in% c("Unlimited")))
eqtotlkg$TotalKg <- as.numeric(eqtotlkg$TotalKg)






library(shiny)
library(shinydashboard)
library(ggplot2)


ui <- dashboardPage(
  dashboardHeader(title = "Analiza Wariancji"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Wykresy Pudełkowe", tabName = "WykresyPudelkowe", icon = icon("chart-bar")),
      menuItem("Statystyki", tabName = "Statystyki", icon = icon("chart-bar")),
      menuItem("Test Normalności", tabName = "TestNormalnosci", icon = icon("chart-bar")),
      menuItem("Test Wariancji", tabName = "TestWariancji", icon = icon("chart-bar"))
    ),
    selectInput("selected_equipment", "Wybierz sprzęt:", choices = unique(eqtotlkg$Equipment)),
    sliderInput("obserwacje", "Liczba obserwacji:", min = 10, max = 500, value = 500)
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "WykresyPudelkowe",
              h2("Wykresy Pudełkowe"),
              fluidRow(
                plotOutput("plot1", height = 500, width = 500)
              )
      ),
      tabItem(tabName = "Statystyki",
              h2("Statystyki"),
              fluidRow(
                box(
                  h1("Podstawowe statystyki opisowe dla konkretnego sprzętu"),
                  tableOutput("stat"),
                  width = 20
                )
              )
      ),
      tabItem(tabName = "TestNormalnosci",
              h2("Test Normalności"),
              fluidRow(
                box(
                  h3("H0 = Dane pochodzą z populacji o rozkładzie normalnym."),
                  h3("H1 = Dane nie pochodzą z populacji o rozkładzie normalnym."),
                  tableOutput("test"),
                  width = 12,
                 
                )
              )
      ),
      tabItem(tabName = "TestWariancji",
              h2("Test Wariancji"),
              fluidRow(
                box(
                  h3("H0 = Dane pochodzą z tego samego rozkładu"),
                  h3("H1 = Dane nie pochodzą z tego samego rozkładu."),
                  tableOutput("war"),
                  width = 12,
                )
              )
      )
    )
  )
)


server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    daneW <- eqtotlkg[eqtotlkg$Equipment == input$selected_equipment, ]
      boxplot(TotalKg ~ Equipment, data = daneW[1:input$obserwacje, ], col = "deepskyblue3", border = "darkorchid4", pch = 15)
  })
  
  
  
  
  output$stat <- renderTable({
    nazwy <- input$selected_equipment
    wyniki <- sapply(nazwy, function(nazwa) {
      dane <- subset(eqtotlkg, Equipment == nazwa)
      dane <- dane[1:input$obserwacje, ]
      c(
        Min. = min(dane$TotalKg),
        Max. = max(dane$TotalKg),
        'Średnia Arytmetyczna' = mean(dane$TotalKg),
        'Odchylenie standardowe' = sd(dane$TotalKg),
        Wariancja = var(dane$TotalKg)
      )
    })
    wynik <- as.data.frame(wyniki)
    rownames(wynik) <- c("Minimum", "Maksimum", "Średnia",
                         "Odchylenie standardowe", "Wariancja")
    return(wynik)  
  }, rownames = TRUE)
  
  output$test <- renderTable({
    raw <- eqtotlkg$TotalKg[eqtotlkg$Equipment == "Raw"][1:input$obserwacje]
    wynik_raw <- shapiro.test(raw)
    
    wraps <- eqtotlkg$TotalKg[eqtotlkg$Equipment == "Wraps"][1:input$obserwacje]
    wynik_wraps <- shapiro.test(wraps)
    
    single <- eqtotlkg$TotalKg[eqtotlkg$Equipment == "Single-ply"][1:input$obserwacje]
    wynik_single <- shapiro.test(single)
    
  
    
    mul <- eqtotlkg$TotalKg[eqtotlkg$Equipment == "Multi-ply"][1:input$obserwacje]
    mul <- shapiro.test(mul)
    
    return(data.frame(
      "Equipment" = c("Raw", "Wraps", "Single-ply", "Multi-ply"),
      "Shapiro-Wilk p-value" = c(round(wynik_raw$p.value, 3), wynik_wraps$p.value, wynik_single$p.value, mul$p.value),
      "Odpowiedź" = ifelse(c(wynik_raw$p.value, wynik_wraps$p.value, wynik_single$p.value, mul$p.value) < 0.05,
                           "Odrzucamy H0",
                           "Nie ma podstaw do odrzucenia H0"
      )))
  })
    
    output$"war" <- renderTable({
      test_raw <- kruskal.test(eqtotlkg$TotalKg ~ eqtotlkg$Equipment)
      table <- data.frame("Chi-squared" = test_raw$statistic,
                          "p-value" = test_raw$p.value,
                          "Stopnie Swobody" = test_raw$parameter)
      return(table)
    })
    
}

shinyApp(ui, server)
