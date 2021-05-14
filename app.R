options(shiny.sanitize.errors = F)
library(shiny);library(shinycustomloader);library(DT);library(jstable);library(stringr)
library(ggpubr);library(data.table)

drugcost <- fread("drug_cost.csv")
aspirin <- fread("aspirin.csv")
metformin <- fread("metformin.csv")
statin <- fread("statin.csv")
tb <- function(x){
    x <- x[grep("1:4", analysisDescription)]
    x <- x[, .(tar = str_sub(analysisDescription, -10, -8), age = str_sub(comparatorName, 9, 10),
               targetir = eventsTarget/targetDays*365*1000, comparatorir = eventsComparator/comparatorDays*365*1000,
               drug = drug)]
    x$targetir <- round(x$targetir, 3)
    x$comparatorir <- round(x$comparatorir, 3)
    x <- x[order(age)]
}
tbm <- function(x){
    x <- x[grep("1:4", analysisDescription)]
    x <- x[, .(tar = str_sub(analysisDescription, -10, -8), age = str_sub(comparatorName, -2, -1),
               targetir = eventsTarget/targetDays*365*1000, comparatorir = eventsComparator/comparatorDays*365*1000,
               drug = drug)]
    x$targetir <- round(x$targetir, 3)
    x$comparatorir <- round(x$comparatorir, 3)
    x <- x[order(age)]
}
tbs <- function(x){
    x <- x[grep("1:4", analysisDescription)]
    x <- x[, .(tar = str_sub(analysisDescription, -10, -8), age = str_sub(comparatorName, -2, -1),
               targetir = eventsTarget/targetDays*365*1000, comparatorir = eventsComparator/comparatorDays*365*1000,
               drug = drug)]
    x$targetir <- round(x$targetir, 3)
    x$comparatorir <- round(x$comparatorir, 3)
    x <- x[order(age)]
}
aspirin <- tb(aspirin)
metformin <- tbm(metformin)
statin <- tbs(statin)

drugcost$상한금액 <- gsub(",", "", drugcost$상한금액)
drugcost <- drugcost[!(grepl("급여정지",`비고`))][!(grepl("집행정지",`비고`))]
aspirin_cost <- sum(as.integer(drugcost[grepl("아스피린",`제품명`)]$상한금액))/nrow(drugcost[grepl("아스피린",`제품명`)])
metformin_cost <- sum(as.integer(drugcost[grepl("메트포르민",`제품명`)]$상한금액))/nrow(drugcost[grepl("메트포르민",`제품명`)])
simvastatin_cost <- sum(as.integer(drugcost[grepl("심바스타틴",`제품명`)]$상한금액))/nrow(drugcost[grepl("심바스타틴",`제품명`)])
atorvastatin_cost <- sum(as.integer(drugcost[grepl("아토르바스타틴",`제품명`)]$상한금액))/nrow(drugcost[grepl("아토르바스타틴",`제품명`)])
lovastatin_cost <- sum(as.integer(drugcost[grepl("로바스타틴",`제품명`)]$상한금액))/nrow(drugcost[grepl("로바스타틴",`제품명`)])

ui <- navbarPage("Cost GC",
                 tabPanel("Data",
                          sidebarLayout(
                              sidebarPanel(
                                  h4("약품"),
                                  radioButtons("drug", "약품종류", c("Aspirin", "Metformin", "Statin"), "Aspirin", inline = T),
                                  radioButtons("day", "복용일수", c(90, 180, 365), inline = T),
                                  radioButtons("tar", "Time at risk", c(180, 365, 730), inline = T),
                                  h4("하루 약값"),
                                  fluidRow(
                                      
                                      #column(3, numericInput("maximum", "하루 복용 용량", min = 0, max = 10000, value = 100)),
                                      #column(3, numericInput("amount", "1알 용량", min = 0, max = 10000, value = 100)),
                                      column(3, numericInput("price_aspirin", "Aspirin", min = 0, max = 10000, value = round(aspirin_cost))),
                                      column(3, numericInput("price_metformin", "Metformin", min = 0, max = 10000, value = round(metformin_cost))),
                                      column(3, numericInput("price_statin1", "Simvastatin", min = 0, max = 10000, value = round(simvastatin_cost))),
                                      column(3, numericInput("price_statin2", "Atorvastatin", min = 0, max = 10000, value = round(atorvastatin_cost))),
                                      column(3, numericInput("price_statin3", "Lovastatin", min = 0, max = 10000, value = round(lovastatin_cost))),
                                      #column(3, numericInput("day", "복용일수", min = 0, max = 10000, value = 30)),
                                      
                                  ),
                                  h4("Statin 비율"),
                                  fluidRow(
                                      column(3, numericInput("statin1", "Simvastatin", min = 0, value = 57.6)),
                                      column(3, numericInput("statin2", "Atorvastatin", min = 0, value = 10.3)),
                                      column(3, numericInput("statin3", "Lovastatin", min = 0, value = 20.4)),
                                  ),
                                  h4("Incidence rate with drug"),
                                  fluidRow(
                                      column(3, numericInput("wdall", "All age", min = 0, value = 2.61)),
                                      #column(3, numericInput("wd1829", "Age 18-29", min = 0, value = 2.61)),
                                      #column(3, numericInput("wd3039", "Age 30-39", min = 0, value = 2.88)),
                                      column(3, numericInput("wd4049", "40세 이상", min = 0, value = 2.88)),
                                      column(3, numericInput("wd5059", "50세 이상", min = 0, value = 2.88)),
                                      column(3, numericInput("wd6069", "60세 이상", min = 0, value = 2.88)),
                                      #column(3, numericInput("wd70", "Age 70-", min = 0, value = 2.88)),
                                      #column(3, numericInput("price_cancer", "암 치료비", min = 0, value = 324152000)),
                                      
                                  ),
                                  h4("Incidence rate without drug"),
                                  fluidRow(
                                      column(3, numericInput("whall", "All age", min = 0, value = 2.61)),
                                      #column(3, numericInput("wh1829", "Age 18-29", min = 0, value = 2.61)),
                                      #column(3, numericInput("wh3039", "Age 30-39", min = 0, value = 2.88)),
                                      column(3, numericInput("wh4049", "40세 이상", min = 0, value = 2.88)),
                                      column(3, numericInput("wh5059", "50세 이상", min = 0, value = 2.88)),
                                      column(3, numericInput("wh6069", "60세 이상", min = 0, value = 2.88)),
                                      #column(3, numericInput("wh70", "Age 70-", min = 0, value = 2.88)),
                                  )
                              ),
                              mainPanel(
                                  
                                  tabsetPanel(type = "pills",
                                              tabPanel("Chart", 
                                                       withLoader(DTOutput("eva"), type="html", loader="loader6")),
                                              tabPanel("Plot",
                                                       withLoader(plotOutput("plot"), type = "html", loader = "loader6"))
                                              
                                  )
                              )
                          )
                 )
                 
                 #"Simvastatin", "Rosuvastatin", "Pravastatin", "Pitavastatin", "Lovastatin", "Fluvastatin", "Atorvastatin"
)
server <- function(input, output, session) {
    vatar <- c(180, 365, 730)
    observe({
        x <- input$day
        updateRadioButtons(session, "tar",
                           choices = unique(aspirin[drug == x]$tar),
                           inline = T)
    })
    
    observeEvent({input$drug
        input$day
        input$tar}, {
            statincost <- list(round(simvastatin_cost), round(atorvastatin_cost), round(lovastatin_cost))
            #statinid <- list(2.16, 2.16, 2.16, 2.16, 2.16, 2.16, 2.16)
            #statini <- list(2.83, 2.83, 2.83, 2.83, 2.83, 2.83, 2.83)
            
            
            
            # Can also set the label and select items
            
            
            map.drug <- list(Aspirin = c(100, 100, 76,
                                         2.61, 0, 0, 
                                         aspirin[tar == input$tar][drug == input$day][age == 40]$targetir,
                                         aspirin[tar == input$tar][drug == input$day][age == 50]$targetir,
                                         aspirin[tar == input$tar][drug == input$day][age == 60]$targetir,
                                         0,
                                         2.88, 0, 0,
                                         aspirin[tar == input$tar][drug == input$day][age == 40]$comparatorir,
                                         aspirin[tar == input$tar][drug == input$day][age == 50]$comparatorir,
                                         aspirin[tar == input$tar][drug == input$day][age == 60]$comparatorir,
                                         0),
                             Metformin = c(2500, 2500, 295,
                                           3.50, 0, 0, 
                                           metformin[tar == input$tar][drug == input$day][age == 40]$targetir,
                                           metformin[tar == input$tar][drug == input$day][age == 50]$targetir,
                                           metformin[tar == input$tar][drug == input$day][age == 60]$targetir,
                                           0,
                                           3.23, 0, 0,
                                           metformin[tar == input$tar][drug == input$day][age == 40]$comparatorir,
                                           metformin[tar == input$tar][drug == input$day][age == 50]$comparatorir,
                                           metformin[tar == input$tar][drug == input$day][age == 60]$comparatorir,
                                           0),
                             Statin = c(100, 100, 0,
                                        2.16, 0, 0, 
                                        statin[tar == input$tar][drug == input$day][age == 40]$targetir,
                                        statin[tar == input$tar][drug == input$day][age == 50]$targetir,
                                        statin[tar == input$tar][drug == input$day][age == 60]$targetir,
                                        0,
                                        2.83, 0, 0,
                                        statin[tar == input$tar][drug == input$day][age == 40]$comparatorir,
                                        statin[tar == input$tar][drug == input$day][age == 50]$comparatorir,
                                        statin[tar == input$tar][drug == input$day][age == 60]$comparatorir,
                                        0))
            
            
            
            #Simvastatin 2*670 Rosuvastatin 2*612, Pravastatin 8 *298, Pitavastatin 2*561, Lovastatin 4*365, Fluvastatin 4*485, Atorvastatin 8*663
            
            updateNumericInput(session, "maximum", value = map.drug[[input$drug]][1])
            updateNumericInput(session, "amount", value = map.drug[[input$drug]][2])
            #updateNumericInput(session, "price_drug", value = map.drug[[input$drug]][3])
            updateNumericInput(session, "wdall", value = map.drug[[input$drug]][4])
            #updateNumericInput(session, "wd1829", value = map.drug[[input$drug]][5])
            #updateNumericInput(session, "wd3039", value = map.drug[[input$drug]][6])
            updateNumericInput(session, "wd4049", value = map.drug[[input$drug]][7])
            updateNumericInput(session, "wd5059", value = map.drug[[input$drug]][8])
            updateNumericInput(session, "wd6069", value = map.drug[[input$drug]][9])
            #updateNumericInput(session, "wd70", value = map.drug[[input$drug]][10])
            updateNumericInput(session, "whall", value = map.drug[[input$drug]][11])
            #updateNumericInput(session, "wh1829", value = map.drug[[input$drug]][12])
            #updateNumericInput(session, "wh3039", value = map.drug[[input$drug]][13])
            updateNumericInput(session, "wh4049", value = map.drug[[input$drug]][14])
            updateNumericInput(session, "wh5059", value = map.drug[[input$drug]][15])
            updateNumericInput(session, "wh6069", value = map.drug[[input$drug]][16])
            #updateNumericInput(session, "wh70", value = map.drug[[input$drug]][17])
            
            
        })
    
    
    price.drug <- reactive({
        list(onedayprice = ifelse(input$drug == "Aspirin", input$price_aspirin, 
                                  ifelse(input$drug == 'Metformin', input$price_metformin, 
                                         (input$price_statin1 * input$statin1 + input$price_statin2 * input$statin2 + input$price_statin3 * input$statin3)/(input$statin1 + input$statin2 + input$statin3))))
    })
    output$eva <- renderDT({
        tab <- icerplot()
        datatable(tab, caption = input$drug, rownames = T, extensions= "Buttons",
                  options = c(opt.tb1("gc")),
                  list(scrollX = TRUE))
    })
    
    icerplot <- reactive({
        drugprice <- c(price.drug()$onedayprice * as.integer(input$day),
                       0)
        #cancerprice <- c(input$price_cancer * input$incidence_drug / 1000,
        #                 input$price_cancer * input$incidence / 1000)
        
        incidence <- c(input$wdall,
                       input$whall)
        
        #age1829 <- c(input$wd1829,
        #             input$wh1829)
        #age3039 <- c(input$wd3039,
        #             input$wh3039)
        age4049 <- c(input$wd4049,
                     input$wh4049)
        age5059 <- c(input$wd5059,
                     input$wh5059)
        age6069 <- c(input$wd6069,
                     input$wh6069)
        #age70 <- c(input$wd70,
        #           input$wh70)
        
        #totalprice <- drugprice + cancerprice
        
        #price_incidence <- c(NA, NA, drugprice/incidence)
        
        #tab <- data.frame(rbind(drugprice, incidence, age1829, age3039, age4049, age5059, age6069, age70))
        tab <- data.frame(rbind(drugprice, incidence, age4049, age5059, age6069))
        tab$diff <- tab[, 2] - tab[, 1]
        tab$icer <- tab[1, 3] / tab[, 3]
        tab$icer[1] <- NA
        tab$icer <- round(tab$icer)
        
        #tab <- rbind(tab, c(0, 0, tab[1, 3]/tab[2, 3]))
        
        rownames(tab) <- c("평균 약물치료비용", "Incidence rate of all age", 
                           "40세 이상", "50세 이상", "60세 이상")
        colnames(tab) <- c(paste0(as.integer(input$day),"일간 복용"), "복용하지 않음", "Diff", "ICER")
        
        return(tab)
    })
    
    output$plot <- renderPlot({
        tab <- icerplot()
        if(is.na(tab[4, 4])){
            
        } else {
            tab <- cbind(c("All age", "40 and above", "50 and above", "60 and above"), tab[2:5, 4])
            tab <- as.data.table(tab)
            colnames(tab) <- c("Age group", "ICER")
            ggbarplot(tab, "Age group", "ICER", orientation = "vertical", fill = "steelblue", color = "steelblue", label = T, label.pos = "out")
            
            #plot(x = c(45, 55, 65), tab[-c(1, 2), 4], ann = FALSE, ylim = c(min(tab[-c(1, 2), 4], tab[2, 4]), max(tab[-c(1, 2), 4], tab[2, 4])))
            #lines(x = c(10, 80), y = c(tab[2, 4], tab[2, 4]), col = 'red', type = 'l', ann = FALSE)
        }
        
    })
    
}
shinyApp(ui, server)


#ss <- rbind(c(1,2,3), c(4,5,6))
#plot(ss)
