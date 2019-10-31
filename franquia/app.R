library(shiny)
library(dplyr)
library(readr)
library(ggplot2)

dados <- read_csv2("data/slr12.csv") %>% 
    select(custo_anual = FrqAnual, custo_inicial = CusInic)

model <- lm(custo_inicial ~ custo_anual, data = dados)


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    titlePanel("Previsão de Custo Inicial para Montar uma Franquia"),
    
    fluidRow(
        column(4,
               h2("Dados"),
               tableOutput("dados")
        ), 
        column(8,
               plotOutput("grafico")
        )
    ),
    
    fluidRow(
        column(6,
               h3("Valor Anual da Franquia:"),
               numericInput("novovalor", "Insira Novo Valor", 1500, min = 1, max = 999999),
               actionButton("processar", "Processar")
        ),
        column(6,
               h1(textOutput("resultado"))
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$dados <- renderTable({
        dados %>% select("Custo Inicial" = custo_inicial,
                         "Custo Anual" = custo_anual) %>% 
            head(10)
    })
    
    output$grafico <- renderPlot({
        dados %>% 
            ggplot(aes(custo_anual, custo_inicial)) +
            geom_point() +
            scale_x_continuous(labels = scales::dollar_format(prefix = "R$ ", big.mark = ".", decimal.mark = ","),
                               breaks = c(700, 900, 1100, 1300)) +
            scale_y_continuous(labels = scales::dollar_format(prefix = "R$ ", big.mark = ".", decimal.mark = ",")) +
            labs(
                x = "Valor Anual",
                y = "Custo Inicial"
            ) +
            geom_smooth(method = "lm", se = FALSE)
    })
    
    observeEvent(input$processar, {
        valor <- input$novovalor %>% as.double()
        previsao <- predict(model, list(custo_anual = valor))
        text_output <- paste0("Previsão de Custo Inicial: R$ ", round(previsao, 2))
        output$resultado <- renderText(text_output)
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
