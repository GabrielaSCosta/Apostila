library(shiny)
library(shinyWidgets)
ui <- fluidPage(
  setBackgroundColor(
    color = "lightgreen",
    gradient = "radial",
    direction = c("top", "left")
  ),
  titlePanel("Testando Hipoteses"),
  "Esse app gera amostras aleatorias e testa hipoteses de acordo com o desejo do usuario.",
  selectInput("dist", label = "Defina qual sera a distribuicao da amostra:",
              choices = c("normal", "poisson", "exponencial"), selected = "normal"),
  numericInput("media", "defina a media da distribuicao:", value = 0),
  numericInput("var", "defina a variancia da distribuicao:", value = 1, min = 0),
  sliderInput("amostra", "defina quantas amostras deseja gerar:", value = 10, min = 1, max = 1000),
  numericInput("n", "defina o tamanho das amostras:", value = 10, min = 1),
  numericInput("teste", "com qual media o teste ira comparar?", value = 0),
  sliderInput("sig", "defina o nivel de significancia (em %):", value = 5, min = 1, max = 10),
  textOutput("res")
)
server <- function(input, output, session) {
  pvals <- reactive({
  if (input$dist == "normal"){
    replicate(input$amostra, t.test(rnorm(input$n,input$media,input$var**0.5),mu = input$teste)$p.value)
  } else if (input$dist == "poisson"){
    replicate(input$amostra, t.test(rpois(input$n,input$media),mu = input$teste)$p.value)
  } else if (input$dist == "exponencial"){
    replicate(input$amostra, t.test(rexp(input$n, 1/input$media),mu = input$teste)$p.value)
  }
})
  rejr <- reactive({length(which(pvals() < (input$sig/100)))})
  output$res = renderText({
    rej <- rejr()
    paste0("A hipotese nula foi rejeitada em ",round((rej/input$amostra)*100,1), "% das vezes (", rej, " de ", input$amostra, ").")
    })
}
shinyApp(ui, server)