library(shiny)
library(shinydashboard)
library(ggplot2)
ui <- dashboardPage(
  header = dashboardHeader(
    dropdownMenu(type = "messages",
                 messageItem(
                   from = "monitores",
                   message = "quer aprender mais? clique aqui",
                   href = "http://www.est.ufmg.br/portal/"
                 )
    )
),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Uma populacao",
        tabName = "uma"
      ),
      menuItem(
        "Duas populacoes",
        tabName = "duas"
      ),
      menuItem(
        "Visualizacao grafica",
        tabName = "graf"
      )
    )
  ),
  body = dashboardBody(
    tabItems(
      tabItem(
        tabName = "uma",
        titlePanel("Testes de hipotese: Uma populacao"),
        textOutput("exp1"),
        numericInput("media", "Defina a media da distribuicao:", value = 0),
        numericInput("var", "Defina a variancia da distribuicao:", value = 1, min = 0.00000001),
        sliderInput("amostra", "Defina quantas amostras deseja gerar:", value = 10, min = 1, max = 1000),
        numericInput("n", "Defina o tamanho das amostras:", value = 10, min = 2),
        selectInput("alt", "Defina a hipotese nula:", choices = c("media = X","media > X","media < X"), selected = "media = X"),
        numericInput("teste", "Defina o valor de X", value = 0),
        sliderInput("sig", "Defina o nivel de significancia (em %):", value = 5, min = 1, max = 10),
        textOutput("res")
      ),
      tabItem(
        tabName = "duas",
        titlePanel("Testes de hipotese: Duas populacoes"),
        textOutput("exp2"),
        numericInput("media1", "Defina a media da distribuicao 1:", value = 0),
        numericInput("var1", "Defina a variancia da distribuicao 1:", value = 1, min = 0.00000001),
        numericInput("media2", "Defina a media da distribuicao 2:", value = 0),
        numericInput("var2", "Defina a variancia da distribuicao 2:", value = 1, min = 0.00000001),
        sliderInput("amostras", "Defina quantos pares de amostras deseja gerar:", value = 10, min = 1, max = 1000),
        numericInput("ns", "Defina o tamanho das amostras:", value = 10, min = 2),
        selectInput("alts", "Defina a hipotese nula:", choices = c("media1 = media2","media1 > media2","media1 < media2"), selected = "media1 = media2"),
        sliderInput("sigs", "Defina o nivel de significancia (em %):", value = 5, min = 1, max = 10),
        textOutput("ress")  
      ),
      tabItem(
        tabName = "graf",
        titlePanel("Testes de hipotese: Visualizacao grafica"),
        textOutput("exp3"),
        sidebarLayout(
          sidebarPanel(
        numericInput("mediax", "Defina a media da distribuicao:", value = 1),
        numericInput("varx", "Defina a variancia da distribuicao:", value = 1, min = 0.00000001),
        numericInput("nx", "Defina o tamanho da amostra:", value = 10, min = 2),
        selectInput("altx", "Defina a hipotese nula:", choices = c("media = X","media > X","media < X"), selected = "media = X"),
        numericInput("testex", "Defina o valor de X", value = 0),
        sliderInput("sigx", "Defina o nivel de significancia (em %):", value = 5, min = 1, max = 10)
          ),
          mainPanel(
            plotOutput("grafico"),
            textOutput("resx"),
            textOutput("msg")
          )
        )
      )
    )
  )
)
server <- function(input,output,session){
  output$exp1 = renderText("Essa aba gera amostras aleatorias usando a funcao rnorm() e realiza testes de hipotese para a media de uma populacao em cada uma das amostras usando a funcao t.test(). Voce pode mudar os parametros das funcoes para observar como isso impacta no resultado dos testes. Aviso: Gerar amostras muito grande pode ocasionar em travamentos.")
  output$exp2 = renderText("Essa aba gera pares de amostras aleatorias usando a funcao rnorm() e realiza testes de hipotese para a media de duas populacoes em cada um dos pares de amostras usando a funcao t.test(). Voce pode mudar os parametros das funcoes para observar como isso impacta no resultado dos testes. Aviso: Gerar amostras muito grande pode ocasionar em travamentos.")
  output$exp3 = renderText("Essa aba gera uma amostra usando a funcao rnorm() e mostra graficamente o que acontece com ela. Voce pode mudar os parametros da funcao e observar como isso impacta nos resultados. Aviso: Gerar amostras muito grande pode ocasionar em travamentos.")
  pvals <- reactive({
    if (input$alt == "media = X"){
      replicate(input$amostra, t.test(rnorm(input$n,input$media,input$var**0.5),mu = input$teste,alternative = "two.sided")$p.value)
    } else if (input$alt == "media < X"){
      replicate(input$amostra, t.test(rnorm(input$n,input$media,input$var**0.5),mu = input$teste,alternative = "greater")$p.value)
    } else if (input$alt == "media > X"){
      replicate(input$amostra, t.test(rnorm(input$n,input$media,input$var**0.5),mu = input$teste,alternative = "less")$p.value)
    }
  })
  rejr <- reactive({length(which(pvals() < (input$sig/100)))})
  output$res = renderText({
    rej <- rejr()
    paste0("A hipotese nula foi rejeitada em ",round((rej/input$amostra)*100,1), "% das vezes (", rej, " de ", input$amostra, ").")
  })
  pvalss <- reactive({
    if (input$alts == "media1 = media2"){
      replicate(input$amostras, t.test(x = rnorm(input$ns,input$media1,input$var1**0.5), y = rnorm(input$ns,input$media2,input$var2**0.5),alternative = "two.sided")$p.value)
    } else if (input$alts == "media1 < media2"){
      replicate(input$amostras, t.test(x = rnorm(input$ns,input$media1,input$var1**0.5), y = rnorm(input$ns,input$media2,input$var2**0.5),alternative = "greater")$p.value)
    } else if (input$alts == "media1 > media2"){
      replicate(input$amostras, t.test(x = rnorm(input$ns,input$media1,input$var1**0.5), y = rnorm(input$ns,input$media2,input$var2**0.5),alternative = "less")$p.value)
    }
  })
  rejrs <- reactive({length(which(pvalss() < (input$sigs/100)))})
  output$ress = renderText({
    rejs <- rejrs()
    paste0("A hipotese nula foi rejeitada em ",round((rejs/input$amostras)*100,1), "% das vezes (", rejs, " de ", input$amostras, ").")
  })
  output$grafico = renderPlot({
    amostrax = rnorm(input$nx, input$mediax, input$varx**0.5)
    plot(density(amostrax), main = "Amostra gerada", ylab = "Densidade")
    abline(v = input$mediax, col = "red")
    abline(v = mean(amostrax), col = "green")
    abline(v = input$testex, col = "blue")
    legend("topright", lty = c(1,1,1), col = c("red", "green", "blue"), c("media pop.", "media amos.", "valor de X"))
  })
  hipotese = reactive({
    if (input$altx == "media = X"){
      (t.test(rnorm(input$nx,input$mediax,input$varx**0.5),mu = input$testex,alternative = "two.sided"))$p.value
    } else if (input$altx == "media < X"){
      (t.test(rnorm(input$nx,input$mediax,input$varx**0.5),mu = input$testex,alternative = "greater"))$p.value
    } else if (input$altx == "media > X"){
      (t.test(rnorm(input$nx,input$mediax,input$varx**0.5),mu = input$testex,alternative = "less"))$p.value
    }
  })
  output$resx = renderText({
    if (hipotese() > input$sigx/100){
      paste0("A hipotese nula nao foi rejeitada. P-valor = ",round(hipotese(),2))
    } else{
      paste0("A hipotese nula foi rejeitada. P-valor = ",round(hipotese(),2))
    }
  })
  output$msg = renderText("Observe que nem sempre que a media amostral for diferente do valor comparado o teste indicara essa diferenca! Isso ocorre pois a amostra nao foi uma evidencia tao forte para rejeitar a hipotese nula. Observe tambem que amostras maiores fornecem curvas mais semelhantes a uma distribuicao normal e tendem a fornecer resultados mais precisos.")
}
shinyApp(ui,server)