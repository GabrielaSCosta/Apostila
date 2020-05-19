library(shinythemes)
library(shiny)
library(shinydashboard)
library(shinyWidgets) 
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
  body = dashboardBody(
    tabItems(
      tabItem(
        tabName = "que",
        tabBox(
          title = "O que sao os testes de hipotese?",
          tabPanel(
          "Introducao",
          textOutput("intro")
          ),
          tabPanel(
          "Pressupostos",
          "Para que o resultado do teste seja confiavel, existem alguns pressupostos que devem ser atendidos. Caso a amostra seja pequena (menor que 30) e necessario que ela siga uma distribuicao aproximadamente normal. Entretanto, quando a amostra e grande, esse pressuposto nao se faz necessario"
          )
        )
      ),
      tabItem(
        tabName = "uma",
        tabBox(
          title = "Uma populacao",
          tabPanel(
            "Introducao",
            "O teste de hipotese para a media de uma populacao tem o objetivo de concluir se a media populacional e maior, menor ou diferente de um determinado valor X."
            ),
          tabPanel(
            "Exemplo",
            "Imagine que voce e o dono de uma fabrica de parafusos. Nesse contexto, imagine que voce deseja descobrir se os parafusos produzidos em sua fabrica estao seguindo as normas do padrao internacional. Seria inviavel medir todos os parafusos produzidos, uma vez que esse processo seria caro e muito demorado. Sendo assim, uma solucao viavel e selecionar uma amostra de parafusos e realizar um teste de hipotese para a media da populacao."
          ),
          tabPanel(
            "Faca o teste!",
            "Essa aba gera amostras aleatorias com distribuicao normal e realiza o teste de hipotese para cada amostra. Defina os parametros como quiser!",
            numericInput("media", "defina a media da distribuicao:", value = 0),
            numericInput("var", "defina a variancia da distribuicao:", value = 1, min = 0),
            sliderInput("amostra", "defina quantas amostras deseja gerar:", value = 10, min = 1, max = 1000),
            numericInput("n", "defina o tamanho das amostras:", value = 10, min = 1),
            selectInput("alt", "defina a hipotese nula:", choices = c("media = X","media > X","media < X"), selected = "media = X"),
            numericInput("teste", "defina o valor de X", value = 0),
            sliderInput("sig", "defina o nivel de significancia (em %):", value = 5, min = 1, max = 10),
            textOutput("res")
          )
        )
      ),
      tabItem(
        tabName = "duas",
        tabBox(
          title = "Duas populacoes",
          tabPanel(
            "Introducao",
            "O teste de hipotese para a media de duas populacoes tem o objetivo de concluir se a media de uma populacao e maior, menor ou diferente da media de outra populacao."
          ),
          tabPanel(
            "Exemplo",
            "Imagine que voce e um pesquisador e esta interessado em entender se existe diferenca entre o tamanho dos dentes de porcos do sexo masculino e feminino. Perceba que seria impossivel medir os dentes de todos os porcos do mundo. Logo, uma solucao possivel seria selecionar uma amostra de cada uma das populacoes e realizar um teste de hipotese."
          ),
          tabPanel(
            "Faca o teste!",
            "Essa aba gera pares de amostras aleatorias com distribuicao normal e realiza o teste de hipotese para cada par de amostra. Defina os parametros como quiser!",
            numericInput("media1", "defina a media da distribuicao 1:", value = 0),
            numericInput("var1", "defina a variancia da distribuicao 1:", value = 1, min = 0),
            numericInput("media2", "defina a media da distribuicao 2:", value = 0),
            numericInput("var2", "defina a variancia da distribuicao 2:", value = 1, min = 0),
            sliderInput("amostras", "defina quantos pares de amostras deseja gerar:", value = 10, min = 1, max = 1000),
            numericInput("ns", "defina o tamanho das amostras:", value = 10, min = 1),
            selectInput("alts", "defina a hipotese nula:", choices = c("media1 = media2","media1 > media2","media1 < media2"), selected = "media1 = media2"),
            sliderInput("sigs", "defina o nivel de significancia (em %):", value = 5, min = 1, max = 10),
            textOutput("ress")          
          )
        )
      )
    )
  ), 
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Introducao",
        tabName = "que"
      ),
      menuItem(
        "Uma populacao",
        tabName = "uma"
      ),
      menuItem(
        "Duas populacoes",
        tabName = "duas"
      )
    )
  )
)
server <- function(input,output,session){
output$intro = renderText("Em muitas situacoes praticas, e comum que tenhamos que decidir se uma afirmacao a respeito de determinado assunto e verdadeira ou nao. No contexto estatistico, as afirmacoes sao chamadas de hipoteses, e o procedimento de tomada de decisao e chamado de teste de hipotese. Ou seja, uma hipotese estatistica e uma afirmacao sobre os parametros de uma ou mais populacoes.")
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
}
shinyApp(ui,server)