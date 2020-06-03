library(shiny)
library(shinydashboard)
ui <- dashboardPage(
    header = dashboardHeader(
        dropdownMenu(type = "messages",
                     messageItem(
                         from = "Monitores",
                         message = "Quer aprender mais? Clique aqui!",
                         href = "http://www.est.ufmg.br/portal/"
                     )
        )
    ),
    sidebar = dashboardSidebar(
        sidebarMenu(
            menuItem(
                "Introdução",
                tabName = "int"
            ),
            menuItem(
                "Uma população",
                tabName = "uma"
            ),
            menuItem(
                "Duas populações",
                tabName = "duas"
            ),
            menuItem(
                "Visualização gráfica",
                tabName = "graf"
            )
        )
    ),
    body = dashboardBody(
        tabItems(
            tabItem(
                tabName = "int",
                titlePanel("Teste de hipóteses"),
                sidebarLayout(position = "right",
                            sidebarPanel(
                            strong("Exemplos de hipóteses nulas:"),
                            p("1 - A produtividade média de café no estado de Minas Gerais é de 4000 kg/ha."),
                            p("2 - A proporção de parafusos defeituosos na fábrica X é 3%."),
                            p("3 - A campanha publicitária utilizada produziu efeito positivo nas vendas.")
                            ),
                              mainPanel(
                                        p("Um teste de hipóteses é basicamente uma metodologia estatística que nos auxilia a tomar decisões sobre uma ou mais populações baseado na informação obtida da amostra, ou seja, nos permite verificar se os dados amostrais trazem evidência que apoie ou não uma hipótese estatística formulada."), 
                                        p("As duas hipóteses complementares em um teste de hipóteses são chamadas de hipótese nula e hipótese alternativa."),
                                        withMathJax(helpText('$$1 - Hipótese\\ nula\\ (H_{0}):$$')),
                                        p("É uma afirmação sobre um parâmetro de uma amostra que deve ser testada. Obs.: A igualdade sempre estará nessa hipótese.", align = "center"),
                                        withMathJax(helpText('$$2 - Hipótese\\ alternativa\\ (H_{1}):$$')),
                                        p("É uma afirmação que contraria a hipótese nula.", align = "center"),
                                        br(),
                                        p("Ao final do teste, temos duas opções:"),
                                        "- Rejeitar a hipótese nula",
                                        p("- Não rejeitar a hipótese nula"),
                                        br(),
                                        p("Nesse aplicativo apresentaremos os testes de hipótese para a média populacional.")
                                        )
                )
            ),
            tabItem(
                tabName = "uma",
                titlePanel("Teste de hipóteses: Uma população"),
                tabBox(width = 12,
                    tabPanel(
                        "O que é?",
                        "Os testes de hipótese para a média de uma população têm como objetivo inferir sobre a média de uma população de interesse a partir das evidências fornecidas pela amostra. Isso é feito por meio de comparações entre esse parâmetro e um valor escalar. Assim, temos três formas de formular as hipóteses:",
                        withMathJax(helpText('$$H_{0}:\\ \\mu\\ =\\ \\theta$$')),
                        withMathJax(helpText('$$H_{1}:\\ \\mu\\ \\neq\\ \\theta$$')),
                        p("ou", align = "center"),
                        withMathJax(helpText('$$H_{0}:\\ \\mu\\ \\leqslant\\ \\theta$$')),
                        withMathJax(helpText('$$H_{1}:\\ \\mu\\ >\\ \\theta$$')),
                        p("ou", align = "center"),
                        withMathJax(helpText('$$H_{0}:\\ \\mu\\ \\geqslant\\ \\theta$$')),
                        withMathJax(helpText('$$H_{1}:\\ \\mu\\ <\\ \\theta$$'))
                    ),
                    tabPanel(
                        "Como fazer?",
                        p("O primeiro passo é calcular a estatística de teste. Esse valor mede a discrepância entre o que foi observado na amostra e o que seria esperado caso a hipótese nula fosse verdadeira."),
                        strong("Variância populacional conhecida:"),
                        withMathJax(helpText('$$Z\\ =\\ \\frac{\\bar{x}\\ -\\ \\theta}{\\sigma/\\sqrt{n}}$$')),
                        withMathJax(helpText('$$\\bar{x}\\ =\\ média\\ amostral$$')),
                        withMathJax(helpText('$$\\sigma\\ =\\ desvio\\ padrão\\ populacional$$')),
                        withMathJax(helpText('$$n\\ =\\ tamanho\\ da\\ amostra$$')),
                        p("Nesse caso, a estatística de teste se baseará na distribuição Normal padronizada, ou seja, média igual a 0 e variância igual a 1."),
                        strong("Variância populacional desconhecida:"),
                        withMathJax(helpText('$$t\\ =\\ \\frac{\\bar{x}\\ -\\ \\theta}{s/\\sqrt{n}}$$')),
                        withMathJax(helpText('$$\\bar{x}\\ =\\ média\\ amostral$$')),
                        withMathJax(helpText('$$s\\ =\\ desvio\\ padrão\\ amostral$$')),
                        withMathJax(helpText('$$n\\ =\\ tamanho\\ da\\ amostra$$')),
                        p("Nesse caso, a estatística de teste se baseará na distribuição t de Studente com n - 1 graus de liberdade."),
                        br(),
                        p('Valor alto para a estatística de teste é indicação de que a hipótese nula não é verdadeira. Assim, rejeita-se a hipótese nula se o valor da estatística de teste é "grande".')
                    ),
                    tabPanel("Visualize",
                        p("Essa aba gera amostras aleatórias usando a função rnorm() e realiza testes de hipótese para a média de uma população em cada uma das amostras usando a função t.test(). Você pode mudar os parâmetros das funções para observar como isso impacta no resultado dos testes. Aviso: Gerar amostras muito grandes pode ocasionar em travamentos."),
                        numericInput("media", "Defina a média da distribuição:", value = 0),
                        numericInput("var", "Defina a variância da distribuição:", value = 1, min = 1),
                        sliderInput("amostra", "Defina quantas amostras deseja gerar:", value = 10, min = 1, max = 1000),
                        numericInput("n", "Defina o tamanho das amostras:", value = 10, min = 2),
                        selectInput("alt", "Defina a hipótese nula:", choices = c("média = X","média >= X","média <= X"), selected = "média = X"),
                        numericInput("teste", "Defina o valor de X:", value = 0),
                        sliderInput("sig", "Defina o nível de significância (em %):", value = 5, min = 1, max = 10),
                        textOutput("res")
                    )
                )
            ),
            tabItem(
                tabName = "duas",
                titlePanel("Teste de hipóteses: Duas populações"),
                tabBox(width = 12,
                    tabPanel(
                        "O que é?",
                        "Os testes de hipótese para a média de duas populações têm como objetivo inferir sobre a média de duas populações de interesse a partir das evidências fornecidas pelas amostras. Isso é feito por meio de comparações entre a média dos dois grupos. Assim, temos três formas de formular as hipóteses:",
                        withMathJax(helpText('$$H_{0}:\\ \\mu_{1}\\ -\\ \\mu_{2}\\ =\\ \\Delta_{0}$$')),
                        withMathJax(helpText('$$H_{1}:\\ \\mu_{1}\\ -\\ \\mu_{2}\\ \\neq\\ \\Delta_{0}$$')),
                        p("ou", align = "center"),
                        withMathJax(helpText('$$H_{0}:\\ \\mu_{1}\\ -\\ \\mu_{2}\\ \\leqslant\\ \\Delta_{0}$$')),
                        withMathJax(helpText('$$H_{1}:\\ \\mu_{1}\\ -\\ \\mu_{2}\\ >\\ \\Delta_{0}$$')),
                        p("ou", align = "center"),
                        withMathJax(helpText('$$H_{0}:\\ \\mu_{1}\\ -\\ \\mu_{2}\\ \\geqslant\\ \\Delta_{0}$$')),
                        withMathJax(helpText('$$H_{1}:\\ \\mu_{1}\\ -\\ \\mu_{2}\\ <\\ \\Delta_{0}$$'))
                    ),
                    tabPanel(
                        "Amostras independentes",
                        p("Nesse caso, a amostra retirada de uma população não tem nenhuma ligação com a amostra retirada da outra população."),
                        strong("Variâncias conhecidas:"),
                        withMathJax(helpText('$$Z\\ =\\ \\frac{(\\bar{X}_{1}\\ -\\ \\bar{X}_{2})\\ -\\ \\Delta_{0}}{\\sqrt{\\frac{\\sigma^2_{1}}{n_{1}}\\ +\\ \\frac{\\sigma^2_{2}}{n_{2}}}}$$')),
                        p("Essa estatística de teste segue uma distribuição Normal padronizada, ou seja, com média igual a zero e variância igual a 1."),
                        p(strong("Variâncias desconhecidas:")),
                        div("- Se podemos supor variâncias iguais:", style = "color:red"),
                        withMathJax(helpText('$$T\\ =\\ \\frac{(\\bar{X}_{1}\\ -\\ \\bar{X}_{2})\\ -\\ \\Delta_{0}}{\\sqrt{\\frac{S^2_{p}}{n_{1}}\\ +\\ \\frac{S^2_{p}}{n_{2}}}}$$')),
                        withMathJax(helpText('$$S^2_{p}\\ =\\ \\frac{(n_{1}\\ -\\ 1)S^2_{1}\\ +\\ (n_{2}\\ -\\ 1)S^2_{2}}{n_{1}\\ +\\ n_{2}\\ -\\ 2}$$')),
                        p("Essa estatística de teste segue uma distribuição t de Student com n1 + n2 - 2 graus de liberdade."),
                        div("- Se supomos variâncias diferentes:", style = "color:red"),
                        withMathJax(helpText('$$T\\ =\\ \\frac{(\\bar{X}_{1}\\ -\\ \\bar{X}_{2})\\ -\\ \\Delta_{0}}{\\sqrt{\\frac{S^2_{1}}{n_{1}}\\ +\\ \\frac{S^2_{2}}{n_{2}}}}$$')),
                        p("Essa estatística de teste segue uma distribuição t de Student com v graus de liberdade:"),
                        withMathJax(helpText('$$v\\ =\\ \\frac{( \\frac{S^2_{1}}{n_{1}}\\ +\\ \\frac{S^2_{2}}{n_{2}} )^2}{\\frac{( S^2_{1}/n_{1} )^2}{n_{1}\\ -\\ 1}\\ +\\ \\frac{( S^2_{2}/n_{2} )^2}{n_{2}\\ -\\ 1}}\\ -\\ 2$$')),
                        p(strong("Notação:"),align = "center"),
                        withMathJax(helpText('$$\\bar{X}_{k}\\ =\\ média\\ da\\ amostra\\ k$$')),
                        withMathJax(helpText('$$\\sigma^2_{k}\\ =\\ variância\\ da\\ população\\ k$$')),
                        withMathJax(helpText('$$S^2_{k}\\ =\\ variância\\ da\\ amostra\\ k$$')),
                        withMathJax(helpText('$$n_{k}\\ =\\ tamanho\\ da\\ amostra\\ k$$'))
                    ),
                    tabPanel(
                        "Amostras dependentes",
                        "text"
                    ),
                    tabPanel("Visualize",
                        p("Essa aba gera pares de amostras aleatórias usando a função rnorm() e realiza testes de hipótese para a média de duas populações em cada um dos pares de amostras usando a função t.test(). Você pode mudar os parâmetros das funções para observar como isso impacta no resultado dos testes. Aviso: Gerar amostras muito grandes pode ocasionar em travamentos."),
                        numericInput("media1", "Defina a média da distribuição 1:", value = 0),
                        numericInput("var1", "Defina a variância da distribuição 1:", value = 1, min = 1),
                        numericInput("media2", "Defina a média da distribuição 2:", value = 0),
                        numericInput("var2", "Defina a variância da distribuição 2:", value = 1, min = 1),
                        sliderInput("amostras", "Defina quantos pares de amostras deseja gerar:", value = 10, min = 1, max = 1000),
                        numericInput("ns", "Defina o tamanho das amostras:", value = 10, min = 2),
                        selectInput("alts", "Defina a hipótese nula:", choices = c("média 1 = média 2","média 1 >= média 2","média 1 <= média 2"), selected = "média 1 = média 2"),
                        sliderInput("sigs", "Defina o nível de significância (em %):", value = 5, min = 1, max = 10),
                        textOutput("ress")  
                    )
                )
            ),
            tabItem(
                tabName = "graf",
                titlePanel("Testes de hipóteses: Visualização gráfica"),
                p("Essa aba gera uma amostra aleatória usando a função rnorm() e mostra graficamente o que acontece com ela. Você pode mudar os parâmetros da função e observar como isso impacta nos resultados. Aviso: Gerar amostras muito grandes pode ocasionar em travamentos."),
                sidebarLayout(
                    sidebarPanel(
                        numericInput("mediax", "Defina a média da distribuição:", value = 1),
                        numericInput("varx", "Defina a variância da distribuição:", value = 1, min = 1),
                        numericInput("nx", "Defina o tamanho da amostra:", value = 10, min = 2),
                        selectInput("altx", "Defina a hipótese nula:", choices = c("média = X","média > X","média < X"), selected = "média = X"),
                        numericInput("testex", "Defina o valor de X:", value = 0),
                        sliderInput("sigx", "Defina o nivel de significância (em %):", value = 5, min = 1, max = 10)
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
    pvals <- reactive({
        if (input$alt == "média = X"){
            replicate(input$amostra, t.test(rnorm(input$n,input$media,input$var**0.5),mu = input$teste,alternative = "two.sided")$p.value)
        } else if (input$alt == "média <= X"){
            replicate(input$amostra, t.test(rnorm(input$n,input$media,input$var**0.5),mu = input$teste,alternative = "greater")$p.value)
        } else if (input$alt == "média >= X"){
            replicate(input$amostra, t.test(rnorm(input$n,input$media,input$var**0.5),mu = input$teste,alternative = "less")$p.value)
        }
    })
    rejr <- reactive({length(which(pvals() < (input$sig/100)))})
    output$res = renderText({
        rej <- rejr()
        paste0("A hipótese nula foi rejeitada em ",round((rej/input$amostra)*100,1), "% das vezes (", rej, " de ", input$amostra, ").")
    })
    pvalss <- reactive({
        if (input$alts == "média 1 = média 2"){
            replicate(input$amostras, t.test(x = rnorm(input$ns,input$media1,input$var1**0.5), y = rnorm(input$ns,input$media2,input$var2**0.5),alternative = "two.sided")$p.value)
        } else if (input$alts == "média 1 <= média 2"){
            replicate(input$amostras, t.test(x = rnorm(input$ns,input$media1,input$var1**0.5), y = rnorm(input$ns,input$media2,input$var2**0.5),alternative = "greater")$p.value)
        } else if (input$alts == "média 1 >= média 2"){
            replicate(input$amostras, t.test(x = rnorm(input$ns,input$media1,input$var1**0.5), y = rnorm(input$ns,input$media2,input$var2**0.5),alternative = "less")$p.value)
        }
    })
    rejrs <- reactive({length(which(pvalss() < (input$sigs/100)))})
    output$ress = renderText({
        rejs <- rejrs()
        paste0("A hipótese nula foi rejeitada em ",round((rejs/input$amostras)*100,1), "% das vezes (", rejs, " de ", input$amostras, ").")
    })
    output$grafico = renderPlot({
        amostrax = rnorm(input$nx, input$mediax, input$varx**0.5)
        plot(density(amostrax), main = "Amostra gerada", ylab = "Densidade")
        abline(v = input$mediax, col = "red")
        abline(v = mean(amostrax), col = "green")
        abline(v = input$testex, col = "blue")
        legend("topright", lty = c(1,1,1), col = c("red", "green", "blue"), c("média pop.", "média amos.", "valor de X"))
    })
    hipotese = reactive({
        if (input$altx == "média = X"){
            (t.test(rnorm(input$nx,input$mediax,input$varx**0.5),mu = input$testex,alternative = "two.sided"))$p.value
        } else if (input$altx == "média <= X"){
            (t.test(rnorm(input$nx,input$mediax,input$varx**0.5),mu = input$testex,alternative = "greater"))$p.value
        } else if (input$altx == "média >= X"){
            (t.test(rnorm(input$nx,input$mediax,input$varx**0.5),mu = input$testex,alternative = "less"))$p.value
        }
    })
    output$resx = renderText({
        if (hipotese() > input$sigx/100){
            paste0("A hipótese nula não foi rejeitada. P-valor = ",round(hipotese(),2))
        } else{
            paste0("A hipótese nula foi rejeitada. P-valor = ",round(hipotese(),2))
        }
    })
    output$msg = renderText("Observe que nem sempre que a média amostral for diferente do valor comparado o teste indicará essa diferença! Isso ocorre pois a amostra não foi uma evidência tão forte para rejeitar a hipótese nula. Observe também que amostras maiores fornecem curvas mais semelhantes a uma distribuição normal e tendem a fornecer resultados mais precisos.")
}
shinyApp(ui,server)
