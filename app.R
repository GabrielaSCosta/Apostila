library(shiny)
library(shinydashboard)
library(ggplot2)
options(encoding = "UTF-8")

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
                "Exercícios resolvidos",
                tabName = "resol"
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
                tabBox(width = 12,
                       tabPanel("O que é?",
                                sidebarLayout(position = "right",
                                              sidebarPanel(
                                                  p(strong("Exemplos de hipóteses nulas:")),
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
                       tabPanel(
                           "Erro tipo 1 e tipo 2",
                           p("No contexto da tomada de decisão entre rejeitar a hipótese nula ou não, existem 4 situações possíveis. São elas:"),
                           "- Rejeitar a hipótese nula sendo que ela realmente é falsa. (Decisão correta)",
                           p("- Não rejeitar a hipótese nula sendo que ela realmente é verdadeira. (Decisão correta)"),
                           "- Rejeitar a hipótese nula sendo que na verdade ela é verdadeira. (Erro tipo 1)",
                           p("- Não rejeitar a hipótese nula sendo que na verdade ela é falsa. (Erro tipo 2)"),
                           p("Na prática não é possível saber se algum dos erros foi cometido, mas é possível avaliar a probabilidade de cometer algum dos erros."),
                           p("A probabilidade de cometer o erro tipo 1 recebe o nome de nível de significância, sendo representada pela letra grega alpha:"),
                           withMathJax(helpText('$$ \\alpha\\ =\\ P(Rejeitar\\ H_{0}\\ |\\ H_{0}\\ é\\ verdadeira)$$')),
                           p("A tomada de decisão irá depender do nível de significância e da chamada Estatística de Teste. Esse valor mede a discrepância entre o que foi observado na amostra e o que seria esperado caso a hipótese nula fosse verdadeira. Seu cálculo varia de acordo com as condições da amostra e será explicado nas próximas abas."),
                           p('Valor alto para a estatística de teste é indicação de que a hipótese nula não é verdadeira. Logo, rejeita-se a hipótese nula quando o valor da estatística de teste é "grande".'),
                           p("Para decidir, o valor da estatística de teste é comparado com alguma distribuição de probabilidade, que depende de cada caso."),
                           p("As próximas duas abas explicarão as duas maneiras de tomar a decisão em um teste de hipóteses."),
                           br(),
                           p("(Obs.: Se você fizer o teste corretamente, as duas formas de decisão levarão à mesma conclusão. Portanto, basta escolher uma das formas)")
                       ),
                       tabPanel(
                           "Região crítica",
                           p("Nesse caso, são os valores para a estatística de teste que conduzem a rejeição (ou não) da hipótese nula."),
                           p("Para isso, definimos um intervalo de valores para a estatística de teste em que não rejeitaremos a hipótese nula. Esse intervalo é construído excluindo uma porcentagem dos dados mais extremos na distribuição de probabilidade, levando em conta o nível de significância e a hipótese nula."),
                           p("Exemplo: Se definimos um nível de significância de 0,05 (5%) para o teste, para construir a região crítica devemos excluir 5% dos valores mais extremos da distribuição de probabilidade. Se for um teste bilateral, excluímos 2,5% de cada lado. Se for unilateral, excluímos os 5% do mesmo lado"),
                           p("Se a estatística de teste estiver entre os 5% mais extremos (usando o exemplo como base), rejeitamos a hipótese nula. Caso contrário, não rejeitamos. Os casos seriam:"),
                           withMathJax(helpText('$$H_{0}:\\ \\mu\\ =\\ \\theta$$')),
                           sidebarLayout(
                               mainPanel(
                                   plotOutput("bila")
                               ),
                               sidebarPanel(
                                   p("Rejeitamos a hipótese nula tanto para valores muito negativos quanto muito positivos, pois estamos testando se a média é igual certo valor.")
                               )
                           ),
                           withMathJax(helpText('$$H_{0}:\\ \\mu\\ \\geqslant\\ \\theta$$')),
                           sidebarLayout(
                               mainPanel(
                                   plotOutput("unie")
                               ),
                               sidebarPanel(
                                   p("Rejeitamos a hipótese nula apenas para valores muito negativos, pois valores positivos seriam uma evidência de que a média populacional é maior ou igual ao valor conhecido.")
                               )
                           ),
                           withMathJax(helpText('$$H_{0}:\\ \\mu\\ \\leqslant\\ \\theta$$')),
                           sidebarLayout(
                               mainPanel(
                                   plotOutput("unid")    
                               ),
                               sidebarPanel(
                                   p("Rejeitamos a hipótese nula apenas para valores muito positivos, pois valores negativos seriam uma evidência de que a média populacional é menor ou igual ao valor conhecido.")
                               )
                           )
                       ),
                       tabPanel(
                           "P-valor",
                           p("O P-valor consiste na probabilidade de se encontrar um valor para a estatística de teste que seja no mínimo tão extremo quanto o observado, supondo que a hipótese nula seja verdadeira."),
                           p("Quando se obtém um P-valor muito pequeno, existem duas possibilidades:"),
                           "- Um evento extremamente raro ocorreu.",
                           p("- A hipótese nula não é verdadeira. (Mais provável)"),
                           p("Assim, rejeitamos a hipótese nula quando o P-valor é menor que o nível de significância."),
                           p("Quanto menor o P-valor, maior a evidência amostral de que a hipótese nula não é verdadeira."),
                           p(strong("Cálculo:")),
                           p("Se o teste for unilateral, você deve olhar na tabela quantos % da distribuição se encontram à esquerda da estatística de teste (no caso de um teste unilateral à esquerda) ou quantos % se encontram à direita da estatística de teste (no caso de um teste unilateral à direita)."),
                           p("Se for um teste bilateral, você deve olhar para qual dos lados sobrou uma menor quantidade da distribuição, olhar na tabela quantos % isso corresponde e multiplicar por 2, pois como se trata de um teste bilateral rejeitamos a hipótese nula tanto para os valores muito positivos quanto para os muito negativos")
                       )
                )
            ),
            tabItem(
                tabName = "uma",
                titlePanel("Teste de hipóteses: Uma população"),
                tabBox(width = 12,
                       tabPanel(
                           "O que é?",
                           p("Os testes de hipótese para a média de uma população têm como objetivo inferir sobre a média de uma população de interesse a partir das evidências fornecidas pela amostra. Isso é feito por meio de comparações entre esse parâmetro e um valor conhecido. Assim, temos três formas de formular as hipóteses:"),
                           br(),
                           p(strong("Teste bilateral:")),
                           withMathJax(helpText('$$H_{0}:\\ \\mu\\ =\\ \\theta$$')),
                           withMathJax(helpText('$$H_{1}:\\ \\mu\\ \\neq\\ \\theta$$')),
                           p(strong("Teste unilateral à direita:")),
                           withMathJax(helpText('$$H_{0}:\\ \\mu\\ \\leqslant\\ \\theta$$')),
                           withMathJax(helpText('$$H_{1}:\\ \\mu\\ >\\ \\theta$$')),
                           p(strong("Teste unilateral à esquerda:")),
                           withMathJax(helpText('$$H_{0}:\\ \\mu\\ \\geqslant\\ \\theta$$')),
                           withMathJax(helpText('$$H_{1}:\\ \\mu\\ <\\ \\theta$$')),
                           withMathJax(helpText('$$(\\theta\\ =\\ valor\\ conhecido)$$'))
                       ),
                       tabPanel(
                           "Estatística de Teste",
                           p(strong("Suposições:")),
                           "- Amostra foram aleatoriamente escolhidas",
                           p("- População tem distribuição Normal"),
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
                           p("Nesse caso, a estatística de teste se baseará na distribuição t de Student com n - 1 graus de liberdade."),
                       ),
                       tabPanel("Visualize",
                                sidebarLayout(
                                    sidebarPanel(
                                        p("Essa aba gera amostras aleatórias usando a função rnorm() e realiza testes de hipótese para a média de uma população em cada uma das amostras. Você pode mudar os parâmetros das funções para observar como isso impacta no resultado dos testes. Aviso: Gerar amostras muito grandes pode ocasionar em travamentos."),
                                        br(),
                                        p(strong("Resultado:")),
                                        textOutput("res")
                                    ),
                                    mainPanel(
                                        numericInput("media", "Defina a média da população:", value = 0),
                                        numericInput("var", "Defina a variância da população:", value = 1, min = 1),
                                        sliderInput("amostra", "Defina quantas amostras deseja gerar:", value = 10, min = 1, max = 1000),
                                        numericInput("n", "Defina o tamanho das amostras:", value = 10, min = 2),
                                        selectInput("alt", "Defina a hipótese nula:", choices = c("média = X","média >= X","média <= X"), selected = "média = X"),
                                        numericInput("teste", "Defina o valor de X:", value = 0),
                                        sliderInput("sig", "Defina o nível de significância (em %):", value = 5, min = 1, max = 10)
                                    )
                                )
                       )
                )
            ),
            tabItem(
                tabName = "duas",
                titlePanel("Teste de hipóteses: Duas populações"),
                tabBox(width = 12,
                       tabPanel(
                           "O que é?",
                           p("Os testes de hipótese para a média de duas populações têm como objetivo inferir sobre a média de duas populações de interesse a partir das evidências fornecidas pelas amostras. Isso é feito por meio de comparações entre a média dos dois grupos e um valor conhecido. Assim, temos três formas de formular as hipóteses."),
                           br(),
                           p(strong("Teste bilateral:")),
                           withMathJax(helpText('$$H_{0}:\\ \\mu_{1}\\ -\\ \\mu_{2}\\ =\\ \\Delta_{0}$$')),
                           withMathJax(helpText('$$H_{1}:\\ \\mu_{1}\\ -\\ \\mu_{2}\\ \\neq\\ \\Delta_{0}$$')),
                           p(strong("Teste unilateral à direita:")),
                           withMathJax(helpText('$$H_{0}:\\ \\mu_{1}\\ -\\ \\mu_{2}\\ \\leqslant\\ \\Delta_{0}$$')),
                           withMathJax(helpText('$$H_{1}:\\ \\mu_{1}\\ -\\ \\mu_{2}\\ >\\ \\Delta_{0}$$')),
                           p(strong("Teste unilateral à esquerda:")),
                           withMathJax(helpText('$$H_{0}:\\ \\mu_{1}\\ -\\ \\mu_{2}\\ \\geqslant\\ \\Delta_{0}$$')),
                           withMathJax(helpText('$$H_{1}:\\ \\mu_{1}\\ -\\ \\mu_{2}\\ <\\ \\Delta_{0}$$')),
                           withMathJax(helpText('$$(\\Delta_{0}\\ =\\ valor\\ conhecido)$$'))
                       ),
                       tabPanel(
                           "Amostras independentes",
                           p("Nesse caso, a amostra retirada de uma população não tem nenhuma ligação com a amostra retirada da outra população."),
                           p(strong("Suposições:")),
                           "- As duas populações são independentes;",
                           p("- As amostras foram aleatoriamente selecionadas em cada caso."),
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
                           "Amostras pareadas",
                           p("Nesse caso, a amostra de uma população tem alguma ligação com a amostra da outra população."),
                           p("Para calcular a estatística de teste devemos primeiramente calcular a diferença entre os pares."),
                           withMathJax(helpText('$$T\\ =\\ \\frac{\\bar{d}\\ -\\ \\Delta_{0}}{s_{d}/\\sqrt{n}}$$')),
                           withMathJax(helpText('$$\\bar{d}\\ =\\ média\\ das\\ diferenças.$$')),
                           withMathJax(helpText('$$s_{d}\\ =\\ desvio\\ padrão\\ das\\ diferenças$$')),
                           withMathJax(helpText('$$n\\ =\\ número\\ de\\ pares$$'))
                       )
                )
            ),
            tabItem(
                tabName = "resol",
                titlePanel("Teste de hipóteses: Exercícios resolvidos"),
                p("Essa aba possui alguns exercícios resolvidos para facilitar seu entendimento da matéria, mas você pode tentar fazê-los antes de olhar a resolução. Lembre-se: É essencial fazer exercícios para fixar o conteúdo aprendido."),
                tabBox(width = 12,
                       tabPanel(
                           "Exemplo 1",
                           p(strong("Enunciado:")),
                           p("Uma fábrica de automóveis anuncia que seus carros consomem, em média, 11 litros de gasolina por 100 km, com variância de 0,64 litro. Um pesquisador decidiu testar essa afirmação e analisou 35 carros dessa marca, obtendo uma média de 11,4 litros por 100 km. Usando uma significância de 5% e admitindo que o consumo tenha distribuição Normal, qual é a conclusão do teste?"),
                           p(strong("Resolução:")),
                           p("Esse é um exemplo de teste de hipóteses para a média de uma população."),
                           p("O primeiro passo é formular as hipóteses. Como queremos testar se a média é igual um valor, se trata de um teste bilateral."),
                           withMathJax(helpText('$$H_{0}:\\ \\mu\\ =\\ 11$$')),
                           withMathJax(helpText('$$H_{1}:\\ \\mu\\ \\neq\\ 11$$')),
                           p("Depois disso, devemos calcular a estatística de teste. Como conhecemos a variância populacional, esse é um caso em que usamos a estatística Z:"),
                           withMathJax(helpText('$$\\sigma\\ =\\ \\sqrt{0,64}\\ =\\ 0,8$$')),
                           withMathJax(helpText('$$Z\\ =\\ \\frac{\\bar{x}\\ -\\ \\theta}{\\sigma/\\sqrt{n}}\\ =\\ \\frac{11,4\\ -\\ 11}{0,8/\\sqrt{35}}\\ =\\ \\frac{0,4}{0,135}\\ =\\ 2,96$$')),
                           p("Esta estatística segue uma distribuição Normal padrão. Olhando a tabela da distribuição, podemos ver que para nosso nível de significância (5%), os valores crítico são -1,96 e 19,6. O P-valor é menor que 1%, ou seja, é menor que o nível de significância."),
                           p("Assim, como a estatística de teste está na região crítica, rejeitamos a hipótese nula. Em outras palavras, a partir desses dados, chegamos à conclusão de que há evidência amostral para dizermos que a média de consumo por 100 km nos carros dessa marca é diferente de 11 litros.")
                       ),
                       tabPanel(
                           "Exemplo 2",
                           p(strong("Enunciado:")),
                           p("Em uma loja de calçados, o gerente decidiu colocar uma meta de vendas. Ele informou aos vendedores que eles deveriam vender, em média, pelo menos 500 reais de calçados por dia. Algum tempo depois, ele decidiu verificar se um de seus funcionários tinha atingido a meta até o momento. Entretanto, ao verificar o banco de dados ele percebeu que só tinha informações do valor de venda para alguns dias aleatórios. Os valores eram:"),
                           withMathJax(helpText('$$(498,\\ 507,\\ 477,\\ 451,\\ 560,\\ 348,\\ 509,\\ 519,\\ 475)$$')),
                           p("Usando 5% de significância e admitindo que os valores de venda seguem distribuição Normal, há evidência de que o funcionário não cumpriu a meta até o momento?"),
                           p(strong("Resolução:")),
                           p("Esse é um exemplo de teste de hipóteses para a média de uma população. Entretanto, não conhecemos a variância populacional."),
                           p("As hipóteses são:"),
                           withMathJax(helpText('$$H_{0}:\\ \\mu\\ \\geqslant\\ 500$$')),
                           withMathJax(helpText('$$H_{1}:\\ \\mu\\ <\\ 500$$')),
                           p("(Lembre-se que o sinal de igualdade sempre estará na hipótese nula)"),
                           p("Fazendo os cálculos, notamos que a média amostral vale 482,67 reais e o desvio padrão vale 59,21. Como não conhecemos a variância populacional devemos usar a estatística de teste t:"),
                           withMathJax(helpText('$$t\\ =\\ \\frac{\\bar{x}\\ -\\ \\theta}{s/\\sqrt{n}}\\ =\\ \\frac{482,67\\ -\\ 500}{59,21/\\sqrt{9}}\\ =\\ -0,878$$')),
                           p("Esta estatística segue uma distribuição t com 8 graus de liberdade. Olhando a tabela da distribuição, podemos ver que para nosso nível de significância (5%), o valor crítico é -1,86 (Só há um valor crítico pois se trata de um teste unilateral). O P-valor é de aproximadamente 20%, ou seja, é maior que o nível de significância."),
                           p("Assim, como a estatística de teste não está na região crítica, não rejeitamos a hipótese nula. Em outras palavras, a partir desses dados, chegamos à conclusão de que não há evidência amostral para dizermos que o funcionário não cumpriu sua meta de vendas.")
                       ),
                       tabPanel(
                           "Exemplo 3",
                           p(strong("Enunciado:")),
                           p("Um pesquisador deseja comparar o tempo de frenagem para dois tipos de pneus diferentes (A e B). Para tal, ele testou o desempenho dos pneus em cinco modelos de carros diferentes (1, 2, 3, 4 e 5) e registrou na tabela abaixo, com os tempos do pneu A na primeira linha e do pneu B na segunda:"),
                           tableOutput("freio"),
                           p("Admitindo que os tempos têm distribuição Normal e usando 5% de significância, podemos concluir que o tempo de frenagem do pneu A é maior que o do pneu B??"),
                           p(strong("Resolução:")),
                           p("Trata-se de um teste de comparação de médias para duas populações diferentes com amostras pareadas. Nossas hipóteses são:"),
                           withMathJax(helpText('$$H_{0}:\\ \\mu_{A}\\ -\\ \\mu_{B}\\ \\leqslant\\ 0$$')),
                           withMathJax(helpText('$$H_{0}:\\ \\mu_{A}\\ -\\ \\mu_{B}\\ >\\ 0$$')),
                           p("O próximo passo é calcular a diferença entre os pares. Assim, chegaremos a esse conjunto de dados:"),
                           withMathJax(helpText('$$(0.2,\\ 0.1,\\ 0.3,\\ -0.2,\\ 0)$$')),
                           p("Fazendo as contas conferimos que a média das diferenças é 0,08 e o desvio padrão é 0,192. Agora basta calcular a estatística de teste e tirar a conclusão:"),
                           withMathJax(helpText('$$T\\ =\\ \\frac{\\bar{d}\\ -\\ \\Delta_{0}}{s_{d}/\\sqrt{n}}\\ =\\ \\frac{0,08\\ -\\ 0}{0,192/\\sqrt{5}}\\ =\\ 0,94$$')),
                           p("Esta estatística segue uma distribuição t com 4 graus de liberdade. Olhando a tabela da distribuição, podemos ver que para nosso nível de significância (5%), o valor crítico é 2,132 (Só há um valor crítico pois se trata de um teste unilateral). O P-valor é de 20%, ou seja, maior que o nível de significância."),
                           p("Assim, como a estatística de teste não está na região crítica, não rejeitamos a hipótese nula. Em outras palavras, a partir desses dados, chegamos à conclusão de que não há evidência amostral para dizermos que o tempo de frenagem médio do pneu do tipo A é maior que o do tipo B.")
                       ),
                       tabPanel(
                           "Exemplo 4",
                           p(strong("Enunciado:")),
                           p("Um cursinho popular deu início a aulas remotas em seu projeto e deseja avaliar o desempenho do novo modelo de ensino. Para isso, dividiram os alunos em dois grupos: Uma amostra com 20 alunos que tiveram lições por meio remoto (amostra A) e uma amostra com 20 alunos que continuaram com o ensino tradicional (amostra B). "),
                           p("Foi aplicada uma prova para fazer o teste. A amostra A obteve uma pontuação média de 72, com uma variância de 6, enquanto a amostra B teve uma média de 79 pontos, com uma variância de 4,3."),
                           p("Teste se as aulas presenciais são mais eficientes que o modelo virtual, usando uma significância de 5%."),
                           p(strong("Resolução:")),
                           p("Primeiro, devemos saber se as variâncias populacionais são iguais ou não. Para isso, usamos o teste F e encontramos que, a um nível de 5% de significância, há evidências de que as variâncias das duas populações são iguais são iguais. (Não iremos explicar o teste pois não é o foco do aplicativo)"),
                           p("As hipóteses são:"),
                           withMathJax(helpText('$$H_{0}:\\ \\mu_{B}\\ -\\ \\mu_{A}\\ \\leqslant\\ 0$$')),
                           withMathJax(helpText('$$H_{0}:\\ \\mu_{B}\\ -\\ \\mu_{A}\\ >\\ 0$$')),
                           p("Como verificamos que as variâncias são igual a estatística de teste é:"),
                           withMathJax(helpText('$$S^2_{p}\\ =\\ \\frac{(n_{A}\\ -\\ 1)S^2_{A}\\ +\\ (n_{B}\\ -\\ 1)S^2_{B}}{n_{A}\\ +\\ n_{B}\\ -\\ 2}\\ =\\ \\frac{19\\ x\\ 6\\ +\\ 19\\ x\\ 4,3}{20\\ +\\ 20\\ -\\ 2}\\ =\\ \\frac{195,7}{38}\\ =\\ 5,15$$')),
                           withMathJax(helpText('$$T\\ =\\ \\frac{(\\bar{X}_{B}\\ -\\ \\bar{X}_{A})\\ -\\ \\Delta_{0}}{\\sqrt{\\frac{S^2_{p}}{n_{A}}\\ +\\ \\frac{S^2_{p}}{n_{B}}}}\\ =\\ \\frac{79\\ -\\ 72\\ -\\ 0}{\\sqrt{\\frac{5,15}{20}\\ +\\ \\frac{5,15}{20}}}\\ = \\frac{7}{\\sqrt{\\frac{5,15}{10}}}\\ \\simeq\\ 9,755$$')),
                           p("Essa estatística de teste segue uma distribuição t com 38 graus de liberdade (20 + 20 - 2). Olhando na tabela dessa distribuição, podemos ver que o valor crítico é aproximadamente 1,684 (Só há um valor crítico pois se trata de um teste unilateral). O P-valor é muito pequeno, menor que 0,5%."),
                           p("Assim, como a estatística de teste está na região crítica, rejeitamos a hipótese nula. Em outras palavras, a partir desses dados, chegamos à conclusão de que há evidência amostral para dizermos que o ensino presencial é mais eficaz do que o ensino remoto.")
                       )
                )
            ),
            tabItem(
                tabName = "graf",
                titlePanel("Testes de hipóteses: Visualização gráfica"),
                p("Essa aba gera uma amostra aleatória usando a função rnorm() e mostra graficamente o que acontece com ela. Você pode mudar os parâmetros da função e observar como isso impacta nos resultados. Aviso: Gerar amostras muito grandes pode ocasionar em travamentos."),
                sidebarLayout(
                    sidebarPanel(
                        numericInput("mediax", "Defina a média da população:", value = 1),
                        numericInput("varx", "Defina a variância da população:", value = 1, min = 1),
                        numericInput("nx", "Defina o tamanho da amostra:", value = 10, min = 2),
                        selectInput("altx", "Defina a hipótese nula:", choices = c("média = X","média >= X","média <= X"), selected = "média = X"),
                        numericInput("testex", "Defina o valor de X:", value = 0),
                        sliderInput("sigx", "Defina o nivel de significância (em %):", value = 5, min = 1, max = 10)
                    ),
                    mainPanel(
                        p("Informações da amostra gerada:"),
                        textOutput("numero1"),
                        textOutput("numero2"),
                        br(),
                        plotOutput("grafico"),
                        br(),
                        textOutput("resx")
                    )
                )
            )
        )
    )
)
server <- function(input,output,session){
    output$bila = renderPlot({
        z <- seq(-4,4, by = .01)
        Densidade <- dnorm(z)
        decisão <- factor(rep("não rejeitar", length(z)), levels=c("não rejeitar", "rejeitar", "tambem rejeitar"))
        decisão[which(z < qnorm(.025))] <- "rejeitar"
        decisão[which(z > qnorm(.975))] <- "tambem rejeitar"
        qplot(z,Densidade, geom=c("path","area"), fill=decisão) +
            scale_fill_manual(breaks=c("não rejeitar","rejeitar"),values=c("grey40", "red", "red"))
    })
    output$unie = renderPlot({
        z <- seq(-4,4, by = .01)
        Densidade <- dnorm(z)
        decisão <- factor(rep("não rejeitar", length(z)), levels=c("não rejeitar", "rejeitar"))
        decisão[which(z < qnorm(.05))] <- "rejeitar"
        qplot(z,Densidade, geom=c("path","area"), fill=decisão) +
            scale_fill_manual(values=c("grey40", "red"))
    })
    output$unid = renderPlot({
        z <- seq(-4,4, by = .01)
        Densidade <- dnorm(z)
        decisão <- factor(rep("não rejeitar", length(z)), levels=c("não rejeitar", "rejeitar"))
        decisão[which(z > qnorm(.95))] <- "rejeitar"
        qplot(z,Densidade, geom=c("path","area"), fill=decisão) +
            scale_fill_manual(values=c("grey40", "red"))
    })
    output$freio = renderTable({
        a = matrix(c(0.6, 0.8, 1.1, 0.4, 0.5, 0.4, 0.7, 0.8, 0.6, 0.5),ncol=5)
        colnames(a) = c("1", "2", "3", "4", "5")
        a
    })
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
    amostrax <- reactive({rnorm(input$nx, input$mediax, input$varx**0.5)})
    output$numero1 = renderText(paste0("média = ", round(mean(amostrax()),2)))
    output$numero2 = renderText(paste0("variância = ", round(var(amostrax()),2)))
    output$grafico = renderPlot({
        plot(density(amostrax()), main = "Amostra gerada", ylab = "Densidade")
        abline(v = input$mediax, col = "red")
        abline(v = mean(amostrax()), col = "green")
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
}
shinyApp(ui,server)