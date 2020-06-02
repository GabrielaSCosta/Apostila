if(!require(shiny)){install.packages("shiny");library(shiny)} 
if(!require(tidyverse)){install.packages("tidyverse");library(tidyverse)} 
if(!require(plotly)){install.packages("plotly");library(plotly)} 


ui <- fluidPage(
  titlePanel("Distribuições de probabilidade"),
  helpText("Este app foi desenvolvido em maio de 2020 por Gabriela Costa, Matheus Gonzaga e Vitor Batista,
    monitores de estatística do
           projeto de monitoria de graduação da UFMG."),
  helpText("Nele, você escolhe uma distribuição de probabilidade e seus parâmetros
             para que seja gerada uma amostra dela, de tamanho também mutável.
             A partir da amostra, criamos um histograma (no painel à direita), com número
             de classes a sua escolha, e mostramos a densidade esperada para o histograma
             em cada ponto(isto é, o valor da função densidade de probabilidade ou a probabilidade
             naquele ponto), para que possamos verificar o quão próximo está o histograma.
             Observe que, quanto maior a amostra, mais próximos tendem a ser o hitograma e
             essa função. Também colocamos, como barras verticais, a média da nossa amostra
             e a média da distribuição(que é o valor esperado da média amostral). Abaixo do gráfico
             está escrita a distância entre a média populacional e a amostral para a amostra em questão.
             Observe que quanto maior o tamanho das amostras, menor tende a ser essa distância."),
  helpText("Observação: Utilizar amostras muito grandes ou criar muitas classes
    para o histograma pode ocasionar em travamentos"),
  sidebarLayout(
    sidebarPanel(
      selectInput('dist','Escolha a distribuição de probabilidade',c("Normal","Uniforme",'Exponencial','Poisson','Binomial',
                                                                     'Geometrica')),
      conditionalPanel(condition="input.dist=='Normal'",
                       numericInput("mean", "Média da população", value = 0),
                       numericInput("sd", "Desvio padrão da população", min = 0, value = 1)
      ),
      conditionalPanel(condition="input.dist=='Uniforme'", 
                       numericInput("min", "Mínimo", value = 0),
                       numericInput("max", "Máximo", value = 1)
      ),
      conditionalPanel(condition="input.dist=='Poisson'", 
                       numericInput("rate", "Lambda", value = 1,min=0)
                       
      ),
      conditionalPanel(condition="input.dist=='Geometrica'", 
                       numericInput("p", "p", value = 0.5),min=0,max=1),
      
      conditionalPanel(condition="input.dist=='Binomial'", 
                       numericInput("trials", "Tentativas", value = 1,min=0),
                       numericInput("q", "p", value = 0.5,min=0,max=1)
      ),
      
      conditionalPanel(condition="input.dist=='Exponencial'",
                       numericInput("lambda", "Lambda", value = 1, min = 0.0001)
      )
      ,
      
      numericInput('n','Defina o tamanho da amostra',value=80),
      sliderInput('class','Defina o número de classes do histograma',value=10,min=2,max=400)
    ),
    mainPanel(
      plotlyOutput("plot",height="750px"),
      textOutput("text"),
      textOutput("text2"),
      textOutput("text3"),
      textOutput("text0")
      
      
    )
  )
)


server <- function(input,output,session){
  
  
  
  Amostra <-reactive({switch(input$dist,Normal=rnorm(input$n,mean=input$mean,sd=input$sd),
                             Uniforme=runif(input$n,min=input$min,max=input$max),
                             Exponencial=rexp(input$n,rate=input$lambda),
                             Poisson=rpois(input$n,lambda=input$rate),
                             Geometrica=rgeom(input$n,prob=input$p),
                             Binomial=rbinom(input$n,size=input$trials,prob=input$q)
  )})
  Media <- reactive({mean(Amostra())})
  MediaReal <-reactive({switch(input$dist,Normal=input$mean,
                               Uniforme=(input$min+input$max)/2,
                               Exponencial=1/input$lambda,
                               Poisson=input$rate,
                               Geometrica=(1/input$p)-1,
                               Binomial=input$trials*input$q
  )})
  
  dafr <- reactive({data.frame(X=Amostra(),MediaAmos=Media(),MediaPop=MediaReal())})
  densidade <- reactive({data.frame(
    x=switch(input$dist,
             Normal=seq(input$mean-5*input$sd
                        ,input$mean+5*input$sd,
                        length=5000),
             Uniforme=seq(input$min,input$max,length=5000),
             Exponencial=seq(0,6/input$lambda,length=5000),
             Poisson=0:6*input$rate,
             Geometrica=0:6%/%input$p,
             Binomial=0:input$trials
    )
    ,y=switch(input$dist,Normal=unlist(lapply(seq(input$mean-5*input$sd
                                                  ,input$mean+5*input$sd,
                                                  length=5000),dnorm,
                                              mean=input$mean,sd=input$sd)),
              Uniforme=unlist(lapply(seq(input$min,input$max,length=5000),
                                     dunif,
                                     min=input$min,max=input$max)),
              Exponencial=unlist(lapply(seq(0,6/input$lambda,length=5000),dexp,
                                        rate=input$lambda)),
              Poisson=unlist(lapply(0:6*input$rate,dpois,
                                    lambda=input$rate)),
              Geometrica=unlist(lapply(0:6%/%input$p,
                                       dgeom,
                                       prob=input$p)),
              Binomial=unlist(lapply(0:input$trials,dbinom,
                                     size=input$trials,prob=input$q)))
  )})
  
  
  geom <- reactive({
    denst <- densidade()
    switch(input$dist,
           Normal= geom_line(data=denst,aes(x=x,y=y)),
           Uniforme=geom_line(data=denst,aes(x=x,y=y)),
           Exponencial=geom_line(data=denst,aes(x=x,y=y)),
           Poisson=geom_point(data=denst,aes(x=x,y=y),size=1.5),
           Geometrica=geom_point(data=denst,aes(x=x,y=y),size=1.5),
           Binomial=geom_point(data=denst,aes(x=x,y=y),size=1.5)
           
           
    )
  })
  
  
  output$plot <- renderPlotly({
    df <- dafr()
    denst <- densidade()
    camada <- geom()
    ggplot()+geom_histogram(data=df,aes(x=X,y=..density..),fill='gray60',bins=input$class)+
      geom_vline(data=df,aes(xintercept=MediaPop),col='navyblue',size=0.5)+
      geom_vline(data=df,aes(xintercept=MediaAmos),col='darkred',size=0.5)+
      camada+ylab('Densidade/Probabilidade')+
      labs(title='Histograma da distribuição')+theme(legend.justification = 'right')+
      theme_minimal()
  })
  output$text0 <- renderText(paste("A distância entre a média amostral e 
                                 a média populacional é de ",
                                   round(abs(MediaReal()-Media()),2)))
  output$text <- renderText("Linha azul: média da população")
  output$text2 <- renderText ("Linha vermelha: média amostral")
  output$text3 <- renderText ("Linha preta/Pontos pretos: Densidade/Probabilidade da
                            distribuição")
  
}


shinyApp(ui=ui,server=server)

