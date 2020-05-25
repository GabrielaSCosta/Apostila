if(!require(shiny)){install.packages("shiny");library(shiny)} 
if(!require(tidyverse)){install.packages("tidyverse");library(tidyverse)} 
if(!require(plotly)){install.packages("plotly");library(plotly)} 
if(!require(DT)){install.packages("DT");library(DT)} 




ui <- fluidPage(
  titlePanel("Intervalos de confiança para médias"),
  helpText("Observação: Criar muitos intervalos ou utilizar amostras muito grandes pode 
           ocasionar em travamentos"),
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
      numericInput('amostras','Defina o número de intervalos a se fazer',value = 20),
      numericInput('n','Defina o tamanho de cada amostra',value=30),
      numericInput('conf','Defina o nível de confiança do intervalo',value=0.95,min=0,max=0.999999999999)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Grafico",plotlyOutput("plot",height="750px"),
                 textOutput("taxa_acerto"),textOutput("aviso")),
        tabPanel("Intervalos Gerados",DTOutput('table')))
      
    )
  )
)



server <- function(input,output,session){
  
  
  
  Amostras <-reactive({switch(input$dist,Normal=(lapply(rep(input$n,input$amostras),rnorm,
                                                        mean=input$mean,sd=input$sd)),
                              Uniforme=(lapply(rep(input$n,input$amostras),runif,
                                               min=input$min,max=input$max)),
                              Exponencial=(lapply(rep(input$n,input$amostras),rexp,
                                                  rate=input$lambda)),
                              Poisson=(lapply(rep(input$n,input$amostras),rpois,
                                              lambda=input$rate)),
                              Geometrica=(lapply(rep(input$n,input$amostras),rgeom,
                                                 prob=input$p)),
                              Binomial=(lapply(rep(input$n,input$amostras),rbinom,
                                               size=input$trials,prob=input$q))
                              
                              
                              
  )})
  Medias <- reactive({unlist(lapply(Amostras(),mean))})
  Desvios <- reactive({unlist(lapply(Amostras(),sd))})
  MediaReal <-reactive({switch(input$dist,Normal=input$mean,
                               Uniforme=(input$min+input$max)/2,
                               Exponencial=1/input$lambda,
                               Poisson=input$rate,
                               Geometrica=(1/input$p)-1,
                               Binomial=input$trials*input$q
  )})
  
  
  ICs <- reactive({
    data.frame(
      id=1:input$amostras,
      mean=Medias(),
      ep=-qt((1-input$conf)/2,input$n-1)*Desvios()/sqrt(input$n))
  })  
  
  output$plot <- renderPlotly({df <- ICs()
  mt <- MediaReal()
  ggplot(df,aes(x=id,y=mean))+geom_point(size=2,col='red')+
    geom_hline(aes(yintercept=mt))+geom_errorbar(aes(ymin=mean+ep,ymax=mean-ep))+
    xlab("")+ylab("Estimativa")})
  acerto <- reactive({ 
    mt <- MediaReal()
    mean=Medias()
    ep=-qt((1-input$conf)/2,input$n-1)*Desvios()/sqrt(input$n)
    LI= mean-ep
    LS=mean+ep
    df2 <- data.frame(LI,LS)
    df2 <- df2 %>% mutate(validacao=ifelse(LI<=mt & LS>=mt,1,0))
    prop <- sum(df2$validacao)/input$amostras
    texto <- paste("A proporção de acerto na amostra foi",round(prop*100,2),'%')
    return(texto)
  }
  )
  output$taxa_acerto <- renderText(acerto())
  
  output$table <- renderDT(data.frame(Media=round(Medias(),2),
                                      LimiteInferior=round(Medias()+qt((1-input$conf)/2,input$n-1)*Desvios()/sqrt(input$n),2),
                                      LimiteSuperior=round(Medias()-qt((1-input$conf)/2,input$n-1)*Desvios()/sqrt(input$n),2)
  ) %>% mutate(Acertou=ifelse(LimiteInferior<=input$mean & LimiteSuperior>=input$mean,'Sim','Não'))
  )
  output$aviso <- renderText(ifelse(input$dist!="Normal"&input$n<30,
                                    "AVISO: INTERVALO NÃO RECOMENDADO. PARA UM FUNCIONAMENTO 
                                    BOM, DEVEMOS TER DADOS COM DISTRIBUIÇÃO NORMAL OU 
                                  AMOSTRAS GRANDES(TAMANHO 30 OU MAIOR)",""))
  
  
}


shinyApp(ui=ui,server=server)







