if(!require(shiny)){install.packages("shiny");library(shiny)} 
if(!require(tidyverse)){install.packages("tidyverse");library(tidyverse)} 
if(!require(plotly)){install.packages("plotly");library(plotly)} 
if(!require(DT)){install.packages("DT");library(DT)} 

ui <- fluidPage(
  titlePanel("Intervalos de confiança para médias"),
  sidebarLayout(
    sidebarPanel(
        selectInput('dist',"Defina a distribuição da população",c("Normal")),
        sliderInput('amostras','Defina o número de intervalos a se fazer',2,200,value = 20),
        sliderInput('n','Defina o tamanho de cada amostra',2,300,value=30),
        numericInput('conf','Defina o nível de confiança do intervalo',value=0.95),
        numericInput('mu','Defina a média da população',value=0),
        numericInput('sigma','Defina o desvio padrão da população',value=1)
    ),
    mainPanel(
        tabsetPanel(
            tabPanel("Grafico",plotlyOutput("plot",height="800px"),
            textOutput("taxa_acerto")),
            tabPanel("Intervalos Gerados",DTOutput('table')))
            
     )
  )
)



server <- function(input,output){
  Amostras <-reactive({(lapply(rep(input$n,input$amostras),rnorm,
                               mean=input$mu,sd=input$sigma))})
  Medias <- reactive({unlist(lapply(Amostras(),mean))})
  Desvios <- reactive({unlist(lapply(Amostras(),sd))})

  ICs <- reactive({
    data.frame(
      id=1:input$amostras,
      mean=Medias(),
      ep=-qt((1-input$conf)/2,input$n-1)*Desvios()/sqrt(input$n))
  })  

output$plot <- renderPlotly({df <- ICs()
  ggplot(df,aes(x=id,y=mean))+geom_point(size=2,col='red')+
    geom_hline(aes(yintercept=input$mu))+geom_errorbar(aes(ymin=mean+ep,ymax=mean-ep))+
    xlab("")+ylab("Estimativa")})
acerto <- reactive({ 
  mean=Medias()
  ep=-qt((1-input$conf)/2,input$n-1)*Desvios()/sqrt(input$n)
  LI= mean-ep
  LS=mean+ep
  df2 <- data.frame(LI,LS)
  df2 <- df2 %>% mutate(validacao=ifelse(LI<=input$mu & LS>=input$mu,1,0))
  prop <- sum(df2$validacao)/input$amostras
  texto <- paste("A proporção de acerto na amostra foi",round(prop*100,2),'%')
  return(texto)
}
)
output$taxa_acerto <- renderText(acerto())

output$table <- renderDT(data.frame(Media=round(Medias(),2),
                         LimiteInferior=round(Medias()+qt((1-input$conf)/2,input$n-1)*Desvios()/sqrt(input$n),2),
                         LimiteSuperior=round(Medias()-qt((1-input$conf)/2,input$n-1)*Desvios()/sqrt(input$n),2)
                         ) %>% mutate(Acertou=ifelse(LimiteInferior<=input$mu & LimiteSuperior>=input$mu,'Sim','Não'))
                         )
}


shinyApp(ui=ui,server=server)







