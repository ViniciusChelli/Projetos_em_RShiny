library(shiny) 
library(shinythemes) 
#cria a interface gráfica
ui <- fluidPage( 
    tags$head( 
        tags$link(rel="stylesheet",type="text/css",href="bootstrap.min.css") 
    ),tags$head( 
        tags$style(HTML(" 
        .table>tbody>tr>td, .table>tbody>tr>th, .table>tfoot>tr>td, .table>tfoot>tr>th, .table>thead>tr>td, .table>thead>tr>th { 
        padding: 8px;line-height:1.42857143;vertical-align:top;border-top:3px solid rgba(255, 9, 255, 0.3); 
                        } 
                        ")) 
        ),
    titlePanel(h1("Treinamento do Perceptron",align="center")),
    
    fluidRow( 
        column(2,h1("Dados"), 
               tableOutput("Dados"),fileInput("arquivo",h3("Escolha o arquivo:",class="text-secondary"),multiple = F,accept = c("text/csv"),buttonLabel = "Escolha o arquivo:",placeholder = "Arquivo Carregado")),helpText("Carregue tabelas do tipo txt/csv, separados por vírgula, com 3 colunas (X1,X2,CLASSE), podendo deixar o cabeçalho, e tente convergir seu modelo com o Perceptron"),column(2,actionButton("Procurar","Procurar",class="btn-secondary",style="padding:8px; font-size:200%"))
        ,column(6, 
                plotOutput("Graf"),helpText("Bolas azuis representam o 1, as vermelhas o 0, realize as epochs até o modelo separar as esferas corretamente")     
        ),column(4,h1("Esquema do Perceptron"),helpText("Segue o modelo Online, onde após cada iteration os pesos/bias são reajustados. Como estamos fazendo Epochs, os pesos serão reajustados vezes o número de linhas que a tabela tiver até aparecerem para vocês"),img(src="Sadssa.png")) 
    ),hr(), 
    fluidRow( 
        column(6, 
               h1("Pesos Sinápticos:"), 
               numericInput("p1valor",h3("Insira o valor para W1"),0.1,min = -9999,max = 9999,step = 0.1), 
               numericInput("p2valor",h3("Insira o valor para W2"),0.1,min = -9999,max = 9999,step = 0.1), 
               numericInput("pVvalor",h3("Insira o novo valor do bias (Θ)"),0.1,min = -9999,max = 9999,step = 0.1), 
               numericInput("gamma",h3("Insira o valor da taxa de aprendizado (γ)"),0.2,min=0.1,max=1,step = 0.1),
               actionButton("Processar","Executar Epoch",class="btn-success",style="padding:12px; font-size:200%"),helpText("Uma epoch equivale a passagem de todas as linhas de dados pelo Perceptron, execute-as até o modelo convergir ou ficar o mais próximo de...")
        ),column(6,
               uiOutput("P"),h1("Função de Ativação g (u) : Degrau"),helpText("Também chamada de Função de Heaviside (escalão unitário), é muito utilizada em RNA. É parcialmente diferenciável."),img(src="php2MUtB2.png"))
    ) 
)
#camada de lógica
server <- function(input, output,session) { 
    observeEvent(input$Procurar,{ 
        output$Dados=renderTable({ 
            class="table-primary"
            inFile=input$arquivo 
            if(is.null(inFile)) 
                return(NULL) 
            Dados=read.csv(inFile$datapath,header = T,sep = ",") 
            })
    }) 
    observeEvent(input$Processar,{
        inFile=input$arquivo 
        if(is.null(inFile)) 
            return(NULL) 
        df=as.data.frame(read.csv(inFile$datapath,header = T,sep = ",")) 
        n=nrow(df)
        W=c(input$p1valor,input$p2valor,input$pVvalor) 
        for (k in 1:n){
            Somatorio=0
            for (i in 1:3){
                Somatorio=Somatorio+df[k,i]*W[i]
            }
            Degrau=ifelse(Somatorio>0,1,0) 
            Erro=((df[k,3])-Degrau)
            Gamma=input$gamma
            #Ajuste do bias e pesos 
            for(i in 1:3){ 
                W[i]=W[i]+input$gamma*Erro*df[k,i] 
            } 
            #plot do hiperplano 
            output$Graf=renderPlot({ 
                x=seq(min(df[,1])-50,max(df[,1]+50),l=150) 
                y=((-W[3]-W[1]*x)/W[2]) 
                par(bg="gray",font=2,font.lab=2) 
                plot(x,y,type="l",xlim=c(min(df[,1])-50,max(df[,1])+50),ylim=c(min(df[,2])-50,max(df[,2])+50),col="green",lwd=3,main="HIPERPLANO",col.main="Purple", 
                     xlab="Registro X1",ylab="Registro X2") 
                for (j in 1:n) {
                 cor=ifelse(df[j,3]==0,"red","blue")   
                    lines(df[j,1],df[j,2],col=cor,type="p",pch=16)
                }
                legend("bottomright",c("Hiperplano","0","1"),col=c("green","red","blue"),pch = c(8,16,16),title = "Legenda",cex = 0.8)
                output$P=renderUI({h1(paste0("Pesos e Bias reajustados -> W1: ",W[1]," / W2: ",W[2]," / Bias: ",W[3]))}) 
                updateNumericInput(session,"p1valor",value = W[1])
                updateNumericInput(session,"p2valor",value=W[2]) 
                updateNumericInput(session,"pVvalor",value = W[3])
            })
        } 
    }
    ) 
}

#app em si
shinyApp(ui = ui, server = server)