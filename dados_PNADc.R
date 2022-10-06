
# Obtendo os dados da PNADc -----------------------------------------------


#base pnad
s#pnad
##############################################################################################################################

# Limpando arquivos armazenados na memoria
rm(list=ls(all=TRUE))

# Definindo limite de memoria para compilacao do programa
aviso <- getOption("warn")
options(warn=-1)
memory.limit(size=20000)
options(warn=aviso)
rm(aviso)

# Definindo opcao de codificacao dos caracteres e linguagem
aviso <- getOption("warn")
options(warn=-1)
options(encoding="latin1")
options(warn=aviso)
rm(aviso)

# Definindo opcao de exibicao de numeros sem exponencial
aviso <- getOption("warn")
options(warn=-1)
options(scipen=999)
options(warn=aviso)
rm(aviso)

# Definindo opcao de repositorio para instalacao dos pacotes necessarios
aviso <- getOption("warn")
options(warn=-1)
options(repos=structure(c(CRAN="https://cran.r-project.org/")))
options(warn=aviso)
rm(aviso)

# Definindo area de trabalho
caminho <- getwd()
setwd(caminho)

# Carregando informacoes do pacote PNADcIBGE
if("PNADcIBGE" %in% rownames(installed.packages())==FALSE)
{
  install.packages("PNADcIBGE", dependencies=TRUE)
}
library("PNADcIBGE")
packageDescription("PNADcIBGE")
help(package="PNADcIBGE")


#Carregando os dados da PNAD
variaveis_selecionadas <- c("V2007", "V2009", "V2010", "VD3004", "VD4019", "VD4031", "V1022", "VD2003")
dadosPNADc <- get_pnadc(year=2020, quarter=3, labels=TRUE, deflator=TRUE, design=FALSE, vars=variaveis_selecionadas)

#selecinando as variáveis de interesse
dadosPNAD = dadosPNADc |> 
  select(variaveis_selecionadas,UF)

#Filtando as observações de interesse
dados = dadosPNAD |> 
  filter(V2009 >=14) |> 
  filter(VD4019>=348) |> 
  filter(V2009<=80)
summary(dados$V2009)

#removendo NAS
dados = na.omit(dadosPNAD)

#Gerando a amostra aleatória da base de dados
PNAD = sample(seq_len(nrow(dados)), size = 10000)
basePNAD = dados[PNAD, ]

#Salvando os arquivos que serão modelados.
save(dados,basePNAD,file = "DadosPnad.RData")

#

