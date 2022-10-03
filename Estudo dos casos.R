#####TCC- parte 2
#carregando pacotes que serão utilizados em ambos os estudos
library(quantreg)
library(tidyverse)
library(flexmix)
library(expss)
library(grid)
library(scales)
library(MASS)
library(jtools)
setwd("~/Etatistica/TCC/aplicacao")


# PNADc -------------------------------------------------------------------

## Base de dados completa 

#realizando as manipulações necessárias
dados$VD3004 = ordered(dados$VD3004,
                       levels =c('Sem instrução ou menos de 1 ano de estudo','Fundamental incompleto ou equivalente', 'Fundamental completo ou equivalente',
                                 'Médio incompleto ou equivalente','Médio completo ou equivalente','Superior incompleto ou equivalente','Superior completo'))
dados = dados |> 
  mutate(VD3004 = dplyr::recode(.x = VD3004, "Sem instrução e menos de 1 ano de estudo" = 'Sem instrução ou menos de 1 ano de estudo'))



dados = dados|> 
  mutate(VD3004 = dplyr::recode(.x = VD3004, "Sem instrução ou menos de 1 ano de estudo" = 'Sem instrução',
                                'Fundamental incompleto ou equivalente' = 'Fundamental incompleto',
                                'Fundamental completo ou equivalente' = 'Fundamental completo',
                                'Médio incompleto ou equivalente' = 'Médio incompleto',
                                'Médio completo ou equivalente' = 'Médio completo',
                                
                                'Superior incompleto ou equivalente' = 'Superior incompleto'))
dados$V2010 = factor(dados$V2010)
dados$V2007 = factor(dados$V2007)
dados$V1022 = factor(dados$V1022)

### Base com 10 mil observações, uqe será utilizada nas análises
#retirandoa variável UF, pois a mesma não será utilizada
Pnad = Pnad |> 
  select(-UF)

##Renomeando as variáveis da base com 10 mil observações
Pnad = PNAD |> 
  rename(Sexo = V2007, Idade = V2009,
         Raca = V2010, Escolaridade = VD3004, Renda = VD4019,
         Horas.trabalhadas.por.semana = VD4031, Região.do.domicílio = V1022,
         Número.de.pessoas.no.domicílio = VD2003)

## Transformano em fator as variáves categóricas

#Sexo
Pnad$Sexo = factor(Pnad$Sexo)

#recodificando a variável Escolaridade e transformando a mesma em um fator
Pnad$Escolaridade = as.character(Pnad$Escolaridade)

Pnad = Pnad |> 
  mutate(Escolaridade = recode(.x = Escolaridade, "Sem instrução e menos de 1 ano de estudo" = 'Sem instrução ou menos de 1 ano de estudo'))

Pnad$Escolaridade = ordered(Pnad$Escolaridade,
                            levels =c('Sem instrução ou menos de 1 ano de estudo','Fundamental incompleto ou equivalente', 'Fundamental completo ou equivalente',
                                      'Médio incompleto ou equivalente','Médio completo ou equivalente','Superior incompleto ou equivalente','Superior completo'))

Pnad = Pnad |> 
  mutate(Escolaridade = dplyr::recode(.x = Escolaridade, "Sem instrução ou menos de 1 ano de estudo" = 'Sem instrução',
                                      'Fundamental incompleto ou equivalente' = 'Fundamental incompleto',
                                      'Fundamental completo ou equivalente' = 'Fundamental completo',
                                      'Médio incompleto ou equivalente' = 'Médio incompleto',
                                      'Médio completo ou equivalente' = 'Médio completo',
                                      'Superior incompleto ou equivalente' = 'Superior incompleto'))
#Raça
Pnad$Raca = factor(Pnad$Raca)

#Região do domicílio
Pnad$Região.do.domicílio = factor(Pnad$Região.do.domicílio)

## Comparação das duas bases

# histograma daa Renda

# 10 mil
h1 = ggplot(data = Pnad) +
  geom_density(mapping = aes(x = Renda,
                               y = ..density..), binwidth = 250, fill = "deepskyblue4",
                 col = "black",alpha = 0.5)+
  scale_x_continuous(breaks = seq(0, 6000, 1000), 
                     limits=c(0, 6000)) +
  scale_y_continuous(label = scales::label_number(big.mark = ",",
                                                  decimal.mark = ".")) +
  labs(title = "Densidade da Amostra de 10 mil.")+
  theme_light() +
  theme(panel.border = element_rect(color = "Black", size = 1), plot.title = element_text(size = 20, hjust=0.5),
        axis.title = element_blank(), axis.text.y = element_blank(), axis.text.x = element_text(size = 17))

# Total
h2 = ggplot(data = dados) +
  geom_density(mapping = aes(x = VD4019,
                             y = ..density..), binwidth = 250, fill = "deepskyblue3",
               col = "black",alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, 6000, 1000), 
                     limits=c(0, 6000)) +
  scale_y_continuous(label = scales::label_number(big.mark = ",",
                                                  decimal.mark = ".")) + 
  ylab("Densidade") + 
  labs(title = "Densidade Amostra Inicial.")+
  theme_light() +
  theme(panel.border = element_rect(color = "Black", size = 1),plot.title = element_text(size = 20, hjust=0.5),
        axis.title.x = element_blank(), axis.text = element_text(size = 17), axis.title.y = element_text(size = 18))

#plotando as duas na mesma imagem
H1 = gridExtra::grid.arrange(h2,h1,ncol = 2,
                             bottom=textGrob("Renda Mensal",gp=gpar(fontsize=20)))

#salvando o arquivo        
ggsave(filename = "h1.png", plot = h1, device = png(width = 1250, height = 700))

#boxplot da renda
b1 = dados |> 
  ggplot(mapping = aes(y = VD4019)) +
  geom_boxplot(fill = "deepskyblue4") +
  labs(y = "Renda mensal")+
  scale_y_continuous(breaks = seq(0, 5000, 1000), 
                     limits=c(0, 5000),label = scales::label_number(big.mark = ",",
                                                                    decimal.mark = ".")) +
  
  labs(title = "Amostra Inicial.") + 
  geom_hline(yintercept = mean(dados$VD4019), col = "tomato3") + 
  theme_light() +
  theme(panel.border = element_rect(color = "Black", size = 1),plot.title = element_text(size = 20, hjust=0.5),
        axis.title.x = element_blank(), axis.text = element_text(size = 17), axis.title.y = element_text(size = 18))

b2 = Pnad |> 
  ggplot(mapping = aes(y = Renda)) +
  geom_boxplot(fill = "deepskyblue4") +
  scale_y_continuous(breaks = seq(0, 5000, 500), 
                     limits=c(0, 5000),label = scales::label_number(big.mark = ",",
                                                                    decimal.mark = ".")) +
  labs(title = "Amostra de 10 mil.") +
  geom_hline(yintercept = mean(Pnad$Renda), col = "tomato3") + 
  theme_light() +
  theme(panel.border = element_rect(color = "Black", size = 1),plot.title = element_text(size = 20, hjust=0.5),
        axis.title.x = element_blank(), axis.text = element_text(size = 17), axis.title.y = element_blank())

B = gridExtra::grid.arrange(b1,b2,ncol = 2)
ggsave(filename = "renda.png", plot = B, device = png(width = 1200, height = 800))

#Idade
h3 = ggplot(data = Pnad) +
  geom_density(mapping = aes(x = Idade,
                             y = ..density..), fill = "deepskyblue4",
               col = "black",alpha = 0.5)+
  scale_x_continuous(limits=c(0, 100)) +
  scale_y_continuous(label = scales::label_number(big.mark = ",",
                                                  decimal.mark = ".")) +
  labs(title = "Amostra de 10 mil.")+
  theme_light() +
  theme(panel.border = element_rect(color = "Black", size = 1), plot.title = element_text(size = 20, hjust=0.5),
        axis.title = element_blank(), axis.text.y = element_blank(), axis.text.x = element_text(size = 17))

h4 = ggplot(data = dados) +
  geom_density(mapping = aes(x = V2009,
                             y = ..density..), fill = "deepskyblue3",
               col = "black",alpha = 0.5) +
  scale_x_continuous(limits=c(0, 100)) +
  scale_y_continuous(label = scales::label_number(big.mark = ",",
                                                  decimal.mark = ".")) + 
  ylab("Densidade") + 
  labs(title = "Amostra Inicial.")+
  theme_light() +
  theme(panel.border = element_rect(color = "Black", size = 1),plot.title = element_text(size = 20, hjust=0.5),
        axis.title.x = element_blank(), axis.text = element_text(size = 17), axis.title.y = element_text(size = 18))
H2 = gridExtra::grid.arrange(h4,h3,ncol = 2,
                             bottom=textGrob("Idade",gp=gpar(fontsize=20)))
ggsave(filename = "idade.png", plot = H2, device = png(width = 1200, height = 800))


#escolaridade
cont_escol1 <- Pnad |>
  count(Escolaridade)
count_escol2 <- dados |> 
  count(VD3004)

Escolaridade1 = cont_escol1 |> 
  mutate(pct = prop.table(n)) |> 
  ggplot(mapping = aes(x = reorder(Escolaridade,-n), 
                       y = pct, 
                       fill = Escolaridade)) + 
  geom_bar(stat = "identity", color = "Black") +
  geom_text(aes(label = percent(pct)), 
            vjust=-0.5,
             size = 7)+
  labs(x = "Escolaridade",
       y = "Frequência",
       fill = "Escolaridade", 
       title = "Amostra de 10 mil.") + theme_light() + guides(fill="none")+
  scale_y_continuous(limits = c(0, 0.4, 0.10), labels = percent) + 
  theme(panel.border = element_rect(color = "Black", size = 1),plot.title = element_text(size = 20, hjust=0.5),
        axis.title = element_text(size = 20), axis.text = element_text(size = 20), 
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1), axis.title.y = element_blank())
Escolaridade2 = count_escol2 |> 
  mutate(pct = prop.table(n)) |> 
  ggplot(mapping = aes(x = reorder(VD3004,-n), y = pct, fill = VD3004)) + 
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = percent(pct)), 
            vjust=-0.5,
            size = 7)+
  labs(x = "Escolaridade",
       y = "Porcentagem",
       fill = "Escolaridade", 
       title = "Amostra Inicial.") + theme_light() + guides(fill="none") + 
  scale_y_continuous(limits = c(0, 0.4, 0.10), labels = percent) +
  theme(panel.border = element_rect(color = "Black", size = 1),plot.title = element_text(size = 20, hjust=0.5),
        axis.title = element_text(size = 20), axis.text = element_text(size = 20), 
        axis.text.x = element_text(angle = 45, 
                                    hjust = 1))

total = gridExtra::grid.arrange(Raca2,Raca1, Escolaridade2, Escolaridade2,ncol = 2)
ggsave(filename = "esco_raca.png", plot = total, device = png(width = 1400, height = 1600))

#raca
cont_raca1 <- Pnad |>
  count(Raca)
count_raca2 <- dados |> 
  count(V2010)

Raca1 = cont_raca1 |> 
  mutate(pct = prop.table(n)) |> 
  ggplot(mapping = aes(x = reorder(Raca,-n), 
                       y = pct, 
                       fill = Raca)) + 
  geom_bar(stat = "identity", color = "Black") +
  geom_text(aes(label = percent(pct)), 
            vjust=-0.5,
            size = 7)+
  labs(x = "Raça",
       y = "Porcentagem",
       fill = "Raça", 
       title = "Amostra de 10 mil.") + theme_light() + guides(fill="none")+
  scale_y_continuous(limits = c(0, 0.5, 0.10), labels = percent) + 
  scale_fill_manual(values = hcl.colors(n = 6, palette = "Viridis")) + 
  theme(panel.border = element_rect(color = "Black", size = 1),plot.title = element_text(size = 20, hjust=0.5),
        axis.title = element_text(size = 19), axis.text = element_text(size = 19), 
        axis.title.y = element_blank())

Raca2 = count_raca2|> 
  mutate(pct = prop.table(n)) |> 
  ggplot(mapping = aes(x = reorder(V2010,-n), y = pct, fill = V2010)) + 
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = percent(pct)), 
            vjust=-0.5,
            size = 7)+
  labs(x = "Raça",
       y = "Porcentagem",
       fill = "Raça", 
       title = "Amostra Inicial.") + theme_light() + guides(fill="none") + 
  scale_y_continuous(limits = c(0, 0.5, 0.10), labels = percent) + 
  scale_fill_manual(values = hcl.colors(n = 6, palette = "Viridis")) + 
  theme(panel.border = element_rect(color = "Black", size = 1),plot.title = element_text(size = 20, hjust=0.5),
        axis.title = element_text(size = 19), axis.text = element_text(size = 19))


#sexo
cont_sexo1 <- Pnad |>
  count(Sexo)
count_sexo2 <- dados |> 
  count(V2007)

sexo1 = cont_sexo1 |> 
  mutate(pct = prop.table(n)) |> 
  ggplot(mapping = aes(x = reorder(Sexo,-n), 
                       y = pct, 
                       fill = Sexo)) + 
  geom_bar(stat = "identity", color = "Black") +
  geom_text(aes(label = percent(pct)), 
            vjust=-0.5,
            size = 7)+
  labs(x = "Sexo",
       y = "Porcentagem",
       fill = "Sexo", 
       title = "Amostra de 10 mil.") + theme_light() + guides(fill="none")+
  scale_y_continuous(limits = c(0, 0.6, 0.10), labels = percent) + 
  scale_fill_manual(values = hcl.colors(n = 2, palette = "Viridis")) + 
  theme(panel.border = element_rect(color = "Black", size = 1),plot.title = element_text(size = 20, hjust=0.5),
        axis.title = element_text(size = 18), axis.text = element_text(size = 17), 
        axis.title.y = element_blank())

sexo2 = count_sexo2|> 
  mutate(pct = prop.table(n)) |> 
  ggplot(mapping = aes(x = reorder(V2007,-n), y = pct, fill = V2007)) + 
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = percent(pct)), 
            vjust=-0.5,
            size = 7)+
  labs(x = "Sexo",
       y = "Porcentagem",
       fill = "Sexo", 
       title = "Amostra Inicial.") + theme_light() + guides(fill="none") + 
  scale_y_continuous(limits = c(0, 0.6, 0.10), labels = percent) + 
  scale_fill_manual(values = hcl.colors(n = 2, palette = "Viridis")) + 
  theme(panel.border = element_rect(color = "Black", size = 1),plot.title = element_text(size = 20, hjust=0.5),
        axis.title = element_text(size = 18), axis.text = element_text(size = 17))




#região
cont_regiao1 <- Pnad |>
  count(Região.do.domicílio)

count_regiao2 <- dados |> 
  count(V1022)

regiao1 = cont_regiao1 |> 
  mutate(pct = prop.table(n)) |> 
  ggplot(mapping = aes(x = reorder(Região.do.domicílio,-n), 
                       y = pct, 
                       fill = Região.do.domicílio)) + 
  geom_bar(stat = "identity", color = "Black") +
  geom_text(aes(label = percent(pct)), 
            vjust=-0.5,
            size = 7)+
  labs(x = "Região do domicílio",
       y = "Porcentagem",
       fill = "Região do domicílio", 
       title = "Amostra de 10 mil.") + theme_light() + guides(fill="none")+
  scale_y_continuous(limits = c(0, 0.9, 0.10), labels = percent) + 
  scale_fill_manual(values = hcl.colors(n = 2, palette = "Viridis")) + 
  theme(panel.border = element_rect(color = "Black", size = 1),plot.title = element_text(size = 20, hjust=0.5),
        axis.title = element_text(size = 18), axis.text = element_text(size = 17),axis.title.y = element_blank())

regiao2 = count_regiao2|> 
  mutate(pct = prop.table(n)) |> 
  ggplot(mapping = aes(x = reorder(V1022,-n), y = pct, fill = V1022)) + 
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = percent(pct)), 
            vjust=-0.5,
            size = 7)+
  labs(x = "Região do domicílio",
       y = "Porcentagem",
       fill = "Região do domicílio", 
       title = "Amostra Inicial.") + theme_light() + guides(fill="none") + 
  scale_y_continuous(limits = c(0, 0.9, 0.10), labels = percent) + 
  scale_fill_manual(values = hcl.colors(n = 2, palette = "Viridis")) + 
  theme(panel.border = element_rect(color = "Black", size = 1),plot.title = element_text(size = 20, hjust=0.5),
        axis.title = element_text(size = 18), axis.text = element_text(size = 17))


fim = gridExtra::grid.arrange(sexo2,sexo1,regiao2, regiao1,ncol = 2)
ggsave(filename = "sexo_regiao.png", plot = fim, device = png(width = 1200, height = 1100))



# Análise exploratória ----------------------------------------------------

# variaveis quantitativas
g1 = ggplot(Pnad, 
            aes(x = Horas.trabalhadas.por.semana, 
                y = Renda)) +
  geom_point(color="#27408B", 
             size = 1, 
             alpha = 1) +
  scale_x_continuous(limits = c(0,110))+  
  ggtitle('Renda mensal por Horas trabalhadas por semana') + 
  labs(x = "Horas trabalhadas por semana", y = "Renda mensal") +
  guides(fill="none")+ theme_light() + 
  theme(axis.text = element_text(size = 17), axis.title = element_text(size = 17),
        panel.border = element_rect(color = "Black", size = 1),plot.title = element_text(size = 20, hjust=0.5))

g2 = ggplot(Pnad, 
            aes(x = Idade, 
                y = Renda)) +
  geom_point(color="#27408B", 
             size = 1, 
             alpha = 1) +
  labs(x = "Idade", y = "Renda mensal") +
  ggtitle('Renda mensal por Idade') + 
  guides(fill="none")+ theme_light()+ 
  theme(axis.text = element_text(size = 17), axis.title = element_text(size = 17),
        panel.border = element_rect(color = "Black", size = 1),plot.title = element_text(size = 20, hjust=0.5))


G1 = gridExtra::grid.arrange(g1,g2,ncol = 2)
ggsave(filename = "quantitavas.png", plot = G1, device = png(width = 1250, height = 800))

#variáveis Qualitativas

g3 = ggplot(Pnad) +
  geom_boxplot(aes(x = Renda, y = Sexo,fill = Sexo), outlier.shape = 1) + 
  scale_fill_manual(values = hcl.colors(n = 2, palette = "Viridis")) +
  guides(fill="none")+theme_light() +
  labs(x = 'Renda mensal', y="Sexo") + 
  ggtitle('Renda mensal por Sexo')+
  scale_x_continuous(limits = c(0, 10000)) +
  theme(panel.border = element_rect(color = "Black", size = 1),plot.title = element_text(size = 20, hjust=0.5),
        axis.text = element_text(size = 17), axis.title = element_text(size = 17))

g4 = ggplot(Pnad) +
  geom_boxplot(aes(x = Renda, y = factor(Número.de.pessoas.no.domicílio) ,fill = factor(Número.de.pessoas.no.domicílio)),outlier.shape = 1)  +
  scale_fill_manual(values = hcl.colors(n = 15,
                                        palette = "Viridis"))+
  guides(fill="none")+theme_light() +
  labs(x = 'Renda mensal', y="N° de pessoas no domicílio") + 
  ggtitle('Renda mensal pelo n° de pessoas no domicílio')+
  scale_x_continuous(limits = c(0, 10000))+
  theme(panel.border = element_rect(color = "Black", size = 1),plot.title = element_text(size = 20, hjust=0.5),
        axis.text = element_text(size = 17), axis.title = element_text(size = 17))

g5 = ggplot(Pnad) +
  geom_boxplot(aes(x = Renda, y = Raca ,fill = Raca),outlier.shape = 1)+ 
  scale_fill_manual(values = hcl.colors(n = 6, palette = "Viridis")) +
  guides(fill="none")+theme_light() +
  labs(x = 'Renda mensal', y="Raça") + 
  ggtitle('Renda mensal por raça')+
  scale_x_continuous(limits = c(0, 10000))+
  theme(panel.border = element_rect(color = "Black", size = 1),plot.title = element_text(size = 20, hjust=0.5),
        axis.text = element_text(size = 17), axis.title = element_text(size = 17))

g6 = ggplot(Pnad) +
  geom_boxplot(aes(x = Renda, y =Região.do.domicílio  ,fill = Região.do.domicílio),outlier.shape = 1)  +
  scale_fill_manual(values = hcl.colors(n = 3, palette = "Viridis"))+
  guides(fill="none")+theme_light() +
  labs(x = 'Renda mensal', y="Região do domicílio") + 
  ggtitle('Renda mensal por região domiciliar')+
  scale_x_continuous(limits = c(0, 10000))+
  theme(panel.border = element_rect(color = "Black", size = 1),plot.title = element_text(size = 20, hjust=0.5),
        axis.text = element_text(size = 17), axis.title = element_text(size = 17))


g7 = ggplot(Pnad) +
  geom_boxplot(aes(x = Renda, y =Escolaridade  ,fill = Escolaridade),outlier.shape = 1)  +
  scale_fill_manual(values = hcl.colors(n = 7, palette = "Viridis"))+
  guides(fill="none")+theme_light() +
  labs(x = 'Renda mensal', y="Escolaridade") + 
  ggtitle('Renda mensal por Escolaridade')+
  scale_x_continuous(limits = c(0, 10000))+
  scale_y_discrete(labels = c("Sem\ninstrução", "Fundamental\nincompleto", 
                              "Fundamental\ncompleto", "Médio\nincompleto", "Médio\ncompleto", 
                              "Superior\nincompleto","Superior\ncompleto"))+
  theme(panel.border = element_rect(color = "Black", size = 1),plot.title = element_text(size = 20, hjust=0.5),
        axis.text = element_text(size = 17), axis.title = element_text(size = 17))

lay <- rbind(c(1,2),
             c(3,4))
G2 = gridExtra::grid.arrange(g3,g4,g5,g6, layout_matrix = lay)

ggsave(filename = "quali2.png", plot = G2, device = png(width = 1400, height = 1100))

#ggsave(filename = "escolaridade.png", plot = g7, device = png(width = 1250, height = 800))


# modelo de regressão linear clássico -------------------------------------

modelo.lm1 = lm(Renda~.,data = Pnad)

#obtendo os coeficientes do modelo
summ(modelo.lm1, digits = 2)

#modelo reduzido com as variáveis explicativas
pnad_nova = Pnad |> 
  dplyr::select(-c(Raca)) |> 
  filter(Escolaridade %in% c("Superior completo", "Superior incompleto" , "Médio completo",
                             "Médio incompleto", "Sem instrução") )

modelo.lm2 = lm(Renda~.,data = pnad_nova)
summ(modelo.lm2)

#modelo reduzido do log da renda
modelo.lm3 = lm(log(Renda)~.,data = pnad_nova)
summ(modelo.lm3)
#histograma do log da renda
h1 = ggplot(data = Pnad) +
  geom_density(mapping = aes(x = log(Renda),
                             y = ..density..), fill = "deepskyblue4",
               col = "black",alpha = 0.5)+
  scale_x_continuous(limits=c(0, 20)) +
  scale_y_continuous(label = scales::label_number(big.mark = ",",
                                                  decimal.mark = ".")) +
  labs(x = "Log(Renda)",
       y = "Densidade")+
  theme_light() +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 17))

# analise dos residuos do modelo do log da renda
res.student <- rstudent(modelo.lm3)

D.2 <- tibble(y.hat = predict(modelo.lm3) , r = res.student)

g.1 = ggplot(D.2, aes(x = y.hat, y = r)) +
  geom_point(size=2) +
  labs(x='Renda ajustada',y='Resíduos')+
  theme_light() + 
  theme(plot.title=element_text(hjust=0.5),
        panel.border = element_blank(),
        axis.title = element_text(size = 18), axis.text = element_text(size = 17)) + 
  geom_hline(aes(yintercept = 0), col="#EE2C2C",lwd = 1) + 
  geom_hline(aes(yintercept = -2), col="#CD2626",lty=2,lwd = 1) + 
  geom_hline(aes(yintercept = 2), col="#8B1A1A",lty=2,lwd = 1)


g.2 = ggplot(D.2,aes(sample=r)) + 
  stat_qq() +
  stat_qq_line(col="#79CDCD", lwd=2) +
  labs(x='Quantis teóricos',y='Quantis amostrais') +
  theme_light() + 
  theme(plot.title=element_text(hjust=0.5),
        panel.border = element_blank(),axis.title = element_text(size = 18), axis.text = element_text(size = 17))

G1 = gridExtra::grid.arrange(g.1,g.2,ncol=2)
ggsave(filename = "log.png", plot = h1, device = png(width = 1200, height = 800))

#testando a normalidade e homocedasticidade do resíduos
library(nortest)
library(lmtest)
bptest(modelo.lm3)
lillie.test(res.student)

#análise da distância de cook, pontos de alavanca e outliers
modelo.lm3 |> resva_norm()+
  labs(y = "Resíduos")

mod2 = modelo.lm3 |>  infl_norm() +
  geom_hline(aes(yintercept = 0.002852253), col="#CD2626",lwd = 2,lty=2) +
  labs(y= "Laverage (h)",
       x = "Índice") +
  theme_light() +
  theme(plot.title=element_text(hjust=0.5, size = 20),
        panel.border = element_blank(),axis.title = element_text(size = 18), axis.text = element_text(size = 17))


mod1 = modelo.lm3 |>  dcook_norm() +
  geom_hline(aes(yintercept = 4/length(pnad_nova$Renda)), col="#EE2C2C",lwd = 2, lty=2) +
  labs(y= "Distância de Cook",
       x = "Índice")+
  theme_light() +
  theme(plot.title=element_text(hjust=0.5, size = 20),
        panel.border = element_blank(),axis.title = element_text(size = 18), axis.text = element_text(size = 17))

mod = gridExtra::grid.arrange(mod1,mod2,ncol=2)
ggsave(filename = "influentes.png", plot = mod, device = png(width = 1200, height = 800))

theme_set(theme_light())

outliers = check_model(modelo.lm3, 
                       check = c("outliers"),
                       panel = F) 
ggsave(filename = "out.png", plot = outliers, device = png(width = 1200, height = 800))

#AIC dos modelos
AIC(modelo.lm1)
AIC(modelo.lm2)
AIC(modelo.lm3)


# Modelo quantílico -------------------------------------------------------

#modelo completo
eq.1 = Renda ~ Idade + Raca + Escolaridade + Sexo + Horas.trabalhadas.por.semana + Região.do.domicílio + Número.de.pessoas.no.domicílio
tau = c(0.10,0.25,0.50,0.75,0.90)

modelo_quant1 <- rq(Renda ~ Idade + Raca + Escolaridade + Sexo + Horas.trabalhadas.por.semana + Região.do.domicílio + Número.de.pessoas.no.domicílio,tau =tau , method = "fn", data = Pnad)

summ(modelo_quant1, se = "boot")
AIC(modelo_quant1)
modelo
Pnad$Escolaridade[Pnad$Escolaridade =="Fundamental incompleto"] = "Sem instrução"

#modelo reduzido

eq.2 = Idade +  Escolaridade + Sexo + Horas.trabalhadas.por.semana + Região.do.domicílio + Número.de.pessoas.no.domicílio
modelo_quant2 <- rq(Renda ~ Idade +  Escolaridade + Sexo + Horas.trabalhadas.por.semana + Região.do.domicílio + Número.de.pessoas.no.domicílio,
                    tau =tau , method = "fn", data = Pnad)
AIC.rqs(modelo_quant2)
boot = summary(modelo_quant2, se = "boot")

#modelo log
eq.3 = log(Renda) ~ Idade +  Escolaridade + Sexo + Horas.trabalhadas.por.semana + Região.do.domicílio + Número.de.pessoas.no.domicílio
modelo_quant3 <- rq(eq.3,tau =tau , method = "fn", data = Pnad)
AIC.rqs(modelo_quant3)

#funções para gerar os gráfico do R¹(\tau)

grafR1 = function(modelo.rqs, trueScale=T){
  taus = modelo.rqs$tau
  rho.c = modelo.rqs$rho
  methods = modelo.rqs$method
  y = modelo.rqs$y
  rho.r = rq(y~ 1, tau = taus, method = methods)$rho
  
  R1 = 1 - rho.c/rho.r
  data.graph =  tibble(taus, R1)
  
  saida = list(values = data.graph, variable = paste(modelo.rqs$call$formula[3], "", sep = ""))
  
  if(trueScale) graph = ggplot(data.graph, aes(x = taus, y = R1)) + ylim(c(0,1)) 
  else graph = ggplot(data.graph, aes(x = taus, y = R1))
  
  graph = graph + geom_line() + ylab(expression(R^{1}*(tau))) + xlab(expression(tau)) +theme_light()+
    theme(axis.title = element_text(size = 18), axis.text = element_text(size = 17)) 
  
  saida.final = list(data = saida, graph = graph)
  return(saida.final)
}
#salvando a saída
r1 = grafR1(modelo_quant2)$graph
r2 = grafR1(modelo_quant1)$graph

#personalisando
modelo2 = r1 + ggtitle("Modelo Reduzido") + theme(plot.title=element_text(hjust=0.5, size = 20))
modelo1 = r2 + ggtitle("Modelo Completo") + theme(plot.title=element_text(hjust=0.5, size = 20))

Modelo = gridExtra::grid.arrange(modelo1,modelo2, ncol = 2)
ggsave(filename = "r1.png", plot = Modelo, device = png(width = 1200, height = 800))

#Gráfico dos coeficientes
grafcoef = function(modelo, level = 0.95, se){
  tau = modelo$tau
  if (se == "boot") info = summary(modelo, se = se) 
  else info = summary(modelo, se = se)
  
  zalpha = qnorm(1 - (1 - level)/2)
  
  if(se != "rank"){
    cf = lapply(info, coef)
    for (i in 1:length(cf)) {
      cfi = cf[[i]]
      cfi = cbind(cfi[,1], cfi[,1] - cfi[,2]*zalpha, cfi[,1] + cfi[,2]*zalpha)
      colnames(cfi) = c("coefficients", "lower bd", "upper bd")
      cf[[i]] =  cfi
    }
  }
  else {
    cf = lapply(info, coef)
    for (i in 1:length(cf)) {
      cfi = cf[[i]]
      cfi = cbind(cfi[,1], cfi[,2], cfi[,3] )
      colnames(cfi) = c("coefficients", "lower bd", "upper bd")
      cf[[i]] =  cfi
    
  }
  }
  lim.inf = as.numeric(unlist(lapply(cf, function(x) x[,2])))
  lim.sup = as.numeric(unlist(lapply(cf, function(x) x[,3])))
  est.coef = as.numeric(unlist(lapply(cf, function(x) x[,1])))
  
  variaveis = rownames(coef(info[[1]]))
  
  dados = tibble(Tau = rep(tau,each = length(variaveis)),
                 Variaveis = rep(variaveis, length(tau)),
                 est.coef, lim.sup,lim.inf)
  
  lapply(variaveis, function(x){
    graph = ggplot(dados[dados$Variaveis==x,], aes(x = Tau, y = est.coef)) +
      facet_wrap(~Variaveis,scales = "free",drop = F)
    graph = graph + xlab(expression(tau)) + ylab("Coeficientes")
    graph = graph + geom_ribbon(aes(ymin = lim.inf, ymax = lim.sup), fill = "grey50") +
      geom_point() +geom_line(linetype = 2)
  })

}
#personalisando
coeficientes = grafcoef(modelo_quant2, se = "boot")

coef1 = coeficientes[[2]] 

coef1 = coef1 + theme_light() + theme(axis.title = element_text(size = 18), axis.text = element_text(size = 17),strip.background = element_blank(),
                       strip.text = element_text(size = 18),plot.title=element_text(hjust=0.5, size = 20)) + ggtitle("Idade")

coef10 = coeficientes[[3]]
coef10 = coef2 + theme_light() + theme(axis.title = element_text(size = 18), axis.text = element_text(size = 17),strip.background = element_blank(),
                                      strip.text = element_text(size = 18),plot.title=element_text(hjust=0.5, size = 20)) + ggtitle("N° de \nIndivíduos")
coef10

var1 = gridExtra::grid.arrange(coef1, coef8,ncol = 2)
var2 = gridExtra::grid.arrange(coef7, coef9,ncol = 2)
var3 = gridExtra::grid.arrange(coef6, coef10,ncol = 2)
var4 = gridExtra::grid.arrange(coef2, coef3,ncol = 2)
var5 = gridExtra::grid.arrange(coef4, coef5,ncol = 2)

ggsave(filename = "var5.png", plot = var5, device = png(width = 1200, height = 800))



# Aplicação 2 ---------------------------------------------------------
library(quantreg)
library(tidyverse)
library(jtools)
library(lmtest)
library(nortest)
library(skimr)
setwd("~/Etatistica/TCC/aplicacao")
#carregando a base de dados
admissao = read_csv(file = "Admission_Predict_ver1.1.csv")

admissao$Research = factor(admissao$Research, labels = c("Não","Sim"))
admissao$`University Rating` = factor(admissao$`University Rating`)

admissao = admissao |> 
  select(-`Serial No.`)
  
base_explo = skim(admissao)
base_explo
# Histograma da variável Chance de admissão

h1 = ggplot(data = admissao) +
  geom_density(mapping = aes(x = `Chance of Admit`,
                             y = ..density..), binwidth = 250, fill = "deepskyblue4",
               col = "black",alpha = 0.5)+
  scale_x_continuous(breaks = seq(0, 1, 0.2), 
                     limits=c(0, 1)) +
  labs(title = "Densidade da Chance de admissão.",
       x = "Chance de admissão",
       y = "Densidade") +
  theme_light() +
  theme(plot.title = element_text(size = 20, hjust=0.5),
        axis.text = element_text(size = 17), axis.title = element_text(size = 18))

#ggsave(filename = "histograma_base2.png", plot = h1, device = png(width = 1250, height = 800))

#Variáveis Qualitativas

#Research
g1 = ggplot(admissao) +
  geom_boxplot(aes(x = `Chance of Admit`, y = Research,fill = Research), outlier.shape = 1) + 
  scale_fill_manual(values = hcl.colors(n = 2, palette = "Viridis")) +
  guides(fill="none")+theme_light() +
  labs(x = "Chance de admissão", y="Experiência em pesquisa") + 
  ggtitle("Chance de admissão por experiência em pesquisa")+
  scale_x_continuous(limits = c(0, 1)) +
  theme(plot.title = element_text(size = 20, hjust=0.5),
        axis.text = element_text(size = 17), axis.title = element_text(size = 18))

# 
g2 = ggplot(admissao |> 
              mutate(Universidade = ordered(`University Rating`))) +
  geom_boxplot(aes(x = `Chance of Admit`, y = Universidade, fill = Universidade), outlier.shape = 1) + 
  scale_fill_manual(values = hcl.colors(n = 5, palette = "Viridis")) +
  guides(fill="none")+theme_light() +
  labs(x = 'Chance de admissão', y="Classificação da Universidade") + 
  ggtitle("Chance de admissão por classificação da Universidade")+
  scale_x_continuous(limits = c(0, 1)) +
  theme(plot.title = element_text(size = 20, hjust=0.5),
        axis.text = element_text(size = 17), axis.title = element_text(size = 18))

#SOR
g3 = ggplot(admissao |> 
              mutate(SOP = ordered(SOP))) +
  geom_boxplot(aes(x = `Chance of Admit`, y = SOP, fill = SOP), outlier.shape = 1) + 
  scale_fill_manual(values = hcl.colors(n = 9, palette = "Viridis")) +
  guides(fill="none")+theme_light() +
  labs(x = 'Chance de admissão', y="Pontuação da declaração de objetivos") + 
  ggtitle("Chance de admissão por pontuação da declaração de objetivos")+
  scale_x_continuous(limits = c(0, 1)) +
  theme(plot.title = element_text(size = 20, hjust=0.5),
        axis.text = element_text(size = 17), axis.title = element_text(size = 18))

#LOR
g4 = ggplot(admissao |> 
              mutate(LOR = ordered(LOR))) +
  geom_boxplot(aes(x = `Chance of Admit`, y = LOR, fill = LOR), outlier.shape = 1) + 
  scale_fill_manual(values = hcl.colors(n = 9, palette = "Viridis")) +
  guides(fill="none")+theme_light() +
  labs(x = 'Chance de admissão', y="Pontuação da Carta de recomendação") + 
  ggtitle("Chance de admissão por pontuação da carta de recomendação")+
  scale_x_continuous(limits = c(0, 1)) +
  theme(plot.title = element_text(size = 20, hjust=0.5),
        axis.text = element_text(size = 17), axis.title = element_text(size = 18))

G1 = gridExtra::grid.arrange(g1,g2,g3,g4, ncol = 2)
#ggsave(filename = "qualitativas.png", plot = G1, device = png(width = 1250, height = 800))

#variáveis Quantitativas

#GRE
g5 = ggplot(admissao, 
            aes(x = `GRE Score`, 
                y = `Chance of Admit`)) +
  geom_point(color="#27408B", 
             size = 2, 
             alpha = 1) +
  scale_x_continuous(limits = c(250,350))+  
  ggtitle("Chance de admissão por pontuação do exame GRE") + 
  labs(y = "Chance de admissão", x = "Pontuação do exame GRE") +
  guides(fill="none")+ theme_light() + 
  theme(axis.text = element_text(size = 17), axis.title = element_text(size = 17),
        plot.title = element_text(size = 20, hjust=0.5))

# TOEFL
g6 = ggplot(admissao, 
            aes(x = `TOEFL Score`, 
                y = `Chance of Admit`)) +
  geom_point(color="#27408B", 
             size = 2, 
             alpha = 1) +
  scale_x_continuous(limits = c(90,130))+  
  ggtitle("Chance de admissão por pontuação do Teste de Inglês") + 
  labs(y = "Chance de admissão", x = "Pontuação do Teste de Inglês") +
  guides(fill="none")+ theme_light() + 
  theme(axis.text = element_text(size = 17), axis.title = element_text(size = 17),
        plot.title = element_text(size = 20, hjust=0.5))

#CGPA
g7 = ggplot(admissao, 
            aes(x = CGPA, 
                y = `Chance of Admit`)) +
  geom_point(color="#27408B", 
             size = 2, 
             alpha = 1) +
  scale_x_continuous(limits = c(5,10))+  
  ggtitle("Chance de admissão por pela média de pontos das notas") + 
  labs(y = "Chance de admissão", x = "Média de pontos da notas") +
  guides(fill="none")+ theme_light() + 
  theme(axis.text = element_text(size = 17), axis.title = element_text(size = 17),
        plot.title = element_text(size = 20, hjust=0.5))

lay <- rbind(c(1,2),
             c(3,NA))
G2 = gridExtra::grid.arrange(g5,g6,g7, layout_matrix = lay)
ggsave(filename = "quantitativas.png", plot = G2, device = png(width = 1250, height = 800))


#modelo de regressão quantílica
tau = c(0.10,0.25,0.50,0.75,0.90)

#modelo completo
fit1 <- rq(`Chance of Admit` ~ Research + `GRE Score` + `TOEFL Score` + `University Rating` + SOP + LOR + CGPA ,tau =0.25, method = "br", data = admissao)

summ(fit1, se = "boot", boot.method = "mcmb", digits = 3)
y = predict(fit1)
max(y)
min(y)
#modelo reduzido

fit2 <- rq(`Chance of Admit` ~ Research + `GRE Score` + `TOEFL Score`  + LOR + CGPA ,tau =tau , method = "br", data = admissao)

summ(fit2, se = "boot", boot.method = "mcmb", digits = 3)

#aic
aic1 = AIC.rqs(object = fit1, k = log(500))
aic2 = AIC.rqs(object = fit2, k = log(500))

#gráfico do R^1

r1 = grafR1(fit1)$graph
r2 = grafR1(fit2)$graph

modelo1 = r1 + ggtitle("Modelo Completo") + theme(plot.title=element_text(hjust=0.5, size = 20))
modelo2 = r2 + ggtitle("Modelo Reduzido") + theme(plot.title=element_text(hjust=0.5, size = 20))

Modelo = gridExtra::grid.arrange(modelo1,modelo2, ncol = 2)
ggsave(filename = "r1_2.png", plot = Modelo, device = png(width = 1200, height = 800))

#coeficientes 
coeficientes = grafcoef(fit2, se = "boot")

coef5 = coeficientes[[6]] 

coef5 = coef5 + theme_light() + theme(axis.title = element_text(size = 18), axis.text = element_text(size = 17),strip.background = element_blank(),
                                      strip.text = element_text(size = 18),plot.title=element_text(hjust=0.5, size = 20)) + ggtitle("Pontuação do GPA")

var6 = gridExtra::grid.arrange(coef2, coef3,ncol = 2)
var7 = gridExtra::grid.arrange(coef4, coef5,ncol = 2)

ggsave(filename = "var2_2.png", plot = var7, device = png(width = 1200, height = 800))

#função para ferar o gráfico dos resíduos quantílicos
grafResiduos = function(modelo, scales = "fixed"){
  tau = modelo$tau
  n = ifelse(length(tau)==1, length(residuals(modelo)), nrow(residuals(modelo)))
  preditos = fitted(modelo)
  rho.hat = modelo$rho/n
  
  if(length(tau) > 1){
    rho.hat = modelo$rho/n
    residuos = as.vector(residuals(modelo))
    preditos = as.vector(fitted(modelo))
    tau = rep(tau, each = n)
    
    dados = data.frame(preditos, residuos, tau)
    graph = ggplot(dados, aes(x = preditos, y = residuos, group = tau)) + geom_point() +
      facet_wrap(~tau,ncol = 3, scales = scales)
    
    graph = graph + geom_hline(aes(yintercept = 0), col="#EE2C2C",lwd = 1) + 
      geom_hline(aes(yintercept = -0.5), col="#CD2626",lty=2,lwd = 1) + 
      geom_hline(aes(yintercept = 0.5), col="#8B1A1A",lty=2,lwd = 1)+
      labs(x = "Valores Preditos",
           y = "Resíduos Quantílicos")
      
  }
  else {
    residuos = residuals(modelo)
    dados = data.frame(preditos, residuos)
    
    graph = ggplot(dados, aes(x = preditos, y = residuos)) + geom_point()
    
    graph = graph + geom_hline(aes(yintercept = 0), col="#EE2C2C",lwd = 1) + 
      geom_hline(aes(yintercept = -0.5), col="#CD2626",lty=2,lwd = 1) + 
      geom_hline(aes(yintercept = 0.5), col="#8B1A1A",lty=2,lwd = 1)+
      labs(x = "Valores Preditos",
           y = "Resíduos Quantílicos")
  }
  return(graph)
}

resi = grafResiduos(fit2)
resi = resi +  theme_light() + theme(axis.title = element_text(size = 18), axis.text = element_text(size = 17),
                                            strip.text = element_text(size = 18, colour = "black"),plot.title=element_text(hjust=0.5, size = 20))

ggsave(filename = "residu.png", plot = resi, device = png(width = 1200, height = 800))

#histograma dos residuos
tau.01 <- rq(`Chance of Admit` ~ Research + `GRE Score` + `TOEFL Score`  + LOR + CGPA ,tau =0.1 , method = "br", data = admissao)
tau.025 <- rq(`Chance of Admit` ~ Research + `GRE Score` + `TOEFL Score`  + LOR + CGPA ,tau =0.25 , method = "br", data = admissao)
tau.05 <- rq(`Chance of Admit` ~ Research + `GRE Score` + `TOEFL Score`  + LOR + CGPA ,tau =0.5 , method = "br", data = admissao)
tau.075 <- rq(`Chance of Admit` ~ Research + `GRE Score` + `TOEFL Score`  + LOR + CGPA ,tau =0.75 , method = "br", data = admissao)
tau.09 <- rq(`Chance of Admit` ~ Research + `GRE Score` + `TOEFL Score`  + LOR + CGPA ,tau =0.9 , method = "br", data = admissao)

residuos = tibble(tau.01 = residuals(tau.01),
                  tau.025 = residuals(tau.025),
                  tau.05 = residuals(tau.05),
                  tau.075 = residuals(tau.075),
                  tau.09 = residuals(tau.09))

t1 = ggplot(residuos, aes(x = tau.01,
                      y = ..density..)) +
geom_histogram(fill = "deepskyblue4", colour = "Black", bins = 30) +
  labs(x = "Resíduos Quantílicos",y = "Densidade",
       title = "Tau = 0,10") + 
  theme_light() + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 17),plot.title=element_text(hjust=0.5, size = 20))

t2 = ggplot(residuos, aes(x = tau.025,
                          y = ..density..)) +
  geom_histogram(fill = "deepskyblue4", colour = "Black", bins = 30) +
  labs(x = "Resíduos Quantílicos",y = "Densidade",
       title = "Tau = 0,25") + 
  theme_light() + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 17),plot.title=element_text(hjust=0.5, size = 20))

t3 = ggplot(residuos, aes(x = tau.05,
                          y = ..density..)) +
  geom_histogram(fill = "deepskyblue4", colour = "Black", bins = 30) +
  labs(x = "Resíduos Quantílicos",y = "Densidade",
       title = "Tau = 0,50") + 
  theme_light() + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 17),plot.title=element_text(hjust=0.5, size = 20))

t4 = ggplot(residuos, aes(x = tau.075,
                          y = ..density..)) +
  geom_histogram(fill = "deepskyblue4", colour = "Black", bins = 30) +
  labs(x = "Resíduos Quantílicos",y = "Densidade",
       title = "Tau = 0,75") + 
  theme_light() + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 17),plot.title=element_text(hjust=0.5, size = 20))

t5 = ggplot(residuos, aes(x = tau.09,
                          y = ..density..)) +
  geom_histogram(fill = "deepskyblue4", colour = "Black", bins = 30) +
  labs(x = "Resíduos Quantílicos",y = "Densidade",
       title = "Tau = 0,90") + 
  theme_light() + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 17),plot.title=element_text(hjust=0.5, size = 20))

hito.resi = gridExtra::grid.arrange(t1,t2,t3,t4,t5)

ggsave(filename = "hitograma_resi.png", plot = hito.resi , device = png(width = 1200, height = 800))
