
# Gráfico dos exemplos da secção Regressão Quantílica ---------------------

#setando a pasta onde está o arquivo
setwd("~/Etatistica/TCC/dados")

#carregando os arquivos
base = read_table(file = "Dados de poluição de cidades norte-americanas.txt")

#gráfico média versus mediana
ggplot(base, aes(x = POP, y = SO2)) +
  geom_point(size=2) +
  labs(x="População (Milhares)",y="SO2 (mg)") +
  theme(plot.title=element_text(hjust=0.5))+
  geom_smooth(method=lm,se=F, aes(colour = "Média"))+
  geom_quantile(quantiles = 0.5, aes(colour = "Mediana")) +
  scale_color_manual(values = c("#CD5B45","#27408B"))  +
  theme_light()+
  labs(color = "Método")+ 
  theme(panel.border = element_rect(color = "black", size = 1))

#gráfico com o ajuste de diferentes quantis 
ggplot(base, aes(x = POP, y = SO2)) +
  geom_point(size=2) +
  labs(x="População (Milhares)",y="SO2 (mg)") +
  geom_quantile(quantiles = c(0.05), aes(color = c("Tau = 0.05"))) +
  geom_quantile(quantiles = c(0.25), aes(color = c("Tau = 0.25")))+
  geom_quantile(quantiles = c(0.5), aes(color = c("Tau = 0.50")))+
  geom_quantile(quantiles = c(0.75), aes(color = c("Tau = 0.75")))+
  geom_quantile(quantiles = c(0.95), aes(color = c("Tau = 0.95")))+
  scale_fill_manual(values = hcl.colors(n = 6, palette = "Viridis"))+
  theme_light()+
  labs(color = "Quantis")+ 
  theme(panel.border = element_rect(color = "black", size = 1))
