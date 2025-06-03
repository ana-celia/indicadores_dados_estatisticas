getwd()
setwd('/Users/anaceliabaiaaraujo/Downloads')

library(descr)
library(dplyr)
#checar o erro na instalaçao da tableone, se requer install.packages("survey")
library(tableone)

install.packages("survey", dependencies = TRUE)
install.packages("descr")
install.packages('dplyr')
install.packages("tableone")

# Inspecionar CSV
file.head("Censo2010_dompes_RN.csv")

# Ler arquivo em CSV
CENSORN = read.csv("Censo2010_dompes_RN.csv",
                   sep=",", header=TRUE)
# Ler arquivo em .RDS
CENSO2010 <- readRDS("Censo2010_RN.rds")

# listar as variaveis do seu objeto
names(CENSORN)
# Selecionando Variaveis para uma base de dados mais reduzida
variaveis <- c("V0001", "code_muni", "V0300",
               "V0011", "V0010", "V1006", "V4001", "V0201",
               "V2011", "V0207", "V6203", "V6204", "V6531",
               "V0601", "V6036", "V0606", "V6400", "V6511",
               "V6910", "V6210", "V5070", "V5090")

# Ver se todas as variaveis da lista estao presentes no banco
which(!variaveis %in% names(CENSORN))

#Criando um objeto contendo apenas as variaveis selecionadas
BaseRN <- CENSORN[variaveis]

# Atribuindo nomes as colunas do banco de dados
colnames(BaseRN) <- c(
  "UF", "code_muni", "ncontrole", "AREAP", "pesopes",
  "situacao_dom", "tipo_dom", "condicao_dom",
  "aluguel", "esgotamento", "densidad_comodo",
  "densidad_dormitorio", "renda_percapita", "sexo",
  "idade", "cor", "escolaridade", "rendimento_trabalho",
  "condicao_ocup", "adeq_moradia", "renda_familiar",
  "composicao_familiar")

View(BaseRN) # Visualizar banco de dados

# SITUACAO DO DOMICILIO (1 - Urbano e 2 - Rural)
BaseRN$situacao_dom<-
  factor(BaseRN$situacao_dom,
         levels=c(1,2), labels=c("Urbano", "Rural"))
Summary(BaseRN$situacao_dom)

# CONDICAO NO DOMICILIO
BaseRN$condicao_dom[BaseRN$condicao_dom ==1 |
                      BaseRN$condicao_dom ==2] <-
  "Proprio"
BaseRN$condicao_dom[BaseRN$condicao_dom ==3] <- "Alugado"
BaseRN$condicao_dom[BaseRN$condicao_dom ==4 |
                      BaseRN$condicao_dom ==5] <- "Cedido"
BaseRN$condicao_dom[BaseRN$condicao_dom ==6] <- "Outro"
# Tratar valores em branco ou "Branco" como NA
BaseRN$condicao_dom[BaseRN$condicao_dom %in% c("Branco", "",
                                               NA)] <- NA

# CONDICAO NO DOMICILIO
BaseRN$condicao_dom[BaseRN$condicao_dom ==1 |
                      BaseRN$condicao_dom ==2] <-
  "Proprio"
BaseRN$condicao_dom[BaseRN$condicao_dom ==3] <- "Alugado"
BaseRN$condicao_dom[BaseRN$condicao_dom ==4 |
                      BaseRN$condicao_dom ==5] <- "Cedido"
BaseRN$condicao_dom[BaseRN$condicao_dom ==6] <- "Outro"
# Tratar valores em branco ou "Branco" como NA
BaseRN$condicao_dom[BaseRN$condicao_dom %in% c("Branco", "",
                                               NA)] <- NA

# Converter em fator com niveis ordenados
BaseRN$condicao_dom <- factor(
  BaseRN$condicao_dom, levels = c("Proprio",
                                  "Alugado", "Cedido", "Outro"))
# Exibe a frequencia absoluta
Table(BaseRN$condicao_dom)

# SEXO
BaseRN$sexo[BaseRN$sexo ==1] = "Homem"
BaseRN$sexo[BaseRN$sexo ==2] = "Mulher"
# Exibe a frequencia absoluta
table(BaseRN$sexo)

# Escolaridade
BaseRN <- BaseRN %>%
  mutate(escolaridade = case_when(escolaridade == 5
                                  ~ NA_integer_, TRUE ~ escolaridade ), escolaridade
         = factor(escolaridade, levels = c(1, 2, 3, 4),
                  labels = c("Sem instrucao e fund. incompleto",
                             "Fund. completo e medio incompleto",
                             "Medio completo e sup. incompleto",
                             "Superior completo"), ordered = TRUE))
table(BaseRN$sexo) # Tabela

#Cor
BaseRN$cor[BaseRN$cor==1] = "Branca"
BaseRN$cor[BaseRN$cor==2] = "Preta"
BaseRN$cor[BaseRN$cor==4] = "Parda"
BaseRN$cor[BaseRN$cor==3| BaseRN$cor==5] <-
  BaseRN$cor[BaseRN$cor==9] = "Ignorado"
table(BaseRN$cor) # Tabela

#GRUPO ETARIO
BaseRN$idade <- cut(BaseRN$idade,
                    breaks=c(-Inf,14,24,34,44,54,64, Inf),
                    labels=c("10 a 14", "15 a 24", "25 a 34", "35 a
44","45 a 54", "55 a 64", "65 e mais"))

#Rendimento domiciliar per capita
BaseRN <- BaseRN %>% mutate(faixa_renda =
                              case_when( renda_percapita <= 0.5 * 510 ~ "Ate 1/2
SM", renda_percapita <= 1 * 510 ~ "1/2 ate 1 SM",
                                         renda_percapita <= 2 * 510 ~ "1 ate 2 SM",
                                         renda_percapita <= 3 * 510 ~ "2 ate 3 SM",
                                         renda_percapita > 3 * 510 ~ "Mais de 3 SM", TRUE ~
                                           NA_character_), faixa_renda = factor(faixa_renda,
                                                                                levels = c("Ate 1/2 SM", "1/2 ate 1 SM", 
                                                                                           "1 ate 2 SM", "2 ate 3 SM", "Mais de 3 SM"), 
                                                                                ordered = TRUE))

# CONDICAO DE OCUPACAO
BaseRN$condicao_ocup[BaseRN$condicao_ocup==1] = "Ocupadas"
BaseRN$condicao_ocup[BaseRN$condicao_ocup==2] = "Desocupadas"
table(BaseRN$condicao_ocup)

# ADEQUACAO DE MORADIA
BaseRN$adeq_moradia[BaseRN$adeq_moradia==1] <-
  "Adequada"
BaseRN$adeq_moradia[BaseRN$adeq_moradia==2] <-
  "Semi-adequada"
BaseRN$adeq_moradia[BaseRN$adeq_moradia==3] <-
  "Inadequada"

# Tratar valores em branco ou "Branco" como NA
BaseRN$adeq_moradia[BaseRN$adeq_moradia
                    %in% c("Branco", "", NA)] <- NA
# Converter em fator com niveis ordenados (se quiser)
BaseRN$adeq_moradia <- factor(
  BaseRN$adeq_moradia, levels = c("Adequada",
                                  "Semi-adequada", "Inadequada"))
table(BaseRN$adeq_moradia)

# TIPO DE COMPOSICAO FAMILIAR
BaseRN <- BaseRN %>%
  mutate(composicao_familiar_agrupada = case_when(
    composicao_familiar %in% c(1, 2) ~ "Casal sem filho(s)",
    composicao_familiar %in% c(3, 4) ~ "Casal com filho(s)",
    composicao_familiar %in% c(5, 6) ~ "Mulher sem conjuge com
filho(s)",
    composicao_familiar %in% c(7, 8) ~ "Homem sem conjuge com
filho(s)",
    composicao_familiar == 9 ~ "Outro",
    is.na(composicao_familiar) ~ "Branco"))

# Salve o banco reduzido e na extensao R
save(BaseRN,file="BaseRN.Rdata")



