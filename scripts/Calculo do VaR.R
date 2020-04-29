#=========================================================================================================================
#
# > Autor: Robson Waite
# > Versão: 1.0
# > Data: 29/04/2020
# > Titulo:  calculo do Var
# > Descrição: criaçao de base, e calculo de VaR para carteira com uma e mais ações.
#
#=========================================================================================================================





#1.0 - Pacotes ===========================================================================================================



#2.0 - Criando Função - VaR ==============================================================================================

calc_var = function(diretorio,data_ini,data_fin, posicao) {                                       # Formato da data 'dd/mm/AAAA'.
  arquivos = list.files(diretorio)                                                                # Lista os arquivos csv dentro da pasta selecionada.
  resultado = data.frame(matrix(ncol=5, nrow = length(arquivos)), row.names = arquivos)                         # Os resultados serão incluidos nas respectivas linhas de cada arquivo.
  colnames(resultado) = c('Posicao', 'DP_RA','M_RA', 'DP_RG', 'M_RG')                             # Nome das colunas: posição; devp. arit; media aritm; devp. geom; media geom. 
  for (csv in arquivos) {                                                                         # Loop pelos nomes da lista.
    dt = read.csv(file = paste0('.//data//portifolio_xp//',csv) ,header = T, encoding = 'UTF-8')  # Carregando Arquivo CSV a partir de nome da lista.
    dt = dt[,-2]                                                                                  # Inicio do tratamento de dados _ Removendo dados não utilizados.
    dt = dt[,-c(4:6)]                                                                             # Removendo dados não utilizados.
    dt[,2] = gsub(',','\\.',dt[,2])                                                               # Trocando virgula por ponto.
    dt[,2] = as.numeric(dt[,2])                                                                   # Transformando dados numericos de strings para numeric.
    dt[,1] = as.character(dt[,1])                                                                 # Transformando a coluna de data de factores para strings.
    dt[,3] = gsub(',','\\.',dt[,3])                                                               # Trocando virgula por ponto.
    dt[,3] = as.numeric(dt[,3])                                                                   # Transformando dados numericos de strings para numeric.
    dt[,1] = as.character(dt[,1])                                                                 # Transformando a coluna de data de factores para strings.
    dt[,1] = as.Date(dt[,1], format = '%d/%m/%Y')                                                 # Transformando a coluna de data de factores para strings.
    data_ini = as.Date(data_ini, format = '%d/%m/%Y')                                             # Formatando data para filtro.
    data_fin = as.Date(data_fin, format = '%d/%m/%Y')                                             # Formatando data para filtro.
    posicao = as.Date(posicao, format = '%d/%m/%Y')                                               # Formatando data para filtro.
    dtp = subset(dt, dt[,1] ==  posicao)
    dtf = subset(dt, dt[,1] > data_ini & dt[,1] < data_fin)                                       # Filtrando data.
    dtf[,4] = 0                                                                                   # Gerando coluna de Retorno Geometrico.
      for (x in 2:(nrow(dtf)-1)) {
        dtf[[x,4]] = log(dtf[x,2])-log(dtf[x+1,2])                                                # Calculando Retorno geometrico => X é a linha hoje, pelos dados ontem será X+1.
      }
    resultado[csv,1] = dtp[1,2]
    resultado[csv,2] = round(sd(dtf[,3]), 4)                                                               # Desvio Padrão do Retorno Aritmetico
    resultado[csv,3] = round(mean(dtf[,3]), 4)                                                              # Média do Retorno Aritimetico
    resultado[csv,4] = round(sd(dtf[,4]), 4)                                                                # Desvio Padrão do Retorno Geometrico
    resultado[csv,5] = round(mean(dtf[,4]),4)                                                              # Média do Retorno Geometrico
  }
  return(resultado)
}

#3 - Aplicação segundo dados propostos no arquivo info.txt ===============================================================


setwd('C://Arquivos//GestaodeRisco')                           # Configurando local de trabalho                 
diretorio = './data/portifolio_xp'                             # Configurando pasta de arquivos
data_ini = '31/01/2019'                                        # data de inicio 
data_fin = '01/04/2019'                                        # data final
posicao = '01/04/2019'                                         # data da posição

var = calc_var(diretorio = diretorio, data_ini = data_ini, data_fin = data_fin, posicao = posicao)






