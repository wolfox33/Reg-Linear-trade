{
  library(quantmod)
  library(data.table)
  library("PerformanceAnalytics")
  
  # Importar Base CSV do Metatrader
  quotes <- read.csv('C:/Users/files/GBPUSD_D1.CSV', skip=5000, header=FALSE)
  df <- as.xts(quotes[,-(1:2)], as.POSIXct(strptime(paste(quotes[,1],quotes[,2]),format="%Y.%m.%d %H:%M")))
  colnames(df) <-c("Open","High","Low","Close","Volume")
  
  #Criar Alvo
  df$Return <- Return.calculate(df$Close, method = c("discrete"))*100
  df$Alvo <- lag.xts(df$Return,k=-1)
  df$Alvo[is.na(df$Alvo)] <- median(df$Alvo, na.rm=TRUE)
  df <- na.omit(df)
  df$Alvo_Bin <- ifelse(df$Alvo > 0, 1,0)
  
  #Criar Zscore
  periodo_mm <- 14
  df$MA_Return <-SMA(df$Return, periodo_mm)
  df$SD_Return <- c(rep(NA, periodo_mm-1),runsd(df$Return, periodo_mm, endrule=c("trim")))
  df$ZScore <- (df$Return-df$MA_Return)/df$SD_Return
  
  #Criar RSL
  df$MA_Close <- SMA(df$Close, periodo_mm) 
  df$RSL <- (df$Close-df$MA_Close)
  
  #Criar shadow
  df$shadow <- ifelse(df$Return > 0, df$Open-df$Low, df$High-df$Open)
  df$MA_shadow <- c(rep(NA, periodo_mm-1), runmean(df$shadow, periodo_mm, alg=c("C"), endrule=c("trim")))
  df <- na.omit(df)
}
  # Criando os indicadores
{  
  df$ATR <- ATR(df[,c("High","Low","Close")],n=14)
  df$Bbands <- BBands(df$Close, 20, "SMA", 2)[,4]
  df$CCI <- CCI(df$Close, 14)
  df$MACD <- MACD(df$Close, 12, 26, 9, "SMA") 
  df$stochWPR <- WPR(df[,c("High","Low","Close")])
  df$MFI <- MFI(df$Close, df$Volume, n=14)
  df$OBV <- OBV(df$Close,df$Volume)
  df$RSI <- RSI(df$Close,n=14)
  

  df$StopLoss <- round(df$atr*0.8,5)
  
  df <- na.omit(df)
}

#Treinamento e teste -
{
  w <- 0.5 #Tamanho do Treinamento
  trainstart <- 1
  trainend <- round(w*length(df[,1]))
  teststart <- round(w*length(df[,1]))+1
  testend <- length(df[,1])
  training <- df[trainstart:trainend,]
  testing <- df[teststart:testend,]
}

# Deixando apenas as variaveis mais relevantes
modelo1 <- lm(Alvo ~ ZScore+MACD+RSL+ATR+Bbands+CCI+stochWPR+MFI+OBV+RSI
                 ,
               data = training)  
summary(modelo1)

{
  corte_up = 0
  corte_low = 0
    # Custos operacionais
  custo_op = 0
  # Teste
  testing$predicao <- predict(modelo1, type='response',testing) 
  retorno_BH <- round(testing$Return,2)
  retorno_BH_acumulado <- cumsum(retorno_BH)
  
  retorno_modelo1 <- ifelse(testing$predicao > corte_up, testing$Alvo - custo_op,
                            ifelse(testing$predicao < corte_low, -1*testing$Alvo-custo_op,0))
  retorno_modelo1_acumulado <- cumsum(retorno_modelo1)

}

{
  retorno_modelo1[1:2] <- 0
  retorno_BH[1:2] <- 0
  names(retorno_BH) <- 'Benchmark'
  names(retorno_modelo1) <- 'Modelo RegLinear'
  charts.PerformanceSummary(cbind(retorno_modelo1,retorno_BH),geometric = FALSE)
}
Performance <- function(x) {
  
  cumRetx = Return.cumulative(x, geometric = FALSE)
  annRetx = Return.annualized(x, scale=100, geometric = FALSE)
  sharpex = SharpeRatio.annualized(x, Rf=retorno_BH, scale=100, geometric = FALSE)
  winpctx = length(x[x > 0])/length(x[x != 0])*100
  annSDx = sd.annualized(x, scale=100 , geometric = TRUE)
  
  DDs <- findDrawdowns(x, geometric = FALSE)
  DDs$return[1]=0
  maxDDx = min(DDs$return, geometric = FALSE) 
  maxLx = max(DDs$length , geometric = FALSE)
  maxPTT = max(DDs$peaktotrough)
  recovery = max(DDs$recovery)
  Perf = round(c(cumRetx, annRetx, sharpex, winpctx, annSDx, maxDDx, maxLx, maxPTT, recovery),2)
  names(Perf) = c("Acumulado", "Retorno /100","Sharpe Ratio /100",
                  "Win %", "Volatilidade /100", "Maximum Drawdown", "Max periodo em Drawdown","Peak to Trough", "Recovery")
  return(Perf)
}
cbind(MODELO=Performance(retorno_modelo1), Benchmark=Performance(retorno_BH))  

