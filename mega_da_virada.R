options(scipen=999, OutDec=",")
if(!("gmp" %in% installed.packages())){install.packages("gmp")}
library(gmp)
mega <- function(n){
  if (!is.numeric(n) || length(n) != 1 || n %% 1 != 0 || n <= 0) {
    stop("'n' deve ser um número inteiro positivo")
  }
  if (n > 60) {stop("Há 60 dezenas no bilhete, logo 'n' não pode ser superior a 60.")}
  if(n < 6){stop("O bilhete deve ter no mínimo 6 dezenas marcadas!")}
  prob_quadra <- as.numeric(chooseZ(n,4)*chooseZ(60-n,2)/chooseZ(60,6))
  prob_quina <- as.numeric(chooseZ(n,5)*chooseZ(60-n,1)/chooseZ(60,6))
  prob_sena <- as.numeric(chooseZ(n,6)/chooseZ(60,6))
  prob_algum_premio <- prob_quadra+prob_quina+prob_sena
  return(message(c(paste0("Em um jogo com ",n," dezenas:"),"\n\n",
                   paste0("A probabilidade de fazer uma quadra é de ",
                          round(100*prob_quadra,6),"%;"),"\n",
                   paste0("A probabilidade de fazer uma quina é de ",
                          round(100*prob_quina,6),"%;"),"\n",
                   paste0("A probabilidade de acertar as seis dezenas é de ",
                          round(100*prob_sena,6),"%;"),"\n",
                   paste0("A probabilidade de ganhar algum prêmio é de ",
                          round(100*prob_algum_premio,6),"%.")
)))
}

# TESTANDO:
mega(6)

mega(8)

mega(10)

mega(12)

mega(15)

mega(20)

mega(57)

mega(58)

mega(59)


mega(60)
