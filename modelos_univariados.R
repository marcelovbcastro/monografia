rm(list = ls())
graphics.off()

library(tsm)
library(mFilter)
library(readxl)
library(rucm)

# Carregando os dados
dados <- read_excel("C:/Users/mvboa/OneDrive/Área de Trabalho/TCC/Dados/Base de dados.xlsx", sheet = "Dados")
pibd <- ts(dados["ln(pibd)"], start = c(1996, 1), end = c(2023,3) , frequency = 4)

# Tendência Linear
lin.mod <- lm(pibd ~ time(pibd))
lin.tendencia <- lin.mod$fitted.values  # Estimando os valores da tendência
linear <- ts(lin.tendencia, start = c(1996, 1), frequency = 4)  # Cria uma séria temporal para a tendência
lin.hiato <- pibd - linear  # O hiato é a diferençla entre o dado real e a tendência
lin.resultado <- data.frame(linear, lin.hiato)
names(lin.resultado) <- c("lin.tendencia","lin.hiato")

# Tendência Quadrática
qua.mod <- lm(formula = pibd~time(pibd)+time(pibd)^2)
qua.tendencia <- qua.mod$fitted.values  # Estimando os valores da tendência
quadratico <- ts(qua.tendencia, start = c(1996, 1), frequency = 4)  # Cria uma séria temporal para a tendência
qua.hiato <- pibd - quadratico  # O hiato é a diferençla entre o dado real e a tendência
qua.resultado <- data.frame(quadratico, qua.hiato)
names(qua.resultado) <- c("qua.tendencia","qua.hiato")

# Filtro HP
hp.decom <- hpfilter(pibd, freq = 1600, type = "lambda")
hp.tendencia <- hp.decom["trend"]
hp.hiato <- hp.decom["cycle"]
hp.tendencia <- as.data.frame(do.call(cbind, hp.tendencia))
hp.hiato <- as.data.frame(do.call(cbind, hp.hiato))
hp.resultado <- data.frame(hp.tendencia, hp.hiato)
names(hp.resultado) <- c("hp.tendencia", "hp.hiato")

# Filtro Baxter-King
bp.decom <- bkfilter(pibd, pl = 6, pu = 32)
bp.tendencia <- bp.decom["trend"]
bp.hiato <- bp.decom["cycle"]
bp.tendencia <- as.data.frame(do.call(cbind, bp.tendencia))
bp.hiato <- as.data.frame(do.call(cbind, bp.hiato))
bp.resultado <- data.frame(bp.tendencia, bp.hiato)
names(bp.resultado) <- c("bp.tendencia", "bp.hiato")

# Filtro Christiano-Fitzgerald
cf.decom <- cffilter(pibd, pl = 6, pu = 32, root = TRUE)
cf.tendencia <- as.data.frame(do.call(cbind, cf.decom["trend"]))
cf.hiato <- as.data.frame(do.call(cbind, cf.decom["cycle"]))
cf.resultado <- data.frame(cf.tendencia, cf.hiato)
names(cf.resultado) <- c("cf.tendencia", "cf.hiato")

# Beveridge-Nelson
bn.decomp <- bnd(pibd, nlag = 8) 
bn.resultado <- data.frame(bn.decomp)
names(bn.resultado) <- c("bn.tendencia", "bn.hiato")

# Componentes Não Observados Univariado
nou.decom <- ucm(formula = pibd~0, data = pibd, cycle = TRUE, cycle.period = 111, slope = TRUE)
nou.tendencia <- as.data.frame(do.call(cbind, nou.decom["s.level"]))
nou.hiato <- as.data.frame(do.call(cbind, nou.decom["s.cycle"]))
nou.resultado <- data.frame(nou.tendencia, nou.hiato)
names(nou.resultado) <- c("nou.tendencia", "nou.hiato")

# Criando dataframe com todos os dados e exportando para excel

modelos_univariados = data.frame(dados["Data"], dados["ln(pibd)"], lin.resultado, qua.resultado,
                                 hp.resultado, bp.resultado, cf.resultado,
                                 bn.resultado, nou.resultado)
write.csv(modelos_univariados, file = "modelos_univariado.csv")





