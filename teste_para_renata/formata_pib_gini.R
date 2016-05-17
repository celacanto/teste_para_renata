setwd("~/Dropbox/Nexo/teste_para_renata/")

pacotes
# ------------------------------------------------------------------------------------
library(plyr)



# variaveis
# ---------------------------------------------------------------------------------

# Tabelas baixas no banco mundial

# http://data.worldbank.org/indicator/SI.POV.GINI
gini_file <- "../Graficos/96. gini x pib/API_SI.POV.GINI_DS2_en_csv_v2/API_SI.POV.GINI_DS2_en_csv_v2.csv"
# http://data.worldbank.org/indicator/NY.GDP.MKTP.KN
gdpLCU_file <- "../Graficos/96. gini x pib/API_NY.GDP.MKTP.KN_DS2_en_csv_v2/API_NY.GDP.MKTP.KN_DS2_en_csv_v2.csv"
# http://data.worldbank.org/indicator/NY.GDP.MKTP.KN
gdpGrow_file <- "../Graficos/96. gini x pib/API_NY.GDP.MKTP.KN_DS2_en_csv_v2/API_NY.GDP.MKTP.KN_DS2_en_csv_v2.csv"


anoInicio <- 1993
paises <- c("Brazil", "Argentina",
            "Chile", "Uruguay")

            
# le as tabelas
# ------------------------------------------------------------------------------------
giniTab <- read.csv(gini_file, stringsAsFactors = FALSE, skip = 4, check.names = FALSE)
gdpTab <- read.csv(gdpLCU_file, stringsAsFactors = FALSE, skip = 4, check.names = FALSE)
gdpGrowTab <- read.csv(gdpGrow_file, stringsAsFactors = FALSE, skip = 4, check.names = FALSE)


get_gdpXgini <- function(country, 
                         LCU = TRUE){
  
  ## gini
  yearsGini <- colnames(giniTab)[grepl("\\d{4,4}", colnames(giniTab))]
  gini <- giniTab[giniTab$`Country Name` == country, yearsGini]
  
  ## gdp
  if(LCU){
    yearGdp <- colnames(gdpLcuTab)[grepl("\\d{4,4}", colnames(gdpLcuTab))]
    gdp <- gdpLcuTab[gdpLcuTab$`Country Name` == country, yearGdp]
  } else {
    yearGdp <- colnames(gdpTab)[grepl("\\d{4,4}", colnames(gdpTab))]
    gdp <- gdpTab[gdpTab$`Country Name` == country, yearGdp]
  }
  
  ## gdp grow
  yearsGdpGrow <- colnames(gdpGrowTab)[grepl("\\d{4,4}", colnames(gdpGrowTab))]
  gspGrow <- gdpGrowTab[gdpTab$`Country Name` == country, yearsGdpGrow]
  
  
  ## merge
  giniGdp <- rbind.fill(list(gini, gdp, gspGrow))
  
  # format
  giniGdp <- t(giniGdp)
  giniGdp <- giniGdp[complete.cases(giniGdp),]
  
  giniGdp <- as.data.frame(giniGdp)
  colnames(giniGdp) <- c("gini", "gdp", "gdpGrow")
  
  # filter
  anos <- as.numeric(rownames(giniGdp))
  giniGdp <- giniGdp[anos >= anoInicio,]
  
  # indexa em 100
  primeiroAno <- anoInicio
  ultimoAno <- as.numeric(tail(rownames(giniGdp),1))
  todosAnos <- primeiroAno:ultimoAno
  nAnos <- length(todosAnos)
  i100 <- vector("numeric", nAnos)
  i100[1] <- 100
  names(i100) <- todosAnos
  for(i in seq_len(nAnos)[-1]){
    v0 <- i100[i-1]
    ano <- as.character(todosAnos[i-1])
    crescimetoAno <-  gspGrow[ano]
    vi <- v0 + (v0 * crescimetoAno/100)
    i100[i] <- vi
  }
  i100 <- unlist(i100)
  i100 <- i100 - 100
  
  giniGdp$gdo_i100 <- i100[rownames(giniGdp)]
  giniGdp$gdp <- giniGdp$gdp/(10^12)
  return(giniGdp)
  
}



# getData
# -----------------------------------------------------------------------------------

giniGdpData <- llply(paises, get_gdpXgini)
names(giniGdpData) <- paises

colnames(giniGdpData) <- c("gini", "gdp", "gdpGrow", "gdp_growSince1993")





# salva as tabelas
# --------------------------------------------------------------------------------------------
for(pais in names(giniGdpData)){
  file <- paste0("gini_pib_", pais, ".csv")
  x <- giniGdpData[[pais]]
  write.csv(x, file)
}
