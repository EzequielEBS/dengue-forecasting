
states <- c(
  "AC", 
  "AL", 
  "AM", 
  "AP", 
  "BA", 
  "CE", 
  "DF", 
  "ES", 
  "GO", 
  "MA", 
  "MG", 
  "MS", 
  "MT", 
  "PA", 
  "PB", 
  "PE", 
  "PI", 
  "PR", 
  "RJ", 
  "RN", 
  "RO", 
  "RR", 
  "RS", 
  "SC", 
  "SE", 
  "SP", 
  "TO"
)

for (uf in states) {
  print(paste0("Fitting SARIMAX for ", uf))
  path <- paste0("code/capital_cities/", uf, "/fit_sarimax.r")
  source(path)
}


