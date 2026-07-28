
# Etiquetas para causas de muerte
cause_names <- c(
  "1" = "Enfermedades circulatorias",
  "2" = "Neoplasias",
  "3" = "Diabetes",
  "4" = "Homicidio y otras causas violentas",
  "5" = "Otras causas externas",
  "6" = "Condiciones del periodo perinatal",
  "7" = "Enfermedades respiratorias",
  "8" = "Enfermedades infecciosas",
  "9" = "Enfermedades digestivas",
  "10" = "Otras causas"
)

# Etiquetas para grupos de edad
age_names <- c(
  "0" = "0", "1" = "1-4", "5" = "5-9", "10" = "10-14",
  "15" = "15-19", "20" = "20-24", "25" = "25-29",
  "30" = "30-34", "35" = "35-39", "40" = "40-44",
  "45" = "45-49", "50" = "50-54", "55" = "55-59",
  "60" = "60-64", "65" = "65-69", "70" = "70-74",
  "75" = "75-79", "80" = "80-84", "85" = "85+"
)

# Funcion estandar que devuelve una tabla de vida con sus columnas basicas.
# nmx: vector de tasas de mortalidad, usualmente aproximadas como defunciones / poblacion a mitad de ano.
# age: vector con la edad inferior de cada grupo de edad.
# nax: vector con valores de nax. Si no se provee, se asigna la mitad de la longitud del intervalo.
LifeTable_function <- function(nmx = mx, age = c(0, 1, seq(5, 85, 5)), nax = NULL) {
  n <- c(diff(age), 999)

  if (is.null(nax)) {
    nax <- 0.5 * n
  }

  nqx <- (n * nmx) / (1 + (n - nax) * nmx)
  nqx <- c(nqx[-length(nqx)], 1)
  nqx[nqx > 1] <- 1

  npx <- 1 - nqx
  lx <- cumprod(c(1, npx))
  ndx <- -diff(lx)
  lxpn <- lx[-1]
  nLxpn <- n * lxpn + ndx * nax
  nLx <- c(nLxpn[-length(nLxpn)], lxpn[length(lxpn) - 1] / nmx[length(nmx)])
  Tx <- rev(cumsum(rev(nLx)))
  lx <- lx[1:length(age)]
  ex <- Tx / lx

  data.frame(cbind(nmx, age, nax, npx, lx, nLx, Tx, ex))
}
