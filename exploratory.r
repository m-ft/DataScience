# R packages
packages <- c("tidyverse", "lattice")

# Install the previously uninstalled packages and load all packages
load_pac <- function(packages) {
 `%notin%` <- Negate(`%in%`)
  for (p in packages) {
    if (p %notin% rownames(installed.packages())) install.packages(p)
    if (p %notin% (.packages())) require(p , character.only = TRUE)
  }
}

# load packages in the vector `packages`
load_pac(packages)

# load assignment data
data <- read_csv("./Assignment.csv")



spec <- list("AGEPH" = "age of the policy holder",
             "duree" = "exposure (fraction of the year the insured is covered)",
             "lnexpo" = "log of exposure",
             "nbrtotc" = "total number of claims during period of exposure",
             "nbrtotan" = "total number of claims during period of exposure/period of exposure",
             "chargtot" = "total claim amount",
             "agecar" = "age of the car: 0 − 1, 2 − 5, 6 − 10, > 10",
             "sexp" = "sex of the policyholder: male or female",
             "fuelc" = "type of fuel: petrol or gasoil",
             "split" = "split of the premium: monthly, once, twice, three times per year",
             "usec" = "use of the car: private or professional",
             "fleetc" = "car belonging to a fleet: yes or no",
             "sportc" = "sport car: yes or no",
             "coverp" = "coverage: MTPL, MTPL+, MTPL+++",
             "powerc" = "power of the car",
             "CODPOSS" = "postal code in Belgium")

# Export plot to png file
export_plot <- function(filename){
    if(!file.exists(filename)) {
      dev.print(png, filename = filename, width = 360, height= 360)
    }
}

# Percentage of total claims
factor_hist <- function(factor, xlabel) {
    covariate <- data[[factor]]
    freq_table <- table(covariate)/length(covariate) * 100
    print(freq_table)
    hist <- histogram(covariate, type="percent", xlab= xlabel)
    print(hist)
    export <- str_interp("${factor}_hist.png")
    return(export_plot(export))
}

# Density plot 
factor_dens <- function(factor, xlabel) {
  covariate <- data[[factor]]
  density <- plot(density(covariate), xlab = xlabel, main = "")
  print(density)
  export <- str_interp("${factor}_density.png")
  return(export_plot(export))
}

factor_hist("nbrtotc", "Number of Claims")
factor_dens("AGEPH", "Age of the policy holder")



