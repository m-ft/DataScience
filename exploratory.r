# R packages
packages <- c("tidyverse", "lattice")

# Install the previously uninstalled packages and load all packages
load_pac <- function(packages) {
 `%notin%` <- Negate(`%in%`)
  for (p in packages) {
    if (p %notin% rownames(installed.packages())) install.packages(p)
    require(p , character.only = T)
  }
}

# load packages in the vector `packages`
load_pac(packages)

# load assignment data
data <- read.csv("./Assignment.csv")

# number of claims during period of exposure
nclaims <- data$nbrtotc

# Percentage of total claims
histogram(nclaims, type="percent", xlab="Number of Claims")
per_tot <- table(nclaims)/length(nclaims) * 100
print(per_tot)


