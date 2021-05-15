# Assignment Data Science for Non-Life Insurance
# Authors:
#  - Robin Beniamino Brambilla (r0735676)
#  - Boonya Chivapong (r0767206)
#  - Emmett Mufti (r0775432)

# shutdown graphics
# graphics.off()

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# R packages
packages <- c("tidyverse", "lattice", "mgcv", "caret", "rmarkdown", "readxl")

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
claims <- read_csv("./Assignment.csv")
postal <- read_excel("./inspost.xls")

# merge data
data <- merge(claims, postal, by="CODPOSS")

# tibble data frame
df <- as_tibble(data)

struct <- list("CODPOSS" = "postal code in Belgium",
               "AGEPH" = "age of the policy holder",
               "nbrtotc" = "number of claims during period of exposure",
               "nbrtotan" = "claim frequency rate",
               "duree" = "unit of exposure (fraction of year)",
               "chargtot" = "total claim amount",
               "lnexpo" = "log of exposure",
               "agecar" = "age of the car",
               "sexp" = "sex of the policyholder",
               "fuelc" = "type of fuel",
               "split" = "split of the premium",
               "usec" = "use of the car",
               "fleetc" = "car belonging to a fleet",
               "sportc" = "sports car",
               "coverp" = "type of coverage",
               "powerc" = "horsepower of the car")

# Categorical variables
factors <- struct[8:length(struct)]

# Continuous variables
continuous <- struct[2:5]

# Select a reference group for the factors
references <- list("2-5", "Female", "Petrol", "Once", "Private", "No", "No", "MTPL", "66-110")  
names(references) <- names(factors)

set_defaults <- function(references) {
  for(n in names(references)) {
    df[[n]] = as.factor(df[[n]])
    df[[n]] = relevel(df[[n]], ref = references[[n]])
  }
} 

set_defaults(references)

### Exploratory Data Analysis

# background color
bg_color <- "#009CFF"

# Export plot to png file
export_plot <- function(filename){
  if(!file.exists(filename)) {
    dev.print(png, filename = filename, width = 360, height= 360)
  }
}

y_label <- "Relative Frequency"

# Bar plot
plot_bar <- function(struct) {
  # convert names to symbols because aes() is a quoting function
  # use syms() to tranform inputs into symbols that can be suitably unquoted
  cols <- syms(names(struct))
  for (col in cols) {
    # use tidy eval operator !! to unquote the symbol
    bar <- ggplot(data = df, aes(as.factor(!!col))) + 
      theme_bw() + geom_bar(aes(y = (..count..)/sum(..count..)), col = bg_color, fill = bg_color, alpha = 0.5) + 
      labs(x = struct[[col]], y = y_label)
    print(bar)
    export <- paste(as_string(col), "_bar.png")
    export_plot(export)
  }
}

plot_hist <- function(struct) {
  cols <- syms(names(struct))
  for (col in cols) {
   hist <-  ggplot(data = df, aes(!!col)) + theme_bw() + 
    geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 1, col = bg_color, fill = bg_color, alpha = 0.5) + 
    labs(x = struct[[col]], y = y_label)
    print(hist)
    export <- paste(as_string(col), "_hist.png")
    export_plot(export)
  }
  
}

# frequency table
freq_table <- function(struct) {
  for (n in names(struct)) {
    covariate <- df[[n]]
    t <- as.data.frame(table(covariate)/length(covariate) * 100)
    names(t)[1] = struct[[n]]
    names(t)[2] = "Rel Freq"
    print(t)  
  }
  
}

# Density plot 
plot_dens <- function(struct) {
  for (n in names(struct)) {
    covariate <- df[[n]]
    density <- plot(density(covariate), xlab = struct[[n]], main = "")
    print(density)
    export <- paste(n,"_density.png")
    export_plot(export)
  }
  
}

plot_bar(factors)
freq_table(factors)
plot_hist(continuous)


