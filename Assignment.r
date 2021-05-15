# Assignment Data Science for Non-Life Insurance
# Authors:
#  - Robin Beniamino Brambilla (r0735676)
#  - Boonya Chivapong (r0767206)
#  - Emmett Mufti (r0775432)

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# R packages
packages <- c("tidyverse", 
              "lattice",
              "mgcv",
              "caret",
              "rmarkdown",
              "readxl",
              "rlang",
              "knitr", 
              "evtree",
              "classInt",
              "sf")

# Install the previously uninstalled packages and load all unloaded packages
load_pac <- function(packages) {
  `%notin%` <- Negate(`%in%`)
  for (p in packages) {
    if (p %notin% rownames(installed.packages())) install.packages(p)
    if (p %notin% (.packages())) require(p , character.only = TRUE)
  }
}

# load packages in the vector "packages"
load_pac(packages)

# load frequency table
load("freq_tables.rda")

# load assignment data
claims <- read_csv("./Assignment.csv")
postal <- read_excel("./inspost.xls")

### Preprocessing
# create new variables and merge claims and postal data
# create log of total claim amount
claims$lnchargtot <- log(claims$chargtot)

data <- merge(claims, postal, by = "CODPOSS")

# rename all variables to lowercase
data <- data %>%  rename_all(function(.name) {
  .name %>% tolower 
})

# replace variable names
data <- rename(data, c("expo" = "duree", "nclaims" = "nbrtotc", "claim_freq" = "nbrtotan"))

# create tibble data frame
df <- as_tibble(data)

struct <- list("coposs" = "postal code in Belgium",
               "ageph" = "age of the policy holder",
               "nclaims" = "number of claims during period of exposure",
               "claim_freq" = "claim frequency rate",
               "expo" = "period of exposure (fraction of year)",
               "chargtot" = "total claim amount",
               "lnchargtot" = "log of total claim amount",
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
factors <- struct[9:length(struct)]

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
  dev.print(png, filename = filename, width = 360, height= 360)
}

y_label <- "Relative Frequency"

# Bar plot
plot_bar <- function(struct, save=F) {
  # convert names to symbols because aes() is a quoting function
  # use syms() to tranform inputs into symbols that can be suitably unquoted
  cols <- syms(names(struct))
  for (col in cols) {
    # use tidy eval operator !! to unquote the symbol
    bar <- ggplot(data = df, aes(as.factor(!!col))) + 
      theme_bw() + geom_bar(aes(y = after_stat(count/sum(count))), col = bg_color, fill = bg_color, alpha = 0.5) + 
      labs(x = struct[[col]], y = y_label)
    print(bar)
    if (save == T) {
      export <- paste(as_string(col), "_bar.png")
      export_plot(export)
    }
  }
}

plot_hist <- function(coll, bin_width, save=F) {
  cols <- syms(names(coll))
  for (col in cols) {
    hist <-  ggplot(data = df, aes(!!col)) + theme_bw() + 
      geom_histogram(aes(y = after_stat(count/sum(count))), binwidth = bin_width, col = bg_color, fill = bg_color, alpha = 0.5) + 
      labs(x = coll[[col]], y = y_label)
    print(hist)
    if (save == T) {
      export <- paste(as_string(col), "_hist.png")
      export_plot(export)
    }
  }
  
}

# frequency table
freq_table <- function(coll) {
  freq_tables <- c()
  for (n in names(coll)) {
    covariate <- df[[n]]
    t <- as.data.frame(table(covariate)/length(covariate) * 100)
    names(t)[1] =  coll[[n]]
    names(t)[2] = "Rel Freq"
    k <- kable(t)
    print(k)
    freq_tables[[n]] <- k
  }
  save(freq_tables, file="freq_tables.rda")
}

# Density plot 
plot_dens <- function(coll, save=F) {
  for (n in names(coll)) {
    covariate <- df[[n]]
    density <- plot(density(covariate), xlab = coll[[n]], main = "")
    print(density)
    if (save == T) {
      export <- paste(n,"_density.png")
      export_plot(export)
    }
  }
  
}

## Plots and frequency tables 
## use "T" or "TRUE" as the last argument to export plots

# Bar plot for all categorical variables
plot_bar(factors, T)

# Histogram number of claims during period of exposure 
plot_hist(struct["ageph"], 0.5, T)

# Histogram log of total claim amount and number of claims 
plot_hist(struct[c("lnchargtot", "nclaims")], 1, T)

# Frequency table for all categorical variables
freq_table(factors)




