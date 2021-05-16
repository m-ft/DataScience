# Assignment Data Science for Non-Life Insurance
# Authors:
#  - Robin Beniamino Brambilla (r0735676)
#  - Boonya Chivapong (r0767206)
#  - Emmett Mufti (r0775432)

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# R packages
packages <- c(
  "tidyverse",
  "lattice",
  "ranger",
  "glm",
  "gbm",
  "caret",
  "rmarkdown",
  "readxl",
  "rlang",
  "knitr",
  "evtree",
  "classInt", 
  "sf",
  "styler",
  "miniUI"
)

# Install the previously uninstalled packages and load all unloaded packages
load_pac <- function(packages) {
  `%notin%` <- Negate(`%in%`)
  for (p in packages) {
    if (p %notin% rownames(installed.packages())) install.packages(p)
    if (p %notin% (.packages())) require(p, character.only = TRUE)
  }
}

# load packages in the vector "packages"
load_pac(packages)

# load frequency table
load("freq_tables.rda")

# load assignment data
claims <- read_csv("./Assignment.csv")
postal <- read_excel("./inspost.xls")

# create new variables and merge claims and postal data
# create log of total claim amount
claims$lnamount <- log(claims$chargtot)
# merge data
data <- merge(claims, postal, by = "CODPOSS")

# rename all variables to lowercase
data <- data %>% rename_all(function(.name) {
  .name %>% tolower()
})

# replace variable names
data <- rename(data, c(
  "expo" = "duree",
  "nclaims" = "nbrtotc",
  "claim_freq" = "nbrtotan",
  "amount" = "chargtot"
))

# create tibble data frame
df <- as_tibble(data)

struct <- list(
  "coposs" = "postal code in Belgium",
  "ageph" = "age of the policy holder",
  "nclaims" = "number of claims during period of exposure",
  "claim_freq" = "claim frequency rate (number of claims/exposure)",
  "expo" = "period of exposure (fraction of year)",
  "chargtot" = "total claim amount",
  "lnamount" = "log of total claim amount",
  "lnexpo" = "log of exposure",
  "agecar" = "age of the car",
  "sexp" = "sex of the policyholder",
  "fuelc" = "type of fuel",
  "split" = "split of the premium",
  "usec" = "use of the car",
  "fleetc" = "car belonging to a fleet",
  "sportc" = "sports car",
  "coverp" = "type of coverage",
  "powerc" = "horsepower of the car"
)

# Description table for variables
description <- data.frame(matrix(unlist(struct), nrow = length(struct), byrow = TRUE), stringsAsFactors = F)
desc_table <- cbind(names(struct), description)
colnames(desc_table) <- c("Name of variable", "Description")
kable(desc_table)

# Categorical variables
factors <- struct[9:length(struct)]

# Select a reference group for the factors
references <- list(
  "2-5",
  "Female",
  "Petrol",
  "Once",
  "Private",
  "No",
  "No",
  "MTPL",
  "66-110"
)

names(references) <- names(factors)

set_defaults <- function(references) {
  for (n in names(references)) {
    df[[n]] <- as.factor(df[[n]])
    df[[n]] <- relevel(df[[n]], ref = references[[n]])
  }
}

set_defaults(references)

### Exploratory Data Analysis

# background color
bg_color <- "#009CFF"

# Export plot to png file
export_plot <- function(filename) {
  dev.print(png, filename = filename, width = 360, height = 360)
}

y_label <- "Relative Frequency"

# Bar plot
plot_bar <- function(struct, save = F) {
  # convert names to symbols because aes() is a quoting function
  # use syms() to transform inputs into symbols that can be suitably unquoted
  cols <- syms(names(struct))
  for (col in cols) {
    # use tidy eval operator !! to unquote the symbol
    bar <- ggplot(data = df, aes(as.factor(!!col))) +
      theme_bw() +
      geom_bar(aes(y = after_stat(count / sum(count))),
        col = bg_color, fill = bg_color, alpha = 0.5
      ) +
      labs(x = struct[[col]], y = y_label)
    # Print bar plot
    print(bar)
    # Export if save is True/T
    if (save == T) {
      export <- paste(as_string(col), "_bar.png", sep = "")
      export_plot(export)
    }
  }
}

plot_hist <- function(coll, bin_width, save = F) {
  # convert names to symbols because aes() is a quoting function
  # use syms() to transform inputs into symbols that can be suitably unquoted
  cols <- syms(names(coll))
  for (col in cols) {
    # use tidy eval operator !! to unquote the symbol
    hist <- ggplot(data = df, aes(!!col)) +
      theme_bw() +
      geom_histogram(aes(y = after_stat(count / sum(count))),
        binwidth = bin_width, col = bg_color, fill = bg_color, alpha = 0.5
      ) +
      labs(x = coll[[col]], y = y_label)
    # Print histogram
    print(hist)
    # Export if save is True/T
    if (save == T) {
      export <- paste(as_string(col), "_hist.png", sep = "")
      export_plot(export)
    }
  }
}

# frequency table
freq_table <- function(coll) {
  freq_tables <- c()
  for (n in names(coll)) {
    covariate <- df[[n]]
    t <- as.data.frame(table(covariate) / length(covariate) * 100)
    names(t)[1] <- coll[[n]]
    names(t)[2] <- "Rel Freq"
    k <- kable(t)
    print(k)
    freq_tables[[n]] <- k
  }
  save(freq_tables, file = "freq_tables.rda")
}

# Density plot
plot_dens <- function(coll, save = F) {
  for (n in names(coll)) {
    covariate <- df[[n]]
    density <- plot(density(covariate), xlab = coll[[n]], main = "")
    print(density)
    if (save == T) {
      export <- paste(n, "_density.png", sep = "")
      export_plot(export)
    }
  }
}

## Plots and frequency tables
## use "T" or "TRUE" as the last argument to export plots

# Bar plot for all categorical variables
# plot_bar(factors)

# Histogram number of claims during period of exposure
# plot_hist(struct["ageph"], 0.5)

# Histogram log of total claim amount and number of claims
# plot_hist(struct[c("lnamount", "nclaims")], 1)

# Frequency table for all categorical variables
# freq_table(factors)

### Spatial data
belgium_shape_sf <- st_read("./shapefileBelgium/npc96_region_Project1.shp", quiet = TRUE)
belgium_shape_sf <- st_transform(belgium_shape_sf, "+proj=longlat +datum=WGS84")


## Total exposure observed over postal code
post_expo <- df %>%
  group_by(codposs) %>%
  summarize(num = n(), total_expo = sum(expo))
tot_post_expo <- post_expo %>%
  slice(1:5) %>%
  kable()
print(tot_post_expo)

## Merge Belgian shape file with constructed summary at postal code level
belgium_shape_sf <- left_join(belgium_shape_sf, post_expo, by = c("POSTCODE" = "codposs"))


# Compute relative exposure over unit area
belgium_shape_sf$freq <- belgium_shape_sf$total_expo / belgium_shape_sf$Shape_Area

# Transform exposure over unit area  to binned version
belgium_shape_sf$freq_class <- cut(belgium_shape_sf$freq,
  breaks = quantile(belgium_shape_sf$freq, c(0, 0.2, 0.8, 1), na.rm = TRUE),
  right = FALSE, include.lowest = TRUE,
  labels = c("low", "average", "high")
)


plot_shape <- function(shape_obj, title, save = F) {
  shape <- ggplot(shape_obj) +
    geom_sf(aes(fill = freq_class),
      colour = "black", size = 0.1
    ) +
    ggtitle(title) +
    labs(fill = "Relative\nExposure") +
    scale_fill_brewer(
      palette = "Blues",
      na.value = "white"
    ) +
    theme_bw()
  print(shape)
  if (save == T) {
    export <- strsplit(title, " ")[[1]]%>% tolower %>% paste(collapse = "_") %>% paste(".png", sep="")
    export_plot(export)
  }
}

# Create a plot with Belgium shape file
# belgium_shape_plot <- plot_shape(belgium_shape_sf, "Spatialized Claim Frequency")



### Predicting Claim frequency with Gradient Boosting and Random Forest

features <- c(
  "agecar",
  "ageph",
  "coverp",
  "fleetc",
  "fuelc",
  "long",
  "lat",
  "powerc",
  "sexp",
  "sportc",
  "usec"
)

model_data <- as.data.frame(df[features])

### Preprocessing 

## Near Zero Variance
# We use the `nearZeroVar` function from R caret package to
# identify the near zero variance feature variables
# near zero variance variables are detected using two metrics:
# the frequency ratio of the most common and the second most common value
# the ratio of unique values relative to the total number of samples
##
nzv <- nearZeroVar(model_data, saveMetrics = T)
# We observe that `fleetc`, `sportc` and `usec` are Near Zero Variance Variables
kable(nzv)

nzv_true <- nzv[nzv$nzv,][1:3,] %>% rownames

kable(nzv_true)

## Train the model using caret train function
# use `pca` as a preprocessing option to deal with nzv variables so that
# instead of throwing out data the Near Zero Variance predictors 
# end up combined into one high variance PCA variable
# Set na.action to na.pass so that training halts if missing values are 
# present
##

freq_model_data <- cbind(nclaims=df$nclaims, model_data)

# create custom folds to use as index in the train control object
freq_folds <- createFolds(freq_model_data$nclaims, k = 10)

# Create a reusable train control object to define train/test split 
train_control_obj <- trainControl(
  verboseIter = TRUE,
  savePredictions = TRUE,
  index = freq_folds
)

pre_process <- c("scale", "center", "pca")

set.seed(698)
freq_model <- function(method) {
  train(
    nclaims ~ .,
    data = freq_model_data,
    method = method,
    trControl = train_control_obj,
    na.action = na.pass,
    preProcess = pre_process
)
}

# GLM 
freq_model_glm <- freq_model("glm")
print(freq_model_glm)
 

# GLMNET
freq_model_glmnet <- freq_model("glmnet")
print(freq_model_glmnet)


# Gradient Boosting Machine 
freq_model_gbm <- freq_model("gbm")
print(freq_model_gbm)


# Random Forest
tune_grid <- data.frame(mtry = c(2, 11, 6, 7, 10, 8))
freq_model_rf <- train(
    nclaims ~ .,
    data = freq_model_data,
    method = "ranger",
    na.action = na.pass,
    preProcess = pre_process,
    tuneLength = 10
)

print(freq_model_rf)


