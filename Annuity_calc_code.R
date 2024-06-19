require(xtable)

I <- 0.025  # Annual risk-free interest rate (as a decimal)
L <- 0.00  # Annual overhead costs loading (as a % of premium)

# Function to calculate the annuity price for a given age (N) and maturity (T)

# P: Positions for annuity price calculation
#   "c" for point forecast, "l" for lower bound and "h" for upper bound
# N: Age annuity payment starts
# T: Annuity payment maturities

calculate_annuity_price_female <- function(P, N, T)
{
    # Calculate the expected present value of future payments
    PV <- 0
    SP = vector("numeric", T)
    for(t in 1:T)
    {
        # Obtain the survival rates
        if(P == "c")
        {
            SP[t] <- SF_female_forecast[N - 64 + t, t+1]/SF_female_forecast[N - 64, 1]
        }
        else if (P == "l")
        {
            SP[t] <- SF_female_forecast_lower[N - 64 +t, t+1]/SF_female_forecast_lower[N - 64, 1]
        }
        else if (P == "h")
        {
            SP[t] <- SF_female_forecast_upper[N - 64 +t, t+1]/SF_female_forecast_upper[N - 64, 1]
        }
    }
    # Calculate the annuity payment for year t
    AP <- 1  # Annuity payment Adjust if needed
    for(t in 1:T)
    {
        PV <- PV + SP[t] * AP * exp(-I*t)
    }

    # Calculate the annual annuity price
    annuity_price <- round((1 + L) * PV, digits = 2)
    return(annuity_price)
}

# Specify ages and maturities

ages <- seq(65, 105, by = 5)
maturities <- seq(5, 45, by = 5)
Positions <- c("c", "l", "h")

# Calculate annuity prices for different age-maturity combinations
results_list <- list()

for(P in Positions)
{
    results <- matrix(NA, nrow = length(ages), ncol = length(maturities))
    colnames(results) <- maturities
    rownames(results) <- ages

    for(N in ages)
    {
        for(T in maturities)
        {
            if((N + T) > 110)
            {
                # Handle cases where indices are out of bounds
                results[as.character(N), as.character(T)] <- "NA"
            }
            else
            {
                results[as.character(N), as.character(T)] <- calculate_annuity_price_female(P, N, T)
            }
        }
    }
    # Store results in a list based on position
    results_list[[P]] <- results
}

# Print the results for each position
for(P in Positions)
{
    cat("Position: ", P, "/n")
    print(results_list[[P]])
    cat("/n")
}

# Convert matrices to data frames and then LaTeX format
results_list_df <- as.data.frame(results_list)
results_c<-results_list_df[1:9]
colnames(results_c) <- paste("T = ", seq(5, 45, by = 5), sep = "")
xtable(results_c, caption = 'AUS female annuity prices with diff ages and maturities (T)',
       align = "|c|c|c|c|c|c|c|c|c|c|",digits = 2)

# Covert pointwise prediction intervals into LaTex format

results_PI <- results_list_df[10:27]
for(i in 1:9)
{
    for(j in 1:9)
    {
        results_PI[j,i] <- paste("(",results_PI[j,i],",",results_PI[j,i+9],")", sep = "")
    }
}
results_PI <- results_PI[1:6]
colnames(results_PI) <- paste("T = ", seq(5, 30, by = 5), sep = "")
xtable(results_PI, caption = '95% pointwise prediction intervals of annuity prices with diff ages and maturities (T)',
       align = "|c|c|c|c|c|c|c|", digits = 2)


#####################################
# Repeat Annuity Calculation for Male
#####################################

calculate_annuity_price_male <- function(P, N, T)
{
  # Calculate the expected present value of future payments
  PV <- 0
  SP = vector("numeric", T)
  for(t in 1:T)
  {
    # Obtain the survival rates
    if(P == "c")
    {
      SP[t] <- SF_male_forecast[N - 64 + t, t+1]/SF_male_forecast[N - 64, 1]
    }
    else if (P == "l")
    {
      SP[t] <- SF_male_forecast_lower[N - 64 +t, t+1]/SF_male_forecast_lower[N - 64, 1]
    }
    else if (P == "h")
    {
      SP[t] <- SF_male_forecast_upper[N - 64 +t, t+1]/SF_male_forecast_upper[N - 64, 1]
    }
  }
  # Calculate the annuity payment for year t
  AP <- 1  # Annuity payment Adjust if needed
  for(t in 1:T)
  {
    PV <- PV + SP[t] * AP * exp(-I*t)
  }
  
  # Calculate the annual annuity price
  annuity_price <- round((1 + L) * PV, digits = 2)
  return(annuity_price)
}

# Specify ages and maturities

ages <- seq(65, 105, by = 5)
maturities <- seq(5, 45, by = 5)
Positions <- c("c", "l", "h")

# Calculate annuity prices for different age-maturity combinations
results_list <- list()

for(P in Positions)
{
  results <- matrix(NA, nrow = length(ages), ncol = length(maturities))
  colnames(results) <- maturities
  rownames(results) <- ages
  
  for(N in ages)
  {
    for(T in maturities)
    {
      if((N + T) > 110)
      {
        # Handle cases where indices are out of bounds
        results[as.character(N), as.character(T)] <- "NA"
      }
      else
      {
        results[as.character(N), as.character(T)] <- calculate_annuity_price_male(P, N, T)
      }
    }
  }
  # Store results in a list based on position
  results_list[[P]] <- results
}

# Print the results for each position
for(P in Positions)
{
  cat("Position: ", P, "/n")
  print(results_list[[P]])
  cat("/n")
}

# Convert matrices to data frames and then LaTeX format
results_list_df <- as.data.frame(results_list)
results_c<-results_list_df[1:9]
colnames(results_c) <- paste("T = ", seq(5, 45, by = 5), sep = "")
xtable(results_c, caption = 'AUS male annuity prices with diff ages and maturities (T)',
       align = "|c|c|c|c|c|c|c|c|c|c|",digits = 2)

# Covert pointwise prediction intervals into LaTex format

results_PI <- results_list_df[10:27]
for(i in 1:9)
{
  for(j in 1:9)
  {
    results_PI[j,i] <- paste("(",results_PI[j,i],",",results_PI[j,i+9],")", sep = "")
  }
}
results_PI <- results_PI[1:6]
colnames(results_PI) <- paste("T = ", seq(5, 30, by = 5), sep = "")
xtable(results_PI, caption = '95% pointwise prediction intervals of annuity prices with diff ages and maturities (T)',
       align = "|c|c|c|c|c|c|c|", digits = 2)
