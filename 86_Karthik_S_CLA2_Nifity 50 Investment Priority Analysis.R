# =====================================================
# NIFTY 50 INVESTMENT PRIORITY ANALYSIS
# TECHNIQUE: MULTIPLE LINEAR REGRESSION
# =====================================================


# =====================================================
# 1. IMPORT DATA
# =====================================================

data <- read.csv(
  "C:/Users/Karthik/Downloads/Nifty50_Investment_Priority_R_Analysis_Complete_50.csv")

# View and inspect the dataset
View(data)

str(data)
dim(data)
colSums(is.na(data))


# =====================================================
# 2. DESCRIPTIVE STATISTICS
# =====================================================

summary(data[, c(
  "ROE_pct",
  "Sales_Growth_pct",
  "Net_Profit_Margin_pct",
  "Debt_Equity",
  "Return_1Y_Pct"
)])


# =====================================================
# 3. MULTIPLE LINEAR REGRESSION
# =====================================================

# Dependent variable:
# Return_1Y_Pct
#
# Independent variables:
# ROE_pct
# Sales_Growth_pct
# Net_Profit_Margin_pct
# Debt_Equity

model <- lm(
  Return_1Y_Pct ~
    ROE_pct +
    Sales_Growth_pct +
    Net_Profit_Margin_pct +
    Debt_Equity,
  data = data
)

# Display regression results
summary(model)


# =====================================================
# 4. MODEL INTERPRETATION
# =====================================================

model_summary <- summary(model)

cat("\n========================================\n")
cat("MODEL INTERPRETATION\n")
cat("========================================\n")

cat(
  "R-squared:",
  round(model_summary$r.squared, 4),
  "\n"
)

cat(
  "Adjusted R-squared:",
  round(model_summary$adj.r.squared, 4),
  "\n"
)

cat(
  "Overall Model p-value:",
  format.pval(
    pf(
      model_summary$fstatistic[1],
      model_summary$fstatistic[2],
      model_summary$fstatistic[3],
      lower.tail = FALSE
    ),
    digits = 4
  ),
  "\n"
)

cat("\nInterpretation:\n")
cat(
  "The regression model is used as an investment screening tool.\n
  The model-predicted returns are used to rank the Nifty 50 companies.\n
  A low R-squared indicates that the selected financial variables\n
  explain only a limited portion of the variation in 1-year returns.\n
  Therefore, the model should not be treated as a standalone\n
  forecasting or investment decision model.\n"
)


# =====================================================
# 5. PREDICTED RETURN & INVESTMENT PRIORITY
# =====================================================

# Generate model-predicted 1-year returns

data$Predicted_Return <- predict(model)


# Rank companies from highest to lowest
# predicted return

data$Rank <- rank(
  -data$Predicted_Return,
  ties.method = "first"
)


# Select variables required for
# investment-priority analysis

top10 <- data[
  order(data$Rank),
  c(
    "Rank",
    "Company",
    "Sector",
    "Return_1Y_Pct",
    "Predicted_Return",
    "ROE_pct",
    "Sales_Growth_pct",
    "Debt_Equity"
  )
]


# Select Top 10 companies

top10 <- top10[1:10, ]


# Display Top 10

cat("\n========================================\n
      TOP 10 INVESTMENT-PRIORITY COMPANIES\n
      ========================================\n")

print(top10)


# =====================================================
# 6. MODEL-BASED INVESTMENT DECISION
# =====================================================

# Decision Rule:
#
# Companies are ranked according to their
# model-predicted 1-year returns.
#
# The 10 highest-ranked companies are selected
# as the investment-priority group.
#
# This converts the statistical output into
# a clear managerial investment-screening decision.


cat("\n========================================\n
MODEL-BASED INVESTMENT DECISION\n
========================================\n")

cat(
  "The Top 10 companies are selected based on\n

  their model-predicted 1-year returns.\n
They represent the investment-priority group\n
  for further managerial evaluation.\n"
)


# =====================================================
# 7. ILLUSTRATIVE ₹10 CRORE ALLOCATION
# =====================================================

# The allocation is proportional to the
# model-predicted returns of the Top 10 companies.

top10$Allocation_Percent <-
  top10$Predicted_Return /
  sum(top10$Predicted_Return) * 100

top10$Allocation_Crore <-
  top10$Allocation_Percent / 100 * 10


# Round the allocation values

top10$Allocation_Percent <-
  round(top10$Allocation_Percent, 2)

top10$Allocation_Crore <-
  round(top10$Allocation_Crore, 2)


# Display final investment table

cat("\n========================================\n")
cat("ILLUSTRATIVE ₹10 CRORE ALLOCATION\n")
cat("========================================\n")

print(top10)


# =====================================================
# 8. ALLOCATION VALIDATION
# =====================================================

cat("\nTotal Allocation Percentage:",
    sum(top10$Allocation_Percent),
    "%\n")

cat(
  "Total Allocation:",
  sum(top10$Allocation_Crore),
  "Crore\n"
)

cat(
  "\nNote: The allocation is illustrative and\n



    proportional to model-predicted returns.\n


    It is not an optimized portfolio or a guarantee\n

    of future investment returns.\n"
)


# =====================================================
# 9. MODEL-BASED ALLOCATION BAR CHART
# =====================================================

par(mar = c(10, 4, 4, 2))

barplot(
  top10$Allocation_Crore,
  names.arg = top10$Company,
  las = 2,
  cex.names = 0.7,
  main = "Model-Based Illustrative Allocation of ₹10 Crore",
  ylab = "Allocation (₹ Crore)",
  xlab = "Top 10 Investment-Priority Companies"
)

par(mar = c(5, 4, 4, 2))


# =====================================================
# 10. SAVE FINAL INVESTMENT-PRIORITY TABLE
# =====================================================

write.csv(
  top10,
  "Nifty50_Top10_Investment_Allocation.csv",
  row.names = FALSE
)


# =====================================================
# 11. FINAL BUSINESS DECISION
# =====================================================

cat("\n========================================\n")
cat("FINAL BUSINESS DECISION\n")
cat("========================================\n")

cat(
  "The regression model provides an initial screening\n"
)

cat(
  "mechanism for identifying investment-priority companies.\n

    The Top 10 companies should be subjected to further\n

    due diligence before actual investment.\n

    Additional factors such as valuation, liquidity,\n
    
    sector concentration, market conditions and risk\n
    
    should be considered before deploying the ₹10 crore.\n"
    
)



