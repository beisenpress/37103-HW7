
# createBins.R
# -----------------------------------------------------------------------------
# Author: Günter J. Hitsch
#
# Function to create RFM indices
# Inputs:  x, a vector of numbers (or a column of a data frame)
#          N, the number of bins (groups) to create


createBins <- function(x, N) {

   # Find cut points based on the quantiles of x corresponding to the evenly-spaced values in probs
   cut_points = quantile(x, probs = seq(1/N, 1 - 1/N, by = 1/N), type = 2)

   # The different quantiles may not be unique if there are many different observations
   # in x that have the same value.  If so, choose only the unique cut points.
   cut_points = unique(cut_points)

   # Now use the cut function in R to assign each observation in x to a bin corresponding
   # to the interval defined by the cut points, and label the observations, 1, 2, ...
   bins = cut(x, c(-Inf, cut_points, +Inf), label = 1:(length(cut_points) + 1))

   # Transform the bins into numbers (by default they are factors) and return them as output
   return(as.numeric(bins))
}

