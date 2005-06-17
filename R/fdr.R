"fdr" <-
function(p, Q = 0.05, method = c("stepup", "adaptive", "stepdown", "dependent"),
    plt = F, subject = 
    "False Discovery Rate (FDR) Controlling Screening of Multiple Tests")
{
# FDR controlling procedures.
#
# The procedures operate on a vector of p-values, 
# which is the results of testing a family of related hypotheses individually. 
# They screen those which remain statistically significant
# in spite of the increased type-I error because of the multiplicity effect.
# The False Discovery Rate (FDR) is the expected proportion of 
# errouneous rejections among the rejections.
# The output is an alpha level so that if hypotheses whose corresponding
# p-values are below this alpha are rejected, then the FDR
# is controlled at the level Q
#
# Stepup procedure: Benjamini and Hochberg (1995) J. Roy. Statist. Soc. B.
# Adaptive procedure: Benjamini and Hochberg (1998) J. Behav. Educ. Statist.
# Stepdown procedure: Benjamini and Liu (1998) J. of Statist. Plan. Inference
# 
# The above three procedures are for independent test statistics.
# Step-up procedure can be used for positive dependent test statistics
# (e.g. pooled estimate of variability, latent variable, many comparisons with
# the same control). See Benjamini and Yekutieli (1997) Working Paper 97-3.
#
# Dependent procedure: Benjamini and Yekutieli (1997) Working Paper 97-3.
# The Dependent procedure is less powerful than the other ones but is valid 
# under ANY dependency structure.
# 
    m <- length(p)
    sortp <- sort(p)
    if(sortp[m] > 1)
        return(NA)
    s <- sort(1 - sortp)/(1:m)
    m0 <- m
    method <- match.arg(method)
    if(method == "dependent")
        Q <- Q/sum(1/(1:m))
    if(method == "stepdown") {
        d <- 1:m
        del <- 1 - (1 - pmin(1, (m/(m + 1 - d)) * Q))^(1/(m + 1 - d))
        i <- 0
        while(i < m && sortp[i + 1] <= del[i + 1]) i <- i + 1
        if(i == 0) {
            alfa <- 0
            reject <- FALSE
        }
        else {
            alfa <- sortp[i + 1]
            reject <- TRUE
        }
    }
    else {
        i <- m
        while(i >= 1 && sortp[i] > (Q * i)/m0) i <- i - 1
        if(i == 0) {
            alfa <- 0
            reject <- FALSE
        }
        else {
            alfa <- sortp[i]
            reject <- TRUE
        }
    }
    if(method == "adaptive" & reject == T) {
        m0raw <- m
        i <- m
        while(i > 1 && s[i] <= s[i - 1]) i <- i - 1
        if(i > 1)
            m0raw <- 1/s[i - 1]
        else m0raw <- 1/s[1]
        m0 <- min(floor(1 + m0raw), m)
        i <- m
        while(i >= 1 && sortp[i] > (Q * i)/m0) i <- i - 1
        if(i == 0)
            alfa <- 0
        else alfa <- sortp[i]
        if(plt == TRUE) {
            plot(0:(m + 1), c(0, sortp, 1),  ylab = 
                " p-value", xlab = "Rank of p-value", main = 
                paste(subject, "", sep = " "), type = "n")
            # xaxs = "s",
            points(1:m, sortp)
            abline(1 - (m + 1)/m0raw, 1/m0raw, lty = 2)
            abline(0, Q/m0)
            abline(0, Q/m, lty = 3)
        }
    }
    alfa
}
