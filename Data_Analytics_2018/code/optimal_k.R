#Optimal k

time2char <- function(x){
  x <- as.character(round(x, 1))
  if (identical("0", x)) return(x)
  gsub("^0+", "", x)
}

harmonicMean <- function(logLikelihoods, precision=2000L) {
  llMed <- Rmpfr::median(logLikelihoods)
  as.double(llMed - log(Rmpfr::mean(exp(-Rmpfr::mpfr(logLikelihoods, prec = precision) + llMed))))
}


optimal_k <- function(x, max.k = 30, harmonic.mean = TRUE, 
                      control = if (harmonic.mean) list(burnin = 500, iter = 1000, keep = 100) else  NULL,
                      method = if (harmonic.mean) "Gibbs" else "VEM", verbose = TRUE, drop.seed = TRUE, ...){
  if (isTRUE(drop.seed)){
    control[["seed"]] <- NULL
  }
  if (isTRUE(harmonic.mean)) {
    optimal_k1(x, max.k = max.k, control = control, method = method, verbose = verbose, ...)
  } else {
    optimal_k2(x, max.k = max.k, control = control, method = method, ...)
  }
}

optimal_k1 <- function(x, max.k = 30, 
                       control = list(burnin = 500, iter = 1000, keep = 100), method = "Gibbs", 
                       verbose = TRUE, ...){
  if (max.k > 20) {
    message("\nGrab a cup of coffee this could take a while...\n")
    flush.console()
  }
  tic <- Sys.time()
  v <- rep(NA, floor(max.k/10))
  dat <- data.frame(k = v, time = v)
  end <- data.frame(k = max.k^2)
  hm_many <- sapply(2:max.k, function(k){
    if (k %% 10 == 0){
      time <- as.numeric(difftime(Sys.time(), tic, units = "mins"))        
      dat[k/10, 1:2] <<- c(k^2, time)          
      if (k/10 > 1) {
        fit <- with(dat, lm(time~k))
        pred <- predict(fit, end) - time
        if (pred < 0) pred <- 0
        est <- paste0("; Remaining: ~", time2char(pred), " mins")
      } else {
        est <- ""
      }
      cur <- format(Sys.time(), format="%I:%M:%S")
      elapsed <- time2char(time)
      #gsub("^0+", "", as.character(round(as.numeric(difftime(Sys.time(), tic, units = "mins")), 1)))
      cat(sprintf("%s of %s iterations (Current: %s; Elapsed: %s mins%s)\n", k, max.k, cur, elapsed, est)); flush.console()
    }
    burnin <- control[["burnin"]]
    keep <- control[["keep"]]
    if (is.null(burnin) | is.null(keep)) stop("Supply burnin & keep to control")
    fitted <- topicmodels::LDA(x, k = k, method = method, control = control)
    logLiks <- fitted@logLiks[-c(1:(burnin/keep))]
    harmonicMean(logLiks)
  })
  out <- c(2:max.k)[which.max(hm_many)]
  if (which.max(hm_many) == max.k) warning("Optimal K is last value; suggest increasing `max.k`")
  class(out) <- c("optimal_k", "optimal_k1", class(out))
  attributes(out)[["k_dataframe"]] <- data.frame(
    k = 2:max.k, 
    harmonic_mean = hm_many
  )
  if (isTRUE(verbose)) cat(sprintf("Optimal number of topics = %s\n",as.numeric(out)))
  out
}

optimal_k2 <- function(x, max.k = 30, control = NULL, method = "VEM", ...){
  if (max.k > 20) {
    message("\nGrab a cup of coffee this could take a while...\n")
    flush.console()
  }
  tic <- Sys.time()
  v <- rep(NA, floor(max.k/10))
  dat <- data.frame(k = v, time = v)
  end <- data.frame(k = max.k^2)
  best_model <- lapply(seq(2, max.k, by=1), function(k){
    if (k %% 10 == 0){
      time <- as.numeric(difftime(Sys.time(), tic, units = "mins"))        
      dat[k/10, 1:2] <<- c(k^2, time)            
      if (k/10 > 1) {
        fit <- with(dat, lm(time~k))
        est <- paste0("; Remaining: ~", time2char(predict(fit, end) - time), " mins")
      } else {
        est <- ""
      }
      cur <- format(Sys.time(), format="%I:%M:%S")
      elapsed <- time2char(time)
      #gsub("^0+", "", as.character(round(as.numeric(difftime(Sys.time(), tic, units = "mins")), 1)))
      cat(sprintf("%s of %s iterations (Current: %s; Elapsed: %s mins%s)\n", k, max.k, cur, elapsed, est)); flush.console()
    }
    topicmodels::LDA(x, k = k, method = method, control = control, ...)
  })
  out <- data.frame(
    k = c(2:max.k), 
    logLik = sapply(best_model, logLik)
  )
  class(out) <- c("optimal_k", "optimal_k2", "data.frame")
  out
}

#' Plots a plot.optimal_k1 Object
plot.optimal_k1 <- function(x, ...){
  y <- attributes(x)[["k_dataframe"]]
  y <- y[y[["k"]] == as.numeric(x), ]
  ggplot2::ggplot(attributes(x)[["k_dataframe"]], ggplot2::aes_string(x="k", y="harmonic_mean")) + 
    ggplot2::xlab(sprintf("Number of Topics (Optimal Number: %s)", as.numeric(x))) + 
    ggplot2::ylab("Harmonic Mean of Log Likelihood") + 
    ggplot2::geom_smooth(method = "loess", fill=NA) + 
    geom_point(data=y, color="red", fill=NA, size = 6, shape = 21) +
    ggplot2::geom_line(size=1) + 
    ggplot2::theme_bw()  + 
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(vjust = -0.25, size = 14),
      axis.title.y = ggplot2::element_text(size = 14, angle=90)
    ) 
}

#' Plots a plot.optimal_k2 Object   
plot.optimal_k2 <- function(x, ...){
  ggplot2::ggplot(x, ggplot2::aes_string(x="k", y="logLik")) + 
    ggplot2::xlab("Number of Topics") + 
    ggplot2::ylab("Log Likelihood") + 
    ggplot2::geom_smooth(size=.8, se=FALSE, method="loess") + 
    ggplot2::geom_line(size=1) + 
    ggplot2::theme_bw()  + 
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(vjust = -0.25, size = 14),
      axis.title.y = ggplot2::element_text(size = 14, angle=90)
    )
}

#' Prints a optimal_k Object
print.optimal_k <- function(x, ...){
  print(graphics::plot(x))
}

if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(ggplot2, topicmodels, Rmpfr)