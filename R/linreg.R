#' This is some description of this function.
#'
#' @field formula .
#' @field data .
#' @field X independent variables.
#' @field y dependent variable.
#' @field beta_hat Regressions coefficients.
#' @field y_hat The fitted values.
#' @field e_hat The residuals.
#' @field df The degrees of freedom.
#' @field res_var The residual variance.
#' @field var_reg_coe The variance of the regression coefficients.
#' @field t_value .
#' @field p_value .
#'
#' @title linear regression function
#'
#' @description use RC class to compute the statistic
#'
#' @details you can use this function to calculate linear regression statistics,then predict the response.
#'
#' @param formula formula is formula expression, like y ~ x1 + x2
#'
#' @return a linreg class
#' @export linreg
#'
#' @import methods
#' @importFrom ggplot2 ggplot
#' @importFrom plyr is.formula
#' @exportClass linreg
#'
linreg <- setRefClass(
  #class name
  "linreg",
  #attributes
  fields = list(
    formula = "formula",
    data = "data.frame",
    X = "matrix",
    y = "numeric",
    beta_hat = "vector",
    y_hat = "matrix",
    e_hat = "matrix",
    d_f = "numeric",
    res_var = "numeric",
    var_reg_coe = "matrix",
    t_value = "vector",
    p_value = "vector",
    print_out = "character",
    dataset_name = "character"
  ),
  methods = list(
    #compute method 1
    initialize = function(formula, data) {
      stopifnot(plyr::is.formula(formula), is.data.frame(data))
      # if (rlang::is_formula(formula) == FALSE |
      #     is.data.frame(data) == FALSE) {
      #   stop()
      # }
      cat("User::initialize")
      formula <<- formula
      data <<- data
      X <<- model.matrix(formula, data)
      y <<- data[, all.vars(formula)[1]]
      beta_hat <<-
        round(as.vector(solve(t(X) %*% X) %*% t(X) %*% y), 3)
      # solve(t(X) %*% X) %*% t(X) %*% y #参数矩阵
      names(beta_hat) <<-  colnames(X)
      y_hat <<- X %*% beta_hat #预测值
      e_hat <<- y - y_hat #残差
      d_f <<- length(y) - ncol(X) #自由度df = n - k, n是样本的数量，k是变量个数
      res_var <<- as.numeric((t(e_hat) %*% e_hat) / d_f) #残差方差
      var_reg_coe <<- res_var * solve(t(X) %*% X) #回归参数的方差
      t_value <<- beta_hat / (sqrt(diag(var_reg_coe))) #t值
      p_value <<- 2 * pt(abs(t_value), d_f, lower.tail = FALSE) #P值
      print_out <<- Reduce(paste, deparse(formula))
      dataset_name <<- deparse(substitute(data))

    },
    # QR decomposition
    #～～～～～～～～～～～～～
    # print()
    print = function() {
      cat(paste(
        "linreg(formula = ",
        print_out,
        ", data = ",
        dataset_name,
        ")\n",
        sep = ""
      ))
      base::print(beta_hat)
      # a = as.character(formula)
      # f = paste(a[2], a[1], a[3])
      # cat(names(beta_hat), "\n", beta_hat[, 1], cat(
      #   paste(
      #     "Call:\nlinreg(formula = ",
      #     f,
      #     ", data = ",
      #     deparse(substitute(data)),
      #     ")\nCoefficient:\n",
      #     sep = " "
      #   )
      # ))
    },
    # plot()
    plot = function() {
      x1 <- as.data.frame(cbind(y_hat, e_hat))
      resm <- mean(e_hat)
      resv1 <- as.numeric(res_var)
      x2 <-
        as.data.frame(cbind(y_hat, sqrt(abs((e_hat - resm) / resv1
        ))))
      graph1 <-
        ggplot(data = x1, aes(x1[, 1], x1[, 2])) + geom_point() +
        labs(x = "Fitted values", y = "Residuals") +
        ggtitle("Residuals vs Fitted") +
        stat_summary(fun = "median",
                     color = "red",
                     geom = "line") +
        scale_y_continuous() +
        theme(
          panel.background  = element_blank(),
          axis.line.x = element_line(color = "black"),
          axis.line.y = element_line(color = "black"),
          legend.background = element_rect(fill = "transparent", color =
                                             "black")
        )
      graph2 <-
        ggplot(data = x2, aes(x2[, 1], x2[, 2])) + geom_point() +
        labs(x = "Fitted values", y = "sqrt(Standardizedresiduals)") +
        ggtitle("Scale-Location") +
        stat_summary(fun = "median",
                     color = "red",
                     geom = "line") +
        scale_y_continuous() +
        theme(
          panel.background  = element_blank(),
          axis.line.x = element_line(color = "black"),
          axis.line.y = element_line(color = "black"),
          legend.background = element_rect(fill = "transparent", color =
                                             "black")
        )
      base::print(list(graph1, graph2))
    },
    #resid(), pred(),coef(),summary()
    resid = function() {
      return(e_hat)
    },
    pred = function() {
      return(y_hat)
    },
    coef = function() {
      return(beta_hat)
    },
    #summary()
    summary = function() {
      #rc = beta_hat
      #t = t_value
      #p = p_value
      rc = round(as.vector(beta_hat), 3)
      t = round(as.vector(t_value), 3)
      p = round(as.vector(p_value), 3)
      s = vector()
      for (i in 1:length(p)) {
        if (p[i] >= 0 & p[i] <= 0.001) {
          s[i] <- "***"
        } else{
          if (p[i] >= 0.001 & p[i] <= 0.01) {
            s[i] <- "**"
          } else{
            if (p[i] >= 0.01 & p[i] <= 0.05) {
              s[i] <- "*"
            } else{
              if (p[i] >= 0.05 & p[i] <= 0.1) {
                s[i] <- "."
              } else{
                if (p[i] >= 0.1 & p[i] <= 1) {
                  s[i] <- " "
                }
              }
            }
          }
        }
      }
      # vrc =  var_reg_coe
      se = round(sqrt(diag(var_reg_coe)), 3)
      # resv = res_var
      esrv = as.vector(sqrt(res_var))
      # df = d_f
      df = as.vector(d_f)
      m = matrix(c(rc, se, t, p, s), ncol = 5)
      rownames(m) = c(colnames(X))
      colnames(m) =
        c("coefficients", "standard error", "t_value", "p_value", "s")
      z =
        paste("Residual standard error:", esrv, "on", df, "degrees of freedom")
      a = as.data.frame(m)
      base::print(a)
      base::print(z)
      #   summary_matrix = matrix(, 3, 4)
      #   rownames(summary_matrix) = colnames(X)
      #   colnames(summary_matrix) = c("Estimate", "Std.Error", "t_value", "Pr(>|t|)")
      #   for (i in 1:nrow(summary_matrix)) {
      #     summary_matrix[i, 1] = as.numeric(beta_hat[i])
      #   }
      #   std_error = round((sqrt(diag(var_reg_coe))), 3)
      #   summary_matrix[, 2] = as.numeric(std_error)
      #   summary_matrix[, 3] = as.numeric(t_value)
      #   summary_matrix[, 4] = as.numeric(p_value)
      #   print(summary_matrix)
    }
  )
)
