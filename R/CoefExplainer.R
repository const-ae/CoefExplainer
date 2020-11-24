

#' Explore the Meaning of Coefficients in a Linear Model with Categorical Covariates
#'
#' The function takes a data frame and a formula. It fits a linear model and parses
#' the formula so that the coefficients can be visuallized easily.
#'
#' @param data a data.frame that contains the covariates of the \code{formula} as
#'   columns.
#' @param formula a standard R formula (e.g. \code{y ~ group + batch + sex}). Unlike
#'   most R functions, the covarites in the formula (e.g. \code{group}, \code{batch} etc.)
#'   can only reference columns of \code{data}.
#'
#' @details
#' Note: the function can only handle categorical covariates, because
#' continuous variables are more difficult to visualize.
#'
#' @return
#'  A list with stuff.
#'
#'
#' @examples
#'
#'   peng <- palmerpenguins::penguins
#'   # Remove any NA's
#'   peng <- peng[! apply(peng, 1, function(row) any(is.na(row))), ]
#'   peng
#'
#'
#'   coefExplFit <- CoefExplainer(peng, flipper_length_mm ~ species + island + sex)
#'
#'   plotModel(coefExplFit)
#'
#'   plotModelMatrix(coefExplFit)
#'
#'   plotCoef(coefExplFit)
#'
#' @export
CoefExplainer <- function(data, formula){
  dependent_var <- as.character(formula.tools::lhs.vars(formula))
  stopifnot(length(dependent_var) == 1)
  pf <- parent.frame()
  data[dependent_var] <- get_var(dependent_var, data, enclos = pf)

  independent_vars <- formula.tools::rhs.vars(formula)
  data[independent_vars] <- lapply(independent_vars, function(v){
    get_var(v, data, enclos = pf)
  })

  if(! dependent_var %in% colnames(data) ||
     ! all(dependent_var %in% colnames(data))){
    stop("All variables of the formula formula must be in the data. ",
         "Unlike 'lm' this function cannot access global variables.")
  }

  # Check that now continuous variable
  is_cont_var <- vapply(independent_vars, function(v) is.numeric(data[[v]]), FUN.VALUE = FALSE)
  if(any(is_cont_var)){
    stop("Variable ", paste0(independent_vars[is_cont_var], collapse = ", "), " are ",
         "continuous. At the moment this function can only handle categorical variables. ",
         "You can convert numeric variables with few levels to a categorical variable with ",
         "'as.factor()'.")
  }

  # Check that there are no NA's in the relevant variables
  if(any(is.na(data[[dependent_var]])) || any(sapply(dependent_var, function(v) any(is.na(data[[v]]))))){
    stop("the variables in data must not contain NA's.")
  }

  # Convert character variables to factors
  is_char_var <- vapply(independent_vars, function(v) is.character(data[[v]]), FUN.VALUE = FALSE)
  data[,independent_vars[is_char_var]] <- lapply(independent_vars[is_char_var], function(v) as.factor(data[[v]]))


  # Sort data by the dependent vars
  data2 <- data
  data2$.old_order_var <- seq_len(nrow(data))
  data2 <- data2[do.call(order, lapply(independent_vars, function(v) data[[v]])), ]

  # Create model_matrix
  model_matrix2 <- model.matrix(formula, data = as.data.frame(data2))
  model_matrix <- model.matrix(formula, data = as.data.frame(data))

  # Identify groups
  max_groups <- 20
  if(! glmGamPoi:::lte_n_equal_rows(model_matrix2, n = max_groups)){
    stop("Too many groups")
  }
  groups2 <- glmGamPoi:::get_row_groups(model_matrix2, n = max_groups)
  n_groups <- max(groups2)
  groups <- groups2[order(data2$.old_order_var)]

  # data2 and model_matrix2 are unnecssary from here on
  example_for_groups_idx <- sapply(seq_len(n_groups), function(g) which(groups == g)[1])
  names(example_for_groups_idx) <- seq_len(n_groups)

  # create labels
  labels <- vapply(example_for_groups_idx, function(idx){
    paste0(independent_vars, "=", unlist(data[idx, independent_vars]), collapse = "\n")
  }, FUN.VALUE = "")

  # Fit a linear model
  linear_model <- lm(formula, data = data)

  list(data = data,
       formula = formula,
       dependent_var = dependent_var,
       independent_vars = independent_vars,
       covariates = colnames(model_matrix),
       model_matrix = model_matrix,
       groups = groups,
       n_groups = length(unique(groups)),
       example_for_groups_idx = example_for_groups_idx,
       linear_model = linear_model,
       beta = coef(linear_model),
       labels = labels)
}



get_var <- function(term, data, enclos = parent.frame()){
  eval(parse(text=term), envir = data, enclos = enclos)
}


