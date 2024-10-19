#' Transposes data frame through wide-to-long pivoting
#'
#' @param df A data frame.
#' @param main_column A numeric or character vector.
#' @param stack_columns A numeric or character vector.
#' @param stacked_new_name A character vector.
#' @param last_column_name A character vector.
#' @returns A data frame.
#' @import tidyr
#' @examples
#' flippr(iris, 5, c(1:4), "Attribute", "Length_Width")
#'
#' flippr(iris, "Species", c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"), "Plant_Attribute", "Measurement")



flippr <- function(df,main_column,stack_columns,stacked_new_name,last_column_name) {

  if(is.character(main_column)){
    main1 <- as.character(main_column)
    z <- df %>% select(all_of(main1))

  }

  if(is.character(stack_columns)){
    char_vars <- as.character(stack_columns)
    y <- df %>% select(all_of(char_vars))

  }

  y <- df[stack_columns]

  z <- df[,main_column]
  b <- data.frame();
  c <- data.frame();
  for (i in 1:ncol(y))
  {
    New <- rep(names(y)[i], times = nrow(df))
    b <- rbind(b, New)
    c <-rbind(c,y[,i])

  }

  Variable2 <- pivot_longer(b, cols = everything(), names_to = "variable", values_to = "value")
  Variable3 <- pivot_longer(c, cols = everything(), names_to = "variable", values_to = "value")




  New_dfset <- data.frame(matrix(nrow = nrow(df)*ncol(y)))
  New_dfset$Variable1 <- rep(z, times = ncol(y))
  New_dfset$Variable2 <- Variable2$value
  New_dfset$Numeric <- Variable3$value
  New_dfset<- New_dfset[,-1]

  z2 <- names(df[main_column])

  colnames(New_dfset) <- c(z2,stacked_new_name,last_column_name)

  return(New_dfset)

}
