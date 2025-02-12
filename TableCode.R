
########################################Create table for each result 

objname <- c(ls(pattern = "n1"),ls(pattern = "n2"),ls(pattern = "n3"),
            ls(pattern = "n4"),ls(pattern = "n5"))
  
obj <- mget(objname)

  
Type1errormatrix <- function(obj){
  
  permutation_matrix <- matrix(nrow = length(obj), ncol = 2)
  
  for (i in seq_along(obj)) {
    permutation_matrix[i, ] <- obj[[i]] 
  }

  rownames(permutation_matrix) <- names(obj)

  colnames(permutation_matrix) <- c("Permutation", "Assymptotic")
  
  row_names <- rownames(permutation_matrix)
  numerical_parts <- as.numeric(gsub("n([0-9]+).*", "\\1", row_names))
  
  sorting_df <- data.frame(numerical_part = numerical_parts, row_name = row_names)
  
  sorting_df_sorted <- sorting_df[order(sorting_df$numerical_part), ]
  
  sorted_row_names <- sorting_df_sorted$row_name
  
  Type1errormatrix <- permutation_matrix[sorted_row_names, ]
  
  newmat <- matrix(NA,nrow = 8,ncol = 6)
  
  w <- 0
  k <- 1
  d <- 0
  for (i in 1:nrow(Type1errormatrix)){
    for (j in 1:ncol(Type1errormatrix)){
      d <- d + 1
      newmat[k,d] <- Type1errormatrix[i,j]
      w <- w+1
      if (w %% 6 == 0 & w !=0){
        k = k + 1
      }
      if (d == 6){
        d <- 0
      }
    }
  }
  
  cat("---------------------------------------------------\n")
  cat("          1. M2n 2. M3n 3. 2Skewt\n")
  cat("---------------------------------------------------\n")
  
  rownames(newmat) <- c("50","100","200","500","1000","2000", "5000","10000")
  colnames(newmat) <- c("Permutations", "Asymptotic" , "Permutations", "Asymptotic" 
                      , "Permutations", "Asymptotic" )
  return(newmat)
  
}

