#' Sampling precision via multivariate dissimilarity-based standard error estimates
#' 
#' Adapted and combined from Jon Lefcheck's method
#' 
#' @author Jon Lefcheck, Martin Jung
#' @source Anderson, MJ and J Santanta-Garcon. 2015. "Measures of precision for dissimilarity-based multivariate analysis of ecological communities." Ecology Letters 18(1): 66-73.
#' 
#' @param D a dist object from species-site matrix
#' @param group A factor for a group (Default = 1)
#' @param nresamp Number of resamples (Default = 1000)
#' @param permanova Conduct a permanova (Default = T)
#' @param mins Estimate the minimum number of samples required (Default = T)
#' @return returns Test results
#' @export

# Function to conduct bootstrap and permutation tests
multSE = function(D, group = 1, nresamp = 1000, permanova = FALSE, mins = TRUE,  ... ) {
  # Ensure distance matrix is of class==matrix and not class==dist
  D = as.matrix(D)
  
  if(permanova == T) {
    
    # Conduct permutation (replace = F) and boostrapped (replace = T) resampling
    mult.SE.list = lapply(c(FALSE, TRUE), function(replace) {
      
      # Remove groups with only a single replicate
      if(min(table(group)) == 1) {
        
        remove = names(table(group)[table(group) == min(table(group))])
        
        D = D[!group %in% remove, !group %in% remove]
        
        group = group[!group %in% remove]
        
        warning("Groups with 1 replicate have been removed from the analysis!")
        
      }
      
      # Bootstrap subsetted distance matrix by smallest sample size across all groups
      do.call(cbind, lapply(2:min(table(group)), function(nsub) {
        
        # Define model parameters
        group.resamp = factor(rep(1:length(unique(group)), each = nsub))
        
        g = length(levels(factor(group.resamp)))
        
        X = model.matrix( ~ factor(group.resamp))
        
        H = X %*% solve(t(X) %*% X) %*% t(X)
        
        # And for each level of number of resamples
        do.call(rbind, lapply(1:nresamp, function(iresamp) {
          
          cols = do.call(c, lapply(unique(group), function(igroup) 
            # Randomly sample columns numbers for each group
            sample(which(group == igroup), size = nsub, replace = replace) ) )
          
          # Sample distance matrix from column numbers
          D.samp = D[cols, cols]
          
          # Calculate multivariate SE based on residuals from PERMANOVA
          N = dim(D.samp)[1]
          
          I = diag(N)
          
          A = -0.5 * D.samp^2 
          
          G = A - apply(A, 1, mean) %o% rep(1, N) - rep(1, N) %o% apply(A, 2, mean) + mean(A) 
          
          MSE = sum(diag((I - H) %*% G))/(N - g)   
          
          sqrt(MSE/nsub)
          
        } ) )
        
      } ) )
      
    } )
    
    #Calculate means and quantiles
    means = colMeans(mult.SE.list[[1]], na.rm = T)
    
    means.p = colMeans(mult.SE.list[[2]], na.rm = T)
    
    lower.ci = apply(mult.SE.list[[2]], 2, function(x) quantile(x, prob = 0.025, na.rm = T))
    
    upper.ci = apply(mult.SE.list[[2]], 2, function(x) quantile(x, prob = 0.975, na.rm = T))
    
    # Return data.frame with means and quantiles
    df = data.frame(
      n.samp = 1:length(means),
      means =  means,
      lower.ci = lower.ci + (means - means.p),
      upper.ci = upper.ci + (means - means.p) )
    
  } else {
    
    # Conduct bootstrapping for each group
    df = do.call(rbind, lapply(unique(group), function(igroup) {
      
      # Subset distance matrix by group
      subset.D = D[group == igroup, group == igroup]
      
      if(sum(subset.D) == 0) {
        data.frame(
          group = igroup,
          n.samp = 1,
          means =  0,
          lower.ci = NA,
          upper.ci = NA) 
        
      } else {
        
        # Conduct permutation (replace = F) and boostrapped (replace = T) resampling
        mult.SE.list = lapply(c(F, T), function(replace) {
          
          # Bootstrap subsetted distance matrix by each sample size
          do.call(cbind, lapply(2:ncol(subset.D), function(nsub) {
            # And for each level of number of resamples
            do.call(rbind, lapply(1:nresamp, function(iresamp) {
              
              # Randomly sample distance matrix
              D.samp = subset.D[sample(1:ncol(subset.D), size = nsub, replace), sample(1:ncol(subset.D), size = nsub, replace)]
              
              # Calculate multivariate SE based on SS
              n = dim(as.matrix(D.samp))[1]
              
              ss = sum(D.samp^2)/n
              
              v = ss/(n-1)
              
              sqrt(v/n)
              
            } ) ) 
          } ) )
        } )
        
        #Calculate means and quantiles
        means = colMeans(mult.SE.list[[1]], na.rm = T)
        
        means.p = colMeans(mult.SE.list[[2]], na.rm = T)
        
        lower.ci = apply(mult.SE.list[[2]], 2, function(x) quantile(x, prob = 0.025, na.rm = T))
        
        upper.ci = apply(mult.SE.list[[2]], 2, function(x) quantile(x, prob = 0.975, na.rm = T))
        
        # Return data.frame with means and quantiles
        data.frame(
          group = igroup,
          n.samp = 2:(length(means) + 1),
          means =  means,
          lower.ci = lower.ci + (means - means.p),
          upper.ci = upper.ci + (means - means.p) )
        
      }
      
    } ) )
  }
  # Minimum samples required?
  if(mins){
    df <- list(multSE = df,
               minimum.sample.size = minsamp(df, group)
               )
  }
  return(df) 
  
}

#' Calculate minimum sample size for each group from a given output
#' 
#' Adapted and combined from Jon Lefcheck's method
#' 
#' @author Jon Lefcheck, Martin Jung
#' @source Anderson, MJ and J Santanta-Garcon. 2015. "Measures of precision for dissimilarity-based multivariate analysis of ecological communities." Ecology Letters 18(1): 66-73.
#' 
#' @param output 
#' @param group A factor for a group (Default = 1)
#' @param ignore.min Number samples to be removed before calculation (Default = NULL)
#' @return returns Test results
#' @keywords internal
#' @export

minsamp = function(output, group = NA, ignore.min = NULL) {
  
  # Remove all samples N = 2 from dataset before calculating minimum
  if(!is.null(ignore.min)) output = subset(output, n.samp > ignore.min)
  
  # Break apart by group
  do.call(rbind, lapply(unique(group), function(i) {
    
    # Subset by group
    if(!all(is.na(group))) output = subset(output, group == i)
    
    # Retrieve minimum number of samples for which error bars still overlap
    if(nrow(output) == 0) 
      
      data.frame(
        group = i,
        min.mean = NA, 
        min.lower.ci = NA, 
        min.upper.ci = NA, 
        min.n = NA
      ) 
    
    else 
      
      data.frame(
        group = i,
        min.mean = min(output$means),
        min.lower.ci = output[which.min(output$means), "lower.ci"],
        min.upper.ci = output[which.min(output$means), "upper.ci"],
        min.n = min(output[output$lower.ci <= output[which.min(output$means), "upper.ci"], "n.samp"])
      )
    
  } ) )
  
}