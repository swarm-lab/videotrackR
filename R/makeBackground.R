makeBackground <- function(source, n = 10, type = "mean") {
  n.frames <- countFrames(source)
  frames <- seq(1, n.frames, length.out = n)
  
  if (type == "mean") {
    
    mat <- array(0, dim = c(videoDim(source), 3))
    
    print("Loading images:")
    pb <- startpb(0, n - 1)
    for (i in 1:length(frames)) {
      mat <- mat + readFrame(frames[i], source)
      setpb(pb, i)
    }
    closepb(pb)
    
    print("Computing average image.")
    
    print("Done.")
    
    return(mat / n)
    
  } else if (type == "median") {
    
    mat <- array(0, dim = c(videoDim(source), 3))
    mat.r <- array(NA, dim = c(videoDim(source), n))
    mat.g <- array(NA, dim = c(videoDim(source), n))
    mat.b <- array(NA, dim = c(videoDim(source), n))
    
    print("Loading images:")
    pb <- startpb(0, n - 1)
    for (i in 1:length(frames)) {
      mat <- readFrame(frames[i], source)
      mat.r[, , i] <- mat[, , 1]
      mat.g[, , i] <- mat[, , 2]
      mat.b[, , i] <- mat[, , 3]
      setpb(pb, i)
    }
    closepb(pb)
    
    print("Computing median image. This is a slow process, please be patient.")
    
    print("   Median red:")
    mat[, , 1] <- pbapply(mat.r, c(1, 2), median.default)
    
    print("   Median green:")
    mat[, , 2] <- pbapply(mat.g, c(1, 2), median.default)
    
    print("   Median blue:")
    mat[, , 3] <- pbapply(mat.b, c(1, 2), median.default)
    
    print("Done.")
    
    return(mat)
    
  } else {
    
    stop("'type' should be 'mean' or 'median'")
    
  }  
}
