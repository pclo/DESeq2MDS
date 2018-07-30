#'Plot MDS for a DESeq2 object
#'
#' @author Alejandro Reyes
#' @param  rld A \code{\link\{DESeq2Transform}} object
#' @export
#' @import DESeq2
#' @importFrom ggplot2 ggplot aes geom_point labs
#' @return A ggplot2 object
#' @examples
#' library(DESeq2)
#' library(ggplot2)
#' library(cowplot)
#' dds <- makeExampleDESeqDataSet(betaSD=1)
#' rld <- rlog(dds)
#' plotMDS(rld)


plotMDS <- function( rld ){
  stopifnot( is( rld, "DESeqTransform" ) )
  d <- dist( t( assay(rld) ) )
  mdsResult <- cmdscale( d )
  mdsResult <- as.data.frame( mdsResult )
  colnames( mdsResult ) <- sprintf( "coord%d", seq_len(ncol(mdsResult)))
  mdsResult$condition <- colData(rld)$condition
  pl <- ggplot( mdsResult, aes(coord1, coord2, col=condition)) +
    geom_point() + labs(x="Coordinate 1", y="Coordinate 2")
  pl
}

