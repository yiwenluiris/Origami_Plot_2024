#' @title Function to generate origami plot
#' @import plotrix
#' @import fmsb
#'
#' @param df input dataframe in the required format
#' @param object the name of the row that user wants to plot
#' @param min_value auxiliary point in the graph, default is min(df)/2
#' @param pcol color of the line of the polygon, default is rgb(0.2,0.5,0.5,1).
#' @param pfcol color to fill the area of the polygon, default is rgb(0.2,0.5,0.5,0.1).
#' @param axistype type of axes. 0:no axis label. 1:center axis label only. 2:around-the-chart label only. 3:both center and around-the-chart labels. Default is 1.
#' @param seg number of segments for each axis, default is 4.
#' @param pty point symbol, default is 16. 32 means not printing the points.
#' @param plty line types for plot data, default is 1:6
#' @param plwd line widths for plot data, default is 1
#' @param pdensity filling density of polygons, default is NULL
#' @param pangle angles of lines used as filling polygons, default is 45
#' @param cglty line type for radar grids, default is 1.4
#' @param cglwd line width for radar grids, default is 0.1
#' @param cglcol line color for radar grids, default is #000000
#' @param axislabcol color of axis label and numbers, default is #808080
#' @param title title of the chart, default is blank
#' @param na.itp logical. If true, items with NA values are interpolated from nearest neighbor items and connect them. If false, items with NA are treated as the origin. Default is TRUE.
#' @param centerzero logical. If true, this function draws charts with scaling originated from (0,0). If false, charts originated from (1/segments). Default is TRUE.
#' @param vlabels character vector for the names for variables, default is NULL
#' @param vlcex font size magnification for vlabels, default is 1
#' @param caxislabels center axis labels, default is seq(0,1,by = 0.25)
#' @param calcex font size magnification for caxislabels, default is NULL
#' @param paxislabels around-the-chart labels, default is NULL
#' @param palcex font size magnification for paxislabels, default is NULL
#' @details This is the main function in the R package that takes a list of data frame(s) and constructs
#' an origami plot. The function plots the main axes of the radar chart as solid lines and marks the score of each
#' variable on these axes with a filled circle. Additionally, it plots auxiliary axes as dashed lines at equal distances
#' between each neighboring pair of primary axes with auxiliary points generated from data_preparation. Finally, the
#' function connects all the points in order and obtain a connected region that resembles an origami star. Through this
#' method, we successfully address the challenge of axis order affecting the area of the connected region in radar plots.
#' The plot generated using ‘origami_plot’ benefit in that the area of the connected region within the origami plot remains
#'  consistent regardless of axis sequence.
#' @return NULL
#'
#' @examples
#' data(sucra)
#' origami_plot(sucra, object="Intravertical PGE2")
#'
#' @export

origami_plot<- function(df, object, min_value=NULL, pcol= rgb(0.2,0.5,0.5,1), pfcol= rgb(0.2,0.5,0.5,0.1),
                        axistype=1, seg=4, pty=16, plty=1:6, plwd=1,
                        pdensity=NULL, pangle=45, cglty=1.4, cglwd=0.1,
                        cglcol="#000000", axislabcol="#808080", title="",
                        na.itp=TRUE, centerzero=TRUE, vlabels=NULL, vlcex=1,
                        caxislabels=seq(0,1,by = 0.25), calcex=NULL,
                        paxislabels=NULL, palcex=NULL) {

  df <- df[row.names(df)==object,]
  #check if object is valid
  if(dim(df)[1]==0){ stop("The object is not present in the dataframe. Please ensure the object matches the row name exactly.")}
  if(dim(df)[1]>1){ stop("The object is duplicated in the dataframe. Please ensure there are no duplicate row names.")}
  if(dim(df)[1]==1){
    df <- data_preparation(df,min_value = min_value)
  }

  n_prime <- ncol(df)/2
  aux_array_odd <- as.vector(seq(1,2*n_prime-1,2))
  aux_array_even <- as.vector(seq(2,2*n_prime,2))

  n_col = dim(df)[2]
  if (!is.data.frame(df)) { stop("The data must  be given as dataframe.\n"); return() }
  if ((n <- length(df))<3) { stop("The number of variables must be 3 or more.\n"); return() }
  plot(c(-1.2, 1.2), c(-1.2, 1.2), type="n", frame.plot=FALSE, axes=FALSE,
       xlab="", ylab="", main=title, asp=1) # define x-y coordinates without any plot
  theta <- seq(90, 450, length=n+1)*pi/180
  theta <- theta[1:n]
  xx <- cos(theta)
  yy <- sin(theta)
  CGap <- ifelse(centerzero, 0, 1)
  points(0,0, pch = 16, col  = rgb(0,0,0,0.2))
  for (ind in 1:n_col){
    if (ind == 1){
      segments(0, 0, 0, 1, lwd = 2, lty = 1, col = rgb(0,0,0,0.2)) # factor 1
    } else{
      if ((ind %% 2) == 0){
        draw.radial.line(0, 1, center=c(0,0), deg = 90+(360/n_col)*(ind-1), lwd = 1, lty = 2, col = rgb(0,0,0,0.4))
      } else{
        draw.radial.line(0, 1, center=c(0,0), deg = 90+(360/n_col)*(ind-1), lwd = 1, lty = 1, col = rgb(0,0,0,0.4))
      }
    }
  }

  pcol <- pcol
  pfcol<- pfcol
  if (centerzero) {
   arrows(0, 0, xx*1, yy*1, lwd=cglwd, lty=cglty, length=0, col=cglcol)
  } else {
   arrows(xx/(seg+CGap), yy/(seg+CGap), xx*1, yy*1, lwd=cglwd, lty=cglty, length=0, col=cglcol)
  }
  PAXISLABELS <- df[1,1:n]
  if (!is.null(paxislabels)) PAXISLABELS <- paxislabels
  if (axistype==2|axistype==3|axistype==5) {
    if (is.null(palcex)) text(xx[1:n], yy[1:n], PAXISLABELS, col=axislabcol) else
      text(xx[1:n], yy[1:n], PAXISLABELS, col=axislabcol, cex=palcex)
  }
  VLABELS <- colnames(df)
  if (!is.null(vlabels)) VLABELS <- vlabels
  if (is.null(vlcex)) text(xx*1.2, yy*1.2, VLABELS) else
    text(xx*1.2, yy*1.2, VLABELS, cex=vlcex)
  series <- length(df[[1]])
  SX <- series-2
  if (length(pty) < SX) { ptys <- rep(pty, SX) } else { ptys <- pty }
  if (length(pcol) < SX) { pcols <- rep(pcol, SX) } else { pcols <- pcol }
  if (length(plty) < SX) { pltys <- rep(plty, SX) } else { pltys <- plty }
  if (length(plwd) < SX) { plwds <- rep(plwd, SX) } else { plwds <- plwd }
  if (length(pdensity) < SX) { pdensities <- rep(pdensity, SX) } else { pdensities <- pdensity }
  if (length(pangle) < SX) { pangles <- rep(pangle, SX)} else { pangles <- pangle }
  if (length(pfcol) < SX) { pfcols <- rep(pfcol, SX) } else { pfcols <- pfcol }


  for (i in 3:series) {
    xxs <- xx
    yys <- yy
    scale <- CGap/(seg+CGap)+(df[i,]-df[2,])/(df[1,]-df[2,])*seg/(seg+CGap)
    if (sum(!is.na(df[i,]))<3) { message(sprintf("[DATA NOT ENOUGH] at %d\n%g\n",i,df[i,])) # for too many NA's (1.2.2012)
    } else {
      for (j in 1:n) {
        if (is.na(df[i, j])) { # how to treat NA
          if (na.itp) { # treat NA using interpolation
            left <- ifelse(j>1, j-1, n)
            while (is.na(df[i, left])) {
              left <- ifelse(left>1, left-1, n)
            }
            right <- ifelse(j<n, j+1, 1)
            while (is.na(df[i, right])) {
              right <- ifelse(right<n, right+1, 1)
            }
            xxleft <- xx[left]*CGap/(seg+CGap)+xx[left]*(df[i,left]-df[2,left])/(df[1,left]-df[2,left])*seg/(seg+CGap)
            yyleft <- yy[left]*CGap/(seg+CGap)+yy[left]*(df[i,left]-df[2,left])/(df[1,left]-df[2,left])*seg/(seg+CGap)
            xxright <- xx[right]*CGap/(seg+CGap)+xx[right]*(df[i,right]-df[2,right])/(df[1,right]-df[2,right])*seg/(seg+CGap)
            yyright <- yy[right]*CGap/(seg+CGap)+yy[right]*(df[i,right]-df[2,right])/(df[1,right]-df[2,right])*seg/(seg+CGap)
            if (xxleft > xxright) {
              xxtmp <- xxleft; yytmp <- yyleft;
              xxleft <- xxright; yyleft <- yyright;
              xxright <- xxtmp; yyright <- yytmp;
            }
            xxs[j] <- xx[j]*(yyleft*xxright-yyright*xxleft)/(yy[j]*(xxright-xxleft)-xx[j]*(yyright-yyleft))
            yys[j] <- (yy[j]/xx[j])*xxs[j]
          } else { # treat NA as zero (origin)
            xxs[j] <- 0
            yys[j] <- 0
          }
        }
        else {
          xxs[j] <- xx[j]*CGap/(seg+CGap)+xx[j]*(df[i, j]-df[2, j])/(df[1, j]-df[2, j])*seg/(seg+CGap)
          yys[j] <- yy[j]*CGap/(seg+CGap)+yy[j]*(df[i, j]-df[2, j])/(df[1, j]-df[2, j])*seg/(seg+CGap)
        }
      }
      if (is.null(pdensities)) {
        polygon(xxs, yys, lty=pltys[i-2], lwd=plwds[i-2], border=pcols[i-2], col=pfcols[i-2])
      } else {
        polygon(xxs, yys, lty=pltys[i-2], lwd=plwds[i-2], border=pcols[i-2],
                density=pdensities[i-2], angle=pangles[i-2], col=pfcols[i-2])
      }
      points(xx[aux_array_odd]*scale[aux_array_odd], yy[aux_array_odd]*scale[aux_array_odd], pch=ptys[i-2], col=pcols[i-2])
      points(xx[aux_array_even]*scale[aux_array_even], yy[aux_array_even]*scale[aux_array_even], pch=ptys[i-2], col=rgb(0,0,0,0.2))
      # points(xx*scale, yy*scale, pch=ptys[i-2], col=rgb(0,0,0,0.2))
    }
  }


  for (i in 1:seg) { # complementary guide lines, dotted navy line by default
    polygon(xx*(i+CGap)/(seg+CGap), yy*(i+CGap)/(seg+CGap), lty=cglty, lwd=cglwd, border=cglcol)
    if (axistype==1|axistype==3) CAXISLABELS <- paste(i/seg*100,"(%)")
    if (axistype==4|axistype==5) CAXISLABELS <- sprintf("%3.2f",i/seg)
    if (!is.null(caxislabels)&(i<length(caxislabels))) CAXISLABELS <- caxislabels[i+1]
    if (axistype==1|axistype==3|axistype==4|axistype==5) {
      if (is.null(calcex)) text(-0.05, (i+CGap)/(seg+CGap), CAXISLABELS, col=axislabcol) else
        text(-0.05, (i+CGap)/(seg+CGap), CAXISLABELS, col=axislabcol, cex=calcex)
    }
  }
}
