# bind global variables
if(getRversion() >= "2.15.1") utils::globalVariables(c("xpos", "value", "Var2", "grp", "prc", "fg", "cprc"))



#' @title Compute hierarchical or kmeans cluster analysis
#' @name sjc.cluster
#' @description Compute hierarchical or kmeans cluster analysis and returns the group
#'                association for each observation as vector.
#' @seealso \code{\link{sjc.dend}} \cr
#'          \code{\link{sjc.grpdisc}} \cr
#'          \code{\link{sjc.elbow}}
#'
#' @param data The data frame containing all variables that should be used for the
#'          cluster analysis.
#' @param groupcount The amount of groups (clusters) that should be retrieved. Use
#'          the \code{\link{sjc.elbow}}-function to determine the group-count depending
#'          on the elbow-criterion. Use \code{\link{sjc.grpdisc}}-function to inspect
#'          the goodness of grouping.
#' @param method Indicates the clustering method. If \code{"h"} (default), a hierachical 
#'          clustering using the ward method is computed. Use any other parameter to compute
#'          a k-means clustering.
#' @return The group classification for each observation as vector. This group
#'           classification is needed for \code{\link{sjc.grpdisc}}-function to
#'           check the goodness of classification.
#' 
#' @examples
#' # Hierarchical clustering of mtcars-dataset
#' groups <- sjc.cluster(mtcars, 5)
#' 
#' # K-means clustering of mtcars-dataset
#' groups <- sjc.cluster(mtcars, 5, method="k")
#' 
#' @import ggplot2
#' @export
sjc.cluster <- function(data, groupcount, method="h") {
  # Prepare Data
  # listwise deletion of missing
  data <- na.omit(data) 
  # --------------------------------------------------
  # Ward Hierarchical Clustering
  # --------------------------------------------------
  if (method=="h") {
    # distance matrix
    d <- dist(data, method="euclidean")
    # hierarchical clustering, using ward
    hc <- hclust(d, method="ward") 
    # cut tree into x clusters
    groups <- cutree(hc, k=groupcount)
  }
  else {
    km <- kmeans(data, groupcount)
    # return cluster assignment
    groups <- km$cluster
  }
  # return group assignment
  return(groups)
}


#' @title Compute hierarchical cluster analysis and visualize group classification
#' @name sjc.dend
#' @description Computes a hierarchical cluster analysis and plots a hierarchical
#'                dendogram with highlighting rectangles around the classified groups.
#'                Can be used, for instance, as visual tool to verify the elbow-criterion
#'                (see \code{\link{sjc.elbow}}).
#' @seealso \code{\link{sjc.cluster}} \cr
#'          \code{\link{sjc.grpdisc}} \cr
#'          \code{\link{sjc.elbow}}
#'
#' @param data The data frame containing all variables that should be used for the
#'          cluster analysis.
#' @param groupcount The amount of groups (clusters) that should be used. Use
#'          the \code{\link{sjc.elbow}}-function to determine the group-count depending
#'          on the elbow-criterion. Use \code{\link{sjc.grpdisc}}-function to inspect
#'          the goodness of grouping.
#' 
#' @import ggplot2
#' @examples
#' # Plot dendogram of hierarchical clustering of mtcars-dataset
#' # and show group classification
#' sjc.dend(mtcars, 5)
#' 
#' @export
sjc.dend <- function(data, groupcount) {
  # Prepare Data
  # listwise deletion of missing
  data <- na.omit(data) 
  # --------------------------------------------------
  # Ward Hierarchical Clustering
  # --------------------------------------------------
  # distance matrix
  d <- dist(data, method="euclidean")
  # hierarchical clustering, using ward
  hc <- hclust(d, method="ward") 
  # display simple dendogram
  plot(hc, main="Cluster Dendogramm", xlab="Hierarchical Cluster Analysis, Ward-Method")
  # draw dendogram with red borders around the clusters 
  rect.hclust(hc, k=groupcount, border="red")
}


#' @title Compute a linear discriminant analysis on classified cluster groups
#' @name sjc.grpdisc
#' @description Computes linear discriminant analysis on classified cluster groups.
#'                This function plots a bar graph indicating the goodness of classification
#'                for each group.
#' @seealso \code{\link{sjc.dend}} \cr
#'          \code{\link{sjc.cluster}} \cr
#'          \code{\link{sjc.elbow}}
#'
#' @param data The data frame containing all variables that should be used for the
#'          check for goodness of classification of a cluster analysis.
#' @param groups The group classification of the cluster analysis that was returned
#'          from the \code{\link{sjc.cluster}}-function.
#' @param groupcount The amount of groups (clusters) that should be used. Use
#'          the \code{\link{sjc.elbow}}-function to determine the group-count depending
#'          on the elbow-criterion.
#' @param showTotalCorrect If \code{TRUE} (default), a vertical line indicating the
#'          overall goodness of classification is added to the plot, so one can see
#'          whether a certain group is below or above the average classification goodness.
#'          
#' @examples
#' # retrieve group classification from hierarchical cluster analysis
#' # on the mtcars data set (5 groups)
#' groups <- sjc.cluster(mtcars, 5)
#' 
#' # plot goodness of group classificatoin
#' sjc.grpdisc(mtcars, groups, 5)
#' 
#' @importFrom MASS lda
#' @import ggplot2
#' @export
sjc.grpdisc <- function(data, groups, groupcount, showTotalCorrect=TRUE) {
  xval <- cbind(1:groupcount)-0.25
  xplotval <- cbind(1:groupcount)
  # ---------------------------------------------------------------
  # compute discriminant analysis of groups on original data frame
  # ---------------------------------------------------------------
  disc <- lda(groups ~ ., data=data, na.action="na.omit", CV=TRUE)
  # ---------------------------------------------------------------
  # Assess the accuracy of the prediction
  # percent correct for each category of groups
  # ---------------------------------------------------------------
  ct <- table(groups, disc$class)
  dg <- diag(prop.table(ct, 1))
  # print correct percentage for each category of groups
  print(dg)
  # ---------------------------------------------------------------
  # print barplot for correct percentage for each category of groups
  # ---------------------------------------------------------------
  perc <- round(100*dg,2)
  percrest <- round(100-perc,2)
  counts <- rbind(perc, percrest)
  # total correct percentage
  totalcorrect <- sum(diag(prop.table(ct)))
  # round total percentages and transform to percent value
  totalcorrect <- round(100*totalcorrect,2)
  print(totalcorrect)
  
  # create three data columns for data frame which is
  # needed to plot the barchart with ggplot
  newdat <- NULL
  tmpdat <- NULL
  filldat <- NULL
  labeldat <- NULL
  
  # data frame has flexible count of rows, depending on
  # the amount of groups in the lda
  for (i in 1:groupcount) {
    # first columns indicates the two parts of each group
    # (correct percentage and remaining percentage untill 100%)
    newdat <- rbind(newdat, c(paste("g",i,sep="")))
    newdat <- rbind(newdat, c(paste("g",i,sep="")))
    # second columns contains the percentage of lda
    # followed by the remaining percentage to 100%
    tmpdat <- rbind(tmpdat, perc[i])
    tmpdat <- rbind(tmpdat, percrest[i])
    # third columns indicates both which data row contains
    # the lda-percentage and which one the remaining percentage
    filldat <- rbind(filldat, "1")
    filldat <- rbind(filldat, "2")
    # last column is created for printing the label-values
    # we need only on percentage value, otherwise double labels are
    # printed
    labeldat <- rbind(labeldat, perc[i])
    labeldat <- rbind(labeldat, 0)
  }
  # create data frame
  mydat <- data.frame(filldat, newdat, tmpdat, labeldat)
  # name columns
  names(mydat) <- c("fg", "grp", "prc", "cprc")
  # fillgroup-indicator ($fg) needs to be a factor
  mydat$fg <- factor(mydat$fg)
  # plot bar charts, stacked proportional
  # this works, because we always have two "values" (variables)
  # for the X-axis in the $grp-columns indicating a group
  classplot <- ggplot(mydat, aes(x=grp, y=prc, fill=fg)) +
    # use stat identity to show value, not count of $prc-variable
    # draw no legend!
    geom_bar(stat="identity", colour="black", show_guide=FALSE) +
    # fill bars
    scale_fill_manual(values=c("#235a80", "#80acc8")) +
    # give chart and X-axis a title
    labs(title="Accuracy of cluster group classification (in %)", x="cluster groups", y=NULL) +
    # print value labels into bar chart
    geom_text(aes(label=cprc, y=cprc), vjust=1.2, colour="white", size=4.5) +
    # larger font size for axes
    theme(axis.line = element_line(colour="gray"), 
          axis.text = element_text(size=rel(1.2)), 
          axis.title = element_text(size=rel(1.2))) +
    # set ticks
    scale_y_continuous(breaks = seq(0, 100, 10)) +
    # change range on x-axis, so the text annotation is visible and
    # beside the bars and not printed into them
    coord_cartesian(ylim=c(0,100), xlim=c(-0.5,groupcount+1))
  if (showTotalCorrect) {
    classplot <- classplot +
    # set line across all bars which indicates the total percentage of
    # correct assigned cases
    geom_hline(yintercept=totalcorrect, linetype=2, colour="#333333") +
      # print text annotation
      annotate("text", x=0, y=totalcorrect, vjust=1.2, label=paste("overall", c(totalcorrect), sep="\n"), size=5, colour="#333333")
  }
  print(classplot)
}


#' @title Plot elbow values of a k-means cluster analysis
#' @name sjc.elbow
#' @description Plot elbow values of a k-means cluster analysis. This function
#'                computes a k-means cluster analysis on the provided data frame
#'                and produces two plots: one with the different elbow values
#'                and a second plot that maps the differences between each
#'                "step" (i.e. between elbow values) on the y-axis. An
#'                increase in the second plot may indicate the elbow criterion.
#' 
#' @seealso \code{\link{sjc.dend}} \cr
#'          \code{\link{sjc.cluster}} \cr
#'          \code{\link{sjc.grpdisc}}
#'
#' @param data The data frame containing all variables that should be used for 
#'          determining the elbow criteria.
#' @param steps The maximum group-count for the k-means cluster analysis for
#'          which the elbow-criterion should be displayed. Default is \code{15}.
#'          
#' @examples
#' # plot elbow values of mtcars dataset
#' sjc.elbow(mtcars)
#' 
#' @importFrom reshape2 melt
#' @import ggplot2
#' @export
sjc.elbow <- function (data, steps=15) {
  # define line linecolor
  lcol <- rgb(128,172,200, maxColorValue=255)
  # calculate elbow values (sum of squares)
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:steps) wss[i] <- sum(kmeans(data,centers=i)$withinss)
  # round and print elbow values
  wssround <- round(wss,0)
  dfElbowValues <- as.data.frame(melt(wssround))
  dfElbowValues <- cbind(dfElbowValues, xpos=1:nrow(dfElbowValues))
  # calculate differences between each step
  diff <- c()
  for (i in 2:steps) diff <- cbind(diff,wssround[i-1]-wssround[i])
  dfElbowDiff <- as.data.frame(melt(diff))
  # --------------------------------------------------
  # Plot diagram with sum of squares
  # all pointes are connected with a line
  # a bend the in curve progression might indicate elbow
  # --------------------------------------------------
  plot(ggplot(dfElbowValues, aes(x=xpos, y=value, label=value)) + 
    geom_line(colour=lcol) + 
    geom_point(colour=lcol, size=3) +
    geom_text(hjust=-0.3, size=4) +
    labs(title="Elbow criterion (sum of squares)", x="Number of clusters", y="elbow value"))
  # --------------------------------------------------
  # Plot diagram with differences between each step
  # increasing y-value on x-axis (compared to former y-values)
  # might indicate elbow
  # --------------------------------------------------
  plot(ggplot(dfElbowDiff, aes(x=Var2, y=value, label=value)) + 
    geom_line(colour=lcol) + 
    geom_point(colour=lcol, size=3) +
    geom_text(hjust=-0.3, size=4) +
    labs(title="Elbow criterion (differences between sum of squares)", x="number of clusters", y="delta"))
}
