sapply(c("gridExtra","ggplot2","dplyr"),
    require, character = TRUE)


rSquared = function(obs,pred){
    R2 = 1 - (sum((obs-pred)^2)/sum((obs-mean(obs))^2))
    R2
}



#' Creates regression plots (Pred vs Obs and Pred vs Res)
#'
#' Utility function to create the plots with in-built color coding (so you won't confuse test and train results)
#'
#' @param obs vector containing **observed** response values
#' @param pred vector containing **predicted** response values
#' @param title title to show on the plot
#' @param trainSet optional boolean. True by default. Set to FALSE if you are plotting a test result. This controls the color-coding
#' @return list of grobs containing the two plots
#'
#' @examples
#' plots = makeRegressionPlots(y, yhat, "Random Forests", TRUE)#'
#' @export
makeRegressionPlots=function(obs,pred,title,trainSet=TRUE){
    lineColor=ifelse(trainSet,"blue","red")
    
    
    errTitle = sprintf("MSE: %.02f\nR^2: %.02f",
                       RMSE(pred = pred, obs = obs), 
                       rSquared(pred = pred, obs = obs))   
    
    predVsObs = data.frame(obs=obs,pred=pred) %>%
            ggplot(aes(pred,obs)) +
                ggtitle(paste0(title," ",ifelse(trainSet,"Train","Test")," Pred vs Obs ")) +                
                geom_point(color=ifelse(trainSet,"black","orange")) + 
                geom_abline(intercept = 0, slope=1, size=0.7, color=lineColor ) +
                xlab("Predicted") +
                ylab("Observed") + 
                theme(plot.title = element_text(size=10))
    

    predVsRes = data.frame(res=(obs-pred),pred=pred) %>%
            ggplot(aes(pred,res)) +
                ggtitle(paste0(title," ",ifelse(trainSet,"Train","Test"), " Pred vs Res"),
                       subtitle=errTitle) +
                geom_point(color=ifelse(trainSet,"black","orange")) + 
                xlab("Predicted") +
                ylab("Residual") + 
                geom_abline(intercept = 0, slope=0, size=0.7,color=lineColor ) +
                theme(plot.title = element_text(size=10))
    
    list(predVsObs=predVsObs,predVsRes=predVsRes)   
}


#' Draws the plots contained in a vector 
#'
#' Utility function to draw plots on a grid
#'
#' @param plots vector containing plots (must be ggplot2 objects)
#' @param gridCols optional, number of plots to be shown side-by-side
#' @return nothing, it just plots
#'
#' @examples
#' plots = c(
#'     makeRegressionPlots(obs = simulated$y,pred = predict(rfTune, simulated),title = "RandomForests"),
#'     makeRegressionPlots(obs =  = test$y,pred = predict(rfTune, test),title = "RandomForests", trainSet = FALSE)
#' )
#' @export
drawPlots=function(plots, gridCols=2){
    grid.arrange(grobs=plots,ncol=gridCols,nrow=ceiling(length(plots)/gridCols))    
}