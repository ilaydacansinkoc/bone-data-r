library("loon")
library("dplyr")

bone = read.delim(file = "bone.data")

age <- bone$age
spnbmd <- bone$spnbmd

p <- l_plot(age, spnbmd,
            color="darkgrey",
            xlabel="age", ylabel="spnbmd",
            showGuides = TRUE, showScales = TRUE,
            itemLabel = paste0("IDnum = ", bone$idnum, "\n", 
                               bone$gender, "\n", 
                               "Age: ", bone$age),
            showItemLabels = TRUE,
            linkingGroup="Bone density",
            title = "Spinal bone mineral density (spnbmd)")

axis <- l_layer_line(p,
                     x=extendrange(age, f=0.5), y=c(0,0),
                     label="axis", linewidth=2,
                     color = "black",
                     dash=c(10,10),
                     index="end")

histogram <- l_hist(as.numeric(bone$gender),
                    binwidth = 1, showBinHandle = FALSE,
                    showStackedColors = TRUE,
                    xlabel = "gender",
                    linkingGroup="Bone density",
                    title = "Gender (female=1; male=2)"
)

library(splines)
fitsmooth <- smooth.spline(age, spnbmd, df=5)
ageOrder <- order(age)
smooth <- l_layer_line(p,
                       x=age[ageOrder],
                       y=predict(fitsmooth,x=age[ageOrder])$y,
                       label="smooth fit", linewidth=4,
                       color = "blue")


## Define the update function
updateSmooth <- function(myPlot, minpts, df, color="blue") {
  ## Get the values for x and y from the plot
  ##
  ## For x
  xnew <- myPlot['xTemp']
  if (length(xnew) == 0) {xnew <- myPlot['x']}
  
  ## For y
  ynew <- myPlot['yTemp']
  if (length(ynew) == 0) {ynew <- myPlot['y']}
  
  ## Now **only** use the active selected points to construct the smooth
  sel <- myPlot['selected'] & myPlot['active']
  xnew <- xnew[sel]
  ynew <- ynew[sel]
  Nsel <- sum(sel)
  
  if (Nsel > 3 & diff(range(xnew)) > 0) {
    ## Find the range of the selected x values
    xrng <- extendrange(xnew)
    xvals.temp <- seq(from=min(xrng),
                      to=max(xrng), 
                      length.out=100)
    
    ## Redo our smooth **only** if we have enough points
    if ((Nsel > minpts) & (minpts > (df + 1))){
      fit.temp <- smooth.spline(xnew, ynew, df=df)
      ypred.temp <- predict(fit.temp,x=xvals.temp)$y
      ## update the smooth
      if (smooth %in% l_layer_ids(myPlot)) {
        ## reconfigure the smooth with new data
        l_configure(smooth, x=xvals.temp, y=ypred.temp)
      } else {
        ## If the smooth has been deleted, then we recreate it 
        ## (N.B. in the global environment)
        smooth <<-  l_layer_line(myPlot,
                                 x=xvals.temp, 
                                 y=ypred.temp,
                                 label="smooth fit", 
                                 linewidth=4,
                                 color = color)
      } 
    }
  }
  ## Update the tcl language's event handler
  tcl('update', 'idletasks')
}


l_bind_state(p, c("selected"),
             function() {updateSmooth(p, 10, 5, "blue")}
)