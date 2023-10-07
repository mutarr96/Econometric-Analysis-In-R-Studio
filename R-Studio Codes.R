# Effects of Being Part of Commonwealth of Nations on Net Exports and Public Debt in The Gambia

# Mbye M. N. Gai
# Ousman Gaye
# Mutarr Jallow
# Muhammed Lamin Ceesay


library("Synth")


    # step_1 : Data preparation #
##############################################################################

setwd("./synth") # this is our directory, you should set it to where you save the file
dir()

mbee <- read.csv("clean data.csv")
str(mbee)
View(mbee)

# the public debt and exchange rate are in the countries national currency
# so we convert them all to dollars to have a uniform currency unit across the 
# countries. Because the exch rate is currently expressed as how much of the 
# country's currency is equivalent to 1 dollar, then we can divide 1 by the national
# currency to get the dollar value. Then we multiply this with the public debt
# to convert it to dollars

mbee$exchange.rate.dollar <- 1/mbee$exchange.rate
mbee$public.debt.billion.dollars <- mbee$exchange.rate.dollar * mbee$public.debt..billions..national.currency.
mbee[,-c(1:7)]

mbee[,"countryname"] <- as.character(mbee[,"countryname"])

names(mbee)


    # Analysis for public debt #
##############################################################################

dataprep.out <- dataprep(
        foo = mbee,
        predictors = c("public.debt.billion.dollars", "FDI..in.billions.USD.", 
                       "exchange.rate.dollar",  "population..in.millions.", 
                       "inflation" ),
        predictors.op = "mean",
        time.predictors.prior = 2009:2013,
        dependent = "public.debt.billion.dollars",
        unit.variable =  "countrynumb",
        time.variable = "year",
        unit.names.variable = "countryname",
        treatment.identifier = 1,
        controls.identifier = c(2:9),
        time.optimize.ssr = 2009:2013,
        time.plot = 2009:2018
        
)


dataprep.out$X0
dataprep.out$X1
dataprep.out$Z0
dataprep.out$Z1


## using the Synth function now

synth.out <- synth( data.prep.obj = dataprep.out, method = "BFGS")


##  obtaining tables
gaps <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)

### to get the percentage above or below
ous <- dataprep.out$Y0plot %*% synth.out$solution.w
which.max(gaps)
gaps[6]/ous[6] ## 0.085
gaps[7]/ous[7]  ## 0.09
gaps[8]/ous[8] ## 27.6% more 
gaps[9]/ous[9]  ## 11.2%
gaps[10]/ous[10]  ## -0.079

mean(c(0.276,0.112,0.085,0.09))
            # this one immediately below comes in handy later...
            # we will summon gaps.dbt later on for convenience (last part)
gaps.dbt <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)

synth.tables <- synth.tab(dataprep.res = dataprep.out, synth.res = synth.out)

names(synth.tables)

synth.tables$tab.pred

synth.tables$tab.w

write.table(synth.tables$tab.w, "table_of_weights.txt", sep = ",",
            quote = FALSE, row.names = FALSE)
write.table(synth.tables$tab.pred, "table_of_pretreatment.txt", sep = ",",
            quote = FALSE, row.names = TRUE)

## plotting the trajectory for debt
path.plot(synth.res = synth.out, dataprep.res = dataprep.out,
          Ylab = "public debt (billions of USD)", Xlab = "year",
          Legend = c("Actual Gambia (withdrew)","Synthetic Gambia (did not withdraw)"), 
          Legend.position = "topright")


# gap.plot
gaps.plot(synth.res = synth.out, dataprep.res = dataprep.out,
          Ylab = "public debt (billions of USD)", Xlab = "year",
          Ylim = c(-1.5, 1.5), Main = NA)


# Mean squared prediction error
#The MSPE refers to the squared deviations between the outcome for the treated unit
#and the synthetic control unit summed over all pre-intervention periods specified in Z1 and Z0

pred_err1 <- dataprep.out$Z1 - (dataprep.out$Z0 %*% synth.out$solution.w)
ms_pred_err <- mean(pred_err1^2)


#######################################################


    # analysis for net exports #
###########################################################################



dataprep.out2 <- dataprep(
        foo = mbee,
        predictors = c("net.exports..in.billions.USD.", "FDI..in.billions.USD.", 
                       "exchange.rate.dollar",  "population..in.millions.", 
                       "inflation" ),
        predictors.op = "mean",
        time.predictors.prior = 2009:2013,
        dependent = "net.exports..in.billions.USD.",
        unit.variable =  "countrynumb",
        time.variable = "year",
        unit.names.variable = "countryname",
        treatment.identifier = 1,
        controls.identifier = c(2:9),
        time.optimize.ssr = 2009:2013,
        time.plot = 2009:2018
        
)


dataprep.out2$X0
dataprep.out2$X1
dataprep.out2$Z0
dataprep.out2$Z1
dataprep.out2$Y0plot[10,7]
dataprep.out2$Y1plot

## using the Synth function now

synth.out2 <- synth( data.prep.obj = dataprep.out2, method = "BFGS")


##  obtaining tables
gaps2 <- dataprep.out2$Y1plot - (dataprep.out2$Y0plot %*% synth.out2$solution.w)
hepta <- dataprep.out2$Y0plot %*% synth.out2$solution.w
        # percentages
gaps2[6:10]/hepta[6:10]
mean(gaps2[6:10]/hepta[6:10])
cbind(dataprep.out2$Y1plot, hepta, gaps)

synth.tables <- synth.tab(dataprep.res = dataprep.out2, synth.res = synth.out2)

names(synth.tables)

synth.tables$tab.pred
write.table(synth.tables$tab.pred, "table_pretreatment_2.txt", sep = ",",
            quote = FALSE, row.names = TRUE )

synth.tables$tab.w
write.table(synth.tables$tab.w, "table_weights_NX_2.txt", sep = ",",
            quote = FALSE, row.names = FALSE )


## plotting the trajectory 
#
path.plot(synth.res = synth.out2, dataprep.res = dataprep.out2,
          Ylab = "Net exports (billions of USD)", Xlab = "year",
          Ylim = c(-2.5,2.5),
          Legend = c("Actual Gambia (withdrew)","Synthetic Gambia (did not withdraw"), 
          Legend.position = "topright")


# gap.plot
gaps.plot(synth.res = synth.out2, dataprep.res = dataprep.out2,
          Ylab = "net exports (billions of USD)", Xlab = "year",
          Ylim = c(-1.5, 1.8), Main = NA)


# Mean squared prediction error for net exports
pred_err2 <- dataprep.out2$Z1 - (dataprep.out2$Z0 %*% synth.out2$solution.w)
ms_pred_err2 <- mean(pred_err2^2)


############################################################################


    # Placebo Test for net exports #
############################################################################

        # then we create a loop for the other countries
 # loop for net exports

A = c(1:9)
emt <- matrix(NA, nrow = 10, ncol = 11)
emt2 <- matrix(NA, nrow = 1, ncol = 8)

for(i in 2:9){
       
    dataprep.out <- dataprep(
            foo = mbee,
            predictors = c("net.exports..in.billions.USD.", "FDI..in.billions.USD.", 
                               "exchange.rate.dollar",  "population..in.millions.", 
                               "inflation" ),
            predictors.op = "mean",
                time.predictors.prior = 2009:2013,
                dependent = "net.exports..in.billions.USD.",
                unit.variable =  "countrynumb",
                time.variable = "year",
                unit.names.variable = "countryname",
                treatment.identifier = i,
                controls.identifier = A[-i],
                time.optimize.ssr = 2009:2013,
                time.plot = 2009:2018
                
        )  
    
    
    synth.out <- synth( data.prep.obj = dataprep.out, method = "BFGS")
    
    # tables
    gaps <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
    
    synth.tables <- synth.tab(dataprep.res = dataprep.out, synth.res = synth.out)
        
     emt[,i+2] <- gaps   
    
     
     pred_err <- dataprep.out$Z1 - (dataprep.out$Z0 %*% synth.out$solution.w)
     ms_err <- mean(pred_err^2)
     
     emt2[,i-1] <- ms_err
     
     print(emt)
        
}

x <- c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)
emt[,1] <- x
emt[,2] <- 0  # this is just so we can draw the line at 0
emt[,3] <- gaps2
colnames(emt) <-  c("Year","zeroline" ,"Gambia" , "Ghana", "Nigeria" ,"Rwanda","SouthAfrica","Kenya",       
                    "Uganda", "Malawi","Tanzania")

emt 


colnames(emt2) <- c( "Ghana", "Nigeria" ,"Rwanda","SouthAfrica","Kenya",       
                    "Uganda", "Malawi","Tanzania")
emt2

### we find the mspe of the placebo countries that are 5 times greater than the 
### mspe of the treatment country. Then we exclude those countries from the placebo
### test as recommended by Abadie et. al. (2010)


which(emt2 > ms_pred_err2*5) # we have that Nigeria, South Africa and Kenya have
                    # mspe more than 5 times greater than that of the treated country

placebo.plotting.nx <- as.data.frame(emt[,-c(5,7,8)])

write.csv(placebo.plotting.nx, file = "gaps_nx_robustness.csv", col.names = TRUE, row.names = FALSE)



library("ggplot2")


ggp1 <- ggplot(placebo.plotting.nx, aes(Year)) +
        geom_line(aes(y= Gambia, color = "red" )) +
        geom_line(aes(y= Ghana, color = "black")) +
        geom_line(aes(y=Rwanda, color = "black")) +
        geom_line(aes(y=Uganda, color = "black")) +
        geom_line(aes(y= Malawi, color = "black")) +
        geom_line(aes(y=Tanzania, color = "black")) +
        geom_line(aes(y=zeroline, color = "grey"))

ggp1 + ggtitle("ROBUSTNESS CHECKS: Net exports") +
        xlab("YEAR") + ylab("GAPS IN NET EXPORTS (BILLIONS USD)") +
        geom_vline(xintercept = 2013, linetype="dotted", 
               color = "blue", size=1.5)

################################################################################


        # Placebo for public debt #
#############################################################################

emt3 <- matrix(NA, nrow = 10, ncol = 11)
emt4 <- matrix(NA, nrow = 1, ncol = 8)

for(i in 2:9){
        
        dataprep.out <- dataprep(
                foo = mbee,
                predictors = c("public.debt.billion.dollars", "FDI..in.billions.USD.", 
                               "exchange.rate.dollar",  "population..in.millions.", 
                               "inflation" ),
                predictors.op = "mean",
                time.predictors.prior = 2009:2013,
                dependent = "public.debt.billion.dollars",
                unit.variable =  "countrynumb",
                time.variable = "year",
                unit.names.variable = "countryname",
                treatment.identifier = i,
                controls.identifier = A[-i],
                time.optimize.ssr = 2009:2013,
                time.plot = 2009:2018
                
        )  
        
        
        synth.out <- synth( data.prep.obj = dataprep.out, method = "BFGS")
        
        # tables
        gaps <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
        
        synth.tables <- synth.tab(dataprep.res = dataprep.out, synth.res = synth.out)
        
        emt3[,i+2] <- gaps 
        
        pred_err <- dataprep.out$Z1 - (dataprep.out$Z0 %*% synth.out$solution.w)
        ms_err <- mean(pred_err^2)
        
        emt4[,i-1] <- ms_err
        
        print(emt3)
        
}


emt3[,1] <- x
emt3[,2] <- 0  # this is just so we can draw the line at 0
emt3[,3] <- gaps.dbt
colnames(emt3) <- c("Year","zeroline" ,"Gambia" , "Ghana", "Nigeria" ,"Rwanda","SouthAfrica","Kenya",       
                    "Uganda", "Malawi","Tanzania")

colnames(emt4) <- c( "Ghana", "Nigeria" ,"Rwanda","SouthAfrica","Kenya",       
                     "Uganda", "Malawi","Tanzania")
emt4

which(emt4 > ms_pred_err*5) # we have that Ghana, South Africa and Kenya  and 
                            # Tanzania have mspe more than 5 times greater than 
                            # that of the treated country (mspe for public debt)

placebo.plotting.dbt <- as.data.frame(emt3[,-c(4,7,8,11)])

write.csv(placebo.plotting.dbt, file = "gaps_dbt_rob.csv", col.names = TRUE, row.names = FALSE)


## plotting


ggp2 <- ggplot(placebo.plotting.dbt , aes(Year)) +
        geom_line(aes(y= Gambia, color = "red" )) +
        geom_line(aes(y=Rwanda, color = "black")) +
        geom_line(aes(y= Nigeria, color = "black" )) +
        geom_line(aes(y=Uganda, color = "black")) +
        geom_line(aes(y= Malawi, color = "black")) +
        geom_line(aes(y=zeroline, color = "grey"))

ggp2 + ggtitle("ROBUSTNESS CHECKS: Public debt") +
        xlab("YEAR") + ylab("GAPS IN PUBLIC DEBT (BILLIONS USD)")+
    geom_vline(xintercept = 2013, linetype="dotted", 
               color = "blue", size=1.5)



#############################################################################


