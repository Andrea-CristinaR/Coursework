#Andrea Cristina, Demography PS1, Life tables

#housekeeping 
setwd("C:/Users/Andrea Cristina/OneDrive - UW-Madison/Documents/PhD Courses/2025 F Courses/Demography/HW")
install.packages ("ggplot2")
install.packages ("gt")
library(gt)
library(ggplot2)

#Import deaths by age for French males in 1985
ps1_data_F2023 <- read.csv("~/PhD Courses/2025 F Courses/Demography/HW/ps1_data_F2023.csv")
View(ps1_data_F2023)


# Extract vectors from the data
x <- ps1_data_F2023$x      # Age 
nax <- ps1_data_F2023$nax  # Avg person-years lived

nNx <- as.numeric(gsub(",", "", ps1_data_F2023$nNx))  # Mid-year population
nDx <- as.numeric(gsub(",", "", ps1_data_F2023$nDx))  # Number of deaths

    #check that this worked. Yes, matches table. 
    x
    nax
    nDx  #character, problem
    nNx  #character, problem
        class(nDx) #character
        class(nNx)
      
        #### Detour issue and troubleshooting steps with questions to follow up on. 
        #learned to remove comas. will need to nest this in the "as number" function for it to work
        #gsub(",","",nDx) and  gsub(",","",nNx) 
        
        #Tried this nDx <- as.numeric(gsub(",", "", data$nDx)) but error said "not subsettable"
        #tried this,  nNx_num <- as.numeric(nNx)  , but NAs itroduced by coersion
        #removed objects from global environment and tried again - maybe the variable names were overlapping? [ask in OH]
        
#Next, I need interval width. Call this n. 
  ## new to me, Length counts number of age groups. Subtracts 2 because first 2 years are special. 
  ## rep () repeats a value a number of time. here I repeat 5 for 17 times.
  ## c() makes a vector with 1, 2, and 5 written 17 times. 

n <- c(1, 4, rep(5, length(x) - 2)) 
      n #checked that it worked. All good, lets continue. 

## Next - calculate components of the life table. 
    ##nmx age specific mortality rate. 
      nmx <- nDx / nNx
    ## nqx probability of dying at age interval
      nqx <- (n * nmx) / (1 + (n - nax) * nmx)
      nqx[length(nqx)] <- 1   #for the last group open interval 
    ##npx
      npx <- 1-nqx

     ##lx calculations, # surviving to age, two approaches a and b
      radix <- 100000
      lx_a <- numeric(length(x))
      lx_a[1] <- radix
      
      for(i in 2:length(x)) {
        lx_a[i] <- lx_a[i-1] * (1 - nqx[i-1])
      }
      
      lx_b <- npx*100000
    
      ## ndx Dying at age interval using lx_a
      ndx <- numeric(length(x))
      for(i in 1:(length(x)-1)) {
        ndx[i] <- lx_a[i] * nqx[i]
      }
      ndx[length(ndx)] <- lx[length(lx)]
      
      #test with different lx.  
      #ndxb <- numeric(length(x))
      #for(i in 1:(length(x)-1)) {
      #  ndxb[i] <- lx_b[i] * nqx[i]
      # }
   
      
      ## nLX
      nLx <- numeric(length(x))
      for(i in 1:(length(x)-1)) {
        nLx[i] <- n[i] * lx_a[i+1] + nax[i] * ndx[i]
      }
      nLx[length(nLx)] <- lx_a[length(lx_a)] / nmx[length(nmx)]
      
      ##Tx
      Tx <- numeric(length(x))
      Tx[length(Tx)] <- nLx[length(nLx)]  #last age group
      for(i in (length(x)-1):1) {
        Tx[i] <- Tx[i+1] + nLx[i]
      }
      
      ##ex
      ex <- Tx / lx_a
      
##Make a table 
      
life_table <- data.frame(
  x = x,
  n = n,
  nNx = nNx,
  nDx = nDx,
  nax = nax,
  nmx = round(nmx, 6),
  npx = round(nmx,6),
  nqx = round(nqx, 6),
  lx_a = round(lx_a, 0),
  ndx = round(ndx, 0),
  nLx = round(nLx, 0),
  Tx = round(Tx, 0),
  ex = round(ex, 2),
  lx_b = round(lx_b, 0)
) 
print("Complete Life Table:")
print(life_table)   

  #fail gt_tbl <- gt(life_table), #gave up making a pretty table. 

write.csv(life_table, "life_table_output3.csv", row.names = FALSE)||
  
##PLOTS##
  install.packages("ggplot2") 
  library(ggplot2)

  lx_plot <- ggplot2(dataframe, aes(x = column_1, lx_a = column_2))