Estimating pi by using Monte Carlo simulation
========================================================
author: Aurel Prosz
date: 12.12.2017
autosize: true

Pi
========================================================
The number π  is a mathematical constant. Originally defined as the ratio of a circle's circumference to its diameter, it now has various equivalent definitions and appears in many formulas in all areas of mathematics and physics. It is approximately equal to 3.14159.(from Wikipedia)

**Goals:**
- Implement a basic Monte Carlo function
- Estimate π with this function
- Develop an application using reactive programming to adjust simulation parameters  

Link to the results:
https://paurel.shinyapps.io/MonteCarlo/

Monte Carlo method
========================================================

Monte Carlo methods (or Monte Carlo experiments) are a broad class of computational algorithms that rely on repeated random sampling to obtain numerical results. Their essential idea is using randomness to solve problems that might be deterministic in principle. (from Wikipedia)
I am going to use it to estimate pi by simulating random values inside a square containing a circle, where the radius of the circle is equal to the half-side length of the square. The developed code can distinguish the random points if they fall inside or outside of the circle, and the ratio of the sum of these points is proportional to pi: $$\pi = \frac{4M}{N}$$ where *M* are the points which fall into the circle, and *N* are the number of all points.


Implementing the simulation
========================================================


```r
set.seed(42)
monte_carlo <- function(simtime, r){ #simtime: how many points, r: radius
fromx <- -r; tox <- r;fromy <- fromx;toy <- tox; #Calculate the boundaries
xr <- seq(fromx, tox, 1/simtime)
yr <- runif(simtime*(tox-fromx)+1, fromy, toy) #generate random points
#Test if value is inside or outside the circle:
df <- data.frame(x = xr, y = yr, 
type = ifelse(test = (xr^2+yr^2) < r^2,yes = "inside", no = "outside"))
count.table <- table(df$type)
pi <- (4*count.table[1])/(count.table[1]+count.table[2]) #Calculate pi
return(as.double(pi))
}
```

Using the product
========================================================
![my image](paintba.PNG)
