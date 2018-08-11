Clustering Models in R

> library(vegan) 
Error in library(vegan) : there is no package called ‘vegan’
> install.packages("vegan")
Installing package into ‘C:/Users/Strickland/Documents/R/win-library/3.1’
(as ‘lib’ is unspecified)
also installing the dependency ‘permute’

trying URL 'http://cran.rstudio.com/bin/windows/contrib/3.1/permute_0.8-3.zip'
Content type 'application/zip' length 506493 bytes (494 Kb)
opened URL
downloaded 494 Kb

trying URL 'http://cran.rstudio.com/bin/windows/contrib/3.1/vegan_2.2-0.zip'
Content type 'application/zip' length 2837471 bytes (2.7 Mb)
opened URL
downloaded 2.7 Mb

package ‘permute’ successfully unpacked and MD5 sums checked
package ‘vegan’ successfully unpacked and MD5 sums checked

The downloaded binary packages are in
	C:\Users\Strickland\AppData\Local\Temp\RtmpS8APAg\downloaded_packages
> library(vegan) 
Loading required package: permute
Loading required package: lattice
This is vegan 2.2-0
> data(dune)
> 
> d <- vegdist(dune)
> 
> par(mfrow=c(1,3))
> 
> par(mfrow=c(3,1))
> 
> par(mfrow=c(1,1))
> 
> par(mar=c(3,4,1,1)+.1) 
> 
> csin <- hclust(d, method="single") 
> 
> csin

Call:
hclust(d = d, method = "single")

Cluster method   : single 
Distance         : bray 
Number of objects: 20 

> plot(csin)
> 
> plot(csin, hang=-1) 
> 
> ccom <- hclust(d, method="complete") 
> plot(ccom, hang=-1) 
> caver <- hclust(d, method="aver") 
> plot(caver, hang=-1)
> 
> vegemite(dune, caver)
                              
          111211 11    11    1
          46507912334891856720
 Comapalu 2.2.................
 Callcusp 43.3................
 Eleopalu 4854.......4........
 Ranuflam 2224....2..2........
 Airaprae ....23..............
 Empenigr .....2..............
 Agrostol 4745...454843.......
 Juncarti .334.......44.......
 Salirepe ...5.3........3.....
 Hyporadi ....25.......2......
 Chenalbu ........1...........
 Alopgeni .4.....857253.....2.
 Sagiproc .....3.42.5222......
 Bracruta .444.3.4.222246262.2
 Cirsarve ..........2.........
 Juncbufo .......43...4....2..
 Scorautu 2.2226.2222325533353
 Elymrepe ......4..44.6..4..4.
 Trifrepe 6.1..2.3221233225256
 Anthodor ....44.........432.4
 Poatriv  .2....2496545..64574
 Poaprat  ....1.4.254444323444
 Lolipere ......7..65427226656
 Rumeacet .......2....2..563..
 Bellpere .........22...22..32
 Vicilath .............21....1
 Planlanc ....2........33555.3
 Achimill ....2.1........22234
 Trifprat ...............252..
 Bromhord ..........3....2.244
  sites species 
     20      30 
> 
> plot(csin, hang=-1) 
> rect.hclust(csin, 3) 
> plot(ccom, hang=-1) 
> rect.hclust(ccom, 3)
> 
> plot(caver, hang=-1) 
> rect.hclust(caver, 3)
> 
> cl <- cutree(ccom, 3) 
> cl
 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
 1  1  2  2  1  1  1  2  2  1  3  2  2  2  2  2  3  3  3  2 
> table(cl)
cl
 1  2  3 
 6 10  4 
> 
> table(cl, cutree(csin, 3)) 
   
cl   1  2  3
  1  6  0  0
  2 10  0  0
  3  2  1  1
> table(cl, cutree(caver, 3))
   
cl  1 2 3
  1 6 0 0
  2 6 4 0
  3 2 0 2
> 
> ord <- cmdscale(d)
> 
> ordiplot(ord)
Warning message:
In ordiplot(ord) : Species scores not available
> 
> ordihull(ord, cl, lty=3) 
> ordispider(ord, cl, col="blue", label=TRUE) 
> ordiellipse(ord, cl, col="red")
> 
> ordiplot(ord, dis="si")
> ordihull(ord, cutree(caver, 3)) 
> ordiplot(ord, dis="si") 
> ordicluster(ord, csin)
> 
> ordiplot(ord, dis="si") 
> ordicluster(ord, caver)
> 
> ordiplot(ord, dis="si") 
> ordicluster(ord, caver, prune=2)
> 
> den <- as.dendrogram(caver)
> 
> x <- scores(ord, display = "sites", choices = 1) 
> oden <- reorder(den, x)
> 
> par(mfrow=c(2,1)) 
> plot(den) 
> plot(oden) 
> par(mfrow=c(1,1))
> 
> vegemite(dune, oden)
                              
             1 11    11 111112
          76502183498321794650
 Trifprat 252.................
 Rumeacet 365......2..2.......
 Planlanc 5553.33.......2.....
 Bromhord 2.244...3...........
 Achimill 22243........12.....
 Vicilath ...1.21.............
 Bellpere ..223.222...........
 Lolipere 66265726524..7......
 Poaprat  432444354442.41.....
 Anthodor 2344..........44....
 Poatriv  54647..6554942...2..
 Elymrepe ..4.4..446...4......
 Trifrepe 2526532213223..26.1.
 Cirsarve ........2...........
 Scorautu 3333555222322.262.22
 Juncbufo 2........4.34.......
 Bracruta 2622.462222.4..3.444
 Sagiproc .....2..52224..3....
 Alopgeni ....2..723558....4..
 Chenalbu ...........1........
 Hyporadi .....2........25....
 Agrostol .......483454...4745
 Juncarti .........44......334
 Salirepe ......3........3...5
 Airaprae ..............23....
 Empenigr ...............2....
 Ranuflam ..........22....2224
 Eleopalu ..........4.....4854
 Comapalu ................2.2.
 Callcusp ................43.3
  sites species 
     20      30 
> 
> tabasco(dune, caver)
> 
> tabasco(dune, caver, Rowv = FALSE) 
> tabasco(dune, oden, Rowv = FALSE)
> 
> mst <- spantree(d)
> 
> ordiplot(ord, dis="si") 
> lines(mst, ord)
> 
> plot(mst, type="t")
Initial stress        : 0.03111
stress after  10 iters: 0.01302, magic = 0.500
stress after  20 iters: 0.01139, magic = 0.500
stress after  30 iters: 0.01118, magic = 0.500
stress after  40 iters: 0.01114, magic = 0.500
> 
> plot(d, cophenetic(csin), asp=1) 
> abline(0, 1) 
> plot(d, cophenetic(ccom), asp=1) 
> abline(0, 1)
> plot(d, cophenetic(caver), asp=1) 
> abline(0, 1)
> 
> cor(d, cophenetic(csin)) 
[1] 0.6601692
> cor(d, cophenetic(ccom)) 
[1] 0.6707479
> cor(d, cophenetic(caver))
[1] 0.8168825
> 
> cl <- factor(cl)
> 
> Moist <- with(dune.env, as.numeric(as.character(Moisture)))
Error in with(dune.env, as.numeric(as.character(Moisture))) : 
  object 'dune.env' not found
> 
> data(dune.env)
> 
> Moist <- with(dune.env, as.numeric(as.character(Moisture)))
> with(dune.env, as.numeric(Moisture))
 [1] 1 1 2 2 1 1 1 4 3 2 1 3 4 4 4 4 2 1 4 4
> 
> boxplot(Moist ~ cl, notch=TRUE)
Warning message:
In bxp(list(stats = c(1, 1, 1, 1, 1, 4, 4, 5, 5, 5, 1, 1, 1.5, 3.5,  :
  some notches went outside hinges ('box'): maybe set notch=FALSE
> 
> anova(lm(Moist ~ cl))
Analysis of Variance Table

Response: Moist
          Df Sum Sq Mean Sq F value    Pr(>F)    
cl         2 36.617 18.3083  12.359 0.0004854 ***
Residuals 17 25.183  1.4814                      
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> 
> anova(rda(Moist ~ cl))
Permutation test for rda under reduced model
Permutation: free
Number of permutations: 999

Model: rda(formula = Moist ~ cl)
         Df Variance      F Pr(>F)   
Model     2   1.9272 12.359  0.003 **
Residual 17   1.3254                 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> with(dune.env, table(cl, Management))
   Management
cl  BF HF NM SF
  1  2  3  0  1
  2  0  2  3  5
  3  1  0  3  0
> 
> install.packages("labdsv")
Installing package into ‘C:/Users/Strickland/Documents/R/win-library/3.1’
(as ‘lib’ is unspecified)
trying URL 'http://cran.rstudio.com/bin/windows/contrib/3.1/labdsv_1.6-1.zip'
Content type 'application/zip' length 216040 bytes (210 Kb)
opened URL
downloaded 210 Kb

package ‘labdsv’ successfully unpacked and MD5 sums checked

The downloaded binary packages are in
	C:\Users\Strickland\AppData\Local\Temp\RtmpS8APAg\downloaded_packages
> library(labdsv)
Loading required package: mgcv
Loading required package: nlme
This is mgcv 1.8-3. For overview type 'help("mgcv-package")'.
Loading required package: MASS

Attaching package: ‘labdsv’

The following object is masked from ‘package:stats’:

    density

> 
> const(dune, cl)
            1   2    3
Achimill 1.00 0.0 0.25
Agrostol 0.00 1.0 0.00
Airaprae 0.00 0.0 0.50
Alopgeni 0.17 0.7 0.00
Anthodor 0.67 0.0 0.50
Bellpere 0.50 0.2 0.25
Bromhord 0.67 0.1 0.00
Chenalbu 0.00 0.1 0.00
Cirsarve 0.00 0.1 0.00
Comapalu 0.00 0.2 0.00
Eleopalu 0.00 0.5 0.00
Elymrepe 0.50 0.3 0.00
Empenigr 0.00 0.0 0.25
Hyporadi 0.00 0.0 0.75
Juncarti 0.00 0.5 0.00
Juncbufo 0.17 0.3 0.00
Lolipere 1.00 0.4 0.50
Planlanc 0.67 0.0 0.75
Poaprat  1.00 0.5 0.75
Poatriv  1.00 0.7 0.00
Ranuflam 0.00 0.6 0.00
Rumeacet 0.50 0.2 0.00
Sagiproc 0.00 0.5 0.50
Salirepe 0.00 0.1 0.50
Scorautu 0.83 0.9 1.00
Trifprat 0.50 0.0 0.00
Trifrepe 0.83 0.8 0.75
Vicilath 0.17 0.0 0.50
Bracruta 0.67 0.8 0.75
Callcusp 0.00 0.3 0.00
> 
> importance(dune, cl)
            1    2    3
Achimill 2.33 0.00 2.00
Agrostol 0.00 4.80 0.00
Airaprae 0.00 0.00 2.50
Alopgeni 2.00 4.86 0.00
Anthodor 3.25 0.00 4.00
Bellpere 2.33 2.00 2.00
Bromhord 3.00 3.00 0.00
Chenalbu 0.00 1.00 0.00
Cirsarve 0.00 2.00 0.00
Comapalu 0.00 2.00 0.00
Eleopalu 0.00 5.00 0.00
Elymrepe 4.00 4.67 0.00
Empenigr 0.00 0.00 2.00
Hyporadi 0.00 0.00 3.00
Juncarti 0.00 3.60 0.00
Juncbufo 2.00 3.67 0.00
Lolipere 5.33 4.25 4.50
Planlanc 4.50 0.00 2.67
Poaprat  3.50 3.80 2.67
Poatriv  4.67 5.00 0.00
Ranuflam 0.00 2.33 0.00
Rumeacet 4.67 2.00 0.00
Sagiproc 0.00 3.00 2.50
Salirepe 0.00 5.00 3.00
Scorautu 3.40 2.11 4.50
Trifprat 3.00 0.00 0.00
Trifrepe 4.00 2.50 2.33
Vicilath 1.00 0.00 1.50
Bracruta 3.00 3.00 4.33
Callcusp 0.00 3.33 0.00
> 
> mod <- indval(dune, as.numeric(cl))
[1] "error code =  0"
> 
> names(mod)
[1] "relfrq" "relabu" "indval" "maxcls" "indcls" "pval"   "error" 
> 
> mod$maxcls 
Achimill Agrostol Airaprae Alopgeni Anthodor Bellpere Bromhord Chenalbu Cirsarve Comapalu Eleopalu 
       1        2        3        2        1        1        1        2        2        2        2 
Elymrepe Empenigr Hyporadi Juncarti Juncbufo Lolipere Planlanc  Poaprat  Poatriv Ranuflam Rumeacet 
       1        3        3        2        2        1        1        1        1        2        1 
Sagiproc Salirepe Scorautu Trifprat Trifrepe Vicilath Bracruta Callcusp 
       2        3        3        1        1        3        3        2 
> mod$pval
Achimill Agrostol Airaprae Alopgeni Anthodor Bellpere Bromhord Chenalbu Cirsarve Comapalu Eleopalu 
   0.002    0.001    0.035    0.014    0.163    0.351    0.015    1.000    1.000    0.329    0.062 
Elymrepe Empenigr Hyporadi Juncarti Juncbufo Lolipere Planlanc  Poaprat  Poatriv Ranuflam Rumeacet 
   0.232    0.190    0.006    0.056    0.349    0.022    0.175    0.094    0.018    0.010    0.064 
Sagiproc Salirepe Scorautu Trifprat Trifrepe Vicilath Bracruta Callcusp 
   0.424    0.114    0.025    0.023    0.468    0.092    0.741    0.208 
> 
> summary(mod)
         cluster indicator_value probability
Achimill       1          0.8235       0.002
Bromhord       1          0.5797       0.015
Lolipere       1          0.5745       0.022
Poatriv        1          0.5714       0.018
Trifprat       1          0.5000       0.023
Agrostol       2          1.0000       0.001
Alopgeni       2          0.6375       0.014
Ranuflam       2          0.6000       0.010
Hyporadi       3          0.7500       0.006
Airaprae       3          0.5000       0.035
Scorautu       3          0.4874       0.025

Sum of probabilities                 =  6.283 

Sum of Indicator Values              =  13.19 

Sum of Significant Indicator Values  =  7.02 

Number of Significant Indicators     =  11 

Significant Indicator Distribution

1 2 3 
5 3 3 
> 
> summary(mod, type = "long")
            1    2    3
Achimill 0.82  .    .  
Agrostol  .   1.00  .  
Airaprae  .    .   0.50
Alopgeni  .   0.64  .  
Anthodor 0.35  .   0.24
Bellpere 0.28  .   0.06
Bromhord 0.58  .    .  
Chenalbu  .   0.10  .  
Cirsarve  .   0.10  .  
Comapalu  .   0.20  .  
Eleopalu  .   0.50  .  
Elymrepe 0.29 0.12  .  
Empenigr  .    .   0.25
Hyporadi  .    .   0.75
Juncarti  .   0.50  .  
Juncbufo  .   0.23  .  
Lolipere 0.57 0.07 0.12
Planlanc 0.40  .   0.30
Poaprat  0.47 0.13 0.20
Poatriv  0.57 0.30  .  
Ranuflam  .   0.60  .  
Rumeacet 0.43  .    .  
Sagiproc  .   0.27 0.23
Salirepe  .    .   0.38
Scorautu 0.26 0.19 0.49
Trifprat 0.50  .    .  
Trifrepe 0.39 0.23 0.19
Vicilath  .    .   0.41
Bracruta 0.17 0.25 0.32
Callcusp  .   0.30  .  
> 
> ckm <- kmeans(decostand(dune, "hell"), 3) 
> ckm$cluster
 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
 2  2  2  2  2  2  2  1  2  2  2  1  1  1  1  1  3  2  3  1 
> 
> ordiplot(ord, dis="si")
> ordihull(ord, ckm$cluster, col="red")
> 
> install.packages("mclust")
Installing package into ‘C:/Users/Strickland/Documents/R/win-library/3.1’
(as ‘lib’ is unspecified)
trying URL 'http://cran.rstudio.com/bin/windows/contrib/3.1/mclust_4.4.zip'
Content type 'application/zip' length 2550363 bytes (2.4 Mb)
opened URL
downloaded 2.4 Mb

package ‘mclust’ successfully unpacked and MD5 sums checked

The downloaded binary packages are in
	C:\Users\Strickland\AppData\Local\Temp\RtmpS8APAg\downloaded_packages
> library(mclust)           # load mclust library
Package 'mclust' version 4.4
Type 'citation("mclust")' for citing this R package in publications.

Attaching package: ‘mclust’

The following object is masked from ‘package:mgcv’:

    mvn

> x = faithful[,1]          # get the first column of the faithful data set
> y = faithful[,2]          # get the second column of the faithful data set
> plot(x,y)                 # plot the spread points before the clustering
> model <- Mclust(faithful) # estimate the number of cluster (BIC), initialize (HC) and clusterize (EM)
> data = faithful           # get the data set 
> plot(model, faithful)     # plot the clustering results
Error in match.arg(what, c("BIC", "classification", "uncertainty", "density"),  : 
  'arg' must be NULL or a character vector
> 
> model <- Mclust(faithful)
> data = faithful
> plot(model, faithful)
Error in match.arg(what, c("BIC", "classification", "uncertainty", "density"),  : 
  'arg' must be NULL or a character vector
> model <- mclust(faithful)
Error: could not find function "mclust"

Restarting R session...

> install.packages("mclust")
Installing package into ‘C:/Users/Strickland/Documents/R/win-library/3.1’
(as ‘lib’ is unspecified)
trying URL 'http://cran.rstudio.com/bin/windows/contrib/3.1/mclust_4.4.zip'
Content type 'application/zip' length 2550363 bytes (2.4 Mb)
opened URL
downloaded 2.4 Mb

Warning in install.packages :
  downloaded length 2550363 != reported length 2550363
package ‘mclust’ successfully unpacked and MD5 sums checked

The downloaded binary packages are in
	C:\Users\Strickland\AppData\Local\Temp\RtmpSeO14i\downloaded_packages
> x = faithful[,1]          # get the first column of the faithful data set
> y = faithful[,2]          # get the second column of the faithful data set
> plot(x,y)                 # plot the spread points before the clustering
> model <- Mclust(faithful) # estimate the number of cluster (BIC), initialize (HC) and clusterize (EM)
Error: could not find function "Mclust"
> data = faithful           # get the data set 
> plot(model, faithful)     # plot the clustering results
Error in xy.coords(x, y, xlabel, ylabel, log) : 
  'x' and 'y' lengths differ
> 
> model <- mclust(faithful)
Error: could not find function "mclust"
> library(mclust)           # load mclust library
Package 'mclust' version 4.4
Type 'citation("mclust")' for citing this R package in publications.
> x = faithful[,1]          # get the first column of the faithful data set
> y = faithful[,2]          # get the second column of the faithful data set
> plot(x,y)                 # plot the spread points before the clustering
> model <- Mclust(faithful) # estimate the number of cluster (BIC), initialize (HC) and clusterize (EM)
> data = faithful           # get the data set 
> plot(model, faithful)     # plot the clustering results
Error in match.arg(what, c("BIC", "classification", "uncertainty", "density"),  : 
  'arg' must be NULL or a character vector
> 
> mydata = USArrests
> mydata <- na.omit(mydata) # listwise deletion of missing
> mydata.orig = mydata #save orig data copy
> mydata <- scale(mydata) # standardize variables
> d <- dist(mydata, method = "euclidean") # distance matrix
> fit <- hclust(d, method="ward")
The "ward" method has been renamed to "ward.D"; note new "ward.D2"
> plot(fit) # display dendogram
> k1 = 2 # eyeball the no. of clusters
> # cut tree into k1 clusters
> groups <- cutree(fit, k=k1)
> # draw dendogram with red borders around the k1 clusters
> rect.hclust(fit, k=k1, border="red")
> 
> # Determine number of clusters #
> wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
> for (i in 2:15) wss[i] <- sum(kmeans(mydata,centers=i)$withinss)
> plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
> # Look for an "elbow" in the scree plot #
> 
> 
> # Use optimal no. of clusters in k-means #
> k1=2
> # K-Means Cluster Analysis
> fit <- kmeans(mydata, k1) # k1 cluster solution
> # get cluster means
> aggregate(mydata.orig,by=list(fit$cluster),FUN=mean)
  Group.1 Murder  Assault UrbanPop     Rape
1       1  4.870 114.4333 63.63333 15.94333
2       2 12.165 255.2500 68.40000 29.16500
> # append cluster assignment
> mydata1 <- data.frame(mydata.orig, fit$cluster)
> # Cluster Plot against 1st 2 principal components
> # vary parameters for most readable graph
> library(cluster)
> clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE,labels=2, lines=0)
> # Model Based Clustering
> library(mclust)
> fit <- Mclust(mydata)
> fit # view solution summary
'Mclust' model object:
 best model: diagonal, equal shape (VEI) with 3 components
> fit$BIC # lookup all the options attempted
        EII       VII       EEI       VEI       EVI       VVI       EEE       EEV       VEV       VVV
1 -583.0950 -583.0950 -594.8311 -594.8311 -594.8311 -594.8311 -524.0454 -524.0454 -524.0454 -524.0454
2 -538.8430 -540.8779 -527.0645 -528.6274 -535.4765 -537.2752 -526.5856 -525.2101 -527.3191 -537.8664
3 -537.5739 -525.7362 -526.2295 -512.9696 -543.7186 -527.7408 -532.9479 -539.6804 -550.5477 -567.0451
4 -521.7731 -526.6936 -517.8151 -519.4226 -543.1230 -543.7168 -534.2090 -562.0097 -575.3697 -588.1424
5 -533.2088 -538.0439 -531.1501 -534.8807 -561.6269 -564.2794 -548.5223 -576.7656 -594.4986 -628.3149
6 -548.2697 -545.5376 -549.8833 -545.3595 -591.5334 -583.3005 -567.0752 -625.1663 -622.9173        NA
7 -554.7524 -564.2459 -555.9070 -562.4990 -596.0957 -601.8503 -567.1581 -623.9653 -643.6280        NA
8 -564.6494 -571.8128 -568.0429 -580.0928 -612.3113 -617.3733 -576.3987 -642.8520 -656.7630        NA
9 -576.0047 -586.1872 -577.0350 -585.9141 -643.0224 -648.6003 -578.1330 -679.3634 -679.5256        NA
attr(,"G")
[1] 1 2 3 4 5 6 7 8 9
attr(,"modelNames")
 [1] "EII" "VII" "EEI" "VEI" "EVI" "VVI" "EEE" "EEV" "VEV" "VVV"
attr(,"oneD")
[1] FALSE
> classif = fit$classification # classifn vector
> mydata1 = cbind(mydata.orig, classif) # append to dataset
> mydata1[1:10,] #view top 10 rows
            Murder Assault UrbanPop Rape classif
Alabama       13.2     236       58 21.2       1
Alaska        10.0     263       48 44.5       1
Arizona        8.1     294       80 31.0       1
Arkansas       8.8     190       50 19.5       2
California     9.0     276       91 40.6       1
Colorado       7.9     204       78 38.7       1
Connecticut    3.3     110       77 11.1       2
Delaware       5.9     238       72 15.8       2
Florida       15.4     335       80 31.9       1
Georgia       17.4     211       60 25.8       1
> 
> # Use only if you want to save the output
> write.table(mydata1,file.choose())#save output
Error in file.choose() : file choice cancelled
> fit$BIC # lookup all the options attempted
        EII       VII       EEI       VEI       EVI       VVI       EEE       EEV       VEV       VVV
1 -583.0950 -583.0950 -594.8311 -594.8311 -594.8311 -594.8311 -524.0454 -524.0454 -524.0454 -524.0454
2 -538.8430 -540.8779 -527.0645 -528.6274 -535.4765 -537.2752 -526.5856 -525.2101 -527.3191 -537.8664
3 -537.5739 -525.7362 -526.2295 -512.9696 -543.7186 -527.7408 -532.9479 -539.6804 -550.5477 -567.0451
4 -521.7731 -526.6936 -517.8151 -519.4226 -543.1230 -543.7168 -534.2090 -562.0097 -575.3697 -588.1424
5 -533.2088 -538.0439 -531.1501 -534.8807 -561.6269 -564.2794 -548.5223 -576.7656 -594.4986 -628.3149
6 -548.2697 -545.5376 -549.8833 -545.3595 -591.5334 -583.3005 -567.0752 -625.1663 -622.9173        NA
7 -554.7524 -564.2459 -555.9070 -562.4990 -596.0957 -601.8503 -567.1581 -623.9653 -643.6280        NA
8 -564.6494 -571.8128 -568.0429 -580.0928 -612.3113 -617.3733 -576.3987 -642.8520 -656.7630        NA
9 -576.0047 -586.1872 -577.0350 -585.9141 -643.0224 -648.6003 -578.1330 -679.3634 -679.5256        NA
attr(,"G")
[1] 1 2 3 4 5 6 7 8 9
attr(,"modelNames")
 [1] "EII" "VII" "EEI" "VEI" "EVI" "VVI" "EEE" "EEV" "VEV" "VVV"
attr(,"oneD")
[1] FALSE
> classif = fit$classification # classifn vector
> mydata1 = cbind(mydata.orig, classif) # append to dataset
> mydata1[1:10,] #view top 10 rows
            Murder Assault UrbanPop Rape classif
Alabama       13.2     236       58 21.2       1
Alaska        10.0     263       48 44.5       1
Arizona        8.1     294       80 31.0       1
Arkansas       8.8     190       50 19.5       2
California     9.0     276       91 40.6       1
Colorado       7.9     204       78 38.7       1
Connecticut    3.3     110       77 11.1       2
Delaware       5.9     238       72 15.8       2
Florida       15.4     335       80 31.9       1
Georgia       17.4     211       60 25.8       1
> 
> # Use only if you want to save the output
> write.table(mydata1,file.choose())#save output
Error in file.choose() : file choice cancelled
> fit1=cbind(classif)
> rownames(fit1)=rownames(mydata)
> library(cluster)
> clusplot(mydata, fit1, color=TRUE, shade=TRUE,labels=2, lines=0)
> # get cluster means
> cmeans=aggregate(mydata.orig,by=list(classif),FUN=mean); cmeans
  Group.1 Murder Assault UrbanPop   Rape
1       1 12.165  255.25    68.40 29.165
2       2  5.965  136.60    69.95 18.460
3       3  2.680   70.10    51.00 10.910
> # Read offers and transaction data
> offers<-read.csv(file="OfferInformation.csv")
Error in file(file, "rt") : cannot open the connection
In addition: Warning message:
In file(file, "rt") :
  cannot open file 'OfferInformation.csv': No such file or directory
> transactions<-read.csv(file="Transactions.csv")
Error in file(file, "rt") : cannot open the connection
In addition: Warning message:
In file(file, "rt") :
  cannot open file 'Transactions.csv': No such file or directory
> 
> OfferInformation <- read.csv("~/R/OfferInformation.csv")
>   View(OfferInformation)
> Transactions <- read.csv("~/R/Transactions.csv")
>   View(Transactions)
> #Load Library
> library(reshape)
Error in library(reshape) : there is no package called ‘reshape’
> 
> # Melt transactions, cast offer by customers
> pivot<-melt(transactions[1:2])
Error: could not find function "melt"
> pivot<-(cast(pivot,value~Customer.Last.Name,fill=0,fun.aggregate=function(x) length(x)))
Error: could not find function "cast"
> 
> # Bind to offers, we remove the first column of our new pivot because it's redundant. 
> pivot<-cbind(offers,pivot[-1])
Error in cbind(offers, pivot[-1]) : object 'offers' not found
> 
> install.packages("reshape")
Installing package into ‘C:/Users/Strickland/Documents/R/win-library/3.1’
(as ‘lib’ is unspecified)
also installing the dependencies ‘Rcpp’, ‘plyr’

trying URL 'http://cran.rstudio.com/bin/windows/contrib/3.1/Rcpp_0.11.3.zip'
Content type 'application/zip' length 3029287 bytes (2.9 Mb)
opened URL
downloaded 2.9 Mb

trying URL 'http://cran.rstudio.com/bin/windows/contrib/3.1/plyr_1.8.1.zip'
Content type 'application/zip' length 1152015 bytes (1.1 Mb)
opened URL
downloaded 1.1 Mb

trying URL 'http://cran.rstudio.com/bin/windows/contrib/3.1/reshape_0.8.5.zip'
Content type 'application/zip' length 127053 bytes (124 Kb)
opened URL
downloaded 124 Kb

package ‘Rcpp’ successfully unpacked and MD5 sums checked
package ‘plyr’ successfully unpacked and MD5 sums checked
package ‘reshape’ successfully unpacked and MD5 sums checked

The downloaded binary packages are in
	C:\Users\Strickland\AppData\Local\Temp\RtmpSeO14i\downloaded_packages
> Melt transactions, cast offer by customers
Error: unexpected symbol in "Melt transactions"
> pivot<-melt(transactions[1:2])
Error: could not find function "melt"
> pivot<-(cast(pivot,value~Customer.Last.Name,fill=0,fun.aggregate=function(x) length(x)))
Error: could not find function "cast"
> 
> # Bind to offers, we remove the first column of our new pivot because it's redundant. 
> pivot<-cbind(offers,pivot[-1])
Error in cbind(offers, pivot[-1]) : object 'offers' not found
> #Load Library
> library(reshape)
> 
> # Melt transactions, cast offer by customers
> pivot<-melt(transactions[1:2])
Error in melt(transactions[1:2]) : object 'transactions' not found
> pivot<-(cast(pivot,value~Customer.Last.Name,fill=0,fun.aggregate=function(x) length(x)))
Error in eval(substitute(subset), data, parent.frame()) : 
  object 'pivot' not found
> 
> # Bind to offers, we remove the first column of our new pivot because it's redundant. 
> pivot<-cbind(offers,pivot[-1])
Error in cbind(offers, pivot[-1]) : object 'offers' not found
> 
> pivot<-cbind(offerinformation,pivot[-1])
Error in cbind(offerinformation, pivot[-1]) : 
  object 'offerinformation' not found
> pivot<-melt(transactions[1:2])
Error in melt(transactions[1:2]) : object 'transactions' not found
> pivot<-(cast(pivot,value~Customer.Last.Name,fill=0,fun.aggregate=function(x) length(x)))
Error in eval(substitute(subset), data, parent.frame()) : 
  object 'pivot' not found
> 

