---
title: "R Notebook"
output:
  pdf_document: default
  html_notebook: default
---


```r
### Code for "Democratization and Economic Output in Sub-Saharan Africa"
### Daniel De Kadt and Stephen B. Wittels

## A note to users:
## Use setwd() to set the working directory to the location where data files are saved. 
## Figures are programmed to be saved automatically to the working directory.
## They are named according to their figure number in the paper.
## The three tables in the paper are saved as the objects "mali.weights," "panel.estimates," and
## "moderators." Code to print these objects to the console is included at the end.


## Options and Libraries
options(scipen = 6, digits = 3)

# Install necessary libraries
if (!require("pacman")) install.packages("pacman")
```

```
## Loading required package: pacman
```

```r
pacman::p_load(foreign, 
  Synth, 
  xtable, 
  rgenoud, 
  reshape2, 
  quadprog, 
  ucminf, 
  Rcgmin, 
  Rvmmin, 
  minqa, 
  Rcpp, 
  ggplot2, 
  plyr, 
  grid, 
  lme4,
  janitor,
  dplyr,
  CausalImpact  # For use in the extension
)
```



```r
## Data
load("afripanel_wdk_final.RData")
a <- read.csv("conditioning_variables1.csv")
panel.reg <- read.dta("panel.reg1.dta")

not_any_na <- function(x) all(!is.na(x))
```

## Replication


```r
# Replication function
replicate <- function(
  unitID,
  fullname,
  begin,
  end,
  tr2,
  final,
  low,
  high
){
  
  data <- afripanel[afripanel$WBCode==unitID | afripanel$cont_dem_ind==1,]
  
  controls <- unique(data$WBCode[data$WBCode!=unitID&data$WBCode!="ETH"&data$WBCode!="SDN"])
  
  prep <- dataprep(
    foo=data, 
    predictors=c(
      "lngdpmadlag",
      "lngdpmadlag2",
      "lngdpmadlag3",
      "lngdpmadlag4",
      "lnpop",
      "ki",
      "openk",
      "civwar",
      "civwarend",
      "pwt_xrate",
      "pwt_xrate_lag1",
      "pwt_xrate_lag2",
      "pwt_xrate_lag3",
      "eximdiff",
      "eximdiff_lag1",
      "eximdiff_lag2"
    ),
    dependent="lngdpmad",
    unit.variable="wbcode2",
    time.variable="year", 
    treatment.identifier=unitID, 
    controls.identifier=controls,          
    time.predictors.prior=c(begin:end),
    time.optimize.ssr=c(begin:tr2),
    time.plot=c(begin:final),
    unit.names.variable="WBCode"
  )
  
  out <- synth(prep)
  
  path.plot(synth.res=out, dataprep.res=prep,
            Ylab="Log GDP per capita", Legend=c(fullname, "Synthetic Counterfactual"), tr.intake=tr2,
            Ylim=c(low,high) , Main=fullname
  )
  
}
```




```r
## Figure 2 Replication
replicate("MLI", "Mali", 1980, 1990, 1991, 2008, 6, 8)  
```

```
## 
##  Missing data- treated unit; predictor: eximdiff_lag1 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data- treated unit; predictor: eximdiff_lag2 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data- treated unit; predictor: eximdiff_lag2 ; for period: 1981 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 2 ; predictor: eximdiff_lag1 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 11 ; predictor: eximdiff_lag1 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 2 ; predictor: eximdiff_lag2 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 2 ; predictor: eximdiff_lag2 ; for period: 1981 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 11 ; predictor: eximdiff_lag2 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 11 ; predictor: eximdiff_lag2 ; for period: 1981 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
## X1, X0, Z1, Z0 all come directly from dataprep object.
## 
## 
## **************** 
##  searching for synthetic control unit  
##  
## 
## **************** 
## **************** 
## **************** 
## 
## MSPE (LOSS V): 0.000999 
## 
## solution.v:
##  0.0833 0.15 0.156 0.211 0.158 0.0000345 0.156 0.0185 0.0112 0.00237 0.00519 0.0101 0.0148 0.00922 0.0000911 0.0139 
## 
## solution.w:
##  0.00000161 0.241 0.101 0.00000575 0.0000143 0.0000165 0.000000394 0.000007 0.00000562 0.00000578 0.0000000098 0.163 0.0000039 0.00000306 0.227 0.268 0.000000904
```

![](ProjectNotebook_files/figure-latex/unnamed-chunk-4-1.pdf)<!-- --> 

```r
## Figure 2 Replication in-time placebo
replicate("MLI", "Mali", 1980, 1985, 1986, 2008, 6, 8)
```

```
## 
##  Missing data- treated unit; predictor: eximdiff_lag1 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data- treated unit; predictor: eximdiff_lag2 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data- treated unit; predictor: eximdiff_lag2 ; for period: 1981 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 2 ; predictor: eximdiff_lag1 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 11 ; predictor: eximdiff_lag1 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 14 ; predictor: eximdiff_lag1 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 2 ; predictor: eximdiff_lag2 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 2 ; predictor: eximdiff_lag2 ; for period: 1981 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 11 ; predictor: eximdiff_lag2 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 11 ; predictor: eximdiff_lag2 ; for period: 1981 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 14 ; predictor: eximdiff_lag2 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 14 ; predictor: eximdiff_lag2 ; for period: 1981 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
## X1, X0, Z1, Z0 all come directly from dataprep object.
## 
## 
## **************** 
##  searching for synthetic control unit  
##  
## 
## **************** 
## **************** 
## **************** 
## 
## MSPE (LOSS V): 0.00088 
## 
## solution.v:
##  0.138 0.177 0.158 0.132 0.0885 0.00037 0.153 0.00231 0.00662 0.0181 0.0163 0.0152 0.0195 0.00283 0.0087 0.0635 
## 
## solution.w:
##  0.00000594 0.221 0.00205 0.0000551 0.0000317 0.0000295 0.000000555 0.000000407 0.00000973 0.0000184 0.00423 0.173 0.000168 0.00000673 0.262 0.338 0.0000133
```

![](ProjectNotebook_files/figure-latex/unnamed-chunk-5-1.pdf)<!-- --> 

```r
## Figure 2 Replication in-space placebo
replicate("BFA", "Burkina Faso", 1980, 1990, 1991, 2008, 6, 8)  
```

```
## 
##  Missing data- treated unit; predictor: eximdiff_lag1 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data- treated unit; predictor: eximdiff_lag2 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data- treated unit; predictor: eximdiff_lag2 ; for period: 1981 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 2 ; predictor: eximdiff_lag1 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 11 ; predictor: eximdiff_lag1 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 2 ; predictor: eximdiff_lag2 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 2 ; predictor: eximdiff_lag2 ; for period: 1981 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 11 ; predictor: eximdiff_lag2 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 11 ; predictor: eximdiff_lag2 ; for period: 1981 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
## X1, X0, Z1, Z0 all come directly from dataprep object.
## 
## 
## **************** 
##  searching for synthetic control unit  
##  
## 
## **************** 
## **************** 
## **************** 
## 
## MSPE (LOSS V): 0.00204 
## 
## solution.v:
##  0.121 0.089 0.0518 0.0149 0.00506 0.389 0.015 0.00874 0.297 0.000311 0.00427 0.000802 0.00313 0.0000237 0.0000827 0.0000795 
## 
## solution.w:
##  0.00000702 0.179 0.0000307 0.0000148 0.0000224 0.0000146 0.000971 0.0000406 0.0000299 0.00674 0.000152 0.0000238 0.307 0.478 0.028 0.00000734
```

![](ProjectNotebook_files/figure-latex/unnamed-chunk-6-1.pdf)<!-- --> 

```r
## Figure 2 Replication in-space placebo
replicate("TCD", "Chad", 1980, 1990, 1991, 2008, 6, 8)  
```

```
## 
##  Missing data- treated unit; predictor: eximdiff_lag1 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data- treated unit; predictor: eximdiff_lag2 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data- treated unit; predictor: eximdiff_lag2 ; for period: 1981 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 2 ; predictor: eximdiff_lag1 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 11 ; predictor: eximdiff_lag1 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 2 ; predictor: eximdiff_lag2 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 2 ; predictor: eximdiff_lag2 ; for period: 1981 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 11 ; predictor: eximdiff_lag2 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 11 ; predictor: eximdiff_lag2 ; for period: 1981 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
## X1, X0, Z1, Z0 all come directly from dataprep object.
## 
## 
## **************** 
##  searching for synthetic control unit  
##  
## 
## **************** 
## **************** 
## **************** 
## 
## MSPE (LOSS V): 0.137 
## 
## solution.v:
##  0.195 0.196 0.185 0.163 0.00631 0.152 0.0817 0.000166 0.00515 0.00198 0.00066 0.00208 0.00693 0.00112 0.000519 0.00337 
## 
## solution.w:
##  0.0000000374 0.000000465 0.000000212 0.000000294 0.0000000066 0.0000000067 2e-10 0.0000000194 0.0000000366 0.0000000293 0.000000642 0.000000011 0.0000000628 0.0000000049 0.0000000974 1
```

![](ProjectNotebook_files/figure-latex/unnamed-chunk-7-1.pdf)<!-- --> 

```r
## Figure 2 Replication in-space placebo
replicate("NGA", "Nigeria", 1980, 1990, 1991, 2008, 6, 8)  
```

```
## 
##  Missing data- treated unit; predictor: eximdiff_lag1 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data- treated unit; predictor: eximdiff_lag2 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data- treated unit; predictor: eximdiff_lag2 ; for period: 1981 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 2 ; predictor: eximdiff_lag1 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 11 ; predictor: eximdiff_lag1 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 2 ; predictor: eximdiff_lag2 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 2 ; predictor: eximdiff_lag2 ; for period: 1981 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 11 ; predictor: eximdiff_lag2 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 11 ; predictor: eximdiff_lag2 ; for period: 1981 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
## X1, X0, Z1, Z0 all come directly from dataprep object.
## 
## 
## **************** 
##  searching for synthetic control unit  
##  
## 
## **************** 
## **************** 
## **************** 
## 
## MSPE (LOSS V): 0.00607 
## 
## solution.v:
##  0.000759 0.00277 0.00137 0.000229 0.000000116 0.000000127 0.000000245 0.0000000019 0.000000124 0.0134 0.098 0.66 0.223 0.0000167 0.00012 0.0000575 
## 
## solution.w:
##  0.137 0.0182 0.0172 0.0183 0.019 0.0138 0.137 0.467 0.024 0.0192 0.018 0.0182 0.00818 0.0176 0.0183 0.0483
```

![](ProjectNotebook_files/figure-latex/unnamed-chunk-8-1.pdf)<!-- --> 

```r
## Figure 2 Replication in-space placebo
replicate("BDI", "Burundi", 1980, 1990, 1991, 2008, 6, 8)  
```

```
## 
##  Missing data- treated unit; predictor: eximdiff_lag1 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data- treated unit; predictor: eximdiff_lag2 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data- treated unit; predictor: eximdiff_lag2 ; for period: 1981 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 2 ; predictor: eximdiff_lag1 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 14 ; predictor: eximdiff_lag1 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 2 ; predictor: eximdiff_lag2 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 2 ; predictor: eximdiff_lag2 ; for period: 1981 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 14 ; predictor: eximdiff_lag2 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 14 ; predictor: eximdiff_lag2 ; for period: 1981 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
## X1, X0, Z1, Z0 all come directly from dataprep object.
## 
## 
## **************** 
##  searching for synthetic control unit  
##  
## 
## **************** 
## **************** 
## **************** 
## 
## MSPE (LOSS V): 0.00101 
## 
## solution.v:
##  0.0105 0.00107 0.000214 0.000229 0.0176 0.0309 0.0000000896 0.000000582 0.000000266 0.0141 0.0919 0.618 0.213 0.00267 0.000375 0.0000147 
## 
## solution.w:
##  0.00137 0.157 0.000636 0.00174 0.108 0.00158 0.115 0.00143 0.00186 0.026 0.0706 0.00207 0.00212 0.507 0.00296 0.00123
```

![](ProjectNotebook_files/figure-latex/unnamed-chunk-9-1.pdf)<!-- --> 

```r
## Figure 2 Replication in-space placebo
replicate("TGO", "Togo", 1980, 1990, 1991, 2008, 6, 8)  
```

```
## 
##  Missing data- treated unit; predictor: eximdiff_lag1 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data- treated unit; predictor: eximdiff_lag2 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data- treated unit; predictor: eximdiff_lag2 ; for period: 1981 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 2 ; predictor: eximdiff_lag1 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 11 ; predictor: eximdiff_lag1 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 2 ; predictor: eximdiff_lag2 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 2 ; predictor: eximdiff_lag2 ; for period: 1981 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 11 ; predictor: eximdiff_lag2 ; for period: 1980 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
##  Missing data - control unit: 11 ; predictor: eximdiff_lag2 ; for period: 1981 
##  We ignore (na.rm = TRUE) all missing values for predictors.op.
## 
## X1, X0, Z1, Z0 all come directly from dataprep object.
## 
## 
## **************** 
##  searching for synthetic control unit  
##  
## 
## **************** 
## **************** 
## **************** 
## 
## MSPE (LOSS V): 0.00313 
## 
## solution.v:
##  0.0239 0.00267 0.00904 0.00021 0.000000499 0.00000214 0.000000173 0.031 0.000000331 0.0126 0.0919 0.619 0.209 0.00000176 0.0000635 0.00000155 
## 
## solution.w:
##  0.000138 0.000551 0.0000373 0.0000122 0.245 0.00972 0.0536 0.000294 0.000279 0.000449 0.578 0.00028 0.000507 0.000267 0.11 0.000304
```

![](ProjectNotebook_files/figure-latex/unnamed-chunk-10-1.pdf)<!-- --> 

## Extensions

### Google Extension (CausalImpact)

```r
# Replication function from Google Extension
show_impact_n <- function(
  Country,
  begin,
  end,
  treatYear
){
  data <- afripanel[which(afripanel$Country == Country), ]
  predictors=c(
      "lngdpmadlag",
      "lngdpmadlag2",
      "lngdpmadlag3",
      "lngdpmadlag4",
      "lnpop",
      "ki",
      "openk",
      "civwar",
      "civwarend",
      "pwt_xrate",
      "pwt_xrate_lag1",
      "pwt_xrate_lag2",
      "pwt_xrate_lag3",
      "eximdiff",
      "eximdiff_lag1",
      "eximdiff_lag2",
      "wbank",
      "wbank_lag1",
      "wbank_lag2"
  )

  outcome <- 'lngdpmad'
  time.points <- as.Date(as.character(data$year), "%Y")
  
  data <- data[, c(outcome, predictors)]
  data<-data[!is.na(data[outcome]),]
  data <- data %>% select_if(not_any_na)
  
  
  data <- zoo(data, time.points)
  data <- data[index(data) > as.Date(begin, '%Y') & index(data) < as.Date(end, '%Y')]
  
  nextYear <- as.Date(as.character(as.numeric(treatYear) + 1), "%Y")
  treatYear <- as.Date(treatYear, "%Y")
  
  start_date <- start(data)
  end_date <- end(data)
  
  pre.period <- as.Date(c(start_date, treatYear))
  post.period <- as.Date(c(nextYear, end_date))
  
  impact <- CausalImpact(data, 
                          pre.period, 
                          post.period,
                          model.args = list(
                            niter = 1000, 
                            nseasons = 52)
                          )
  return(impact)
}
```
# Graphs:

```r
impact <- show_impact_n('Mali', '1980', '2005', '1990')
plot(impact) + ggtitle ("Causal Impact on Mali beyond 1991")
```

```
## Warning: Removed 24 rows containing missing values (geom_path).
```

```
## Warning: Removed 48 rows containing missing values (geom_path).
```

![](ProjectNotebook_files/figure-latex/unnamed-chunk-12-1.pdf)<!-- --> 

```r
impact <- show_impact_n('Mali', '1980', '2005', '1985')
plot(impact) + ggtitle("Causal Impact on Mali beyond 1985")
```

```
## Warning: Removed 24 rows containing missing values (geom_path).
```

```
## Warning: Removed 48 rows containing missing values (geom_path).
```

![](ProjectNotebook_files/figure-latex/unnamed-chunk-13-1.pdf)<!-- --> 

```r
impact <- show_impact_n('Burkina Faso', '1980', '2005', '1990')
plot(impact) + ggtitle("Causal Impact on Burkina Faso beyond 1990")
```

```
## Warning: Removed 24 rows containing missing values (geom_path).
```

```
## Warning: Removed 48 rows containing missing values (geom_path).
```

![](ProjectNotebook_files/figure-latex/unnamed-chunk-14-1.pdf)<!-- --> 

```r
impact <- show_impact_n('Mali', '1980', '2005', '1985')
plot(impact) + ggtitle("In-time placebo (Mali)")
```

```
## Warning: Removed 24 rows containing missing values (geom_path).
```

```
## Warning: Removed 48 rows containing missing values (geom_path).
```

![](ProjectNotebook_files/figure-latex/unnamed-chunk-15-1.pdf)<!-- --> 

```r
impact <- show_impact_n('Burkina Faso', '1980', '2005', '1990')
plot(impact) + ggtitle("In-space placebo (Burkina Faso)")
```

```
## Warning: Removed 24 rows containing missing values (geom_path).
```

```
## Warning: Removed 48 rows containing missing values (geom_path).
```

![](ProjectNotebook_files/figure-latex/unnamed-chunk-16-1.pdf)<!-- --> 
