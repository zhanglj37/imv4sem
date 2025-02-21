# imv4sem

The InterModel Vigorish (IMV) index was initially developed to assess predictive performance in logistic regression with binary outcomes. We have since expanded this index for use in structural equation modeling (SEM) for model comparison (Zhang et al., 2023).   Our goal is to develop the `imv4sem` package, which will support model selection and predictive performance evaluation specifically for structural equation models. 

# Example Usage

## Download the package from GitHub
```
devtools::install_github('zhanglj37/imv4sem')
```

## Load the package and data
```
library(imv4sem)

data_path <- system.file("extdata", "BPI.dat", package = "imv4sem")
data <- read.table(data_path, header = TRUE)


# Define the variables to be analyzed
vary <- colnames(data)
```


## IMV - Comparison between hypothesis model and baseline model (model based on prevalence)
```
# Define the first model (6 factors)
     model_6f <- '
     AnxDep =~ NA*ad1 + ad2 + ad3 + ad4 + ad5
     Headstr =~ NA*hs1 + hs2 + hs3 + hs4 + hs5
     Antisoc =~ NA*as1 + as2 + as3 + as4 + as5 + as6
     Hyperac =~ NA*hy1 + hy2 + hy3 + hy4 + hy5
     PeerProb =~ NA*pp1 + pp2 + pp3
     Depend =~ NA*de1 + de2 + de3 + de4
     AnxDep ~~ 1*AnxDep
     Headstr ~~ 1*Headstr
     Antisoc ~~ 1*Antisoc
     Hyperac ~~ 1*Hyperac
     PeerProb ~~ 1*PeerProb
     Depend ~~ 1*Depend
     '

imv6f_base <- imvsem(model1 = model_6f, 
  vary = vary, data = data, nfold = 5)
plot4imv(imv6f_base)
```

## IMV - Comparison between two factor structures
```
# Define the second model (5 factors)
     model_5f <- '
     AnxDep =~ NA*ad1 + ad2 + ad3 + ad4 + ad5
     Headstr =~ NA*hs1 + hs2 + hs3 + hs4 + hs5
     Antisoc =~ NA*as1 + as2 + as3 + as4 + as5 + as6
     Hyperac =~ NA*hy1 + hy2 + hy3 + hy4 + hy5
     PeerProb =~ NA*pp1 + pp2 + pp3 + de1 + de2 + de3 + de4
     AnxDep ~~ 1*AnxDep
     Headstr ~~ 1*Headstr
     Antisoc ~~ 1*Antisoc
     Hyperac ~~ 1*Hyperac
     PeerProb ~~ 1*PeerProb
     '


imvsem(model1 = model_5f, model2 = model_6f, 
  vary = vary, data = data, nfold = 5)
```

Reference: 

Zhang, L., Kanopka, K., Rahal, C., Ulitzsch, E., Zhang, Z., & Domingue, B. (2023). <i>The InterModel Vigorish for Model Comparison in Confirmatory Factor Analysis with Binary Outcomes.</i> https://doi.org/10.31234/osf.io/tv9bd

Domingue, B. W. (2023). IMV: An R package for InterModel Vigorish. GitHub repository. Retrieved from https://github.com/ben-domingue/imv
