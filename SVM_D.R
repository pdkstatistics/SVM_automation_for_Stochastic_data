## PACKAGES --------------------------------------------------------------------
easypackages::packages("tibble","psych","dplyr","magrittr", "data.table",
                       "plyr","tidyr","ggplot2","cowplot","rje")
easypackages::packages("forecast","lmtest","MASS","tseries",
                       "gridExtra","e1071","DescTools")
options(max.print=999999)
#detach("package:rcompanion")





## EXPORT RESULT ---------------------------------------------------------------

#sink("D:/RESULTS/R/SVM/SVM_Data.txt")  # START
#sink()  # END





## DATA IMPORT -----------------------------------------------------------------

DT=xlsx::read.xlsx(file.choose(),sheetIndex = 1)
#E:\STATISTICS\PUBLICATIONS\PP ANN Sugarcane TN +2\Analysis
TS=ts(DT[,2],start=1961)#
TS





## LAG SELECTION ---------------------------------------------------------------

#1. PACF
Pacf(TS,
     main="",lag.max = 50,#
     lwd=1,col=2)
L=3





## SCALING AND SPLITTING -------------------------------------------------------

# 1. LAG DATA
LAG=as.vector(TS)
for (i in 1:L) {
    LAG=cbind(LAG,quantmod::Lag(as.vector(TS),k=i))
    if(i==L){
        LAG=LAG[-(1:L),] 
    }
}
LAG


# 2. SCALING
#T.LAG=apply(LAG,2,mmx_scale)
#summary(T.LAG)
#(Scaled internally)

# 3. SPLITTING
T=1:round(0.90*nrow(LAG)) #
Tr=LAG[T,]  ;dim(Tr)
Tt=LAG[-T,] ;dim(Tt)





# SVR MODELLING-----------------------------------------------------------------

# 1. Inputs
Tr %>% class()

# Type of regression (i1: 3,4)
TYPE    =c("eps-regression","nu-regression")

# COST(C): constant of the regularization
C       =c(1,2,4,8,16,25)
EPS     =seq(0,1,0.25)
NU      =0.1 #


# KERNELS (i2: 0,1,2,3)
KERNEL  =c("linear","polynomial","radial","sigmoid")        

# DEGREE:{kernel:polynomial (default: 3)}
DEG=seq(1,4,1)

# GAMMA :all kernels except linear (default: 1/(data dimension))
GAMMA=seq(0.1,1,0.2)


# coef0	:{kernels: polynomial and sigmoid (default: 0)}
COEF=c(0.1,0.5,1,2)

# 2. Tuning the models
L.SVM=list()

set.seed(11)
for (i1 in 1:length(TYPE)) {
    for (i2 in 1:length(KERNEL)) {
        
        
        # Fitting the models
        L.SVM=c(L.SVM,list(tune.svm(LAG~.,
                                    data    =as.data.frame(Tr),
                                    scale   =TRUE,
                                    type    =TYPE[i1],
                                    nu      =NU,
                                    epsilon =EPS,
                                    cost    =C,
                                    kernel  =KERNEL[i2],
                                    gamma   =GAMMA,
                                    coef0   =COEF, 
                                    degree  =DEG,
                                    fitted  =TRUE)))
    }
}
L.SVM %>% str()

?svm
L.SVM[[1]]$performances
tune(L.SVM[[1]]$best.model)
## MODEL DAIGNOSTIC MEASURES----------------------------------------------------
D.Tr=c()
D.Tt=c()
PARAMETERS=c()

for (i in 1:8) {
    
    # Parameters used
    PARAMETERS=rbind.fill(PARAMETERS,
                          data.frame(Type  =L.SVM[[i]]$best.model$type,
                                     Kernel=L.SVM[[i]]$best.model$kernel,
                                     Nu    =L.SVM[[i]]$best.model$nu,
                                     Eps   =L.SVM[[i]]$best.model$epsilon,
                                     Cost  =L.SVM[[i]]$best.model$cost,
                                     Degree=L.SVM[[i]]$best.model$degree,
                                     Gamma =L.SVM[[i]]$best.model$gamma,
                                     Coef0 =L.SVM[[i]]$best.model$coef0))
    
    
    ## TRAIN
    # Table Obs and Fitted
    X.Tr=tibble(Y= Tr[,1],
             Ey=L.SVM[[i]]$best.model$fitted,
             diff=Y-Ey)
    # Model Diagnostics
    D.Tr=rbind(D.Tr,
               as.data.frame(with(X.Tr,
                                  list(MAE=MAE(Ey,Y),
                                       MAPE=MAPE(Ey,Y),
                                       SMAPE=SMAPE(Ey,Y),
                                       MSE=MSE(Ey,Y),
                                       RMSE=RMSE(Ey,Y),
                                       TheilU=TheilU(Y,Ey),
                                       ACCURACY=acc(Ey,Y)))))
    
    
    
    ## TEST 
    # Table Obs and Fitted
    X.Tt=tibble(Y= Tt[,1],
                Ey=predict(L.SVM[[i]]$best.model,Tt[,-1]),
                diff=Y-Ey)
    # Model Diagnostics
    D.Tt=rbind(D.Tt,
               as.data.frame(with(X.Tt,
                                  list(MAE=MAE(Ey,Y),
                                       MAPE=MAPE(Ey,Y),
                                       SMAPE=SMAPE(Ey,Y),
                                       MSE=MSE(Ey,Y),
                                       RMSE=RMSE(Ey,Y),
                                       TheilU=TheilU(Y,Ey),
                                       ACCURACY=acc(Ey,Y)))))
    
    
}
PARAMETERS
D.Tr
D.Tt

## MODEL SELECTION--------------------------------------------------------------

# 1. TRAIN SET
{R.D.Tr=c()
for (i in 1:dim(D.Tr)[2]) {
    
    if(colnames(D.Tr)[i]!="ACCURACY"){
        R.D.Tr=cbind(R.D.Tr,rank(D.Tr[[i]]))
        colnames(R.D.Tr)[i]=colnames(D.Tr)[i]
    }else{
        R.D.Tr=cbind(R.D.Tr,rank(-D.Tr[[i]]))
        colnames(R.D.Tr)[i]=colnames(D.Tr)[i]
    }
}
R.D.Tr =as.data.frame(R.D.Tr)
}

R.D.Tr
R.D.Tr=cbind(R.D.Tr,Avg=rowMeans(R.D.Tr))
R.D.Tr[order(R.D.Tr[,"Avg"]),]



# 2. TEST SET
{R.D.Tt=c()
    for (i in 1:dim(D.Tt)[2]) {
        
        if(colnames(D.Tt)[i]!="ACCURACY"){
            R.D.Tt=cbind(R.D.Tt,rank(D.Tt[[i]]))
            colnames(R.D.Tt)[i]=colnames(D.Tt)[i]
        }else{
            R.D.Tt=cbind(R.D.Tt,rank(-D.Tt[[i]]))
            colnames(R.D.Tt)[i]=colnames(D.Tt)[i]
        }
    }
    R.D.Tt =as.data.frame(R.D.Tt)
}

R.D.Tt
R.D.Tt=cbind(R.D.Tt,Avg=rowMeans(R.D.Tt))
R.D.Tt[order(R.D.Tt[,"Avg"]),]

# 3. SELECTION

RANK=(R.D.Tr[,"Avg"]+R.D.Tt[,"Avg"])/2
RANK=as.data.frame(RANK)


## BEST FIT---------------------------------------------------------------------
L.SVM[[2]]
PARAMETERS[2,]
TYPE
KERNEL
LAG

for (i in 2) {
    BF.SVM=tune.svm(LAG~.,
                    data    =as.data.frame(LAG),
                    scale   =TRUE,
                    type    =TYPE[1],
                    nu      =0.1,
                    epsilon =PARAMETERS[i,][,4],
                    cost    =PARAMETERS[i,][,5],
                    kernel  =KERNEL[2],
                    degree  =PARAMETERS[i,][,6],
                    gamma   =PARAMETERS[i,][,7],
                    coef0   =PARAMETERS[i,][,8],
                    fitted  =TRUE)
}

# Table Obs and Fitted
X.BF.SVM=tibble(Y= LAG[,1],
            Ey=BF.SVM$best.model$fitted,
            Residuals=BF.SVM$best.model$residuals) %>% print(n=Inf)

# Model Diagnostics
D.BF.SVM=as.data.frame(with(X.BF.SVM,
                   list(MAE=MAE(Ey,Y),
                        MAPE=MAPE(Ey,Y),
                        SMAPE=SMAPE(Ey,Y),
                        MSE=MSE(Ey,Y),
                        RMSE=RMSE(Ey,Y),
                        TheilU=TheilU(Y,Ey),
                        ACCURACY=acc(Ey,Y))))



## RESIDUAL DIAGNOSTICS --------------------------------------------------------


RESID=X.BF.SVM$Residuals

# 1. Normality tests
# 1.1 Box-Pierce test
Box.test(RESID, lag=10, fitdf=0)             ####
# 1.2 Box-Ljung test
Box.test(RESID,lag=10, fitdf=0, type="Lj")   ####
# 1.3 Shapiro Wilks test
shapiro.test(RESID)                          ####


# 2. Residual plots
# 2.1 Final table
X.BF.SVM$Date=time(TS)[-(1:L)]

# 2.2 Original Series
B=ggplot(X.BF.SVM,aes(Date,Y))+
    #scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0)) +
    geom_line(colour="Blue")+
    labs(title = "",
         x = "Time",
         y = "Production(MT)") +
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))


# 2.2 plot residuals
C=ggplot(X.BF.SVM,aes(Date,Residuals))+
    scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0)) +
    geom_line(colour="Blue")+
    geom_hline(yintercept=0, color = "red")+
    labs(title = "Residual Plot",
         x = "Time",
         y = "Residuals") +
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))


# 2.3 Histogram of Residuals
D=ggplot(X.BF.SVM,aes(Residuals))+
    scale_x_continuous(expand = c(0,0)) +
    geom_histogram(fill="#66FF99",bins=7)+
    geom_vline(xintercept=0, color = "red")+
    labs(title = "Histogram of Residuals",
         x = "Time",
         y = "Residuals") +
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))


# 2.4 ACF of residuals
E=ggAcf(X.BF.SVM$Residuals, lag.max =50)+
    labs(title = "ACF of residuals")+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))



## FORECAST---------------------------------------------------------------------
BF.SVM$best.model
F=7 # F : forecast length
L    # L: Lag variables 
S=LAG[,1]


{FOR.X=matrix(NA,1,L)
    for(i in 1:F){
        for (j in 1:L){
            if (i==1){
                FOR.X[i,j]=S[length(S)+1-j]
            } 
            else{
                if(j==1){
                    FOR.X=rbind(FOR.X,matrix(NA,1,L))
                    M=t(as.matrix(FOR.X[i-1,]))
                    FOR.X[i,j]=predict(BF.SVM$best.model,M) 
                }
                else{
                    FOR.X[i,j]=FOR.X[i-1,j-1]
                }
            }
        }
    }
    FOR.X  # input for forecast
    FOR.Y=cbind(predict(BF.SVM$best.model,FOR.X)) # forecast
}
FOR.X;FOR.Y
## FINALIMAGES------------------------------------------------------------------

ggdraw() +
    draw_plot(B, x = 0, y = .5, width = 1, height = .50) +
    draw_plot(A, x = 0, y = 0, width = 1, height = .50) +
    draw_plot_label(label = c("A","B"), size = 10,
                    x = c(0, 0), y = c(1, 0.5))


ggdraw() +
    draw_plot(C, x = 0, y = .5, width = .6, height = .5) +
    draw_plot(E, x = 0, y = 0, width = .6, height = .5) +
    draw_plot(D, x =0.6, y = 0.1, width = 0.4, height = 0.8) +
    draw_plot_label(label = c("A", "C", "B"), size = 10,
                    x = c(0, 0,0.6), y = c(1, 0.5, 0.9))
################################################################################

