Evo_FeatSel<-function(Data,Target,Norm = F,Keep = NULL,Ban = NULL,Treat_Out = F,Data_Split = "Random",
                      Train_Size = 0.8,Model = "RF",Rsq_Threshold = 0.7,Eval_Metric,Maximize = F,Par = T, ...){
  
  init_time<-Sys.time()
  
  suppressMessages({
    suppressWarnings({
      suppressPackageStartupMessages({
        req_pkg<-c("data.table","Metrics","randomForest","GA","parallel","doParallel")
        ins_pkg<-req_pkg[!(req_pkg %in% installed.packages()[,"Package"])]
        if(length(ins_pkg)) install.packages(ins_pkg)
        
        require(data.table)
        require(Metrics)
        require(randomForest)
        require(GA)
      })
    })
  })
  
  dt_raw<-as.data.table(na.omit(Data))
  dt_out<-dt_raw[,which(colnames(dt_raw) %in% Ban == F),with = F]
  
  dt_out<-dt_out[,lapply(.SD,function(x){
    if(is.character(x)){
      x<-as.factor(x)
    }
    return(x)
  }),.SDcols = colnames(dt_out)]
  
  if(Treat_Out == T){
    dt_sub<-dt_out[,lapply(X = .SD,FUN = function(y){
      if((class(y) %in% c("integer","numeric")) & (length(unique(y)) > 2)){
        y_dec<-quantile(x = y,probs = seq(from = 0,to = 1,by = 0.1),type = 5,na.rm = T)
        y_qnt<-quantile(x = y,na.rm = T)
        y[which(y > (y_qnt[4] + (1.5*IQR(x = y,na.rm = T))))]<-y_dec[10]
        y[which(y < (y_qnt[2] - (1.5*IQR(x = y,na.rm = T))))]<-y_dec[2]
      }
      return(y)
    }),.SDcols = colnames(dt_out)[-which(colnames(dt_out) == Target)]]
    dt_out<-setDT(cbind.data.frame(dt_sub,dt_out[,Target,with = F]))
  }
  
  if(Norm == T){
    dt_sub<-dt_out[,lapply(X = .SD,FUN = function(x){
      if(class(x) %in% c("integer","numeric")){
        min_x<-min(x,na.rm = T)
        max_x<-max(x,na.rm = T)
        x<-(x - min_x)/(max_x - min_x)
      }
      return(x)
    }),.SDcols = colnames(dt_out)[-which(colnames(dt_out) == Target)]]
    dt_out<-setDT(cbind.data.frame(dt_sub,dt_out[,Target,with = F]))
  }
  
  if(Data_Split == "Random"){
    set.seed(666)
    trn_id<-sample(seq_len(nrow(dt_out)),size = floor(Train_Size * nrow(dt_out)))
    dt_trn<-dt_out[trn_id,]
    dt_tst<-dt_out[-trn_id,]
  }
  
  else if(Data_Split == "Ordered"){
    dt_trn<-dt_out[1:Train_Size,]
    dt_tst<-dt_out[(Train_Size + 1):nrow(dt_out)]
  }
  
  fit_func<-function(x){
    res_eval<-tryCatch({
      sel_var<-unique(c(Keep,colnames(dt_out)[colnames(dt_out) %in% c(Keep,Target) == F][x]))
      
      if(Model == "LR"){
        lm_form<-as.formula(paste(Target,paste(sel_var,collapse = " + "),sep = " ~ "))
        lm_mod<-lm(formula = lm_form,data = dt_trn)
        lm_sum<-summary(lm_mod)
        adj_rsq<-lm_sum$adj.r.squared
        
        if(adj_rsq < Rsq_Threshold){
          res_eval<-ifelse(Maximize == F,Inf,-Inf)
        }
        
        else {
          res_eval<-do.call(Eval_Metric,args = list("actual" = dt_tst[[Target]],
                                                    "predicted" = predict(object = lm_mod,newdata = dt_tst)))
        }
      }
      
      else if(Model == "RF" & class(dt_out[[Target]]) %in% c("numeric","integer")){
        rf_mod<-randomForest(x = dt_trn[,sel_var,with = F],y = dt_trn[[Target]],na.action = na.omit,
                             xtest = dt_tst[,sel_var,with = F],ytest = dt_tst[[Target]],importance = T)
        psd_rsq<-rf_mod$rsq
        
        if(psd_rsq < Rsq_Threshold){
          res_eval<-ifelse(Maximize == F,Inf,-Inf)
        }
        
        else {
          res_eval<-do.call(Eval_Metric,args = list("actual" = dt_tst[[Target]],
                                                    "predicted" = rf_mod$test$predicted))
        }
      }
      
      else {
        rf_mod<-randomForest(x = dt_trn[,sel_var,with = F],y = dt_trn[[Target]],na.action = na.omit,
                             xtest = dt_tst[,sel_var,with = F],ytest = dt_tst[[Target]],importance = T)
        res_eval<-do.call(Eval_Metric,args = list("actual" = dt_tst[[Target]],
                                                  "predicted" = rf_mod$test$predicted))
      }
    },error = function(e){
      return(ifelse(Maximize == F,Inf,-Inf))
    })

    return(res_eval)
  }
  
  if(Maximize == F){
    ga_mod<-ga(type = "binary",fitness = function(x){-fit_func(x)},
               nBits = ncol(dt_out) - length(c(Target,Keep)),popSize = 100,pcrossover = 0.8,
               pmutation = 0.2,maxiter = 5000,run = 100,monitor = T,
               seed = 666,parallel = Par,keepBest = T)
  }
  
  else {
    ga_mod<-ga(type = "binary",fitness = function(x){fit_func(x)},
               nBits = ncol(dt_out) - length(c(Target,Keep)),popSize = 100,pcrossover = 0.8,
               pmutation = 0.2,maxiter = 5000,run = 1000,monitor = T,
               seed = 666,parallel = Par,keepBest = T)
  }

  sol<-ga_mod@solution[which.min(apply(X = ga_mod@solution,MARGIN = 1,FUN = sum)),]
  print(apply(X = ga_mod@solution,MARGIN = 1,FUN = sum))
  sol_var<-unique(c(Keep,colnames(dt_out)[colnames(dt_out) %in% c(Keep,Target) == F][sol]))
  
  if(Model == "LR"){
    sol_form<-as.formula(paste(Target,paste(sol_var,collapse = " + "),sep = " ~ "))
    sol_mod<-lm(formula = sol_form,data = dt_trn)
    sol_pred<-predict(object = sol_mod,newdata = dt_tst)
    sol_eval<-do.call(Eval_Metric,args = list("actual" = dt_tst[[Target]],"predicted" = sol_pred))
    sol_cor<-numeric()
    for(i in 1:length(sol_var)){
      sol_cor<-c(sol_cor,round(cor(x = dt_trn[[sol_var[i]]],y = dt_trn[[Target]]),digits = 2))
    }
    names(sol_cor)<-sol_var
    
    fin_res<-list("Sol_Variables" = sol_var,"Predictions" = sol_pred,"Actual" = dt_tst[[Target]],
                  "Eval_Res" = sol_eval,"Correlation" = sol_cor,"Model" = sol_mod,
                  "Model_Summary" = summary(sol_mod),"GA_Model" = ga_mod,"GA_Summary" = summary(ga_mod))
  }
  
  else if(Model == "RF" & class(dt_out[[Target]]) %in% c("numeric","integer")){
    sol_mod<-randomForest(x = dt_trn[,sol_var,with = F],y = dt_trn[[Target]],na.action = na.omit,
                          xtest = dt_tst[,sol_var,with = F],ytest = dt_tst[[Target]],importance = T)
    sol_pred<-sol_mod$test$predicted
    sol_eval<-do.call(Eval_Metric,args = list("actual" = dt_tst[[Target]],"predicted" = sol_pred))

    fin_res<-list("Sol_Variables" = sol_var,"Predictions" = sol_pred,"Actual" = dt_tst[[Target]],
                  "Eval_Res" = sol_eval,"Model" = sol_mod,"Model_Summary" = summary(sol_mod),
                  "GA_Model" = ga_mod,"GA_Summary" = summary(ga_mod))
  }
  
  else {
    sol_mod<-randomForest(x = dt_trn[,sol_var,with = F],y = dt_trn[[Target]],na.action = na.omit,
                          xtest = dt_tst[,sol_var,with = F],ytest = dt_tst[[Target]],importance = T)
    sol_pred<-sol_mod$test$predicted
    sol_eval<-do.call(Eval_Metric,args = list("actual" = dt_tst[[Target]],"predicted" = sol_pred))

    fin_res<-list("Sol_Variables" = sol_var,"Predictions" = sol_pred,"Actual" = dt_tst[[Target]],
                  "Eval_Res" = sol_eval,"Model" = sol_mod,"Model_Summary" = summary(sol_mod),
                  "Conf_Matrix" = sol_mod$test$confusion,"Pred_Prob" = as.data.table(sol_mod$test$votes),
                  "GA_Model" = ga_mod,"GA_Summary" = summary(ga_mod))
  }
  
  end_time<-Sys.time()
  print(end_time - init_time)

  return(fin_res)
}

chk<-Evo_FeatSel(Data = iris,Target = "Species",Norm = T,Treat_Out = T,Maximize = T,Eval_Metric = "accuracy")
