# Significance Tables with pixiedust
library(pixiedust)
library(DataCombine)
#########


# Create Summary Table with Significance

make_table_diri = function(..., oddsRatios = F) {
  library(Compositional)
  insertRow_f <- function(existingDF, newrow, r) {
    existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
    existingDF[r,] <- newrow
    existingDF
  }
  
  make_dust_data = function(model_obj) {
    sum_obj = summary(model_obj)
    
    vars_per_profile = ((sum_obj$npar-1)/3)-1
    
    summary_table = data.frame(
      term = rownames(sum_obj$coef.mat),
      sum_obj$coef.mat,
      stringsAsFactors = F,
      row.names = NULL)

    if (oddsRatios == T){
      summary_table$Estimate[-length(summary_table$Estimate)] = exp( summary_table$Estimate[-length(summary_table$Estimate)] )
      
    }

    summary_table_gaze = summary_table %>% 
      bind_cols(
        "sig" = ifelse(summary_table$Pr...z.. < 0.001, "***", 
                       ifelse(summary_table$Pr...z.. < 0.01, "**",
                              ifelse(summary_table$Pr...z.. < 0.05, "*",
                                     ifelse(summary_table$Pr...z.. < 0.1, "+", ""))))
      )  %>% 
      mutate_if(is.numeric, funs(round(.,2))) %>% 
      mutate(Std..Error = paste("(", Std..Error, ")", sep="")) %>% 
      select(term, Estimate, Std..Error, sig) %>% 
      unite("Estimate", Estimate, sig, sep=" ") %>% 
      unite("Estimate", Estimate, Std..Error, sep="<br>")
    
    
    # Change Intercept to Intercept + DemocracyProfile
    rownames = rownames(sum_obj$coef.mat)
    border_intercept = which(rownames == "(Intercept)")
    
    y_names = sum_obj$varnames
    y_names = gsub("X_", "", y_names )
    summary_table_gaze$term[border_intercept[-which(border_intercept == max(border_intercept))]] = paste("Intercept", y_names[2:4])
    summary_table_gaze$term[-border_intercept] = paste(summary_table_gaze$term[-border_intercept], rep(y_names[2:4], each = vars_per_profile))

    # change last intercept to phi
    summary_table_gaze$term[max(border_intercept)] = "phi"
    
    # Profiles Separator    
    new = matrix( cbind(y_names[2:4], ""), nrow=3, ncol=2)
    adder = 1
    
    summary_table_gaze = rbind(new[1,], summary_table_gaze)
    border_intercept_adapted = array(NA, (length(y_names[2:4])-1))
    for (i in 2:length(y_names[2:4])) {
      
      border_intercept_adapted[i-1] = border_intercept[i] + adder
      
      summary_table_gaze = InsertRow(summary_table_gaze, 
                                     NewRow = new[i,], 
                                     RowNum = border_intercept[i] + adder)
      adder = adder + 1
    }
    
 
    # Create Custom Foot with AIC, BIC, N etc.
    LL = round(sum_obj$logLik,2)
    AIC = round(sum_obj$aic,2)
    R2 = round(totvar(fitted(model_obj))/totvar(model_obj$Y),2)
    #BIC = round(sum_obj$bic,2) // works bad compared to AIC in dirichlet context (own simulation)
    N = sum_obj$nobs
    
    foot_matrix = data.frame(name = rbind("LL",
                                          "AIC",
                                          "R2",
                                          #"BIC", 
                                          "N", 
                                          paste("Ref.:", y_names[1])), 
                             values= rbind(LL, 
                                           AIC,
                                           R2,
                                           #BIC, 
                                           N,
                                           ""))
    
    
    return(list(summary_table_gaze, foot_matrix, border_intercept=border_intercept_adapted))
  }
  ###################################
  
  # Input into list
  sum_obj = list(...)
  
  # Apply make_dust_data to list with objects
  dust_data = lapply(sum_obj, make_dust_data)  
  
  
  # Create main DF
  dust_data_join = dust_data[[1]][[1]] 
  if (length(dust_data) > 1) {
    for (i in 2:length(dust_data)) {
      dust_data_join = dust_data_join %>% 
        full_join(dust_data[[i]][[1]], by="term")
    }
  }

  dust_data_join = dust_data_join %>% 
    separate(term, c("term", "profile"), sep=" ", fill="left") %>% 
    arrange(profile) %>% 
    unite(term, "term", "profile") %>% 
    mutate(term = gsub("NA_", "", term))
  
  #Rename Columns and Rows
  colnames(dust_data_join)[-1] = paste("M", 1:length(dust_data), sep="")

  dust_data_join$term = gsub("_Fec", "", dust_data_join$term) 
  dust_data_join$term = gsub("_fEc", "", dust_data_join$term) 
  dust_data_join$term = gsub("_fEC", "", dust_data_join$term) 
  dust_data_join$term = gsub("_FeC", "", dust_data_join$term) 
  
  ###################################
  # Create foot DF
  foot_data_join = dust_data[[1]][[2]] 
  if (length(dust_data) > 1) {
    for (i in 2:length(dust_data)) {
      foot_data_join = foot_data_join %>% 
        full_join(dust_data[[i]][[2]], by="name")
    }  
  }
  
  border_intercept = dust_data_join %>% 
    select(term) %>% 
    filter(grepl("caus", term) | grepl("Inter", term)) %>% 
    summarise(rows = n()/3 + 1) %>% 
    mutate(rows2 = 2*rows) %>% 
    as.numeric() +1
    
  

  ###################################
  # Create Table with pixiedust
  dust(dust_data_join, glance_foot = F)  %>% 
    redust(foot_data_join, part="foot") %>% 
    # rename
    sprinkle_colnames("term" = "Term") %>%
    
    sprinkle(halign = "center", part=c("head")) %>% 
    
    sprinkle(cols = 2:length(dust_data), halign = "center", valign = "middle", part=c("body")) %>%
    sprinkle(cols = 2:length(dust_data), halign = "center", part=c("foot")) %>% 
    
    sprinkle(rows = 1, italic = TRUE) %>%  
    sprinkle(rows = border_intercept, border = "top", border_color = "black", italic = TRUE) %>% 
    sprinkle(rows = dim(dust_data_join)[1], border = "top", border_color = "black") %>% 
    
    sprinkle(rows = 1, border = "top", border_color = "black", border_thickness=2) %>%
    sprinkle(rows=1, border = "top", part = "foot", border_thickness=2)  %>% 
    sprinkle(rows=dim(foot_data_join)[1], border = "top", part = "foot", bold = TRUE, border_thickness=1)  %>% 
    # font size
    sprinkle(font_size = 10, font_size_units = "pt", part="head") %>% 
    sprinkle(font_size = 9, font_size_units = "pt", part="body") %>% 
    sprinkle(font_size = 9, font_size_units = "pt", part="foot") %>% 
    # NAs
    sprinkle_na_string(na_string = "") %>% 
    sprinkle_print_method("html")
}


# make_table_diri(m1, m2, m3, oddsRatios=F)
# make_table_diri(m1, m2, m3, m4, m5, m6, m7)

# For Rendering in knitr, see Setup/knitR_tab/table_word.Rmd
# my_table = make_table_diri(m1, m2, m3, m4, m5, m6, m7)


