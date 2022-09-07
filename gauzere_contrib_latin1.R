bootstrap_cwi <-
  function(df, trait_val_col="trait_val", k , bootstrap_ci ,bessel){
    
    #      Bootstrap estimator of CWM/CWV.
    # Bootstrap is performed /at individual level/. 
    # 
    # Args:
    # df (pandas.DataFrame): Census dataframe.
    
    # k (int): Number of bootstrap re-sampling.
    # bootstrap_ci (num): Percentile of bootstrap confidence interval.
    # bessel (bool): If true, use bessel correction for an unbiased variance
    # estimator (N/(N-1)).
    # 
    # Returns:
    # A dict. with the bootstrap estimators of CWM/CWV and the 
    # corresponding confidence interval.
    
    
    
    
    
    # Get relative abundances: this is the distribution from
    # which the bootstrap sample will be drawn.
    N = sum(df$n) # number of individuals. 
    df$rN <- df$n/N 
    
    # Sort by relative abundances to speed up computations.
    df<-df[order(df$rN, decreasing = T),]
    
    # Do the cummulative sum of all relative abundances to
    # divide the segment [0,1] between all species. 
    df$proba =  cumsum(df$rN)
    sp = length(df$proba) # number of species.
    
    # Perform k bootstrap.
    cwm = array(0,k)
    cwv = array(0,k)
#    for (i in 1:k){
#      
#      # Draw a new community composition from the distribution.
#      df$n<-hist(runif(N),breaks=c(0,df$proba), plot=F)$count
#      
#      # Compute the indicies. 
#      cwm[i] = cwmean(df)
#      cwv[i] = cwvar(df, cwm=cwm[i])
#      
#    }
    
    for(i in 1:k){
      set.seed(i)
      n <- sample(as.integer(length(unique(df$species))*3/4):length(unique(df$species)) - 1, 1)
      sp <- sample(df$species, n)
      dfi <- subset(df, df$species %in% sp)
      cwm[i] <- cwmean(dfi)
      cwv[i] <- cwvar(dfi, cwm = cwm[i])
      }

#    samp3 <- samp2 %>%
#      group_by(YEAR) %>%
#      summarise(CTI_hiv = sum(STI_hiv_mean * TOTAL_COUNT)/sum(TOTAL_COUNT))
    
    
    
    
    # Bootstrap estimators are derived from the bootstrap distribution. 
    out <- data.frame(
      bootstrap_cwm = mean(cwm),
      bootstrap_cwv = mean(cwv),
      bootstrap_cwm_lower_ci = quantile(cwm,1 - (bootstrap_ci/100), na.rm = TRUE),
      bootstrap_cwv_lower_ci = quantile(cwv,1 - (bootstrap_ci/100), na.rm = TRUE),
      bootstrap_cwm_higher_ci = quantile(cwm,bootstrap_ci/100, na.rm = TRUE),
      bootstrap_cwv_higher_ci = quantile(cwv,bootstrap_ci/100, na.rm = TRUE)
    )
    
    return(out)
  }



contrib <-
  function(census_i, census_f, species, trait_val_col="trait_val"){

    browser()
    #   # Check columns name sanity.
    #   col_names = col_names_checker(col_names,[census_f.columns,
    #                                            census_i.columns,
    #                                            species.columns])  
    # colnames(species)[2:3]<- c('trait_val', 'trait_var')
    
    
    
    
    # Group observations by species and merge them. 
    # census_i<-ddply(census_i, .(species), function(x){sum(x$n)})
    census<-join(census_i, census_f, type='full')
    
    
    # Filter species list and merge them.
    
    species <- arrange(species, species)
    species = species[, c("species",trait_val_col)]
    species <- species[species$species %in% unique(census$species),]
    
    # Originality is the diffenrence to the mean trait value.
    species$originality = species[,trait_val_col] - mean(species[,trait_val_col])
    
    # Variance originality
    species$v_originality = (species[,trait_val_col] ^2) - mean(species[,trait_val_col] ^2)
    
    # Variance cross corrective term.
    #histoire du dictionnaire nom col voir fonction s3c.index.mean
    # cnames_i = col_names.copy()
    # cnames_i["n"] = col_names["n"] + "_i"
    # cnames_f = col_names.copy()
    # cnames_f["n"] = col_names["n"] + "_f"
    
    census<- join(census, species)
    
    # source("index.R")
    
    S = cwmean(df=census[census$date==1982,],trait_val_col) + cwmean(df=census[census$date==2007,],trait_val_col)
    species$v_cross =  species$originality * S
    
    # Drop unwanted columns
    # census.drop([col_names["trait_val"],col_names["trait_var"]],1,inplace=True)
    
    
    # Compute differences in relative abundances.
    
    rN_i <-daply(census_i, .(species), function(x)sum(x$n))/sum(census_i$n)
    rN_f <-daply(census_f, .(species), function(x)sum(x$n))/sum(census_f$n)
    species$dp <- rN_f - rN_i
    # Contribution is the product originalitt * dp.
    species$contrib   = species$originality * species$dp
    species$v_contrib = species$dp * (species$v_originality - species$v_cross)  
    
    
    return(species)}



cwi <-
  function(census, traits, trait_val_col="trait_val",
           bootstrap = FALSE, bootstrap_n = 100, bootstrap_ci=95,
           bessel = TRUE){
    
    #   Compute community weighted indexes of a community.
    # 
    # Bootstrap is performed /at individual level/. 
    # 
    # Args:
    # census (data.frame)  : Census dataframe. Required columns are "n" (number of individual) & "species".
    # traits (data.frame)  : trait value dataframe. Required columns are "species" and the trait value colums defined in trait_val_col argument.
    # trait_val_col (text) : name of column in "traits" for which cwi will be applied.       
    # bootstrap (bool)     : Perform bootstrap if true.
    # bootstrap_n (int)    : Number of bootstrap re-sampling.
    # bootstrap_ci (num)   : Percentile of bootstrap confidence interval.
    # bessel (logical)     : If TRUE, use bessel correction for an unbiased variance estimator (N/(N-1)).
    # 
    # Returns:
    # A data.frame with CWM/CWV estimation and bootstraps estimators and the corresponding confidence interval if required.
    
    
    
    
    # check present species.
    census$species <- droplevels(as.factor(census$species))
    present_sp     <- unique(census$species)
    traits<-traits[traits$species %in% census$species,]
    
    # Group all individuals of a given species.
    census = ddply(census, .(species), summarize, n=sum(n), .drop=F)
    
    # Merge census with species traits. 
    merged = merge(census, traits, by="species")
    
    # Check trait name columns name sanity.            
    merged$trait_val<-merged[[trait_val_col]]
    
    # Compute indexes.
    out = data.frame(cwm=999, cwv=999)
    out$cwm = cwmean(merged)
    out$cwv = cwvar(merged,cwm=out[["cwm"]])
    
    # Perform bootstrap if needed.
    if (bootstrap==TRUE){
      out_boot = bootstrap_cwi(merged, k=bootstrap_n, bootstrap_ci=bootstrap_ci, bessel=T)
      out<-cbind(out, out_boot)}
    
    return(out)}



cwi_stratified <-
  function(census, traits, trait_val_col="trait_val",
           bootstrap = FALSE, bootstrap_n = 100, bootstrap_ci=95,
           bessel = TRUE){
    ### Stratified computation of community weighted indexes of a community.
    
    # Observations will be grouped by site and date.
    # Bootstrap is performed /at individual level/. 
    # 
    # Args:
    # census (data.frame)  : Census dataframe. Required columns are "n" (number of individual, numeric), "species", "site", "date".
    # traits (data.frame)  : trait value dataframe. Required columns are "species" and the trait value colums defined in trait_val_col argument.
    # trait_val_col (text) : name of column in "traits" for which cwi will be applied.       
    # bootstrap (bool)     : Perform bootstrap if true.
    # bootstrap_n (int)    : Number of bootstrap re-sampling.
    # bootstrap_ci (num)   : Percentile of bootstrap confidence interval.
    # bessel (logical)     : If TRUE, use bessel correction for an unbiased variance estimator (N/(N-1)).
    # 
    # Returns:
    # A data.frame with CWM/CWV estimation and bootstraps estimators and the corresponding confidence interval if required.
    
    # check present species.
    census$species <- droplevels(as.factor(census$species))
    present_sp     <- unique(census$species)
    traits<-traits[traits$species %in% census$species,]
    
    
    # Merge census with species traits. 
    merged = merge(census, traits, by="species")
    
    # Check trait name columns name sanity.
    if(trait_val_col %in% colnames(traits)) {merged$trait_val<-merged[[trait_val_col]]
    } else {print("trait value columns is not defined")}
    
    # Compute indexes and sum of observation 
    out<-ddply(merged, .(date), function(x){
      return(cbind(cwm=cwmean(x), cwv=cwvar(x, cwm=cwmean(x)), n=sum(x$n)))})
    
    # Perform bootstrap if needed.
    if (bootstrap==TRUE){
      #out_boot <- ddply(merged, .(date), function(x){
      #  return(bootstrap_cwi(group_by(merged, date), k=bootstrap_n, bootstrap_ci=bootstrap_ci, bessel=T))})
      
      #newcol <- c("bootstrap_cwm", "bootstrap_cwv",
      #            "bootstrap_cwm_lower_ci", "bootstrap_cwv_lower_ci",
      #            "bootstrap_cwm_higher_ci", "bootstrap_cwv_higher_ci")
      
      #out[newcol] <- NA
      
      out_boot_tot <- data.frame(date = NA, bootstrap_cwm = NA, bootstrap_cwv = NA,
                                 bootstrap_cwm_lower_ci = NA, bootstrap_cwv_lower_ci = NA,
                                 bootstrap_cwm_higher_ci = NA, bootstrap_cwv_higher_ci = NA)
      
      for(dd in out$date){
        df <- subset(merged, merged$date == dd)
        out_boot = bootstrap_cwi(df, k=bootstrap_n, bootstrap_ci=bootstrap_ci, bessel=T)
        out_boot$date <- dd
        out_boot_tot <- rbind(out_boot_tot, out_boot)
      }
      
      out <- merge(out, out_boot_tot, by = "date")
    }
    
    return (out)}



cwmean <-
  function(df, trait_val_col="trait_val"){
    
    #   Compute the community weighted mean of the dataframe.
    # 
    #     Args:
    #         df (dataframe)       : Containing a column "n" (number of individuals) and a column referencing the trait value.
    #         trai_val_col (text)  : names of the column referencing trait value 
    # 
    #     Return: 
    #         Community weighted mean.
    
    
    return(  sum( df[,trait_val_col] *  df$n / sum(df$n))) }



cwvar <-
  function(df, trait_val_col="trait_val", bessel=TRUE, cwm=NA){
    #   Compute the community weighted mean and community weighted variance of the dataframe.
    # 
    #     Args:
    #         df (dataframe)       : Containing a column "n" (number of individuals) and a column referencing the trait value.
    #         trai_val_col (text)  : names of the column referencing trait value 
    #         bessel (bool)        : If TRUE, use bessel correction for an unbiased variance estimator (N/(N-1)).
    #         cwm                  : If Community weighted means has already been computed (it is only to speed up computations)
    #     Return: 
    #         Community weighted mean and community weighted variance.
    
    df$trait_val<-df[[trait_val_col]]
    
    if (is.na(cwm) == T) {cwm <- cwmean(df)}
    
    if (bessel==TRUE){
      corrective_term = sum(df$n)  / (sum(df$n) -1)
    } else {
      corrective_term = 1
    }
    
    n2  <- sum ( df$trait_val^2 * df$n / sum(df$n))
    return(cwv=corrective_term * (n2 - cwm^2))}



trend_contrib <-
  function(census, traits, trait_val_col="trait_val"){
    
    browser()
    
    # Check trait name columns name sanity.
    if(trait_val_col %in% colnames(traits)) {traits$trait_val<-traits[[trait_val_col]]
    }else {print("trait value columns is not defined")}
    
    traits <- arrange(traits, species)
    
    
    # Present species.
    census$species<-droplevels(as.factor(census$species))
    present_sp = unique(census$species)
    
    
    # colnames(species)[2:3]<- c('trait_val', 'trait_var')
    species<-traits[traits$species %in% census$species,]
    species<-species[,c("species", "trait_val")]
    species$species<-droplevels(as.factor(species$species))
    
    census<-census[census$species %in% species$species,]
    census$species<-droplevels(as.factor(census$species))
    
    
    
    # Compute originality.
    species$originality = species$trait_val - mean(species$trait_val)
    species$v_originality = (species$trait_val^2) - mean(species$trait_val^2)
    
    # Compute relative abundance trends.
    census<-ddply(census, .(species, date), summarize, N=sum(n))
    census<-ddply(census, .(date),mutate, n_by_date=sum(N))
    census$rN<-census$N / census$n_by_date
    
    check_rep_time<-ddply(census,.(species), summarize, n_year=length(unique(date)))
    print(paste("species with less than 2 time occurences were removed from analysis : ", 
                as.character(check_rep_time[check_rep_time$n_year<3, "species"])))
    census<-census[census$species %in% check_rep_time[check_rep_time$n_year>2, "species"],]
    
    species <- arrange(species, species)
    
    species$dp<-daply(census, .(species), function(x){
      mod<-lm(rN ~ date, data=x)
      return(summary(mod)$coef[2,1])})
    
    # Compute S
    census = merge(census, species, by="species")
    S = (cwmean(census[census$date==min(census$date),]) 
         + cwmean(census[census$date==max(census$date),]))
    
    # Compute contributions
    species$contrib = species$originality * species$dp
    species$v_contrib = species$dp  *  (species$v_originality - species$originality * S)
    return(species)}
