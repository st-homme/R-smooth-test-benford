source("probabilite_benford.R")
library("polynom")
library("gtools")

# Fonction prenant en parametre 
# data : les données 
# K :  le nombre de Tk
# digit :  Par défaut 1
# optimal.t.permutations = Pour une permutation ou non
ksmooth.test <- function(data, K, digits= 1,
                         optimal.t.permutations=FALSE ){
  generer_probabilite_theorique(digit=1)
  result <- calcul_tk(data, K,support.vector, proba.vector)
  
  taille <- length(data)
  if(optimal.t.permutations){
    resultat.tmp <- vector()
    # for(base in base.vector){
      result <- c(result, calcul_permutations_tk_matrix(data, K))
    # }
    
  }
  
  return(result)
}
###############################################################################################################

# Fonction retournant le polynôme
get_polynome_h_for_j <- function(j){
  if(j==1){
    return(polynomial(c(-1.3979030247488394121040493613182696526739584777368 ,
                        0.40633916736200686478542965064543693283491024599939)))
  }else if(j==2){
    return(polynomial(c( 2.2835458942827613994342486254403905701270610581437, 
                         -1.6127530382510774426950563380168084894310786771510 ,
                         0.18247002110725297036924261140018110452702111183442)))
  }else if(j==3){
    return(polynomial(c(-4.0815121188328689936236326398218860591304380838753 ,
                        4.5719372742522112044947303129821412996094568232480 ,
                        -1.2052506737490148470311505196645954099462916875905,
                        0.086173299060539497073071021550861682063512004101864)))
  }else if(j==4){
    return(polynomial(c( 8.0795029241386908450486312960577659502034469468917, 
                         -12.094575500356753355982353234285254134889081144404 ,
                         5.1951355207630563189562812915032734479956498201195,
                         -0.82485600201281199967265529487511547787181262658796 , 
                         0.043126384085651298652336797627506539669328872876724 )))
  }else if(j==5){
    return(polynomial(c( -18.108494394772161751612380555693712085844319263352 ,
                         33.142284947991963143915043695725705210025788289119 ,
                         - 19.722974020860465175272441917939990432050441230630,
                         5.0173393268200010622998332406879860011024286123698 ,
                         - 0.56656631604890066847536348651878387350781426664251 ,
                         0.023336822888880408751909327047967310412185600689676 )))
  }else if(j==6){
    return(polynomial(c( 47.539025123688956939839276197426162757171984295486, 
                         -100.97034588448909037802964553469158020757299454309 ,
                         75.855344772076234632745120190525279742211301165629,
                         -26.682825198088079673933396924974441588864816295226 , 
                         4.7568124723412390834620445451992011968266182408605 ,
                         -0.41562928281524381714954870899838795772527009926521 , 
                         0.014116816834808702805742210360195029761782308937628 )))
  }else if(j==7){
    return(polynomial(c(-155.23869158405472059098634159455986865585205117244 ,
                        370.17852496798326169953247150561067308461284772334,
                        -331.11599730173645571307956788170061765775873248699,
                        147.72553389848838714703346742044011676782185532865,
                        -36.176614461752994684988881308483105755882446054477,
                        4.9340195510159607707278807129264094913710664275380,
                        - 0.35100285248652007809095369751269381430321388096641,
                        0.010139198243335377835234192471425857563997234141646)))
  }else{
    return(NA)
    
  }
  
}

calcul_U_for_j <- function(j, data, taille, polynome.h.j){
  return((1/sqrt(taille)) * sum(predict(polynome.h.j, data)))
}

##### Calcul de  $T_1,...,T_k$
calcul_tk<- function(data, K, support.vector=c(1:9), proba.vector=rep(1/9, 9)){
  #print(polynome.h)
  resultat <- vector()
  taille <- length(data)
  for(j in 1:K ){
    if (j == 1 ){
      resultat[j] = (calcul_U_for_j(j, data, taille, get_polynome_h_for_j(j))^2)
    }else{
      resultat[j] = resultat[j-1] +  (calcul_U_for_j(j, data, taille,get_polynome_h_for_j(j) )^2)
    }
  }
  return(resultat)
}

##################### Gestion optimum Calcul de  $ T_{\widehat{k}$####################################
calcul_tk_widehat <- function(Tki, K ,taille, base){
  return(Tki[which((Tki - (1:K) * log(taille, base))== max(Tki - (1:K) * log(taille, base)) )])
}

######################### Gestion Permutation ########################"
calcul_permutations_tk_matrix <- function(data, K){
  base <-  10
  vector.permutations <-  permutations(n=3, r=3, v=c(1:K))
  #vector.permutations <- matrix(c(1, 2, 3, 2, 1, 3, 2 , 3 ,1) , nrow = 3, ncol = 3, byrow = TRUE)
  vector.uj <- calcul_permutations_uj(data, K)
  
  res <- vector()
  n  <- length(data)
  #print(vector.permutations)
  #Ti.matrix <- matrix(NA , factorial(3), K)
  for(i in 1: nrow(vector.permutations)){
    Ti <- calcul_permutations_tk( c(vector.uj[c(vector.permutations[i,],4:K)]), K)
    res[i] <- calcul_tk_widehat(Ti, K, n , base)
  }
  
  return(res)
}

# ################################################
calcul_permutations_tk<- function(uj.vector,K){
  return(cumsum(uj.vector^2))
}

# ##### Calcul de  $T_{permutations}$
calcul_permutations_uj <- function(data, K){
  resultat <- vector()
  taille <- length(data)
  for(j in 1:K ){
    resultat[j] <- (calcul_U_for_j(j, data, taille, get_polynome_h_for_j(j)))
  }
  return(resultat)
}

