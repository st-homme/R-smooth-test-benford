generer_probabilite_theorique <- function(digit=1){
  return(get_probabilite_benford_for_digit_support(digit, base=10))
}

#Calcul les probabilités de benford par rapport au digit
get_probabilite_benford_for_digit_support<- function(digit=1, base=10){
  chiffre.generer <- list()
  for (i in 1:(digit)){
    if(i == 1){
      chiffre.generer[[i]] <- c(1:9)
    }
    else{
      chiffre.generer[[i]] <- c(0:9)
    }
  }
  res <- vector()
  chiffre.generer <- expand.grid(chiffre.generer) 
  for(i in 1:nrow(chiffre.generer)){
    tmp <- paste(chiffre.generer[i, 1:digit], collapse ="")
    res[i] <- as.numeric(tmp)
  }
  return(get_probabilite_conjointe_n_number(sort(res), digit, base))
}


#calcul la probabilité conjointe
get_probabilite_conjointe_n_number <- function(data, max.digits=1, base=10){
  proba.vector <- vector()
  list.of.digits <- get_n_element_des_nombres(data, max.digits)
  k <- 1
  for(number in list.of.digits){
    l <- 1
    tmp <- 0
    for(chiffre in number){
      tmp <- tmp + ( (10^(max.digits - l)) * chiffre)
      l <- l+1
    }
    
    proba.vector[k] <- (log((1 + (1 / tmp)), base))
    k <- k+1
  }
  return(proba.vector)
}

#Fonction get_n_element_des_nombres
#Parametre : vecteur de nombres
#         : digit
#retourne pour chacun la liste des chiffres
get_n_element_des_nombres <- function(data, digits=1){
  data.digits <- list()
  for(number in 1:length(data)){
    list_of_number <- unlist(strsplit(as.character(data[number]), ""))
    d <- 1
    number.vector <- vector()
    for(i in list_of_number){
      if (d <= digits){
        if (as.numeric(i) %in% 0:9){
          if (d == 1){
            if (as.numeric(i) %in% 1:9){
              number.vector[d] <- as.numeric(i)
              d <- d + 1
            }
          }
          else{
            number.vector[d] <- as.numeric(i)
            d <- d + 1
          }
        }
      }
    }
    data.digits[[number]] <- number.vector
  }
  return(data.digits)
}