#' dfSummary_simple
#'
#'
#' @param bdd Completer
#' @param nom_var Completer
#' @param nom_croisement Completer
#' @param column_weight Completer
#' @param filtres Completer
#' @param header_perso Completer
#' @author antuki
#' @export
dfSummary_simple <- function(bdd,
                             nom_var,
                             nom_croisement=NULL,
                             column_weight=NULL,
                             filtres=NULL,
                             header_perso=TRUE) {

  if(!is.null(filtres)){
    bdd <- bdd %>% filter(eval(parse(text=filtres)))
  }
  
  if(is.null(nom_croisement)){
    bdd <- bdd %>%
      select(c(nom_var,column_weight))
    
    df <- dfSummary(bdd,headings = FALSE,header_perso=header_perso,column_weight = column_weight,var.col=FALSE,valid.col=FALSE)
    
    
    
  } else{
    
    df <- NULL
    
    modalites <- setNames(unique(as.numeric(bdd[,nom_croisement])), unique(bdd[,nom_croisement]))
    modalites <- modalites[order(modalites)]
    for(modalite in modalites) {
      bdd_filtree <- bdd %>%
        filter(eval(parse(text=paste0("as.numeric(",nom_croisement,")=='",modalite,"'")))) %>% 
        select(c(nom_var,column_weight))
      df_prov <- dfSummary(bdd_filtree,headings = FALSE,header_perso=header_perso,column_weight = column_weight)
      #df_prov$Variable <- paste0(df_prov$Variable,"\\\nfiltre : ",nom_croisement,"=",names(modalites[which(modalites==modalite)]))
      df <- rbind(df,df_prov)
    }
    
  }
  
    
  #Ajout du header
  if(header_perso){
   # attr(df, "format_info")$header_perso_txt <- "###Résultats\n**h** : KA</br>**h** : KB\n"
     attr(df, "format_info")$header_perso_txt <- paste(c(
       # paste0("Résultats sur la variable : ",paste0(nom_var, " [",paste(class(bdd[,nom_var]),"]", collapse = "")),"\n"),
        paste0("</br></br>**Résultats sur la variable : ", nom_var, " [",class(bdd[,nom_var]),"]**","</br>"),
        switch(!is.null(filtres),paste0("**Filtre** : ", filtres,"</br>"),paste0("**Filtre** : ", "Aucun","</br>")),
      switch(!is.null(column_weight),paste0("**Ponderation** : ", column_weight,"</br>" ),paste0("**Ponderation** : ","Aucune","</br>" )),
      switch(!is.null(nom_croisement), paste0("**Croisement** : ", nom_croisement,"</br>" ),NULL),
      "\n"
    ),collapse="")

  }
   
  #Redéfinir colonne variable
  if(!is.null(nom_croisement)){
    df$Variable <- names(modalites)
  } else{
    
  }
  
    df$No <- seq_len(nrow(df))
  
  return(df)
  
}


#' header_perso_func
#'
#' @param df Completer
#' @author antuki
#' @export
header_perso_func <- function(df) {
  if(attr(df, "format_info")$header_perso & !is.null(attr(df, "format_info")$header_perso_txt)){
    return(attr(df, "format_info")$header_perso_txt)
  } else{
    return(NULL)
  }

}

