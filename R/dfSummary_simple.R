#' New functions from antuki
#'
#'
#' @param bdd Compléter
#' @param nom_var Compléter
#' @param poids Compléter
#' @param filtres Compléter
#' @author antuki
#' @export
dfSummary_simple <- function(bdd,
                             nom_var,
                             nom_croisement=NULL,
                             poids=NULL,
                             filtres=NULL,
                             header_perso=TRUE,
                             ...) {

  if(!is.null(filtres)){
    bdd <- bdd %>% filter(eval(parse(text=filtres)))
  }
  
  if(is.null(nom_croisement)){
    bdd <- bdd %>%
      select(c(nom_var,poids))
    
    df <- dfSummary(bdd,headings = FALSE,header_perso=header_perso,column_weight = "poids")
  } else{
    
    df <- NULL
    
    modalites <- setNames(unique(as.numeric(bdd[,nom_croisement])), unique(bdd[,nom_croisement]))
    modalites <- modalites[order(modalites)]
    for(modalite in modalites) {
      bdd_filtree <- bdd %>%
        filter(eval(parse(text=paste0("as.numeric(",nom_croisement,")=='",modalite,"'")))) %>% 
        select(c(nom_var,poids))
      df_prov <- dfSummary(bdd_filtree,headings = FALSE,header_perso=header_perso,column_weight = "poids")
      df_prov$Variable <- paste0(df_prov$Variable,"\\\nfiltre : ",nom_croisement,"=",names(modalites[which(modalites==modalite)]))
      df <- rbind(df,df_prov)
    }
    
  }
  
  df$No <- seq_len(nrow(df))
  
  #Ajout du header
  if(header_perso){
  #if(1+1==3){
    attr(df, "format_info")$header_perso_txt <- c(
      ifelse(!is.null(filtres),paste0("**Champ : **", filtres,"</br>"),NULL),
      ifelse(!is.null(poids),paste0("**Pondération : **", poids,"</br>" ),NULL),
      ifelse(!is.null(nom_croisement),paste0("**Croisement : **", nom_croisement,"</br>" ),NULL)
    )
  }
 
  return(df)
  
}



#' @param df compléter
#' @author antuki
#' @export
header_perso_func <- function(df,...) {
  #return("\n#HOHO</br>mhhh\n*h*</br>#HIHI\n")
  if(attr(df, "format_info")$header_perso & !is.null(attr(df, "format_info")$header_perso_txt)){
    return(attr(df, "format_info")$header_perso_txt)
  } else{
    return(NULL)
  }

}
