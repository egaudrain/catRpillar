
#' confusion.matrix
#'
#' Generates the confusion matrix from a categorisation data frame.
#'
#' @param d A `data.frame` containing one trial per row.
#' @param presented The name of the factor used for the presented category (as a string).
#' @param responded The name of the factor used for the responded category (as a string).
#' @param grouping A vector of variable names used to group the matrices by.
#'
#' @return A `data.frame` with the following fieds:
#' \itemize{
#'   \item `Presented` containing the name of the presented category
#'   \item `Responded` containing the name of the responded category
#'   \item `Count` the number of occurences
#'   \item and the factors used for grouping
#' }
#'
#' @examples
#' data_cm = confusion.matrix(dat, 'Presented', 'Responded', c('Condition'))
#' ggplot(data_cm, aes(x=Presented, y=Responded)) +
#' geom_point(aes(size=Count, colour=Count)) +
#'     scale_colour_viridis_c() +
#'     facet_wrap(~Condition) +
#'     coord_fixed() +
#'     guides(size=F) +
#'     theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=.5))
#'
#' @export

confusion.matrix = function(d, presented, responded, grouping){
    if(missing(grouping)){
        cm = as.data.frame(table(d[[presented]], d[[responded]]))
        colnames(cm) <- c('Presented', 'Responded', 'Count')
        if(is.factor(d[[presented]])){
            var_levels = levels(d[[presented]])
            cm$Presented = factor(cm$Presented, levels=var_levels)
            cm$Responded = factor(cm$Responded, levels=var_levels)
        }
        return(cm)
    } else {
        cm = data.frame()
        grp_levels = interaction(d[,grouping])
        for(g in levels(grp_levels)){
            s = (grp_levels == g)
            if(sum(s)==0){
                next
            }
            cm_i = confusion.matrix(d[s,], presented, responded)
            for(gl in grouping){
                cm_i[[gl]] = d[s,gl][1]
            }
            cm = rbind(cm, cm_i)
        }
        for(gl in grouping){
            if(is.factor(d[[gl]])){
                cm[[gl]] = factor(cm[[gl]], levels=levels(d[[gl]]))
            }
        }
        return(cm)
    }
}

#' fita
#'
#' `fita` calculates the mutual information between presented and responded categories.
#'
#' @param cm The confusion matrix (see \code{\link{confusion.matrix}}).
#' @param grouping A vector of strings containing the names of the variables used to group the FITA by.
#'
#' @return A `data.frame` with fields:
#' \itemize{
#'   \item `Hf` The total information entropy.
#'   \item `t.abs` The absolute mutual information.
#'   \item `t.rel` The relative mutual information (`t.abs`/`Hf`).
#' }
#'
#' @seealso \code{\link{confusion.matrix}}
#'
#' @references Based on Oosthuizen & Hanekom (2016, Speech Comm.) and Miller & Nicely (1955, JASA).
#'
#' @export

fita = function(cm, grouping){
    if(missing(grouping)){
        R = list()

        t_abs = 0

        n = sum(cm$Count)

        Hf = 0
        for(fi in unique(cm$Presented)){
            Pui = sum(cm$Count[cm$Presented == fi]) / n
            Hf = Hf + Pui * log2(Pui)

            for(fj in unique(cm$Responded)){
                Pui.vj = sum(cm$Count[cm$Presented==fi & cm$Responded==fj]) / n
                Pvj    = sum(cm$Count[cm$Responded==fj]) / n
                if(Pui.vj!=0){
                    t_abs = t_abs + Pui.vj * log2(Pui * Pvj / Pui.vj)
                }
            }
        }
        t_abs = - t_abs

        Hf = - Hf
        R[['Hf']] = Hf
        R[['t.abs']] = t_abs
        R[['t.rel']] = t_abs / Hf

        return(R)
    } else {
        R = data.frame()
        grp_levels = interaction(cm[,grouping])
        for(g in levels(grp_levels)){
            s = (grp_levels == g)
            if(sum(s)==0){
                next
            }
            R_i = fita(cm[s,])
            for(gl in grouping){
                R_i[[gl]] = cm[s,gl][1]
            }
            R = rbind(R, R_i)
        }
        for(gl in grouping){
            if(is.factor(cm[[gl]])){
                R[[gl]] = factor(R[[gl]], levels=levels(cm[[gl]]))
            }
        }
        return(R)
    }

}
