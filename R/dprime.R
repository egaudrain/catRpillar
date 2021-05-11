#' dprime
#'
#' Calculate d' for categorisation data. This applies to data where an item pertaining to a category
#' is presented, and the participant answers a category by choosing amongst a set of choices.
#'
#' @param dat A `data.frame` with the responses in long format (one row per trial).
#' @param grouping The factors used to group the d' calculation by (you probably at least want your participant id there). This should be specified as a vector of strings.
#' @param presented A string containing the name of the `data.frame` field containing the names of the presented categories. (Defaults to "Presented")
#' @param responded A string containing the name of the `data.frame` field containing the names of the responded categories. (Defaults to "Responded")
#'
#' @return A `data.frame` with the grouping factors, the presented factor, and two fields `d` and `c` that contain the d' and criterion.
#' @export


dprime = function(dat, grouping, presented='Presented', responded='Responded'){
    if(missing(grouping)){
        d = NULL
        for(cd in unique(dat[[presented]]))
        {
            # Hits
            s = dat[[presented]]==cd
            nHI = sum(s)

            if(nHI==0) {
                next
            }

            HI  = sum(s & dat[[responded]]==cd) / nHI

            if(HI==1) {
                HI = 1-1/(2*nHI)
            } else if(HI==0) {
                HI = 1/(2*nHI)
            }

            # False-alarms
            s = dat[[presented]]!=cd
            nFA = sum(s)

            if(nFA==0) {
                next
            }

            FA = sum(s & dat[[responded]]==cd) / nFA

            if(FA==1){
                FA = 1-1/(2*nFA)
            } else if(FA==0) {
                FA = 1/(2*nFA)
            }

            zHI = qnorm(HI)
            zFA = qnorm(FA)


            d_i = data.frame(d=(zHI - zFA)/sqrt(2), c = -(zHI+zFA)/2)
            d_i[[presented]] = cd
            d = rbind(d, d_i)

        }

        return(d)

    } else {
        d = data.frame()
        grp_levels = interaction(dat[,grouping])
        for(g in levels(grp_levels)){
            s = (grp_levels == g)
            if(sum(s)==0){
                next
            }
            d_i = dprime(dat[s,], presented=presented, responded=responded)
            for(gl in grouping){
                d_i[[gl]] = dat[s,gl][1]
            }
            d = rbind(d, d_i)
        }
        for(gl in c(grouping, presented)){
            if(is.factor(dat[[gl]])){
                d[[gl]] = factor(d[[gl]], levels=levels(dat[[gl]]))
            }
        }
        return(d)
    }
}
