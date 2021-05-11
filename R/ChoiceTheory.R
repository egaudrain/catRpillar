#' ChoiceTheory
#'
#' Calculate a sensitivity and bias measure for categorisation data according to the Choice Theory. This applies to data where an item pertaining to a category
#' is presented, and the participant answers a category by choosing amongst a set of choices.
#'
#' @param dat A `data.frame` with the responses in long format (one row per trial).
#' @param grouping The factors used to group the d' calculation by (you probably at least want your participant id there). This should be specified as a vector of strings.
#' @param presented A string containing the name of the `data.frame` field containing the names of the presented categories. (Defaults to "Presented")
#' @param responded A string containing the name of the `data.frame` field containing the names of the responded categories. (Defaults to "Responded")
#'
#' @return A `data.frame` with the grouping factors, the presented factor, and fields `log.alpha.i` (which is the sensitivity measure)
#' and `bias...` for each pair of category (because the bias measure is relative).
#'
#' @references The calculation are made according to Macmillan and Creelman (2005), p.247.
#' @export


ChoiceTheory = function(dat, grouping, presented='Presented', responded='Responded'){
    if(missing(grouping)){
        df = NULL
        presented.stimuli = unique(dat[[presented]])
        m = length(presented.stimuli)

        for(i in presented.stimuli)
        {
            s = dat[[presented]]==i
            nSi = sum(s)

            if(nSi==0) {
                next
            }

            # P(Ri|Si)

            P.RiSi = sum(s & dat[[responded]]==i) / nSi


            if(P.RiSi==0){
                P.RiSi = .5/nSi
            }

            # P(Rj|Si)
            Slog.odd.i = 0
            for(j in presented.stimuli){
                P.RjSi = sum(s & dat[[responded]]==j) / nSi
                if(P.RjSi==0){
                    P.RjSi = .5/nSi
                }
                Slog.odd.i = Slog.odd.i + log(P.RiSi/P.RjSi)
            }
            Slog.odd.i = Slog.odd.i / (sqrt(2)*(m-1))

            df_i = data.frame(log.alpha.i=Slog.odd.i)
            df_i[[presented]] = i

            for(j in presented.stimuli){
                log.bi.bj = 0
                for(k in presented.stimuli){
                    nSk = sum(dat[[presented]]==k)
                    P.RiSk = sum(dat[[responded]]==i & dat[[presented]]==k) / nSk
                    P.RjSk = sum(dat[[responded]]==j & dat[[presented]]==k) / nSk
                    if(P.RiSk==0){
                        P.RiSk = .5/nSk
                    }
                    if(P.RjSk==0){
                        P.RjSk = .5/nSk
                    }
                    log.bi.bj = log.bi.bj + log(P.RiSk/P.RjSk)
                }
                log.bi.bj = log.bi.bj / m
                df_i[[paste('bias', j, sep='.')]] = log.bi.bj
            }

            df = rbind(df, df_i)

        }

        return(df)

    } else {
        d = data.frame()
        grp_levels = interaction(dat[,grouping])
        for(g in levels(grp_levels)){
            s = (grp_levels == g)
            if(sum(s)==0){
                next
            }
            d_i = ChoiceTheory(dat[s,], presented=presented, responded=responded)
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
