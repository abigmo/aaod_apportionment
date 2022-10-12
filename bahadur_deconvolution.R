## Function to apportion AAOD following Bahadur et al. 2012 PNAS (mainly eq. 4)
## Alessandro Bigi (alessandro.bigi@unimore.it)
## University of Modena and Reggio Emilia
## December 2021
## originally applied to level 1.5(maybe?) version 2 AERONET
## Here it is used for version 3

### aaod.870 = aaod.870.bc + aaod.870.dust
### aaod.675 = aaod.870.bc * (675/870)^aae2.bc + aaod.870.dust * (675/870)^aae2.dust + aaod.440.brc * (675/440)^aae1.brc
### aaod.440 = aaod.675.bc * (440/675)^aae1.bc + aaod.675.dust * (440/675)^aae1.dust + aaod.675.brc * (440/675)^aae1.brc

## building LHS of the equation system
lhs.set <- function(aaod.870, aaod.675, aaod.440){
    lhs <- matrix(c(aaod.870,
                    aaod.675,
                    aaod.440,
                    rep(0,5)), nrow = 8)
    lhs } ## return the LHS 

## buidling RHS of the equation system
## elements are: aaod.870.bc, aaod.870.dust,
## aaod.675.bc, aaod.675.dust, aaod.675.brc
## aaod.440.bc, aaod.440.dust, aaod.440.brc
rhs.set <- function(aae2.bc, aae2.dust, aae1.bc, aae1.dust, aae1.brc, w1, w2){
    rhs <- matrix(c(1, 1, ## first eq.
                    rep(0,3),
                    rep(0,3),
                    0, 0, ## second eq.
                    rep(1, 3),
                    rep(0, 3),
                    0, 0, ## third eq.
                    0, 0, 0,
                    1, 1, 1,
                    w2^aae2.bc, 0,  ## fourth eq.
                    -1, 0, 0,
                    0, 0, 0,
                    0, w2^aae2.dust, ## fifth eq.
                    0, -1, 0,
                    0, 0, 0,
                    0, 0, ## sixth eq.
                    0, 0, -1,
                    0, 0, (1/w1)^aae1.brc,
                    0, 0, ## seventh eq.
                    w1^aae1.bc, 0, 0,
                    -1, 0, 0,
                    0, 0, ## eight eq.
                    0, w1^aae1.dust, 0,
                    0, -1, 0),
                  byrow = TRUE,
                  ncol = 8)
    rhs } ## return the RHS

## Modena AERONET values
## aae1.bc median = 1.12 +/- 0.11 (mad)
## aae2.bc median = 1.10 +/- 0.11 (mad)
## aae1.dust median = 2.83 +/- 0.69 (mad)
## aae2.dust median = 1.06 +/- 0.57 (mad)
## aae1.brc median = 4.35 +/- 1.28 (mad)

## Ispra AERONET values
## aae1.bc median = 1.11 +/- 0.10 (mad)
## aae2.bc median = 1.13 +/- 0.10 (mad) 
## aae1.dust median = 3.51 +/- 0.97 (mad)
## aae2.dust median = 0.97 +/- 0.56 (mad) 
## aae1.brc median = 4.33 +/- 1.04 (mad)

## Bahadur et al. values
## aae1.bc <- -0.55 ##+/- 0.24
## aae2.bc <- -0.85 ##+/- 0.40
## aae1.dust <- -2.20 ##+/- 0.50
## aae2.dust <- -1.15 ##+/- 0.50
## aae1.brc <- -4.55 ##+/- 2.01


a.q <- NULL ## dummy variable to store the results
w2 <- 675/870
w1 <- 440/675

## input dataset is the AERONET data with the expected column names are:
## Absorption_AOD.870nm.
## Absorption_AOD.675nm.
## Absorption_AOD.440nm.
## The dataset should be shaped as a rectangle with 3 columns for AAOD and eventually one column for date named `date'
input.dataset <- USE.YOUR.INPUT

for (j in 1:nrow(input.dataset)){

    print(paste0("analyzing row ", j, " of ", nrow(input.dataset)))
    print("")

    ## apportionment of Modena
    aaod.870 <- input.dataset$Absorption_AOD.870nm.[j]
    aaod.675 <- input.dataset$Absorption_AOD.675nm.[j]
    aaod.440 <- input.dataset$Absorption_AOD.440nm.[j]

    a <- data.frame(matrix(NA, nrow = 0, ncol = 13))

    ## ~~~~~~~~ ##
    ## parallel ##
    ## ~~~~~~~~ ##
    library(foreach)
    library(doParallel)

    ## setup parallel backend to use more processors
    cores <- detectCores()
    cl <- makeCluster(cores[1] - 1) ## not to overload your computer
    registerDoParallel(cl)

    a <- foreach(i = 1:1E4, .combine = rbind, .packages=c("magrittr")) %dopar% {

        ## Modena values
        ## aae1.bc <- rnorm(1, -1.12, 0.11)
        ## aae2.bc <- rnorm(1, -1.10, 0.11)
        ## aae1.dust <- rnorm(1, -2.83, 0.69)
        ## aae2.dust <- rnorm(1, -1.06, 0.57)
        ## aae1.brc <- rnorm(1, -4.35, 1.28) 

        ## Ispra values
        aae1.bc <- rnorm(1, -1.11, 0.10)
        aae2.bc <- rnorm(1, -1.13, 0.10)
        aae1.dust <- rnorm(1, -3.51, 0.97)
        aae2.dust <- rnorm(1, -0.97, 0.56)
        aae1.brc <- rnorm(1, -4.33, 1.04) 

        lhs <- lhs.set(aaod.870, aaod.675, aaod.440)
        rhs <- rhs.set(aae2.bc, aae2.dust, aae1.bc, aae1.dust, aae1.brc, w1, w2)
        
        a <- solve(rhs, lhs) %>%
            as.data.frame() %>%
            t() %>%
            cbind.data.frame(aae1.bc, aae2.bc, aae1.dust, aae2.dust, aae1.brc, .)
        
        a
    }

    ## stop cluster
    stopCluster(cl)

    names(a) <- c("aae1.bc", "aae2.bc", "aae1.dust", "aae2.dust", "aae1.brc", "aaod.870.bc", "aaod.870.dust", "aaod.675.bc", "aaod.675.dust", "aaod.675.brc", "aaod.440.bc", "aaod.440.dust", "aaod.440.brc")
    
    ## keep only row where the portions are positive
    a.q <- apply(a[,6:13], 1, function(x) sum(x >= 0)) %>% ## find the rows with all positive values
        data.frame("pos" = ., a) %>% ## filter works only on tbl/dataframe
        dplyr::filter(pos == 8) %>%  ## filter only positive
        dplyr::select(-pos) %>%
        summarise_all( .funs = list( median = ~ quantile(x = ., probs = 0.5, na.rm = TRUE),
                                    q75 = ~ quantile(., probs = 0.75, na.rm = TRUE),
                                    q25 = ~ quantile(., probs = 0.25, na.rm = TRUE))) %>%
        cbind.data.frame("date" = input.dataset$date[j], .) %>% ## add date
        rbind.data.frame(a.q, .) ## add to the list
}   


## let's remove the attributes
for (i in 1:ncol(a.q)){
    attr(a.q[,i],"names") <- NULL
}

## now let's do a scatterplot of the reconstructed sum and the observed sum
plot( USE.YOUR.INPUT$Absorption_AOD.440nm., rowSums(a.q[, grep("440.*_median", names(a.q))]), cex = .1)
abline(0,1, lwd =.5, col = 2, lty = 2)

points( USE.YOUR.INPUT$Absorption_AOD.870nm., rowSums(a.q[, grep("870.*_median", names(a.q))]), cex = .1, col = 3)

points( USE.YOUR.INPUT$Absorption_AOD.675nm., rowSums(a.q[, grep("675.*_median", names(a.q))]), cex = .1, col = 4)

require("ggplot2")

## plot the time series of AAE ##
## Figure S3 ##
a.qq <- a.q %>%
    mutate("date.hh" = as.POSIXct(floor ( as.numeric( date - 0 ) / 60 / 60) * 60 * 60, origin = "1970-01-01") + 3600) %>% ## find in which 5 minute slots is the data
    ## if you want you can remove data with AOD_440 > 0.1
    ## merge(., l15.all[-1, c("date.hh", "AOD_Extinction.Total.440nm.")], by = "date.hh") %>% ## 
    ## dplyr::filter(AOD_Extinction.Total.440nm. > 0.1) %>%
    mutate(row = 1:nrow(.)) %>%
    dplyr::select(matches("date$|aae|row")) %>%
    tidyr::pivot_longer(., cols = -c("date","row")) %>% ## make it all long, but keep time as an ID separated columns
    tidyr::separate(., col = name, sep = "_", into = c("poll", "stat")) %>% ## split the column "name" in two, and it fills two new columns with the result
    tidyr::pivot_wider(., names_from = c(stat), values_from = c(value)) %>%
    mutate(poll = factor(poll, levels = unique(poll))) %>%
    mutate("lab" = format.POSIXct(date, "%d %b"))

pp <- a.qq %>%
    ggplot(., aes (x = row, y = -median)) +
    geom_point(size = 0.3) +
    scale_x_continuous(breaks = a.qq$row[seq(1, nrow(a.qq), 150)], labels = a.qq$lab[seq(1, nrow(a.qq), 150)] ) +
    geom_errorbar(aes(ymin = -q25, ymax = -q75), width = 0, size = 0.1) + 
    ggtitle("YOUR SITE") +
    theme(
        plot.title = element_text(hjust = .5, vjust = 0, size = 8, margin=margin(t=0, b=.2), face = "bold"),
        panel.spacing = unit(.1, "lines"),
        plot.margin = margin(2, 1, 1, 1),
        legend.position = c(.05,.95),
        legend.text = element_text(size = 8),
        legend.key = element_rect(fill = NA),
        legend.background = element_blank(),
        legend.title = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray", linetype=2, size=.1),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_line(size = .1),
        axis.ticks.length=unit(.5, "mm"),
        axis.line = element_line(size = 0.1, linetype = "solid", colour = "black"),
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text.y = element_text(size = 8, margin = margin(0,0,0,0, "mm"))) +
    facet_wrap( ~ poll, ncol = 2, strip.position = "left", scales="free", labeller = as_labeller( c("aae1.bc" = "AAE1 BC", "aae2.bc" = "AAE2 BC", "aae1.dust" = "AAE1 Dust", "aae2.dust" = "AAE2 Dust", "aae1.brc" = "AAE1 BrC")))

print(pp)

## scatterplot works just very fine
## let's create manually a stacked line, summing AAOD from BC to dust, and then dust+bc to BrC.
## Figure S6 ##
a.qq <-  a.q %>%
    mutate("date.hh" = as.POSIXct(floor ( as.numeric( date - 0 ) / 60 / 60) * 60 * 60, origin = "1970-01-01") + 3600) %>% ## find in which 5 minute slots is the data
    ## if you want, include the AOD_440 and remove data with AOD_440 > 0.1
    ## merge(., USE.YOUR.INPUT[-1,c("date.hh", "AOD_Extinction.Total.440nm.")], by = "date.hh") %>%
    ## dplyr::filter(AOD_Extinction.Total.440nm. > 0.1) %>%
    dplyr::select(matches("date$|aaod")) %>%
    mutate_at(vars(contains("870.dust")), ~ (. + aaod.870.bc_median)) %>% ## summing 870.bc median to all 870.dust (median, 75, 25)
    mutate_at(vars(contains("675.dust")), ~ (. + aaod.675.bc_median)) %>% ## summing 675.bc median to all 675.dust (median, 75, 25)
    mutate_at(vars(contains("675.brc")), ~ (. + aaod.675.dust_median)) %>% ## adding also 675 brc to 675 dust median
    mutate_at(vars(contains("440.dust")), ~ (. + aaod.440.bc_median)) %>% 
    mutate_at(vars(contains("440.brc")), ~ (. + aaod.440.dust_median)) %>% 
    mutate(row = 1:nrow(.)) %>% 
    dplyr::select(matches("date|aaod|row")) %>% 
    tidyr::pivot_longer(., cols = -c("date","row")) %>% ## make it all long, but keep time as an ID separated columns
    tidyr::separate(., col = name, sep = "_", into = c("poll", "stat")) %>% ## split the column "name" in two, and it fills two new columns with the result
    tidyr::pivot_wider(., names_from = c(stat), values_from = c(value)) %>% 
    mutate("wavel" = gsub("aaod|\\.|[aA-zZ]", "",  poll)) %>% ## build new column with wavelength
    mutate("poll" = gsub("\\.\\d\\d\\d","", poll)) %>% ## build new column with species
    mutate(poll = dplyr::recode(poll, aaod.bc = "BC", aaod.dust = "Dust", aaod.brc = "BrC")) %>%
    mutate(poll = factor(poll, levels = unique(poll))) %>%
    mutate(wavel = factor(wavel, levels = c("870", "675", "440"))) %>% ## new column with wavelength
    mutate("lab" = format.POSIXct(date, "%d %b"))

p <- a.qq %>%
    ggplot(., aes (x = row, y = median, colour = poll)) +
    geom_line(size = 0.5) +
    scale_x_continuous(breaks = a.qq$row[seq(1, nrow(a.qq), 150)], labels = a.qq$lab[seq(1, nrow(a.qq), 150)] ) +
    scale_colour_manual(values = c("BC" = "gray10", "Dust" = "burlywood", "BrC" = "forestgreen")) +
    geom_errorbar(aes(ymin = q25, ymax = q75), width = 0, size = 0.2) +
    ggtitle("YOUR SITE") +
    theme(
        plot.title = element_text(hjust = .5, vjust = -1, size = 10, margin=margin(t=0, b=0), face = "bold"),
        panel.spacing = unit(.1, "lines"),
        plot.margin = margin(2, 1, 1, 1),
        legend.position = c(.90,.98),
        legend.text = element_text(size = 8),
        legend.key = element_rect(fill = NA),
        legend.background = element_blank(),
        legend.title = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray", linetype=2, size=.1),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_line(size = .1),
        axis.ticks.length=unit(.5, "mm"),
        axis.line = element_line(size = 0.1, linetype = "solid", colour = "black"),
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text.y = element_text(size = 8, margin = margin(0,0,0,0, "mm"))) +
    facet_wrap( ~ wavel, ncol = 1, strip.position = "left", scales="free", labeller = as_labeller( c("870" = "AAOD, 870 nm", "675" = "AAOD, 675 nm", "440" = "AAOD, 440 nm")))

print(p)

