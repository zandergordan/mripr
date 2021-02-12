#' Calculate catch
#'
#' @param intdir the path and main directory (in quotes) under which raw intercept sub-directories are stored.
#' @param common the common name of a species provided at the MRFSS website in quotes (e.g., "STRIPED BASS".). If common = NULL, quantities for all species are generated.
#' @param st state(s) code of data to include, as an integer.  A complete list of state codes is provided at the MRFSS website. FL = 12
#' @param styr Start year
#' @param endyr End year
#' @param dom optional custom domain
#'
#' @return a dataframe with catch estimates and PSEs
#' @export
#'
#'@examples calculate_catch(intdir = system.file("extdata", package = "mripr"),
#'                          common = 'GAG',
#'                          st = 12,
#'                          styr = 2018,
#'                          endyr = 2018,
#'                          dom = list(mode_fx = list(c(1,2,3,4,5,7)),
#'                                     area_x = list(c(1,2,3,4,5)),
#'                                     sub_reg = list(c(7)))
#'                         )[c("Domain", "total.catch")]
#'                         
calculate_catch <- function(
  intdir = NULL,
  common = NULL,
  st = NULL,
  styr = NULL,
  endyr = NULL,
  dom = NULL
) {
  if (is.null(intdir)) stop("Need main directory location of intercept files.")
  if (is.null(st)) stop("No state code was specified.")
  if (is.null(styr)) stop("Starting year is missing.")
  if (is.null(endyr)) stop("Ending year is missing.")
  # If species equal NULL all species are used
  if (length(grep("/", intdir)) == 1) {
    din <- ifelse(
      substr(intdir, nchar(intdir), nchar(intdir)) %in% "/",
      paste0(intdir, "int"),
      paste0(intdir, "/int")
    )
  }
  if (length(grep("\\\\", intdir)) == 1) {
    din <- ifelse(substr(intdir, nchar(intdir), nchar(intdir)) %in% "\\",
      paste0(intdir, "int"), paste0(intdir, "\\int")
    )
  }

  if (!is.null(common)) common <- tolower(common)
  st <- as.character(st)
  styr <- as.character(styr)
  endyr <- as.character(endyr)
  wave <- as.character(c(1, 2, 3, 4, 5, 6))
  temp <- NULL
  temp1 <- NULL
  rbind2 <- function(input1, input2) {
    if (!is.null(ncol(input1))) {
      n_input1 <- ncol(input1)
      n_input2 <- ncol(input2)

      if (n_input2 < n_input1) {
        tf_names <- which(names(input2) %in% names(input1))
        column_names <- names(input2[, tf_names])
      }
      else {
        tf_names <- which(names(input1) %in% names(input2))
        column_names <- names(input1[, tf_names])
      }
      return(rbind(input1[, column_names], input2[, column_names]))
    }
    if (is.null(ncol(input1))) {
      return(rbind(input1, input2))
    }
  }

  for (yr in styr:endyr) {
    for (j in 1:as.numeric(length(wave))) {
      # Get catch
      wv <- wave[j]
      t3 <- utils::read.csv(
        paste0(din, yr, "/", "catch_", yr, wv, ".csv"),
        colClasses = "character"
      )
      t3 <- t3[t3$ST %in% st, ]
      names(t3) <- tolower(names(t3))
      temp <- methods::rbind2(temp, t3)
      # Get trips
      t4 <- utils::read.csv(
        paste0(din, yr, "/", "trip_", yr, wv, ".csv"),
        colClasses = "character"
      )
      t4 <- t4[t4$ST %in% st, ]
      names(t4) <- tolower(names(t4))
      temp1 <- methods::rbind2(temp1, t4)
    }
  }
  convtolow <- function(x) {
    for (i in seq_len(ncol(x))) {
      x[, i] <- tolower(x[, i])
    }
    return(x)
  }
  temp <- convtolow(temp)
  temp1 <- convtolow(temp1)
  temp <- temp[, c(
    "common", "strat_id", "psu_id", "st", "id_code", "sp_code",
    "claim", "release", "harvest",
    "tot_len_a", "wgt_a",
    "tot_len_b1", "wgt_b1",
    "fl_reg", "tot_cat",
    "wgt_ab1", "tot_len", "landing"
  )]
  temp <- temp[order(temp$strat_id, temp$psu_id, temp$id_code), ]
  temp1 <- temp1[order(temp1$strat_id, temp1$psu_id, temp1$id_code), ]
  dataset <- merge(temp1, temp,
                   by.x = c("strat_id", "psu_id", "id_code", "st"),
                   by.y = c("strat_id", "psu_id", "id_code", "st"),
                   all.x = FALSE,
                   all.y = FALSE
  )
  dataset$common <- as.character(dataset$common)
  dataset$common <- ifelse(is.na(dataset$common), "", dataset$common)
  if (!is.null(common)) {
    if (!any(dataset$common == common)) stop("Species not found.")
  }
  # Construct Domain
  dom_ids <- NULL
  mainlev <- length(dom)
  if (length(dom) > 0) {
    for (l in 1:mainlev) {
      if (!any(names(dom)[l] == names(dataset))) {
        stop(paste("Variable ", names(dom[l]), "not found in MRIP dataset"))
      }
      if (any(names(dom)[l] == names(dataset))) {
        dataset[, ncol(dataset) + 1] <- "DELETE"
        names(dataset)[ncol(dataset)] <- paste0(names(dom)[l], "1")
        colpos <- which(names(dataset) == names(dom[l]))
        sublev <- length(dom[[l]])
        for (k in 1:sublev) {
          dataset[, ncol(dataset)] <- ifelse(
            dataset[, colpos] %in% as.character(dom[[l]][[k]]),
            paste0(names(dom)[l], k),
            dataset[, ncol(dataset)]
          )
        }
        dom_ids <- c(dom_ids, names(dom[l]))
      }
    }
    test <- c("year", "wave", "st", "sub_reg", "mode_fx", "area_x")
    for (gg in 1:as.numeric(length(dom_ids))) {
      if (!any(dom_ids[gg] == test)) {
        test[as.numeric(length(test) + 1)] <- paste0(dom_ids[gg], "1")
      }
      if (any(dom_ids[gg] == test)) {
        colpos <- which(test == dom_ids[gg])
        test[colpos] <- paste0(dom_ids[gg], "1")
      }
    }

    ## add Non-domain names to values in columns
    for (gg in 1:as.numeric(length(test))) {
      if (substr(test[gg], nchar(test[gg]), nchar(test[gg])) != "1") {
        eval(parse(text = paste0(
          "dataset$",
          test[gg],
          '<-paste("',
          as.character(test[gg]),
          '",dataset$',
          test[gg],
          ',sep="")'
        )))
      }
    }
    for (hh in 1:as.numeric(length(test))) {
      if (hh == 1) texter <- paste0("dataset$", test[hh])
      if (hh > 1) texter <- paste0(texter, ",", "dataset$", test[hh])
    }
    eval(parse(text = paste0("dataset$dom_id<-c(paste(", texter, ",sep=''))")))
  }

  if (length(dom) == 0) {
    dataset$year <- paste0("year", dataset$year)
    dataset$wave <- paste0("wave", dataset$wave)
    dataset$st <- paste0("st", dataset$st)
    dataset$sub_reg <- paste0("sub_reg", dataset$sub_reg)
    dataset$mode_fx <- paste0("mode_fx", dataset$mode_fx)
    dataset$area_x <- paste0("area_x", dataset$area_x)
    dataset$dom_id <- paste0(dataset$year, dataset$wave,
                             dataset$st, dataset$sub_reg,
                             dataset$mode_fx, dataset$area_x
    )
  }
  dataset$tot_cat <- as.numeric(dataset$tot_cat)
  dataset$landing <- as.numeric(dataset$landing)
  dataset$claim <- as.numeric(dataset$claim)
  dataset$harvest <- as.numeric(dataset$harvest)
  dataset$release <- as.numeric(dataset$release)
  dataset$wgt_ab1 <- as.numeric(dataset$wgt_ab1)
  dataset$wp_int <- as.numeric(dataset$wp_int)


  if (!is.null(common)) {
    dataset$dcomm <- common
    dataset$dtotcat <- ifelse(dataset$common == common, dataset$tot_cat, 0)
    dataset$dlandings <- ifelse(dataset$common == common, dataset$landing, 0)
    dataset$dclaim <- ifelse(dataset$common == common, dataset$claim, 0)
    dataset$dharvest <- ifelse(dataset$common == common, dataset$harvest, 0)
    dataset$drelease <- ifelse(dataset$common == common, dataset$release, 0)
    dataset$dwgt_ab1 <- ifelse(dataset$common == common, dataset$wgt_ab1, 0)
    dataset$dom_id <- paste0(dataset$dom_id, common)
    dataset1 <- stats::aggregate(
      cbind(
        dataset$dtotcat, dataset$dlandings, dataset$dclaim, dataset$dharvest,
        dataset$drelease, dataset$dwgt_ab1
      ),
      list(dataset$strat_id, dataset$psu_id, dataset$id_code, dataset$wp_int, dataset$dom_id), sum
    )
    names(dataset1) <- c(
      "strat_id", "psu_id", "id_code", "wp_int", "dom_id", "total.catch",
      "harvest.A.B1", "claim.A", "reported.B1", "released.B2", "weight"
    )
    dfpc <- survey::svydesign(
      ids = ~psu_id, strata = ~strat_id,
      weights = ~wp_int, nest = TRUE, data = dataset1
    )
    options(survey.lonely.psu = "certainty")
    results <- survey::svyby(
      ~ total.catch + harvest.A.B1 + claim.A + reported.B1 + released.B2 + weight,
      ~dom_id,
      dfpc,
      survey::svytotal,
      vartype = c("se", "cv"),
      keep.names = FALSE
    )
    names(results) <- c(
      "Domain", "total.catch", "harvest.A.B1", "claim.A",
      "reported.B1", "released.B2", "weight", "SE.total.catch",
      "SE.harvest.A.B1", "SE.claim.A", "SE.reported.B1", "SE.released.B2",
      "SE.weight", "PSE.total.catch", "PSE.harvest.A.B1", "PSE.claim.A",
      "PSE.reported.B1", "PSE.released.B2", "PSE.weight"
    )
    if (length(grep("DELETE", results$Domain, fixed = TRUE)) > 0) {
      results <- results[-grep("DELETE", results$Domain, fixed = TRUE), ]
      results <- results[order(results$Domain), ]
    }
  }
  if (is.null(common)) {
    dataset$dom_id <- paste0(dataset$dom_id, dataset$common)
    dfpc <- survey::svydesign(
      ids = ~psu_id, strata = ~strat_id,
      weights = ~wp_int, nest = TRUE, data = dataset
    )
    options(survey.lonely.psu = "certainty")
    results <- survey::svyby(~ tot_cat + landing + claim + harvest + release + wgt_ab1,
      ~dom_id, dfpc, survey::svytotal,
      vartype = c("se", "cv"), keep.names = FALSE
    )
    names(results) <- c(
      "Domain", "total.catch", "harvest.A.B1", "claim.A",
      "reported.B1", "released.B2", "weight", "SE.total.catch",
      "SE.harvest.A.B1", "SE.claim.A", "SE.reported.B1", "SE.released.B2",
      "SE.weight", "PSE.total.catch", "PSE.harvest.A.B1", "PSE.claim.A",
      "PSE.reported.B1", "PSE.released.B2", "PSE.weight"
    )
    if (length(grep("DELETE", results$Domain, fixed = TRUE)) > 0) {
      results <- results[-grep("DELETE", results$Domain, fixed = TRUE), ]
    }
    results <- results[order(results$Domain), ]
  }
  return(results)
}
