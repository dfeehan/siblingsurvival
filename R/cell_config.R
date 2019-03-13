cell_config <- function(age.groups,
                        time.periods,
                        start.obs,
                        end.obs,
                        event,
                        age.offset,
                        time.offset,
                        exp.scale=1/12) {
  # TODO - eventually, we'll make this function more useful by
  #        having it come with a bunch of pre-specified options, age groups, etc

  cell_config_res <- list()

  if(is.character(age.groups)) {
    if (age.groups == '5yr') {
      min.age <- 15
      max.age <- 65
      age.groups <- age.gps5 <- make.even.age.groups(5, min.age=min.age, max.age=max.age)
    } else {
      stop(glue("No setting found for age.groups {age.groups}."))
    }
  } else {
    stop("No age groups specified.")
  }

  if(is.character(time.periods)) {
    if(time.periods == '5yr_beforeinterview') {
      time.periods <- make.time.periods(start=-12*5,
                                        durations=12*5,
                                        names=c("0-4"))
    } else {
      stop(glue("No setting found for time.periods {time.periods}."))
    }
  } else{
    stop("No time periods specified.")
  }

  cell_config_res$start.obs <- start.obs
  cell_config_res$end.obs <- end.obs
  cell_config_res$event <- event
  cell_config_res$age.groups <- age.groups
  cell_config_res$age.offset <- age.offset
  cell_config_res$time.periods <- time.periods
  cell_config_res$time.offset <- time.offset
  cell_config_res$exp.scale <- exp.scale

  class(cell_config_res) <- 'cell_config'

  return(cell_config_res)

}

############################################################
##' make labels for age groups
##'
##' helper function; see make.even.age.groups for more
##'
##' @param min.age the first age
##' @param max.age the highest age
##' @param width the width of each group
##' @return see make.age.groups
agenames <- function(aw, min.age, max.age) {

  vals.lhs <- seq(from=min.age, to=(max.age-aw), by=aw)
  vals.rhs <- seq(from=(min.age+aw), to=max.age, by=aw)
  return(as.character(glue::glue('[{vals.lhs},{vals.rhs})')))
}

############################################################
##' make an age.groups object with evenly-sized intervals
##'
##' see make.age.groups for more
##'
##' @param min.age the first age
##' @param max.age the highest age
##' @param width the width of each group
##' @return see make.age.groups
##' @export
make.even.age.groups <- function(min.age, max.age, width) {
  widths <- rep(width*12, ((max.age-min.age)/width))
  names <- agenames(width, min.age, max.age)
  return(make.age.groups(start=min.age*12,
                         widths=widths,
                         names=names))
}

############################################################
##' make an age.groups object
##'
##' TODO -- would be nice if this could take either widths
##' or bins / breaks
##'
##' @param start the first age
##' @param widths the widths of the subsequent age groups
##' @param names the names of the age groups
##' @return an object (list) with the widths, names, and
##' number of age groups, as well as a matrix called
##' template which has the start and end of each age interval.
##' the intervals in template are close on the left but not
##' the right; eg, exp.start of 10 and exp.end of 20
##' means [10, 20) in terms of exact ages
##' (this is useful later on, for making individual age
##'  schedules based on, eg, birth dates)
##' @export
make.age.groups <- function(start, widths, names) {

  lhs <- start + c(0, cumsum(widths[-length(widths)]))

  rhs <- lhs + widths

  template <- cbind(exp.start=lhs, exp.end=rhs)

  if (is.null(names)) {
    names <- paste(lhs)
  } else if (length(names) > nrow(template)) {
    warning("too many names specified for age groups; truncating...")
    names <- names[1:nrow(template)]
  }

  return(list(widths=widths,
              names=names,
              template=template,
              num.groups=nrow(template)))
}

############################################################
##' make a time.periods object
##'
##' @param start the start of the time of interest
##' @param durations the durations of the subsequent time periods
##' @param names the names of the time periods
##' @return an object (list) with the widths, names, and
##' number of time periods
##' as well as a matrix called
##' template which has the start and end of each time period.
##' the intervals in template are closed on the left but not
##' on the right; that is, start of 1900 and end of 1910
##' means [1900, 1910) in terms of exact times.
##' @export
make.time.periods <- function(start, durations, names) {

  lhs <- start + c(0, cumsum(durations[-length(durations)]))

  rhs <- lhs + durations

  template <- cbind(start=lhs, end=rhs)

  if (is.null(names)) {
    names <- paste(lhs)
  } else if (length(names) > nrow(template)) {
    warning("too many names specified for time periods; truncating...")
    names <- names[1:nrow(template)]
  }

  return(list(durations=durations,
              names=names,
              template=template,
              num.groups=nrow(template)))
}
