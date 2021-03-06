% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/summarySEwithin.R
\name{summarySEwithin}
\alias{summarySEwithin}
\title{Summarizes data, handling within-subjects variables by removing inter-subject variability.
Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
It will still work if there are no within-S variables.
Gives count, un-normed mean, normed mean (with same between-group mean),
standard deviation, standard error of the mean, and confidence interval.
If there are within-subject variables, calculate adjusted values using method from Morey (2008).}
\usage{
summarySEwithin(data,Value)
}
\arguments{
\item{data}{A data frame}

\item{measurevar:}{the name of a column that contains the variable to be summarized}

\item{betweenvars:}{a vector containing names of columns that are between-subjects variables}

\item{withinvars:}{a vector containing names of columns that are within-subjects variables}

\item{idvar:}{the name of a column that identifies each subject (or matched subjects)}

\item{na.rm:}{a boolean that indicates whether to ignore NA's}

\item{conf.interval:}{the percent range of the confidence interval (default is 95%)}
}
\description{
Summarizes data, handling within-subjects variables by removing inter-subject variability.
Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
It will still work if there are no within-S variables.
Gives count, un-normed mean, normed mean (with same between-group mean),
standard deviation, standard error of the mean, and confidence interval.
If there are within-subject variables, calculate adjusted values using method from Morey (2008).
}
\author{
R Cookbook (http://www.cookbook-r.com/Manipulating_data/Summarizing_data/)
}
\keyword{loading}
\keyword{package}

