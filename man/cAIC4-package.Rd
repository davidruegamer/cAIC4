\name{cAIC4-package}
\alias{cAIC4-package}
\alias{cAIC4}
\docType{package}
\title{
\packageTitle{cAIC4}
}
\description{
\packageDescription{cAIC4}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{cAIC4}
\packageIndices{cAIC4}
}
\author{
\packageAuthor{cAIC4}

Maintainer: \packageMaintainer{cAIC4}
}
\references{

Saefken, B., Kneib T., van Waveren C.-S. and Greven, S. (2014) A unifying 
approach to the estimation of the conditional Akaike information in generalized 
linear mixed models. Electronic Journal Statistics Vol. 8, 201-225.

Greven, S. and Kneib T. (2010) On the behaviour of marginal and conditional AIC 
in linear mixed models. Biometrika 97(4), 773-789.

Efron , B. (2004) The estimation of prediction error. J. Amer. Statist. Ass. 
99(467), 619-632. 

}

\keyword{ package }
\seealso{
\code{\link[lme4:lme4-package]{lme4}}
}
\examples{
b <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)

cAIC(b)
}
