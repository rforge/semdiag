% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/webpower.R
\name{wp.blcsm}
\alias{wp.blcsm}
\title{Statistical Power Curve for Bivariate Latent Change Score Models based on Monte Carlo Simulation}
\usage{
wp.blcsm(N = 100, T = 5, R = 1000, betay = 0, my0 = 0, mys = 0,
  varey = 1, vary0 = 1, varys = 1, vary0ys = 0, alpha = 0.05,
  betax = 0, mx0 = 0, mxs = 0, varex = 1, varx0 = 1, varxs = 1,
  varx0xs = 0, varx0y0 = 0, varx0ys = 0, vary0xs = 0, varxsys = 0,
  gammax = 0, gammay = 0, ...)
}
\arguments{
\item{N}{Sample size. It is 100 by default.}

\item{T}{Number of measurement occasions. It is 5 by default.}

\item{R}{Number of replications for the Monte Carlo simulation. It is 1000 by default.}

\item{betay}{Parameter in the model: The compound rate of change for variable y. Its default value is 0.}

\item{my0}{Parameter in the model: Mean of the initial latent score for variable y. Its default value is 0.}

\item{mys}{Parameter in the model: Mean of the linear constant effect for variable y. Its default value is 0.}

\item{varey}{Parameter in the model: Variance of the measurement error/uniqueness score for variable y. Its default value is 1.}

\item{vary0}{Parameter in the model: Variance of the initial latent score for variable y. Its default value is 1.}

\item{varys}{Parameter in the model: Variance of the linear constant effect for variable y. Its default value is 0.}

\item{vary0ys}{Parameter in the model: Covariance of the initial latent score and the linear constant effect for variable y. Its default value is 0.}

\item{alpha}{significance level chosed for the test. It equals 0.05 by default.}

\item{betax}{Parameter in the model: The compound rate of change for variable x. Its default value is 0.}

\item{mx0}{Parameter in the model: Mean of the initial latent score for variable x. Its default value is 0.}

\item{mxs}{Parameter in the model: Mean of the linear constant effect for variable x. Its default value is 0.}

\item{varex}{Parameter in the model: Variance of the measurement error/uniqueness score for variable x. Its default value is 1.}

\item{varx0}{Parameter in the model: Variance of the initial latent score for variable x. Its default value is 1.}

\item{varxs}{Parameter in the model: Variance of the linear constant effect for variable x. Its default value is 0.}

\item{varx0xs}{Parameter in the model: Covariance of the initial latent score and the linear constant effect for variable x. Its default value is 0.}

\item{varx0y0}{Parameter in the model: Covariance of the initial latent scores for y and x. Its default value is 0.}

\item{varx0ys}{Parameter in the model: Covariance of the initial latent score for x and the linear constant effect for y. Its default value is 0.}

\item{vary0xs}{Parameter in the model: Covariance of the initial latent score for y and the linear constant effect for x. Its default value is 0.}

\item{varxsys}{Parameter in the model: Covariance of the linear constant effects for y and x. Its default value is 0.}

\item{gammax}{Coupling parameter in the model: The effect of variable x on the change score of variable y. Its default value is 0.}

\item{gammay}{Coupling parameter in the model: The effect of variable y on the change score of variable x. Its default value is 0.}

\item{...}{Extra arguments. It is not required.}
}
\value{
An object of the power analysis.
The output of the R function includes 4 main pieces of information for each parameter in the model. The first is the Monte Carlo estimate (mc.est).
It is calculated as the mean of the R sets of parameter estimates from the simulated data.
Note that the Monte Carlo estimates should be close to the population parameter values used in the model.
The second is the Monte Carlo standard deviation (mc.sd), which is calculated as the standard deviation of the R sets of parameter estimates.
The third is the Monte Carlo standard error (mc.se), which is obtained as the average of the R sets of standard error estimates of the parameter estimates.
Lastly, mc.power is the statistical power for each parameter.
}
\description{
A longitudinal design often involves data collection on multiple variables from multiple participants at multiple times.
Growth curve models (GCM) are structural equation models for longitudinal data analysis (\cite{McArdle & Epstein, 1987; McArdle & Nesselroade, 2014}). 
Latent change score models (LCSM) combine difference equations with growth curves to investigate change in longitudinal studies .
LCSM provied an efficient way to model nonlinear trajectory (e.g., \cite{McArdle, 2000; McArdle & Hamagami, 2001; Hamagami et al., 2010}).
This function is used to conduct power analysis for bivariate LCSMs based on a Monte Carlo method ( a method also used by \cite{Muthén & Muthén, 2002; Thoemmes et al., 2010; Zhang & Wang, 2009; Zhang, 2014}).
For each Monte Carlo replication, the Maximum likelihood ratio test is used for the model, while the Wald test is used for the parameter test.
The method can obtain the power for testing each individual parameter of the models such as the change rate and coupling parameters.
}
\examples{
\dontrun{
#To conduct power analysis for a bivariate LCSM with sample size equal to 100:
wp.blcsm(N=100, T=5, R=1000, betay=0.08, my0=20, mys=1.5, varey=9,
     vary0=3, varys=1, vary0ys=0, alpha=0.05, betax=0.2, mx0=20, mxs=5,
         varex=9, varx0=3, varxs=1, varx0xs=0, varx0y0=1, varx0ys=0,
                            vary0xs=0, varxsys=0, gammax=0, gammay=-.1)
#             pop.par mc.est  mc.sd  mc.se  mc.power N T
#    betax    0.20     0.230  0.260  0.187  0.241   100 5
#    betay    0.08     0.164  0.572  0.435  0.081   100 5
#    gammax   0.00    -0.033  0.234  0.178  0.112   100 5
#    gammay  -0.10    -0.175  0.641  0.458  0.075   100 5
#    mx0     20.00    20.004  0.336  0.326  1.000   100 5
#    mxs      5.00     5.933  7.848  5.615  0.167   100 5
#    my0     20.00    20.019  0.346  0.326  1.000   100 5
#    mys      1.50     0.451  6.933  5.321  0.156   100 5
#    varex    9.00     8.941  0.744  0.732  1.000   100 5
#    varey    9.00     8.939  0.749  0.720  1.000   100 5
#    varx0    3.00     3.029  1.243  1.222  0.739   100 5
#    varx0xs  0.00    -0.210  0.768  0.767  0.030   100 5
#    varx0y0  1.00     1.052  0.840  0.835  0.226   100 5
#    varx0ys  0.00    -0.012  0.668  0.601  0.017   100 5
#    varxs    0.60     2.343  6.805  2.687  0.090   100 5
#    varxsys  0.00     0.072  3.559  1.740  0.019   100 5
#    vary0    3.00     2.951  1.423  1.245  0.684   100 5
#    vary0xs  0.00     0.198  2.263  1.629  0.031   100 5
#    vary0ys  0.00    -0.371  1.970  1.511  0.106   100 5
#    varys    0.05     1.415  3.730  2.096  0.024   100 5

#To conduct power analysis for a bivariate LCSM with sample size equal to 500:
wp.blcsm(N=500, T=5, R=1000, betay=0.08, my0=20, mys=1.5, varey=9,
      vary0=3, varys=1, vary0ys=0, alpha=0.05, betax=0.2, mx0=20
           , mxs=5, varex=9, varx0=3, varxs=1, varx0xs=0, varx0y0=1,
                 varx0ys=0, vary0xs=0, varxsys=0, gammax=0, gammay=-.1)
#           pop.par mc.est mc.sd mc.se  mc.power N  T
#    betax    0.20  0.2009 0.031 0.031   1.000  500 5
#    betay    0.08  0.0830 0.070 0.068   0.199  500 5
#    gammax   0.00 -0.0014 0.030 0.029   0.057  500 5
#    gammay  -0.10 -0.1022 0.072 0.073   0.271  500 5
#    mx0     20.00 19.9911 0.145 0.145   1.000  500 5
#    mxs      5.00  5.0308 0.939 0.942   1.000  500 5
#    my0     20.00 19.9999 0.143 0.146   1.000  500 5
#    mys      1.50  1.4684 0.889 0.885   0.420  500 5
#    varex    9.00  8.9836 0.340 0.328   1.000  500 5
#    varey    9.00  8.9961 0.341 0.328   1.000  500 5
#    varx0    3.00  3.0052 0.524 0.523   1.000  500 5
#    varx0xs  0.00 -0.0144 0.222 0.230   0.047  500 5
#    varx0y0  1.00  1.0064 0.360 0.360   0.808  500 5
#    varx0ys  0.00 -0.0012 0.199 0.201   0.051  500 5
#    varxs    1.00  1.0312 0.180 0.189   1.000  500 5
#    varxsys  0.00  0.0028 0.161 0.163   0.045  500 5
#    vary0    3.00  2.9777 0.519 0.547   1.000  500 5
#    vary0xs  0.00  0.0072 0.286 0.294   0.035  500 5
#    vary0ys  0.00 -0.0135 0.252 0.257   0.043  500 5
#    varys    1.00  1.0246 0.260 0.253   0.999  500 5
}

}
\references{
Hamagami, F., Zhang, Z., & McArdle, J. J. (2010). Bayesian discrete dynamic system by latent difference score structural equations models for multivariate repeated measures data. In S.-M. Chow, E. Ferrer, & F. Hsieh (Eds.), Statistical methods for modeling human dynamics: An interdisciplinary dialogue (pp. 319–348). New York, NY: RoutledgeTaylor & Francis Group.

McArdle, J. J. & Epstein, D. (1987). Latent growth curves within developmental structural equation models. Child Psychology, 58, 110–133.

McArdle, J. J. & Nesselroade, J. R. (2014). Longitudinal data analysis using structural equation models. American Psychological Association.

McArdle, J. J. (2000). A latent difference score approach to longitudinal dynamic structural analyses. In R. Cudeck, S. du Toit, & D. Sôrbom (Eds.), Structural Equation Modeling: Present and future (pp. 342–380). Lincolnwood, IL: Scientific Software International.

McArdle, J. J. & Hamagami, F. (2001). Latent difference score structural models for linear dynamic analyses with incomplete longitudinal data. In L. M. Collins & A. G. Sayer (Eds.), New methods for the analysis of change (pp. 139–175). Washington, DC: American Psychological Association.

Muthén, L. K. & Muthén, B. O. (2002). How to use a monte carlo study to decide on sample size and determine power. Structural Equation Modeling, 9(4), 599–620.

Rosseel, Y. (2012). Lavaan: An R package for structural equation modeling and more. Version 0.5–12 (BETA). Ghent, Belgium: Ghent University.

Thoemmes, F., MacKinnon, D. P., & Reiser, M. R. (2010). Power analysis for complex mediational designs using monte carlo methods. Structural Equation Modeling, 17, 510–534.

Zhang, Z. (2014). Monte carlo based statistical power analysis for mediation models: Methods and software. Behavior research methods, 46(4), 1184–1198.

Zhang, Z. & Wang, L. (2009). Statistical power analysis for growth curve models using sas. Behavior Research Methods, 41, 1083–1094.
}
