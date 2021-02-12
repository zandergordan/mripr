\name{MRIP.dirtrips}
\alias{MRIP.dirtrips}
\title{Calculate Number of Directed Angler Trips from Marine Recreational Information 
   Program (MRIP) Data
}
\description{Function is a translation of domain_directed_trips.SAS from the MRIP program. This program uses
  the catch_yyyyw.CSV and trip_yyyyw.CSV files provided on the MRIP website where yyyy is the year
   and w is the wave. 
}
\usage{MRIP.dirtrips(intdir = NULL, common = NULL, st = NULL, styr = NULL, endyr = NULL,
      trips = c(1, 2, 3, 4, 5), dom = NULL)
}
\arguments{
 \item{intdir}{the path and main directory (in quotes) under which raw intercept sub-directories are stored.}
 \item{common}{the common name of a species provided at the MRFSS website in quotes (e.g., "STRIPED BASS".)}
 \item{st}{state(s) code of data to include.  A complete list of state codes is provided at the MRFSS website.
	Atlantic and Gulf States: 23= ME, 33= NH, 25= MA, 44= RI, 9= CT, 
      36= NY, 34= NJ, 10= DE, 24= MD, 51= VA, 37= NC, 45= SC, 
      13= GA, 12= FL (121= East FL, 122= West FL), 1= AL,
      28= MS, and 22= LA. Any combination of states can be included in c().
      North Atlantic= c(9, 23, 25, 33, 44), 
	Mid Atlantic= c(10, 24, 34, 36, 51), 
	South Atlantic= c(13, 37, 45, 121), 
	Gulf of Mexico= c(1, 22, 28, 122).}
  \item{styr}{ the starting year of data to include.}
  \item{endyr}{  the ending year of data to include.}
  \item{trips}{ the type of catch information to include in the calculation: 
    1 = Primary Target, 
    2 = Secondary Target,
    3 = Caught (Available Catch Type A)
    4 = Harvested (Unavailable Catch Type B1)
    5 = Released (Unavailable Catch Type B2)
    Only one of these categories can be used at a time for accurate calculations.
}
  \item{dom}{domain definition for data analysis.}
}      
\details{
        The sub-directory for each year of intercept data should be named \emph{intyyyy} (e.g., int2013 for year 2013).
    Extracting and estimating quantities from the MRIP data require the user to define domains which are sub-population of the data collected
    since MRIP now uses a stratified mutlistage cluster sampling design. Domains can be developed from most variables collected in sampling.
    For example, if the user wanted to calculate the number of fish harvested by mode,  domains would have to be created for each level.
 
   The variable names and variable data loaded using this function are all converted to lower case because the original names and variables
    contain a mixture of cases and R is case-sensitive.  Therefore, anytime a domain is defined (below), the MRIP variable needs
    to be referred to in the lower case (e.g., MODE_FX is now mode_fx).  Estimates of quantities are calculated using
    the R package \emph{survey}.

The first step is to use the \emph{st}, \emph{common}, \emph{styr}, and \emph{endyr} to extract what levels of factors 
will be used in the analysis. The next step is to create a domain statement if estimates for groups of 
factor levels are desired. Do not create a domain statement if you would like  estimates for each level of 
\emph{year},\emph{wave},\emph{sub_reg},\emph{mode_fx} and \emph{area_x}. 

 
A domain statement looks something like: 

\preformatted{dom1<-list(mode_fx=list(c(3),c(7)),area_x=list(c(1,2,3,4,5)),cnty=list(c(9,21,23,25),c(1,5,7,19)))}

    The entire domain statement is a series of lists defining whether the variables are to be grouped into one or more 
categories. In the above example, the catch will be estimated for shore (3) and private boat (7) modes separately 
since the \emph{mode_fx} statement is a list with two c() functions.  The \emph{area_x} is a list with only one c(), 
so all levels of \emph{area_x}will be combined as one sub-population.  The variable name \emph{cnty} creates two 
sub-populations of county codes like \emph{mode_fx}. The resulting statement will provide estimates of the number of
directed trips for two county groups, two fishing modes and all areas combined.

  The codes for wave, mode_fx and area_x are:
  A. wave = Bimonthly Waves: 1= Jan-Feb, 2= Mar-Apr, 3= May-Jun, 4=J ul-Aug, 5= Sept-Oct, and 6= Nov-Dec.
  B. mode_fx = From MODE_FX record definitions (on website): 1 = Man-Made, 2 = Beach/Bank, 3= Shore, 
     4= Head Boat, 5= Charter Boat, and 7= Private & Rental Boats.           	
  C. area = From AREA_X record definitions: 1= State Territorial Seas (Ocean<=3 mi excluding Inland), 
       2= Exclusive Economic Zone (Ocean>3 mi), 3= Ocean <=10 mi West FL and TX, 4= Ocean > 10 mi West FL and TX, 
       5= Inland, 6= Unknown. Other variable codes can be found in the MRIP_Survey_Variables.xls file on the website
        under Data Downloads.


As another example:

 \preformatted{dom1<-list(mode_fx=list(c(3,7)),area_x=list(c(1,2),c(3,4,5)),cnty=list(c(9,21,23,25)))}

 \emph{mode_fx} contains only one c(), so estimates will be provided for shore and private modes combined. \emph{area_x}
 will be grouped into two categories since there are two c()s, and \emph{cnty} will contain one group combining four 
counties. 
 
 Whenever levels of a factor are combined, the domain labels for each group starts at 1 and ends with the
 number of groups following alpha-numeric order. 
}
\value{
  \item{\emph{survey} object}{output with estimates , standard errors and coefficients of variation by domain}
}
\references{Marine Recreational Information Program 
\url{http://www.st.nmfs.noaa.gov/recreational-fisheries/index}
}
\author{Gary A. Nelson, Massachusetts Division of Marine Fisheries \email{gary.nelson@state.ma.us}}
\examples{
\dontrun{dom1<-list(mode_fx=list(c(3),c(7)),
    area_x=list(c(1,2,3,4,5)),cnty=list(c(9,21,23,25),c(1,5,7,19)))

   MRIP.dirtrips(intdir="P:/MRIP Analysis",common="SCUP", st=25,styr=2013,
   endyr=2013,trips=1, dom=dom1)}
}
\keyword{misc}
