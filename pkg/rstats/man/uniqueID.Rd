\name{uniqueID}
\alias{uniqueID}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate a unique (hopefully) id to identify a user
}
\description{
The unique information is only used to make sure a user won't rate a package multiple times within a short period of time. No private inforamtion can be obtained from such uqniue id. 
}
\usage{
uniqueID()
}
\details{
To generate such an ID, information from functions \code{\link{Sys.getenv}} and \code{\link{Sys.info}}is used. Sepcifically, the output of \code{\link{Sys.getenv}} is converted to a string and then every other three letters are chosen to form a meaningless string vector such as \code{CPgmt:rr l 8\\DPCUrza4pa\\an:rr l\\cstPPk0 \\fl:rr l\\mnis\\oaFe(6Cm lCPgmisooFeZN-5\\nwst3c.e\\oaa\\AeetPttn-1\\e\\hg:ssznCPgmtKbok5oCUrza4pa\\c\\ZN-5\\oaFeMro Ca 0RI\\\\oaFeMro Ca 0RL\\8CPgmisiofH c282ia68nwN:tlb;\\osc4.b;\\oaFe(6A Pix_;\\oaFe(6A Pix;\\oaFeMro Ca 0RB\\:io\\sm;\\nwCWdsye2b;\\nwSt3WdswSlv0CPgmisx)TThli\\ICCetiCPgmisx)rhz.\\n:rr l 8\\KX.mt\\nCPgmisx)ed\\nCPgmised\\nCPgmisx)pA\\mn:rr l 8\\eFCe\\oaCPgmispA\\mn:rr l\\eFCe\\oaCPgmisooeNiCPgmis\\3.b\\4:rr l\\cstePtrItl\\:rr l 8\\cstSN\\PEW g\\.;\\oaFe(6Wdsi\\0io rrn oi;\\oaFeMro Lee1\\o\\n;\\oaFe(6Mro slti10Cia6CRo\\n:tlg-6/nC;X.TC;B.EJ.EW;S.CDA6Fi  d Spn0AhtA22CPgmt:rr lCPgmisx)\\oaFe:io\\sm\\nwore\\.Mus:ssuixCPG~R-~0\\e\\hgRilry.:ssznCse1/psEc06gt06CWds\\e\\hgADaoleCUrza4pa\\c\\mzn-5hg:ssznCPgmisrlVtlx:rr l 8\\cstiaSd .Cm7os:rrDaceDkpoco:io nw 4i 0 rcPkZA4C86znza4hg} on my own PC. 

Note that information regarding OS is also used. For example, on my PC, it is \code{Windows;7 x64;build 7601, Service Pack 1;x86-64}.

On our webserver, the unique information is converted to md5 harsh. 
}