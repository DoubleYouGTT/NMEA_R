# This file contains the package variables.


packagename          = "NMEA"
packagetitle         = "NMEA parsing"
packageversion_major = 1                        #version numbers will be generated to follow major.minor.build
packageversion_minor = 0                        #the build number will be automatically incremented each time this makepackagehere file is ran
packageauthors = list(
  person(
    given="Joost",
    family="Kuckartz",
    email="joost.kuckartz@arcadis.com",
    role=c("aut","cph","cre")                   #options: aut=author, com=compiler, ctb=contributor, cph=copyright holder, cre=maintainer, ctr=contractor, dtc=data contributor, ths=thesis advisor, trl=translator
  )
)
packagedesctext = "This package enables the processing of NMEA compliant log files to data.tables."
packagelicense = "Undecided"