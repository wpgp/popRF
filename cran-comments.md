## 24 July 2021
* Included a references describing the methods into the package DESCRIPTION file
* Cleaned up the code to make sure there is no console output if "verbose" is FALSE. popRF is the package to work with a raster files, therefore calculation can take some time and for a user convenience parameter "verbose" is TRUE by default .
Only the output which can not be suppressed in popRF package is from tuneRF function from randomeForest package. Even if "trace" paramenter is "False" in tuneRF function, there is a place where tuneRF function is using "cat" to print text (cat(Improve, improve, "\n")).
* There were no ERRORs or WARNINGs after "R CMD check results" 

## 22 July 2021
* Omitted the licence file and the reference as it was advised by CRAN team member after first submution 
* Removed DOI: 10.5258/SOTON/WP00715  which was "Not Found" 
* There were no ERRORs or WARNINGs after "R CMD check results" 

## Tested on environments
* Red Hat Enterprise Linux Server release 7.9 R-version 3.6.2 
* win-builder (devel and release)
* Ubuntu 20.04.2 LTS (GNU/Linux 5.8.0-50-generic x86_64) R-version 4.0.5
* Windows 10/64 R-version 4.0.5
* Southampton University HPC cluster Iridis5 RH8 R-version 3.6.2

## Tested on R-hub building platforms
* Debian Linux, R-devel, clang, ISO-8859-15 locale (debian-clang-devel)
* Debian Linux, R-devel, GCC (debian-gcc-devel)
* Debian Linux, R-release, GCC (debian-gcc-release)
* Fedora Linux, R-devel, clang, gfortran (fedora-clang-devel)
* CentOS 8, stock R from EPEL (linux-x86_64-centos-epel)
* Debian Linux, R-devel, GCC ASAN/UBSAN (linux-x86_64-rocker-gcc-san)
* macOS 10.13.6 High Sierra, R-release, brew (macos-highsierra-release)
* macOS 10.13.6 High Sierra, R-release, CRAN's setup (macos-highsierra-release-cran)
* Ubuntu Linux 20.04.1 LTS, R-devel, GCC (ubuntu-gcc-devel)
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit (windows-x86_64-devel)
* Windows Server 2008 R2 SP1, R-release, 32/64 bit (windows-x86_64-release)

## R CMD check results
There were no ERRORs or WARNINGs. 

## Downstream dependencies
There are no downstream dependencies for this package.