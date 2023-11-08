##### Installation instructions #####

# Before installing the packages needed for this short workshop please be sure you have the adequate compilers. 

##### Windows #####

# If you are a Windows user, you will need "RTools". 
# To check if you have RTools installed, the simplest way is just to look at the installed software 
# by clicking on the Windows start button. Scroll down until R and see if there is an extra folder named RTools - Just that!!! 

# If you don't have RTools, download the package from here: https://cran.r-project.org/bin/windows/Rtools/ 
# Always double-check which R version you are using. Further instructions are provided on the same webpage.

##### MAC ######

# If you are a Mac user you just need "Xcode". To check if you have installed Xcode on your Mac, use the following steps:

# Step 1. Open terminal 
# Step 2. type: xcodebuild -version 
# Step 2.1. if everything is OK then it will print the version of your Xcode

# If you don't have "Xcode" installed there are a couple of options but I strongly recommend installing Xcode using the terminal

# Step 1. Open terminal
# Step 2. type: xcode-select –install
# Step 2.1. Follow the instructions to complete the installation - this will take some time, please be patient.
# Step 3. To check if the installation was correct, open the terminal 
# Step 3.1. type: xcodebuild -version "or" xcode-select -p

# Hooray!!! 

##### Required packages #####

# For this workshop we will use the following packages 
# 1. {tidyverse} 
# 2. {rstan} 
# 3. {ape} 
# 4. {cmdstanr}
# 5. {brms}
# 6. {posterior}
# 7. {bayesplot}

# {tidyverse} {ape} can be installed by running the following code line

install.packages(c("tidyverse", "ape")) 

### Installing {rstan}

# To install {rstan} please run
install.packages("rstan", type = "source") 

### To install {cmdstanr} follow the next steps 

# Step 1 - run
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos"))) 

# Step 2 - run
library(cmdstanr)

# Step 3 -  double check that your toolchain is set up properly
check_cmdstan_toolchain()
# if successfull it will print the following message The C++ toolchain required for CmdStan is setup properly!

# Step 4 - If your toolchain is configured correctly then CmdStan can be installed
install_cmdstan()
# This step will take some time, please be patient!

# Step 5 - Check the path to the CmdStan installation and the CmdStan version number
cmdstan_path()

cmdstan_version()

# Further instructions can be found here: https://mc-stan.org/cmdstanr/articles/cmdstanr.html

### To install {brms} you just can run 
install.packages("brms")

# However, if your are confident enough you can run
install.packages("brms", type = "source")

### Hooray!!!
