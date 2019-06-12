# top-incomes-chile
Data and codes behind our paper: "Top Incomes in Chile: A Historical Perspective of Income Inequality (1964-2017)"


All the tables and figures that are presented in our paper 
can be reproduced using the STATA codes and excel data files in this repository. 

The benchmark series of estimates in the paper corresponds to all figures with which name ends by 'fiscal'.
In addition, the code generates the same figures, using a definition of income that includes an imputation 
of social security contributions (see section A.4 of Appendix)

#Do_Part1.do 
This file produces the first half of the figures. it also arranges tax-income declarations in a format that 
the GPinter program (wid.world/gpinter/) can read and interpolate. These can be found in the repository: Data/Gpinter/input/

#Do_Part2.do 
This file produces the rest or figures. It uses the output of the GPinter program as an input to analyse top income 
concentration series. This do-file also uses data from the WID database (www.wid.world). To use it, you have to install 
the wid package previously (by typing ‘’ssc install wid’’ on STATA)

