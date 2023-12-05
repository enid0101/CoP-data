#
rm(list=ls())
gc()
#install.packages("pracma")#"#" must be removed for the first installation
#install.packages("EMD")#"#" must be removed for the first installation
#install.packages("DescTools")#"#" must be removed for the first installation

# signal: COPx, COPy
t1=Sys.time();print(t1)
rawdata=read.csv("tt.csv",header=T);dim(rawdata);#Just change tt and make changes according to the input data
tt=rawdata[,1];copx=rawdata[,2]; copy=rawdata[,3];
char2="_result.csv";
char1="tt";char3=paste(char1,char2, sep=''); #Just change tt

# EMD
library(EMD)
library("DescTools")

E1=emd(copx,tt,boundary="wave");
E1.no=E1$nimf;
E1.imf=E1$imf
E1.residue=E1$residue

E2=emd(copy,tt,boundary="wave");
E2.no=E2$nimf;
E2.imf=E2$imf
E2.residue=E2$residue

# time domain features
Result_all=matrix(0,1,148);
	# Feature-1: RMSD

ME00=mean(copx);Result_all[1]=ME00
ME01=mean(E1.imf[,1]);Result_all[2]=ME01
ME02=mean(E1.imf[,2]);Result_all[3]=ME02
ME03=mean(E1.imf[,3]);Result_all[4]=ME03
ME04=mean(E1.imf[,4]);Result_all[5]=ME04
ME05=mean(E1.imf[,5]);Result_all[6]=ME05
ME06=mean(E1.imf[,6]);Result_all[7]=ME06

ME10=mean(copy);Result_all[8]=ME10
ME11=mean(E2.imf[,1]);Result_all[9]=ME11
ME12=mean(E2.imf[,2]);Result_all[10]=ME12
ME13=mean(E2.imf[,3]);Result_all[11]=ME13
ME14=mean(E2.imf[,4]);Result_all[12]=ME14
ME15=mean(E2.imf[,5]);Result_all[13]=ME15
ME16=mean(E2.imf[,6]);Result_all[14]=ME16

SD0E1=sd(copx);	Result_all[15]=SD0E1  	
SD1E1=sd(E1.imf[,1]);Result_all[16]=SD1E1
SD2E1=sd(E1.imf[,2]);Result_all[17]=SD2E1
SD3E1=sd(E1.imf[,3]);Result_all[18]=SD3E1
SD4E1=sd(E1.imf[,4]);Result_all[19]=SD4E1
SD5E1=sd(E1.imf[,5]);Result_all[20]=SD5E1
SD6E1=sd(E1.imf[,6]);Result_all[21]=SD6E1

SD0E2=sd(copy);Result_all[22]=SD0E2
SD1E2=sd(E2.imf[,1]);Result_all[23]=SD1E2
SD2E2=sd(E2.imf[,2]);Result_all[24]=SD2E2
SD3E2=sd(E2.imf[,3]);Result_all[25]=SD3E2
SD4E2=sd(E2.imf[,4]);Result_all[26]=SD4E2
SD5E2=sd(E2.imf[,5]);Result_all[27]=SD5E2
SD6E2=sd(E2.imf[,6]);Result_all[28]=SD6E2

Result_all[29]=SD0E1/ME00;
Result_all[30]=SD1E1/ME01;
Result_all[31]=SD2E1/ME02;
Result_all[32]=SD3E1/ME03;
Result_all[33]=SD4E1/ME04;
Result_all[34]=SD5E1/ME05;
Result_all[35]=SD6E1/ME06;

Result_all[36]=SD0E2/ME10;
Result_all[37]=SD1E2/ME11;
Result_all[38]=SD2E2/ME12;
Result_all[39]=SD3E2/ME13;
Result_all[40]=SD4E2/ME14;
Result_all[41]=SD5E2/ME15;
Result_all[42]=SD6E2/ME16;

m0E1=sqrt(mean((copx-mean(copx))^2))            ;Result_all[43]=m0E1 #x.0.1
m1E1=sqrt(mean((E1.imf[,1]-mean(E1.imf[,1]))^2));Result_all[44]=m1E1 #x.1.1
m2E1=sqrt(mean((E1.imf[,2]-mean(E1.imf[,2]))^2));Result_all[45]=m2E1 #x.2.1
m3E1=sqrt(mean((E1.imf[,3]-mean(E1.imf[,3]))^2));Result_all[46]=m3E1 #x.3.1
m4E1=sqrt(mean((E1.imf[,4]-mean(E1.imf[,4]))^2));Result_all[47]=m4E1 #x.4.1
m5E1=sqrt(mean((E1.imf[,5]-mean(E1.imf[,5]))^2));Result_all[48]=m5E1 #x.5.1
m6E1=sqrt(mean((E1.imf[,6]-mean(E1.imf[,6]))^2));Result_all[49]=m6E1 #x.6.1

m0E2=sqrt(mean((copy-mean(copy))^2))            ;Result_all[50]=m0E2 #y.0.1
m1E2=sqrt(mean((E2.imf[,1]-mean(E2.imf[,1]))^2));Result_all[51]=m1E2#y.1.1
m2E2=sqrt(mean((E2.imf[,2]-mean(E2.imf[,2]))^2));Result_all[52]=m2E2#y.2.1
m3E2=sqrt(mean((E2.imf[,3]-mean(E2.imf[,3]))^2));Result_all[53]=m3E2#y.3.1
m4E2=sqrt(mean((E2.imf[,4]-mean(E2.imf[,4]))^2));Result_all[54]=m4E2#y.4.1
m5E2=sqrt(mean((E2.imf[,5]-mean(E2.imf[,5]))^2));Result_all[55]=m5E2#y.5.1
m6E2=sqrt(mean((E2.imf[,6]-mean(E2.imf[,6]))^2));Result_all[56]=m6E2#y.6.1

SD0E1=sd(copx);	  	
SD1E1=sd(E1.imf[,1]);
SD2E1=sd(E1.imf[,2]);
SD3E1=sd(E1.imf[,3]);
SD4E1=sd(E1.imf[,4]);
SD5E1=sd(E1.imf[,5]);
SD6E1=sd(E1.imf[,6]);

SD0E2=sd(copy)
SD1E2=sd(E2.imf[,1]);
SD2E2=sd(E2.imf[,2]);
SD3E2=sd(E2.imf[,3]);
SD4E2=sd(E2.imf[,4]);
SD5E2=sd(E2.imf[,5]);
SD6E2=sd(E2.imf[,6]);

V00=sqrt(mean(diff(copx)^2) );Result_all[57]=V00
V01=sqrt(mean(diff(E1.imf[,1])^2) );Result_all[58]=V01
V02=sqrt(mean(diff(E1.imf[,2])^2) );Result_all[59]=V02
V03=sqrt(mean(diff(E1.imf[,3])^2) );Result_all[60]=V03
V04=sqrt(mean(diff(E1.imf[,4])^2) );Result_all[61]=V04
V05=sqrt(mean(diff(E1.imf[,5])^2) );Result_all[62]=V05
V06=sqrt(mean(diff(E1.imf[,6])^2) );Result_all[63]=V06

V10=sqrt(mean(diff(copy)^2) );Result_all[64]=V10
V11=sqrt(mean(diff(E2.imf[,1])^2) );Result_all[65]=V11
V12=sqrt(mean(diff(E2.imf[,2])^2) );Result_all[66]=V12
V13=sqrt(mean(diff(E2.imf[,3])^2) );Result_all[67]=V13
V14=sqrt(mean(diff(E2.imf[,4])^2) );Result_all[68]=V14
V15=sqrt(mean(diff(E2.imf[,5])^2) );Result_all[69]=V15
V16=sqrt(mean(diff(E2.imf[,6])^2) );Result_all[70]=V16

# http://127.0.0.1:17734/library/pracma/html/entropy.html
	#Feature-2:  Approximate Entropy 
 library(pracma)
print("Approximate Entropy E1");Sys.time()
AP0E1=approx_entropy(copx,edim=2,r=0.2*SD0E1,elag = 1);	 Result_all[71]=AP0E1; #x.0.4
AP1E1=approx_entropy(E1.imf[,1],edim=2,r=0.2*SD1E1,elag = 1);Result_all[72]=AP1E1; #x.1.4
AP2E1=approx_entropy(E1.imf[,2],edim=2,r=0.2*SD2E1,elag = 1);Result_all[73]=AP2E1; #x.2.4
AP3E1=approx_entropy(E1.imf[,3],edim=2,r=0.2*SD3E1,elag = 1);Result_all[74]=AP3E1; #x.3.4
AP4E1=approx_entropy(E1.imf[,4],edim=2,r=0.2*SD4E1,elag = 1);Result_all[75]=AP4E1; #x.4.4
AP5E1=approx_entropy(E1.imf[,5],edim=2,r=0.2*SD5E1,elag = 1);Result_all[76]=AP5E1; #x.5.4
AP6E1=approx_entropy(E1.imf[,6],edim=2,r=0.2*SD6E1,elag = 1);Result_all[77]=AP6E1; #x.6.4


print("Approximate Entropy E2");Sys.time()
AP0E2=approx_entropy(copy,edim=2,r=0.2*SD0E2,elag = 1);	 Result_all[78]=AP0E2; #y.0.4
AP1E2=approx_entropy(E2.imf[,1],edim=2,r=0.2*SD1E2,elag = 1);Result_all[79]=AP1E2; #y.1.4
AP2E2=approx_entropy(E2.imf[,2],edim=2,r=0.2*SD2E2,elag = 1);Result_all[80]=AP2E2; #y.2.4
AP3E2=approx_entropy(E2.imf[,3],edim=2,r=0.2*SD3E2,elag = 1);Result_all[81]=AP3E2; #y.3.4
AP4E2=approx_entropy(E2.imf[,4],edim=2,r=0.2*SD4E2,elag = 1);Result_all[82]=AP4E2; #y.4.4
AP5E2=approx_entropy(E2.imf[,5],edim=2,r=0.2*SD5E2,elag = 1);Result_all[83]=AP5E2; #y.5.4
AP6E2=approx_entropy(E2.imf[,6],edim=2,r=0.2*SD6E2,elag = 1);Result_all[84]=AP6E2; #y.6.4


	#Feature-3:  Sample entropy

print("Sample Entropy E1");Sys.time()
SA0E1=sample_entropy(copx,edim=2,r=0.2*SD1E1,tau = 1);	Result_all[85]=SA0E1; #x.0.5
SA1E1=sample_entropy(E1.imf[,1],edim=2,r=0.2*SD1E1,tau = 1);Result_all[86]=SA1E1; #x.1.5
SA2E1=sample_entropy(E1.imf[,2],edim=2,r=0.2*SD2E1,tau = 1);Result_all[87]=SA2E1; #x.2.5
SA3E1=sample_entropy(E1.imf[,3],edim=2,r=0.2*SD3E1,tau = 1);Result_all[88]=SA3E1; #x.3.5
SA4E1=sample_entropy(E1.imf[,4],edim=2,r=0.2*SD4E1,tau = 1);Result_all[89]=SA4E1; #x.4.5
SA5E1=sample_entropy(E1.imf[,5],edim=2,r=0.2*SD5E1,tau = 1);Result_all[90]=SA5E1; #x.5.5
SA6E1=sample_entropy(E1.imf[,6],edim=2,r=0.2*SD6E1,tau = 1);Result_all[91]=SA6E1; #x.6.5

print("Sample Entropy E2");Sys.time()
SA0E2=sample_entropy(copy,edim=2,r=0.2*SD1E1,tau = 1);	Result_all[92]=SA0E2; #y.0.5
SA1E2=sample_entropy(E2.imf[,1],edim=2,r=0.2*SD1E2,tau = 1);Result_all[93]=SA1E2; #y.1.5
SA2E2=sample_entropy(E2.imf[,2],edim=2,r=0.2*SD2E2,tau = 1);Result_all[94]=SA2E2; #y.2.5
SA3E2=sample_entropy(E2.imf[,3],edim=2,r=0.2*SD3E2,tau = 1);Result_all[95]=SA3E2; #y.3.5
SA4E2=sample_entropy(E2.imf[,4],edim=2,r=0.2*SD4E2,tau = 1);Result_all[96]=SA4E2; #y.4.5
SA5E2=sample_entropy(E2.imf[,5],edim=2,r=0.2*SD5E2,tau = 1);Result_all[97]=SA5E2; #y.5.5
SA6E2=sample_entropy(E2.imf[,6],edim=2,r=0.2*SD6E2,tau = 1);Result_all[98]=SA6E2; #y.6.5

# Mutual information 
M00=MutInf(copx, copy, base=exp(1)); Result_all[99]=M00;
M01=MutInf(copx, copy, base=2); Result_all[100]=M01;
E01=Entropy(table(copx), base=exp(1)); Result_all[101]=E01;
E02=Entropy(table(copy), base=exp(1)); Result_all[102]=E02;
E03=Entropy(table(copx), base=2); Result_all[103]=E03;
E04=Entropy(table(copy), base=2); Result_all[104]=E04;
M10=MutInf(E1.imf[,1], E2.imf[,1], base=exp(1)); Result_all[105]=M10;
M11=MutInf(E1.imf[,1], E2.imf[,1], base=2); Result_all[106]=M11;

#correlation coefficient
C00=cor(copx,copy); Result_all[107]=C00
C01=cor(E1.imf[,1],E2.imf[,1]); Result_all[108]=C01
C02=cor(E1.imf[,2],E2.imf[,2]); Result_all[109]=C02
C03=cor(E1.imf[,3],E2.imf[,3]); Result_all[110]=C03
C04=cor(E1.imf[,4],E2.imf[,4]); Result_all[111]=C04
C05=cor(E1.imf[,5],E2.imf[,5]); Result_all[112]=C05
C06=cor(E1.imf[,6],E2.imf[,6]); Result_all[113]=C06

V20=sqrt(mean(diff(copx)^2+diff(copy)^2)); Result_all[114]=V20
V21=sqrt(mean(diff(E1.imf[,1])^2+diff(E2.imf[,1])^2)); Result_all[115]=V21
V22=sqrt(mean(diff(E1.imf[,2])^2+diff(E2.imf[,2])^2)); Result_all[116]=V22
V23=sqrt(mean(diff(E1.imf[,3])^2+diff(E2.imf[,3])^2)); Result_all[117]=V23
V24=sqrt(mean(diff(E1.imf[,4])^2+diff(E2.imf[,4])^2)); Result_all[118]=V24
V25=sqrt(mean(diff(E1.imf[,5])^2+diff(E2.imf[,5])^2)); Result_all[119]=V25
V26=sqrt(mean(diff(E1.imf[,6])^2+diff(E2.imf[,6])^2)); Result_all[120]=V26


	# Feature-4 Frequency power 
# install.packages("psd")
print("Frequency domain features");Sys.time()
library(psd)
f0E1=pspectrum(copx);
f1E1=pspectrum(E1.imf[,1]);
f2E1=pspectrum(E1.imf[,2]);
f3E1=pspectrum(E1.imf[,3]);
f4E1=pspectrum(E1.imf[,4]);
f5E1=pspectrum(E1.imf[,5]);
f6E1=pspectrum(E1.imf[,6]);

P0E1=sum(f0E1$spec); Result_all[121]=P0E1; #x.0.3
P1E1=sum(f1E1$spec); Result_all[122]=P1E1; #x.1.3
P2E1=sum(f2E1$spec); Result_all[123]=P2E1; #x.2.3
P3E1=sum(f3E1$spec); Result_all[124]=P3E1; #x.3.3
P4E1=sum(f4E1$spec); Result_all[125]=P4E1; #x.4.3
P5E1=sum(f5E1$spec); Result_all[126]=P5E1; #x.5.3
P6E1=sum(f6E1$spec); Result_all[127]=P6E1; #x.6.3

f0E2=pspectrum(copy);
f1E2=pspectrum(E2.imf[,1]);
f2E2=pspectrum(E2.imf[,2]);
f3E2=pspectrum(E2.imf[,3]);
f4E2=pspectrum(E2.imf[,4]);
f5E2=pspectrum(E2.imf[,5]);
f6E2=pspectrum(E2.imf[,6]);

P0E2=sum(f0E2$spec); Result_all[128]=P0E2; #y.0.3
P1E2=sum(f1E2$spec); Result_all[129]=P1E2; #y.1.3
P2E2=sum(f2E2$spec); Result_all[130]=P2E2; #y.2.3
P3E2=sum(f3E2$spec); Result_all[131]=P3E2; #y.3.3
P4E2=sum(f4E2$spec); Result_all[132]=P4E2; #y.4.3
P5E2=sum(f5E2$spec); Result_all[133]=P5E2; #y.5.3
P6E2=sum(f6E2$spec); Result_all[134]=P6E2; #y.6.3

	# Feature-5 median frequency, unit=Hz
c0E1=diff(sign(cumsum(f0E1$spec)-(P0E1/2)));Result_all[135]=which.max(c0E1)/60; #x.0.2
c1E1=diff(sign(cumsum(f1E1$spec)-(P1E1/2)));Result_all[136]=which.max(c1E1)/60; #x.1.2
c2E1=diff(sign(cumsum(f2E1$spec)-(P2E1/2)));Result_all[137]=which.max(c2E1)/60; #x.2.2
c3E1=diff(sign(cumsum(f3E1$spec)-(P3E1/2)));Result_all[138]=which.max(c3E1)/60; #x.3.2
c4E1=diff(sign(cumsum(f4E1$spec)-(P4E1/2)));Result_all[139]=which.max(c4E1)/60; #x.4.2
c5E1=diff(sign(cumsum(f5E1$spec)-(P5E1/2)));Result_all[140]=which.max(c5E1)/60; #x.5.2
c6E1=diff(sign(cumsum(f6E1$spec)-(P6E1/2)));Result_all[141]=which.max(c6E1)/60; #x.6.2

c0E2=diff(sign(cumsum(f0E2$spec)-(P0E2/2)));Result_all[142]=which.max(c0E2)/60; #y.0.2
c1E2=diff(sign(cumsum(f1E2$spec)-(P1E2/2)));Result_all[143]=which.max(c1E2)/60; #y.1.2
c2E2=diff(sign(cumsum(f2E2$spec)-(P2E2/2)));Result_all[144]=which.max(c2E2)/60; #y.2.2
c3E2=diff(sign(cumsum(f3E2$spec)-(P3E2/2)));Result_all[145]=which.max(c3E2)/60; #y.3.2
c4E2=diff(sign(cumsum(f4E2$spec)-(P4E2/2)));Result_all[146]=which.max(c4E2)/60; #y.4.2
c5E2=diff(sign(cumsum(f5E2$spec)-(P5E2/2)));Result_all[147]=which.max(c5E2)/60; #y.5.2
c6E2=diff(sign(cumsum(f6E2$spec)-(P6E2/2)));Result_all[148]=which.max(c6E2)/60; #y.6.2




print(Result_all);
print("Sample Entropy E2");Sys.time()
write.csv(Result_all,  file =char3)
