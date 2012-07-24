setwd('C:/My Projects/Zheng - Adjust Trust Rating - RAD -355/');
loan3<-read.csv("3loans.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);#first half year of 2011

library(ROCR);
library(nnet);
load.session("trnn.Rda");

set.seed(303)  #preferred
nn<-nnet(ThirdBad~FirstLoanNumber+FirstMixedScore+FirstDelinquency+FirstTotalTop+SecondLoanNumber+SecondDaysSinceLastProposal+SecondLoanTerm+SecondMixedScore+SecondDelinquency+SecondTotalExt+SecondTotalTop+utilization2+SecondDaysSinceLastPayOff+SecondLoanIncome,data=train,size=20,linout=FALSE,decay=0.025,maxit=5000);

save.session("trnnet1st2011") # the update model is saved in this session.

loan3$FirstApplicationDate<-as.Date(as.character(loan3$FirstApplicationDate),format="%Y%m%d");
loan3$SecondApplicationDate<-as.Date(as.character(loan3$SecondApplicationDate),format="%Y%m%d");
loan3$FirstPaybackDate<-as.Date(as.character(loan3$FirstPaybackDate),format="%Y%m%d");
loan3$SecondPaybackDate<-as.Date(as.character(loan3$SecondPaybackDate),format="%Y%m%d");

loan3$SecondDaysSinceLastPayOff<-loan3$SecondApplicationDate-loan3$FirstPaybackDate;

loan3$AmountOrigDiff<-loan3$SecondLoanAmountOriginal - loan3$FirstLoanAmountOriginal;
loan3$AmountAgreeDiff<-loan3$SecondLoanAmountAgreed - loan3$FirstLoanAmountAgreed;
loan3$FirstAnnualIncome<-ifelse(loan3$FirstAnnualIncome==0,12,loan3$FirstAnnualIncome) #12 is the second minumim number in the list.
loan3<-loan3[!is.na(loan3$FirstMixedScore),]; #missing value, fill in the mean value.
loan3$ScoreDiff<-loan3$SecondMixedScore - loan3$FirstMixedScore;
loan3$utilization1<-loan3$FirstLoanAmountAgreed/loan3$FirstTrustRating;
loan3$utilization2<-loan3$SecondLoanAmountAgreed/loan3$SecondTrustRating;
loan3$utilization1<-ifelse(loan3$utilization1>1,1,loan3$utilization1)
loan3$utilization2<-ifelse(loan3$utilization2>1,1,loan3$utilization2)
loan3$utilRatio<-loan3$utilization2/loan3$utilization1;
loan3$utilDiff<-loan3$utilization2 - loan3$utilization1;
loan3$IncomeDiff<-loan3$SecondAnnualIncome - loan3$FirstAnnualIncome;
loan3<-loan3[!is.na(loan3$utilRatio),];
loan3<-loan3[!is.na(loan3$SecondMixedScore),];
loan3$trratio<-loan3$SecondTrustRating/loan3$FirstTrustRating;
loan3$utitrratio<-loan3$utilRatio/loan3$trratio;
loan3$logutitrratio<-log(loan3$utilRatio/loan3$trratio);
loan3$FirstLoanIncome<-loan3$FirstLoanAmountOriginal/(loan3$FirstAnnualIncome/12);
loan3$SecondLoanIncome<-loan3$SecondLoanAmountOriginal/(loan3$SecondAnnualIncome/12); 

loan3$FirstTotalExt<-ifelse(is.na(loan3$FirstTotalExt),0,loan3$FirstTotalExt);
loan3$FirstTotalTop<-ifelse(is.na(loan3$FirstTotalTop),0,loan3$FirstTotalTop);

loan3$SecondTotalExt<-ifelse(is.na(loan3$SecondTotalExt),0,loan3$SecondTotalExt);
loan3$SecondTotalTop<-ifelse(is.na(loan3$SecondTotalTop),0,loan3$SecondTotalTop);

trainind2<-sample(1:431078,301754,replace=FALSE);
train2<-loan3[trainind2,];
test2<-loan3[-trainind2,];

nn$wts10<-round(nn$wts,digits=10);
set.seed(303)  #preferred, the weights are more robust and give the consistent result, using the 1st half year of 2011.
nn<-nnet(ThirdBad~FirstLoanNumber+FirstMixedScore+FirstDelinquency+FirstTotalTop+SecondLoanNumber+SecondDaysSinceLastProposal+SecondLoanTerm+SecondMixedScore+SecondDelinquency+SecondTotalExt+SecondTotalTop+utilization2+SecondDaysSinceLastPayOff+SecondLoanIncome,data=train2,size=20,linout=FALSE,decay=0.025,maxit=5000);

nn.pred<-predict(nn,train);
pred<-prediction(nn.pred,train$ThirdBad);
auc<-performance(pred,"auc");
auc; 

-------------------load new data set for test and evaluation-------------
nxloan<-read.csv("prednextloan2.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);#2nd half year of 2011
colnames(nxloan)[1]<-"ProposalID";
nxloannr<-nxloan;
nxloannr<-nxloannr[complete.cases(nxloannr$SecondTrustRating),];
nxloannr<-nxloannr[complete.cases(nxloannr$FirstTrustRating),];
nxloannr<-nxloannr[complete.cases(nxloannr$FirstMixedScore),];
nxloannr<-nxloannr[complete.cases(nxloannr$SecondMixedScore),];
nxloannr<-nxloannr[complete.cases(nxloannr$ThirdTrustRating),];

nxloannr$FirstTotalExt<-ifelse(is.na(nxloannr$FirstTotalExt),0,nxloannr$FirstTotalExt);
nxloannr$SecondTotalExt<-ifelse(is.na(nxloannr$SecondTotalExt),0,nxloannr$SecondTotalExt);
nxloannr$FirstTotalTop<-ifelse(is.na(nxloannr$FirstTotalTop),0,nxloannr$FirstTotalTop);
nxloannr$SecondTotalTop<-ifelse(is.na(nxloannr$SecondTotalTop),0,nxloannr$SecondTotalTop);

nxloannr$FirstApplicationDate<-as.Date(as.character(nxloannr$FirstApplicationDate),format="%Y%m%d");
nxloannr$SecondApplicationDate<-as.Date(as.character(nxloannr$SecondApplicationDate),format="%Y%m%d");
nxloannr$FirstPaybackDate<-as.Date(as.character(nxloannr$FirstPaybackDate),format="%Y%m%d");
nxloannr$SecondPaybackDate<-as.Date(as.character(nxloannr$SecondPaybackDate),format="%Y%m%d");

nxloannr$SecondDaysSinceLastPayOff<-nxloannr$SecondApplicationDate-nxloannr$FirstPaybackDate;
nxloannr$SecondDaysSinceLastPayOff<-as.integer(nxloannr$SecondDaysSinceLastPayOff)

nxloannr$AmountOrigDiff<-nxloannr$SecondLoanAmountOriginal - nxloannr$FirstLoanAmountOriginal;
nxloannr$AmountAgreeDiff<-nxloannr$SecondLoanAmountAgreed - nxloannr$FirstLoanAmountAgreed;
nxloannr<-nxloannr[!is.na(nxloannr$FirstMixedScore),]; #missing value, fill in the mean value.
nxloannr$ScoreDiff<-nxloannr$SecondMixedScore - nxloannr$FirstMixedScore;
nxloannr$utilization1<-nxloannr$FirstLoanAmountAgreed/nxloannr$FirstTrustRating;
nxloannr$utilization2<-nxloannr$SecondLoanAmountAgreed/nxloannr$SecondTrustRating;
nxloannr$utilization1<-ifelse(nxloannr$utilization1>1,1,nxloannr$utilization1)
nxloannr$utilization2<-ifelse(nxloannr$utilization2>1,1,nxloannr$utilization2)
nxloannr$utilRatio<-nxloannr$utilization2/nxloannr$utilization1;
nxloannr$utilDiff<-nxloannr$utilization2 - nxloannr$utilization1;
nxloannr$IncomeDiff<-nxloannr$SecondAnnualIncome - nxloannr$FirstAnnualIncome;
nxloannr<-nxloannr[!is.na(nxloannr$utilRatio),];
nxloannr<-nxloannr[!is.na(nxloannr$SecondMixedScore),];
nxloannr$trratio<-nxloannr$SecondTrustRating/nxloannr$FirstTrustRating;
nxloannr$utitrratio<-nxloannr$utilRatio/nxloannr$trratio;
nxloannr$FirstLoanIncome<-nxloannr$FirstLoanAmountOriginal/(nxloannr$FirstAnnualIncome/12);
nxloannr$SecondLoanIncome<-nxloannr$SecondLoanAmountOriginal/(nxloannr$SecondAnnualIncome/12);

nn.pred<-predict(nn,nxloannr);
pred<-prediction(nn.pred,nxloannr$ThirdBad);
auc<-performance(pred,"auc");
auc; 

nxloannr<-data.frame(nxloannr,nn.pred);
nxloannr$TrUpdate<-ifelse((nxloannr$ThirdTrustRating-nxloannr$SecondTrustRating)>0,1,ifelse((nxloannr$ThirdTrustRating-nxloannr$SecondTrustRating)<0,-1,0));
table(nxloannr$TrUpdate,nxloannr$ThirdBad);
nxloannr$PredOutcome<-ifelse(nxloannr$nn.pred<0.5,0,1);

data1<-nxloannr[which(nxloannr$TrUpdate>0&nxloannr$ThirdLoanAmountAgreed>nxloannr$SecondTrustRating),];
data1$PredOutcome<-ifelse(data1$nn.pred<0.15,0,1);
sum(data1$ThirdBad==1&data1$PredOutcome==1);
sum(data1$ThirdBad==1);
sum(data1$PredOutcome==1);

data2<-nxloannr[which(nxloannr$TrUpdate==0|(nxloannr$TrUpdate>0&nxloannr$ThirdLoanAmountAgreed<=nxloannr$SecondTrustRating)),];
data2$PredOutcome<-ifelse(data2$nn.pred<0.15,0,1);
sum(data2$ThirdBad==1&data2$PredOutcome==1);
sum(data2$ThirdBad==1);
sum(data2$PredOutcome==1);

datacom<-rbind(data1,data2)


---------Anna A gave me another query to query the ratio of the customer reach 1000 TR and decline on the next loans as follows--------------------
select COUNT(*)as Total_Apps,
count(case when TrustRating = 1000 then ProposalID end )as TrustRating_1000,
count(case when TrustRating = 1000 and Outcome in ('declined', 'Referred','Deferred') then ProposalID end )as Not_Accepted,
count(case when TrustRating = 1000 and Outcome = ('Accepted') then ProposalID end )as Accepted,
1.00*count(case when TrustRating = 1000 and Outcome in ('declined', 'Referred','Deferred') then ProposalID end )/count(case when TrustRating = 1000 then ProposalID end ) as TrustRating_1000_Not_Accepted,
1.00*count(case when TrustRating = 1000 and Outcome = ('Accepted') then ProposalID end )/count(case when TrustRating = 1000 then ProposalID end )TrustRating_1000_Accepted
from WongaLook.Result
where LogDate > = '20110101' and LogDate < '20120101'
and LoanNumber > 0 and Retailer = 'wonga' ;

#the proportion of reject trust rating 1000 is more than 15%.
-----------------------------------------------------------------------------------------------------------------------------------------------------
threshold<-seq(0.001,1,by=0.001);
data1<-nxloannr[which(nxloannr$TrUpdate>0&nxloannr$ThirdLoanAmountAgreed>nxloannr$SecondTrustRating),];
precision<-c();
arrear<-c();
cutoff<-c();
for(i in threshold)
{
	data1$PredOutcome<-ifelse(data1$nn.pred<i,0,1);
	if(sum(data1$PredOutcome==1)>0&sum(data1$PredOutcome==0)>0)
	{
	precision<-c(precision,sum(data1$ThirdBad==1&data1$PredOutcome==1)/sum(data1$PredOutcome==1));
	arrear<-c(arrear,sum(data1$ThirdBad==1&data1$PredOutcome==0)/sum(data1$PredOutcome==0));
	cutoff<-c(cutoff,i);
	}
}
results<-data.frame(cutoff,precision,arrear);


data1<-nxloannr[which(nxloannr$TrUpdate>0&nxloannr$ThirdLoanAmountAgreed>nxloannr$SecondTrustRating),];
frozenrate<-c();
arrear<-c();
cutoff<-c();
for(i in threshold)
{
	data1$PredOutcome<-ifelse(data1$nn.pred<i,0,1);
	if(sum(data1$PredOutcome==1)>0&sum(data1$PredOutcome==0)>0)
	{
	arrear<-c(arrear,sum(data1$ThirdBad==1&data1$PredOutcome==1)/sum(data1$PredOutcome==1));
	frozenrate<-c(frozenrate,sum(data1$PredOutcome==1)/dim(data1)[1]);
	cutoff<-c(cutoff,i);
	}
}
results<-data.frame(cutoff,frozenrate,arrear);

increaserate<-c();
arrearforincrease<-c();
frozenrate<-c();
arrearforfrozen<-c();
cutoff<-c();
for(i in threshold)
{
	data1$PredOutcome<-ifelse(data1$nn.pred<i,0,1);
	if(sum(data1$PredOutcome==1)>0&sum(data1$PredOutcome==0)>0)
	{
	arrearforincrease<-c(arrearforincrease,sum(data1$ThirdBad==1&data1$PredOutcome==0)/sum(data1$PredOutcome==0));
	increaserate<-c(increaserate,sum(data1$PredOutcome==0)/dim(data1)[1]);
	frozenrate<-c(frozenrate,sum(data1$PredOutcome==1)/dim(data1)[1]);
	arrearforfrozen<-c(arrearforfrozen,sum(data1$ThirdBad==1&data1$PredOutcome==1)/sum(data1$PredOutcome==1));
	cutoff<-c(cutoff,i);
	}
}
results<-data.frame(cutoff,increaserate,arrearforincrease,frozenrate,arrearforfrozen);


arrear<-c();
rate<-c();
type<-c();
cutoff<-c();
for(i in threshold)
{
	data1$PredOutcome<-ifelse(data1$nn.pred<i,0,1);
	if(sum(data1$PredOutcome==1)>0&sum(data1$PredOutcome==0)>0)
	{
	arrear<-c(arrear,sum(data1$ThirdBad==1&data1$PredOutcome==0)/sum(data1$PredOutcome==0));
	rate<-c(rate,sum(data1$PredOutcome==0)/dim(data1)[1]);
	type<-c(type,"Increase TR");
	rate<-c(rate,sum(data1$PredOutcome==1)/dim(data1)[1]);
	arrear<-c(arrear,sum(data1$ThirdBad==1&data1$PredOutcome==1)/sum(data1$PredOutcome==1));
	type<-c(type,"Frozen TR");
	cutoff<-c(cutoff,i);
	}
}
results<-data.frame(cutoff,rate,arrear,type);

increaserate<-c();
arrearforincrease<-c();
frozenrate<-c();
arrearforfrozen<-c();
cutoff<-c();
for(i in threshold)
{
	datacom$PredOutcome<-ifelse(datacom$nn.pred<i,0,1);
	if(sum(datacom$PredOutcome==1)>0&sum(datacom$PredOutcome==0)>0)
	{
	arrearforincrease<-c(arrearforincrease,sum(datacom$ThirdBad==1&datacom$PredOutcome==0)/sum(datacom$PredOutcome==0));
	increaserate<-c(increaserate,sum(datacom$PredOutcome==0)/dim(datacom)[1]);
	frozenrate<-c(frozenrate,sum(datacom$PredOutcome==1)/dim(datacom)[1]);
	arrearforfrozen<-c(arrearforfrozen,sum(datacom$ThirdBad==1&datacom$PredOutcome==1)/sum(datacom$PredOutcome==1));
	cutoff<-c(cutoff,i);
	}
}
results<-data.frame(cutoff,increaserate,arrearforincrease,frozenrate,arrearforfrozen);


----------test on 2012 data on the same model----------------
nxloan2012<-read.csv("3loan2012.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);#2nd half year of 2011
colnames(nxloan2012)[1]<-"ProposalID";
nxloannr2012<-nxloan2012;
nxloannr2012<-nxloannr2012[complete.cases(nxloannr2012$SecondTrustRating),];
nxloannr2012<-nxloannr2012[complete.cases(nxloannr2012$FirstTrustRating),];
nxloannr2012<-nxloannr2012[complete.cases(nxloannr2012$FirstMixedScore),];
nxloannr2012<-nxloannr2012[complete.cases(nxloannr2012$SecondMixedScore),];
nxloannr2012<-nxloannr2012[complete.cases(nxloannr2012$ThirdTrustRating),];

nxloannr2012$FirstTotalExt<-ifelse(is.na(nxloannr2012$FirstTotalExt),0,nxloannr2012$FirstTotalExt);
nxloannr2012$SecondTotalExt<-ifelse(is.na(nxloannr2012$SecondTotalExt),0,nxloannr2012$SecondTotalExt);
nxloannr2012$FirstTotalTop<-ifelse(is.na(nxloannr2012$FirstTotalTop),0,nxloannr2012$FirstTotalTop);
nxloannr2012$SecondTotalTop<-ifelse(is.na(nxloannr2012$SecondTotalTop),0,nxloannr2012$SecondTotalTop);

nxloannr2012$FirstApplicationDate<-as.Date(as.character(nxloannr2012$FirstApplicationDate),format="%Y%m%d");
nxloannr2012$SecondApplicationDate<-as.Date(as.character(nxloannr2012$SecondApplicationDate),format="%Y%m%d");
nxloannr2012$FirstPaybackDate<-as.Date(as.character(nxloannr2012$FirstPaybackDate),format="%Y%m%d");
nxloannr2012$SecondPaybackDate<-as.Date(as.character(nxloannr2012$SecondPaybackDate),format="%Y%m%d");

nxloannr2012$SecondDaysSinceLastPayOff<-nxloannr2012$SecondApplicationDate-nxloannr2012$FirstPaybackDate;
nxloannr2012$SecondDaysSinceLastPayOff<-as.integer(nxloannr2012$SecondDaysSinceLastPayOff)

nxloannr2012$AmountOrigDiff<-nxloannr2012$SecondLoanAmountOriginal - nxloannr2012$FirstLoanAmountOriginal;
nxloannr2012$AmountAgreeDiff<-nxloannr2012$SecondLoanAmountAgreed - nxloannr2012$FirstLoanAmountAgreed;
nxloannr2012<-nxloannr2012[!is.na(nxloannr2012$FirstMixedScore),]; #missing value, fill in the mean value.
nxloannr2012$ScoreDiff<-nxloannr2012$SecondMixedScore - nxloannr2012$FirstMixedScore;
nxloannr2012$utilization1<-nxloannr2012$FirstLoanAmountAgreed/nxloannr2012$FirstTrustRating;
nxloannr2012$utilization2<-nxloannr2012$SecondLoanAmountAgreed/nxloannr2012$SecondTrustRating;
nxloannr2012$utilization1<-ifelse(nxloannr2012$utilization1>1,1,nxloannr2012$utilization1)
nxloannr2012$utilization2<-ifelse(nxloannr2012$utilization2>1,1,nxloannr2012$utilization2)
nxloannr2012$utilRatio<-nxloannr2012$utilization2/nxloannr2012$utilization1;
nxloannr2012$utilDiff<-nxloannr2012$utilization2 - nxloannr2012$utilization1;
nxloannr2012$IncomeDiff<-nxloannr2012$SecondAnnualIncome - nxloannr2012$FirstAnnualIncome;
nxloannr2012<-nxloannr2012[!is.na(nxloannr2012$utilRatio),];
nxloannr2012<-nxloannr2012[!is.na(nxloannr2012$SecondMixedScore),];
nxloannr2012$trratio<-nxloannr2012$SecondTrustRating/nxloannr2012$FirstTrustRating;
nxloannr2012$utitrratio<-nxloannr2012$utilRatio/nxloannr2012$trratio;
nxloannr2012$FirstLoanIncome<-nxloannr2012$FirstLoanAmountOriginal/(nxloannr2012$FirstAnnualIncome/12);
nxloannr2012$SecondLoanIncome<-nxloannr2012$SecondLoanAmountOriginal/(nxloannr2012$SecondAnnualIncome/12);

nn.pred<-predict(nn,nxloannr2012);
pred<-prediction(nn.pred,nxloannr2012$ThirdBad);
auc<-performance(pred,"auc");
auc; 

nxloannr2012<-data.frame(nxloannr2012,nn.pred);
nxloannr2012$TrUpdate<-ifelse((nxloannr2012$ThirdTrustRating-nxloannr2012$SecondTrustRating)>0,1,ifelse((nxloannr2012$ThirdTrustRating-nxloannr2012$SecondTrustRating)<0,-1,0));
table(nxloannr2012$TrUpdate,nxloannr2012$ThirdBad);
nxloannr2012$PredOutcome<-ifelse(nxloannr2012$nn.pred<0.5,0,1);

data2012<-nxloannr2012[which(nxloannr2012$TrUpdate>=0),];
threshold<-seq(0.001,1,by=0.001);
increaserate<-c();
arrearforincrease<-c();
frozenrate<-c();
arrearforfrozen<-c();
cutoff<-c();
for(i in threshold)
 {
     data2012$PredOutcome<-ifelse(data2012$nn.pred<i,0,1);
     if(sum(data2012$PredOutcome==1)>0&sum(data2012$PredOutcome==0)>0)
     {
         arrearforincrease<-c(arrearforincrease,sum(data2012$ThirdBad==1&data2012$PredOutcome==0)/sum(data2012$PredOutcome==0));
         increaserate<-c(increaserate,sum(data2012$PredOutcome==0)/dim(data2012)[1]);
         frozenrate<-c(frozenrate,sum(data2012$PredOutcome==1)/dim(data2012)[1]);
         arrearforfrozen<-c(arrearforfrozen,sum(data2012$ThirdBad==1&data2012$PredOutcome==1)/sum(data2012$PredOutcome==1));
         cutoff<-c(cutoff,i);
     }
 }
results<-data.frame(cutoff,increaserate,arrearforincrease,frozenrate,arrearforfrozen);
results$score<-1000*(1-results$cutoff);

write.csv(results,file="results2012tr.csv");
data2012$PredOutcome<-ifelse(data2012$nn.pred<0.08,0,1);
sum(data2012$ThirdBad==1&data2012$PredOutcome==1);
sum(data2012$PredOutcome==1);
sum(data2012$ThirdBad==1);

data2012$util3<-data2012$ThirdLoanAmountAgreed/data2012$ThirdTrustRating;
data2012$util3<-ifelse(data2012$util3>1,1,data2012$util3);

data2012$highscore<-ifelse(data2012$SecondMixedScore>975,1,0);

sum(data2012$highscore==1&data2012$TrUpdate==0)
[1] 128408
sum(data2012$highscore==1&data2012$TrUpdate==0&data2012$ThirdBad==1)
[1] 3778
sum(data2012$highscore==1&data2012$TrUpdate==1)
[1] 96359
sum(data2012$highscore==1&data2012$TrUpdate==1&data2012$ThirdBad==1)
2605

data2012$highscore<-ifelse(data2012$ThirdMixedScore>950,1,0);
sum(data2012$highscore==1&data2012$TrUpdate==0)
[1] 151128
sum(data2012$highscore==1&data2012$TrUpdate==0&data2012$ThirdBad==1)
[1] 5642
sum(data2012$highscore==1&data2012$TrUpdate==1)
[1] 174409
sum(data2012$highscore==1&data2012$TrUpdate==1&data2012$ThirdBad==1)
[1] 7085

-----significant test----------------------------------------------------------------
M<-as.table(rbind(c(5642,145486),c(7085,167324)))
dimnames(M)<-list(tr=c("frozen","increase"),outcome=c("bad","good"))
chisq.test(M)

arrear<-c(5642,7085)
total<-c(151128,174409)
prop.test(arrear,total)

t.test(data2012$SecondMixedScore[which(data2012$highscore==1&data2012$TrUpdate==1&data2012$ThirdBad==1)],data2012$SecondMixedScore[which(data2012$highscore==1&data2012$TrUpdate==0&data2012$ThirdBad==1)])
t.test(data20122$ThirdMixedScore[which(data20122$highscore==1&data20122$TrUpdate==1&data20122$ThirdBad==1)],data20122$ThirdMixedScore[which(data20122$highscore==1&data20122$TrUpdate==0&data20122$ThirdBad==1)])
---------------------------------------------------------------------------------------


ggplot(data=data2012[which(data2012$highscore==1&data2012$ThirdBad==1),],aes(as.factor(TrUpdate),SecondMixedScore))+geom_boxplot()