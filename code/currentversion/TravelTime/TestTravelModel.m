region=8;
k=[5;5];
lambda = 0.0001;
logflag=2;
plotflag=1;
distflag=1; % 1 TT = f(x1,y1,distance,theta)
DataDir = '/Users/larsnesheim/Documents/research/hedonic/NIC/data/TravelTime/data';
OutDir  = '/Users/larsnesheim/Documents/research/hedonic/NIC/code/currentversion/output/TravelTime';

travelmodel(region,k,lambda,logflag,distflag,plotflag,DataDir,OutDir)