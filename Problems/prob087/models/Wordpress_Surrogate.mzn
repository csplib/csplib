% This script was made by Andrei Iovescu andrei.iovescu99@e- uvt.ro)

% number of wordpress instances
int : n;

% corresponds to Wordpress component
var 0..1024 : v1;
% corresponds to MySql component
var 0..1024 : v2;
% corresponds to DNS Balancer component
var 0..1024 : v3;
% corresponds to HTTP Balancer component
var 0..1024 : v4;
% corresponds to Varnish component
var 0..1024 : v5;

% creates a require provide relation between components alpha and beta
predicate requireProvide( var int: alphaCompId, var int: betaCompId, int: alphaCompIdInstances, int: betaCompIdInstances ) 
          = alphaCompId * alphaCompIdInstances <= betaCompId * betaCompIdInstances ;
          
predicate Provide(var int: alphaCompId, var int: betaCompId, int: alphaCompIdInstances)
          = alphaCompId <= alphaCompIdInstances * betaCompId;

% sets a lower bound for the parameter compId
predicate lowerBound (var int: compId, int: bound) =  compId >= bound;

% sets a upper bound for the parameter compId
predicate upperBound (var int: compId, int: bound) =  compId <= bound;

predicate exclusiveDeployment( var int: alphaCompId, var int: betaCompId)
          = (alphaCompId > 0 /\ betaCompId = 0) \/ (alphaCompId = 0 /\ betaCompId > 0);


%%%%%%%%%%%%%%%%
%Constraints from wordpress problem specification
%%%%%%%%%%%%%%%%

% create constraint 1 
constraint v3 > 0 -> Provide(v1, v3, 7);

constraint v4 > 0 -> Provide(v1, v4, 3);
% create constraint 3 
constraint requireProvide(v1, v2, 2, 3);
% create constraint 4 
constraint lowerBound(v5,2);
% create constraint 5 
constraint lowerBound(v2, 2);
% create constraint 6
constraint upperBound(v3, 1);
% test for different number of wordpress instances
constraint lowerBound (v1, n);

constraint exclusiveDeployment(v3,v4);



solve minimize v1+v2+v3+v4+v5;
