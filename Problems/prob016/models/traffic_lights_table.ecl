%
% ECLiPSe sample code
% Author: Joachim Schimpf
% Licenced under CC-BY-4.0 : http://creativecommons.org/licenses/by/4.0/
%
% Traffic lights (CSPLib problem 16, see http://csplib.org/Problems/prob016)
%
% Specification:
% Consider a four way traffic junction with eight traffic lights. 
% Four of the traffic lights are for the vehicles and can be
% represented by the variables V1 to V4 with domains {r,ry,g,y} (for
% red, red-yellow, green and yellow).  The other four traffic lights
% are for the pedestrians and can be represented by the variables P1
% to P4 with domains {r,g}.  The constraints on these variables can be
% modelled by quaternary constraints on (Vi, Pi, Vj, Pj ) for
% 1<=i<=4, j=(1+i)mod 4 which allow just the tuples
% {(r,r,g,g), (ry,r,y,r), (g,g,r,r), (y,r,ry,r)}.
%
% We are interested in the set of all globally consistent 8-tuples
% (which reflects the evolution of the traffic light sequence).
%
%
% Sample run:
%     % eclipse -f traffic_lights_table.ecl -e main
%     [g, g, r, r, g, g, r, r]
%     [r, r, g, g, r, r, g, g]
%     [ry, r, y, r, ry, r, y, r]
%     [y, r, ry, r, y, r, ry, r]
%
% Using symbolic domains and a generalized propagation table constraint
%
 
:- lib(sd).             % symbolic domain solver
:- lib(propia).         % generalized propagation

main :-
        length(Xs, 8),
        Xs &:: [r,ry,y,g],
        append(Xs, Xs, XsXs),
        (
            for(_,1,4),
            fromto(XsXs, [Vi,Pi,Vj,Pj|XsXs1], [Vj,Pj|XsXs1], _)
        do
            allowed(Vi,Pi,Vj,Pj) infers sd      % domain propagation
        ),
        labeling(Xs),   % search
        writeln(Xs),
        fail.

allowed(r, r,g ,g).
allowed(ry,r,y ,r). 
allowed(g, g,r ,r).
allowed(y, r,ry,r).

