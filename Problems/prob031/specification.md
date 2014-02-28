Title:    Rack Configuration Problem
Proposer: Zeynep Kiziltan
          Brahim Hnich
Category: Design and configuration


The rack configuration problem consists of plugging a set of electronic cards into racks with electronic connectors. Each card plugged into a rack uses a connector. In order to plug a card into a rack, the rack must be of a rack model.


Each card is characterised by the power it requires. Each rack model is characterised by the maximal power it can supply, its number of connectors, and its price. The problem is to decide how many of the available racks are actually needed, and which rack is of which rack model model such that

* every card is plugged into one rack
* the total power demand and the number of connectors required by the cards does not exceed that available for a rack
* the total price is minimised.

A reference cite{sabin1999optimization}