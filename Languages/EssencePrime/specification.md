---
Title: "Essence'"
Extensions: 
	- eprime
	- SavileRow
---


ESSENCE' is a constraint specification language. The language was originally designed as a subset of [ESSENCE](../Essence), but now has language features which are not in ESSENCE, such as matrix comprehension.

The main feature missing from ESSENCE' which is in ESSENCE is high-level types, ESSENCE' contains only integers, Booleans and matrices of these types.

There is one actively maintained implementation of ESSENCE':

* [Savile Row](http://savilerow.cs.st-andrews.ac.uk/) provides a complete implementation of the ESSENCE' language, mapping ESSENCE' to the [Minion Input Language](../Minion) or to FlatZinc suitable for [Gecode](http://www.gecode.org).

There is one inactive implementation of ESSENCE'

* Tailor, built by Andrea Rendl and described in her PhD cite{RendlPhDThesis}, implemented an earlier version of ESSENCE'.
