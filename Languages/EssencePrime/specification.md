---
Title: "EssencePrime"
Extensions:
    - eprime
---


EssencePrime is a constraint specification language. The language was originally designed as a subset of [ESSENCE](../Essence), but now has language features which are not in ESSENCE, such as matrix comprehension.

The main feature missing from EssencePrime which is in ESSENCE is high-level types, EssencePrime contains only integers, Booleans and matrices of these types.

There is one actively maintained implementation of EssencePrime:

* [Savile Row](http://savilerow.cs.st-andrews.ac.uk/) provides a complete implementation of the EssencePrime language, mapping EssencePrime to the [Minion Input Language](../Minion) or to FlatZinc suitable for [Gecode](http://www.gecode.org).

There is one inactive implementation of EssencePrime

* Tailor, built by Andrea Rendl and described in her PhD cite{RendlPhDThesis}, implemented an earlier version of EssencePrime.
