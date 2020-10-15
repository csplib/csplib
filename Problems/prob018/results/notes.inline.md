[Robert Matthews](https://en.wikipedia.org/wiki/Robert_Matthews_(scientist)) suggested that the problem can be turned into a maze if one denotes "position" in the maze by the contents of the three buckets in fixed order, with the problem being to get from "8-0-0" - that is, 8 pints in the biggest bucket, with the others empty, to "4-4-0" - four pints in the 8 and 5-pint buckets, the other one empty. He reports that Ian Stewart at Warwick has shown that "depth-first search" can solve this maze problem.

In a later article, he reports that several people have sent him solutions involving seven transfers of water (some using ingenious graphical solution methods). He asks if this is optimal since it seems rather high.

A simple [Prolog program](../models/enumerate.pl) for enumerating solutions shows that 7 pourings is indeed optimal and the solution is unique.

	8-0-0,3-5-0,3-2-3,6-2-0,6-0-2,1-5-2,1-4-3,4-4-0

Some LPA Prolog [code](http://colin.barker.pagesperso-orange.fr/lpa/jugs.htm) to solve the problem is also available online.
