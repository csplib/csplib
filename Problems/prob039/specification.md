Title:    The Rehearsal Problem
Proposer: Barbara Smith
Category: Scheduling and related problems

<H3>The Rehearsal Scheduling Problem</H3>

This problem originated at Lancaster University in the 1970s. It is reported to have been devised by a member of staff in the Management Science department, who was a member of an amateur orchestra and formalized the problem whilst waiting to play during a rehearsal.

A concert is to consist of nine pieces of music of different durations each involving a different combination of the five members of the orchestra.

Players can arrive at rehearsals immediately before the first piece in which they are involved and depart immediately after the last piece in which they are involved. The problem is to devise an order in which the pieces can be rehearsed  so as to minimize the total time that players are waiting to play, i.e. the total time when players are present but not currently playing.

In the table below, 1 indicates that the player is required for the corresponding  piece, 0 otherwise. The duration (i.e. time required to rehearse each piece) is in some unspecified time units.


<TABLE>
   <TR><TD>Piece </TD><TD>   1 </TD><TD>  2</TD><TD>   3 </TD><TD>  4 </TD><TD>  5</TD><TD>   6 </TD><TD>  7</TD><TD>   8</TD><TD>   9 </TR>
   <TR><TD>Player 1 </TD><TD> 1 </TD><TD>  1 </TD><TD>  0</TD><TD>   1</TD><TD>   0</TD><TD>   1</TD><TD>   1</TD><TD>   0</TD><TD>   1 </TR>
   <TR><TD>Player 2</TD><TD>  1</TD><TD>   1</TD><TD>   0</TD><TD>   1</TD><TD>   1</TD><TD>   1</TD><TD>   0</TD><TD>   1</TD><TD>   0 </TR>
   <TR><TD>Player 3</TD><TD>  1 </TD><TD>  1 </TD><TD>  0 </TD><TD>  0 </TD><TD>  0 </TD><TD>  0 </TD><TD>  1 </TD><TD>  1 </TD><TD>  0 </TR>
   <TR><TD>Player 4 </TD><TD> 1 </TD><TD>  0 </TD><TD>  0 </TD><TD>  0 </TD><TD>  1 </TD><TD>  1 </TD><TD>  0 </TD><TD>  0 </TD><TD>  1 </TR>
   <TR><TD>Player 5</TD><TD>  0 </TD><TD>  0 </TD><TD>  1 </TD><TD>  0 </TD><TD>  1 </TD><TD>  1 </TD><TD>  1 </TD><TD>  1 </TD><TD>  0 </TR>
<TR><TD>Duration </TD><TD> 2 </TD><TD>  4 </TD><TD>  1 </TD><TD>  3 </TD><TD>  3 </TD><TD>  2 </TD><TD>  5 </TD><TD>  7 </TD><TD>  6 </TR>
</TABLE>


For example, if the nine  pieces were rehearsed in numerical order as given above, then the total waiting time would be:

Player 1: 1+3+7=11

Player 2: 1+5=6

Player 3: 1+3+3+2=9

Player 4: 4+1+3+5+7=20

Player 5: 3

giving a total of 49 units.  The optimal sequence gives 17 units waiting time.

<H3>The Talent Scheduling Problem</H3>

A very similar problem occurs in devising a schedule for shooting a film. Different days of shooting require different subsets of the cast, and cast members are paid for days they spend on set waiting. The only difference between <EM>talent scheduling problem</EM> and the rehearsal problem is that different cast members are paid at different rates, so that the cost of waiting time depends on who is waiting. The objective is to minimize the total cost of paying cast members to wait.

The first problem, <I>Film1</I>,  is based on  one given by Cheng, Diamond and Lin (see <A href="references/">references</A>).

<TABLE>
 <TR><TD>  Day   </TD><TD>  1 </TD><TD > 2 </TD><TD >  3 </TD><TD >  4 </TD><TD >  5 </TD><TD >  6 </TD><TD >  7 </TD><TD >  8 </TD><TD >  9 </TD><TD >  10 </TD><TD >  11
</TD><TD >  12 </TD><TD >  13 </TD><TD >  14 </TD><TD >  15 </TD><TD >  16 </TD><TD >  17 </TD><TD >  18 </TD><TD >  19 </TD><TD >  20 </TD><TD >  Cost/100  </TD></TR>
<TR><TD>Actor 1   </TD><TD> 1 </TD><TD> 1 </TD><TD>  1 </TD><TD>  1 </TD><TD>  0 </TD><TD>  1 </TD><TD>  0 </TD><TD>  1 </TD><TD>  0 </TD><TD>  1 </TD><TD>  1 </TD><TD>  0
 </TD><TD>  0 </TD><TD>  0 </TD><TD>  0 </TD><TD>  0 </TD><TD>  0 </TD><TD>  0 </TD><TD>  0 </TD><TD>  0 </TD><TD>  10</TD></TR>
<TR><TD>Actor 2   </TD><TD> 1 </TD><TD> 1 </TD><TD>  1 </TD><TD>  0 </TD><TD>  0 </TD><TD>  0 </TD><TD>  1 </TD><TD>  1 </TD><TD>  0 </TD><TD>  1 </TD><TD>  0 </TD><TD>  0
 </TD><TD>  1 </TD><TD>   1 </TD><TD>   1 </TD><TD>   0 </TD><TD>   1 </TD><TD>   0 </TD><TD>   0 </TD><TD>   1</TD><TD>  4</TD></TR>
<TR><TD>Actor 3   </TD><TD> 0 </TD><TD> 1 </TD><TD>  1 </TD><TD>  0 </TD><TD>  1 </TD><TD>  0 </TD><TD>  1 </TD><TD>  1 </TD><TD>  0 </TD><TD>  0 </TD><TD>  0 </TD><TD>  0
 </TD><TD>  1 </TD><TD>   1 </TD><TD>   1 </TD><TD>   0 </TD><TD>   0 </TD><TD>   0 </TD><TD>   0 </TD><TD>   0</TD><TD>  5</TD></TR>
<TR><TD>Actor 4   </TD><TD> 0 </TD><TD> 0 </TD><TD>  0 </TD><TD>  0 </TD><TD>  0 </TD><TD>  0 </TD><TD>  0 </TD><TD>  0 </TD><TD>  0 </TD><TD>  1 </TD><TD>  1 </TD><TD>  1
 </TD><TD>  1 </TD><TD>   0 </TD><TD>   0 </TD><TD>   0 </TD><TD>   0 </TD><TD>   0 </TD><TD>   0 </TD><TD>   0</TD><TD>  5</TD></TR>
<TR><TD>Actor 5   </TD><TD> 0 </TD><TD> 1 </TD><TD>  0 </TD><TD>  0 </TD><TD>  0 </TD><TD>  0 </TD><TD>  1 </TD><TD>  1 </TD><TD>  0 </TD><TD>  0 </TD><TD>  0 </TD><TD>  1
 </TD><TD>  0 </TD><TD>   1 </TD><TD>   0 </TD><TD>   0 </TD><TD>   0 </TD><TD>   1 </TD><TD>   1 </TD><TD>   1</TD><TD>  5</TD></TR>
<TR><TD>Actor 6   </TD><TD> 0 </TD><TD> 0 </TD><TD>  0 </TD><TD>  0 </TD><TD>  0 </TD><TD>  0 </TD><TD>  0 </TD><TD>  0 </TD><TD>  0 </TD><TD>  0 </TD><TD>  0 </TD><TD>  0
 </TD><TD>  0 </TD><TD>   1 </TD><TD>   1 </TD><TD>   1 </TD><TD>   1 </TD><TD>   1 </TD><TD>   0 </TD><TD>   0</TD><TD>  40 </TD></TR>
<TR><TD>Actor 7   </TD><TD> 0 </TD><TD> 0 </TD><TD>  0 </TD><TD>  0 </TD><TD>  1 </TD><TD>  0 </TD><TD>  1 </TD><TD>  1 </TD><TD>  0 </TD><TD>  0 </TD><TD>  0 </TD><TD>  0
 </TD><TD>  0 </TD><TD>   0 </TD><TD>   1 </TD><TD>   0 </TD><TD>   0 </TD><TD>   0 </TD><TD>   0 </TD><TD>   0</TD><TD> 4 </TD></TR>
<TR><TD>Actor 8   </TD><TD> 0 </TD><TD> 0 </TD><TD>  0 </TD><TD>  0 </TD><TD>  0 </TD><TD>  1 </TD><TD>  1 </TD><TD>  1 </TD><TD>  1 </TD><TD>  0 </TD><TD>  0 </TD><TD>  0
 </TD><TD>  0 </TD><TD>   0 </TD><TD>   0 </TD><TD>   0 </TD><TD>   0 </TD><TD>   0 </TD><TD>   0 </TD><TD>   0</TD><TD>  20</TD></TR>
<TR><TD>Duration   </TD><TD> 2 </TD><TD> 1 </TD><TD>  1 </TD><TD>  1 </TD><TD>  1 </TD><TD>  3 </TD><TD>  1 </TD><TD>  1 </TD><TD>  1 </TD><TD>  2 </TD><TD>  1 </TD><TD>  1
 </TD><TD>  2 </TD><TD>  1 </TD><TD>  2 </TD><TD>  1 </TD><TD>  1 </TD><TD>  2 </TD><TD>  1 </TD><TD>  1</TD></TR>
</TABLE>


The problem below, <I>Film2</I>, is also based on real film data (although the costs are purely fictitious).  It is easier to solve than <I>Film1</I>.


<TABLE>
<TR><TD>Day     </TD><TD>   1 </TD><TD>  2 </TD><TD>  3 </TD><TD>  4 </TD><TD>  5 </TD><TD>  6 </TD><TD>  7 </TD><TD>  8 </TD><TD>  9 </TD><TD>  10 </TD><TD>  11 </TD><TD> 12 </TD><TD>  13</TD><TD >  Cost/100  </TD></TR>
<TR><TD>Actor 1 </TD><TD>  0 </TD><TD>  0 </TD><TD> 1 </TD><TD> 0 </TD><TD> 0 </TD><TD> 0 </TD><TD> 0 </TD><TD> 0 </TD><TD> 1 </TD><TD> 1 </TD><TD> 1 </TD><TD> 1 </TD><TD> 0 </TD><TD> 40 </TD></TR>
<TR><TD>Actor 2 </TD><TD>  1 </TD><TD> 1 </TD><TD> 0 </TD><TD> 0 </TD><TD> 1 </TD><TD> 1 </TD><TD> 1 </TD><TD> 1 </TD><TD> 1 </TD><TD> 1 </TD><TD> 1 </TD><TD> 0 </TD><TD> 1 </TD><TD> 20 </TD></TR>
<TR><TD>Actor 3  </TD><TD> 0 </TD><TD> 1 </TD><TD> 0 </TD><TD> 0 </TD><TD> 0 </TD><TD> 0 </TD><TD> 0 </TD><TD> 1 </TD><TD> 0 </TD><TD> 0 </TD><TD> 0 </TD><TD> 0 </TD><TD> 0 </TD><TD> 20  </TD></TR>
<TR><TD>Actor 4  </TD><TD> 1 </TD><TD> 0 </TD><TD> 0 </TD><TD> 1 </TD><TD> 1 </TD><TD> 1 </TD><TD> 1 </TD><TD> 1 </TD><TD> 1 </TD><TD> 1 </TD><TD> 0 </TD><TD> 0 </TD><TD> 1</TD><TD> 10 </TD></TR>
<TR><TD>Actor 5  </TD><TD> 0 </TD><TD> 0 </TD><TD> 0 </TD><TD> 1 </TD><TD> 0 </TD><TD> 0 </TD><TD> 0 </TD><TD> 0 </TD><TD> 0 </TD><TD> 1 </TD><TD> 0 </TD><TD> 0 </TD><TD> 0 </TD><TD> 5 </TD></TR>
<TR><TD>Actor 6  </TD><TD> 1 </TD><TD> 0 </TD><TD> 0 </TD><TD> 0 </TD><TD> 0 </TD><TD> 1 </TD><TD> 1 </TD><TD> 0 </TD><TD> 1 </TD><TD> 1 </TD><TD> 1 </TD><TD> 1 </TD><TD> 0 </TD><TD> 10 </TD></TR>
<TR><TD>Actor 7  </TD><TD> 0 </TD><TD> 1 </TD><TD> 0 </TD><TD> 0 </TD><TD> 1 </TD><TD> 0 </TD><TD> 0 </TD><TD> 0 </TD><TD> 1 </TD><TD> 1 </TD><TD> 1 </TD><TD> 0 </TD><TD> 0 </TD><TD> 5 </TD></TR>
<TR><TD>Actor 8  </TD><TD> 0 </TD><TD> 0 </TD><TD> 0 </TD><TD> 0 </TD><TD> 0 </TD><TD> 1 </TD><TD> 0 </TD><TD> 0 </TD><TD> 0 </TD><TD> 1 </TD><TD> 0 </TD><TD> 0 </TD><TD> 0  </TD><TD> 4 </TD></TR>
<TR><TD>Actor 9  </TD><TD> 0 </TD><TD> 0 </TD><TD> 0 </TD><TD> 0 </TD><TD> 0 </TD><TD> 0 </TD><TD> 0 </TD><TD> 0 </TD><TD> 0 </TD><TD> 0 </TD><TD> 1 </TD><TD> 0 </TD><TD> 1 </TD><TD> 5 </TD></TR>
<TR><TD>Actor 10 </TD><TD> 0 </TD><TD> 0 </TD><TD> 0 </TD><TD> 0 </TD><TD> 0 </TD><TD> 0 </TD><TD> 0 </TD><TD> 0 </TD><TD> 1 </TD><TD> 1 </TD><TD> 0 </TD><TD> 0 </TD><TD>  0 </TD><TD>  4 </TD></TR>
<TR><TD>Duration   </TD><TD> 1 </TD><TD> 1 </TD><TD>  1 </TD><TD>  1 </TD><TD>  3 </TD><TD>  1 </TD><TD>  1 </TD><TD>  1 </TD><TD>  1 </TD><TD>  1 </TD><TD>  1 </TD><TD>  1 </TD><TD> 1</TD></TR>
</TABLE>

