Title:    ACC Basketball Schedule
Proposer: Toby Walsh 
Category:


The problem is finding a timetable for the 1997/98 Atlantic Coast Conference (ACC) in basketball. It was first tackled by Nemhauser and Trick.

The 9 basketball teams in the tournament are Clemson (Clem), Duke (Duke), Florida State (FSU) , Georgia Tech (TT), Maryland (UMD), North Carolina (UNC), North Carolina State (NCSt), Virginia (UVA), and Wake Forest (Wake). The problem is to determine a double round robin schedule for these 9 teams subject to some additional constraints. In a double round robin, each team places each other, once at home, once away. The schedule is to be played over 18 dates. The first and all subsequent odd dates are weekday fixtures. The second and all subsequent even dates are weekend fixtures. There are nine other sets of constraints.

**1. Mirroring**. The dates are grouped into pairs (r1, r2), such that each team will get to play against the same team in dates r1 and r2. Such a grouping is called a mirroring scheme. Nemhauser and Trick use the mirroring scheme {(1, 8), (2, 9), (3, 12), (4, 13), (5, 14), (6, 15), (7, 16), (10, 17), (11, 18)}

**2. No Two Final Aways**. No team can play away on both last dates.

**3. Home/Away/Bye Pattern Constraints**. No team may have more than two away matches in a row. No team may have more than two home matches in a row. No team may have more than three away matches or byes in a row. No team may have more than four home matches or byes in a row.

**4. Weekend Pattern**. Of the weekends, each team plays four at home, four away, and one bye.

**5. First Weekends**. Each team must have home matches or byes at least on two of the first five weekends.

**6. Rival Matches**. Every team except FSU has a traditional rival. The rival pairs are Duke-UNC, Clem-GT, NCSt-Wake, and UMD-UVA. In the last date, every team except FSU plays against its rival, unless it plays against FSU or has a bye.

**7. Constrained Matches**. The following pairings must occur at least once in dates 11 to 18: Wake-UNC, Wake-Duke, GT-UNC, and GT-Duke.

**8. Opponent Sequence Constraints**. No team plays in two consecutive dates away against UNC and Duke. No team plays in three consecutive dates against UNC, Duke and Wake (independent of home/away).

**9. Other Constraints**. UNC plays its rival Duke in the last date and in date 11. UNC plays Clem in the second date. Duke has a bye in date 16. Wake does not play home in date 17. Wake has a bye in the first date. Clem, Duke, UMD and Wake do not play away in the last date. Clem, FSU, GT and Wake do not play away in the first date. Neither FSU nor NCSt have a bye in last date. UNC does not have a bye in the first date.
