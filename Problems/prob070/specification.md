---
Title:  Distributed Channel Assignment Problem
Proposer: Ferdinando Fioretto
Category: 
	- Distributed CSP/COP
---

### Overview

The performance of a wireless local area network (WLAN) depends on the channel assignments among neighboring access points (APs). Neighboring transmissions occurring in APs on the same channel or adjacent channels degrade network performance due to transmission interference. Typically, in dense urban areas, APs may belong to different administrative domains, whose control are delegated to different entities. Thus, a distributed approach to the channel assignment is often necessary.

### Distributed COP Formulation

In a cooperative distributed channel assignment problem (DCA), APs need to be configured in order to reduce the overall interference between simultaneous transmissions on neighboring channels. In a distributed COP based approach each AP is represented by an agent, which controls a decision variable modeling a choice for the AP channel. The signal-to-interference-and-noise ratio (SINR) perceived by an AP is modeled as a cost function, as the overall concurrent transmissions occurring in the same channel and in partially overlapped adjacent channels. The constraint graph $G=(V,E)$ of the problem defines the interfering neighbors, where $V$ represents the APs, and $E$ describes the set of interfering APs.  
<!--These can be computed using the receive signal strength indicator (RSSI) cite{monteiro:12}.-->
The goal is to find an assignment of channels to APs that minimizes the sum of the interferences experienced in the WLAN, at the APs.

*The general definition of the DCA problem is as follows:*

- A set $A$ of $p$ APs.
- A set $X$ of $p$ AP's channels choices.
- Each channel choice $c_i \in X$ represents the channel selection number of the AP $a_i \in A$. 
- The domain of $D_i$ of $c_i$ is the set {$1, 2, \ldots, 11$}, describing the possible channels choices.
- For a given pair of APs $a_i$ and $a_j$, such that there exists an edge $(c_i, c_j) \in E$, the SINR function is defined as $s_{ij} : D_i \times D_j \to \mathbb{R}$ which represent the interference overlap factors summarized in the following Table.

|   $D_i$ \ $D_j$      | 0              | 1               | 2             | 3              | 4              | 5              | 6              | 7             | 8              | 9              | 10            | 11           | 
|         | :---:           | :---:          | :---:          | :---:           | :---:          | :---:          | :---:          | :---:          | :---:           | :---:          | :---:          | :---:          | 
| $0$ | $1$          | $0.7272$ | $0.2714$ | $0.0375$ | $0.0054$ | $0.0008$ | $0.0002$ | $0$          | $0$          | $0$          | $0$          | $0$          |
| $1$ | $0.7272$ | $1$          | $0.7272$ | $0.2714$ | $0.0375$ | $0.0054$ | $0.0008$ | $0.0002$ | $0$          | $0$          | $0$          | $0$          | 
| $2$ | $0.2714$ | $0.7272$ | $1$          | $0.7272$ | $0.2714$ | $0.0375$ | $0.0054$ | $0.0008$ | $0.0002$ | $0$          | $0$          | $0$          | 
| $3$ | $0.0375$ | $0.2714$ | $0.7272$ | $1$          | $0.7272$ | $0.2714$ | $0.0375$ | $0.0054$ | $0.0008$ | $0.0002$ | $0$          | $0$          |
| $4$ | $0.0054$ | $0.0375$ | $0.2714$ | $0.7272$ | $1$          | $0.7272$ | $0.2714$ | $0.0375$ | $0.0054$ | $0.0008$ | $0.0002$ | $0$          |
| $5$ | $0.0008$ | $0.0054$ | $0.0375$ | $0.2714$ | $0.7272$ | $1$          | $0.7272$ | $0.2714$ | $0.0375$ | $0.0054$ | $0.0008$ | $0.0002$ |
| $6$ | $0.0002$ | $0.0008$ | $0.0054$ | $0.0375$ | $0.2714$ | $0.7272$ | $1$          | $0.7272$ | $0.2714$ | $0.0375$ | $0.0054$ | $0.0008$ |
| $7$ | $0$          | $0.0002$ | $0.0008$ | $0.0054$ | $0.0375$ | $0.2714$ | $0.7272$ | $1$          | $0.7272$ | $0.2714$ | $0.0375$ | $0.0054$ |
| $8$ | $0$          | $0$          | $0.0002$ | $0.0008$ | $0.0054$ | $0.0375$ | $0.2714$ | $0.7272$ | $1$          | $0.7272$ | $0.2714$ | $0.0375$ |
| $9$ | $0$          | $0$          | $0$          | $0.0002$ | $0.0008$ | $0.0054$ | $0.0375$ | $0.2714$ | $0.7272$ | $1$          | $0.7272$ | $0.2714$ |
| $10$ | $0$        | $0$          | $0$          | $0$          | $0.0002$ | $0.0008$ | $0.0054$ | $0.0375$ | $0.2714$ | $0.7272$ | $1$          | $0.7272$ |
| $11$ | $0$        | $0$        | $0$          | $0$          | $0$          | $0.0002$ | $0.0008$ | $0.0054$ | $0.0375$ | $0.2714$ | $0.7272$ | $1$            |


The above Table illustrates the normalized values of the spectrum overlapping factors. Detailed information on how these values are derived can be found in cite{monteiro:12}.
<!--
| Channel Spacing | Overlapping Factor |
| :-----: | :------: | 
| $0$   | $1$    | 
| $1$   | $0.7272$  | 
| $2$   | $0.2714$  | 
| $3$   | $0.0375$  | 
| $4$   | $0.0054$  | 
| $5$   | $0.0008$  | 
| $6$   | $0.0002$  | 
| $7-10$   | $0$ |
-->