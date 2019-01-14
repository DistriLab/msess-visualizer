# reduce-party-heads
Calculate the state of each party, and show it.
Each party is either:
- Waiting to send, or
- Waiting to receive, or
- Transmitting to another party.

## Overview
- Project global protocol onto each party, obtaining the mapping from each 
  party to their projected party protocol, `mappingPartyProtocol`.
- Draw the state of all parties in the GUI, depending on their current protocol 
  heads.
  - If sending, draw upward triangle.
  - If receiving, draw downward triangle.
- Write a function that looks for matches in the heads of all values in 
  `mappingPartyProtocol`. There is a match between two parties if one is 
  sending and one is receiving.
  - If there is a match, draw a transmission between the matching parties.
  - If there is no match, the protocol processing ends.
- Cannot use neither `networkProcessor` nor `processStep` anymore. This is 
  because they operate on the global protocol. whereas this patch operates on 
  the per-party protocol. Need define new processing functions.
