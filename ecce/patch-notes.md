# generalize-networks
The functions in `Processor.hs` and `Frontend.hs` have types that are too 
specific.  They need to, instead, describe the top-level behavior, and leave 
the implementation to the specific type of processor.

This is motivated by the problems of transitioning from a transmission-based 
simulation, to an event-based simulation. Too many functions had to be changed.

## Overview
We describe the functions here, with as-of-now undefined types. In the later 
sections, the meaning of the types will be made clear, and the function 
descriptions may be rewritten. If the function descriptions are not rewritten 
in those sections, then it is assumed that they are the same as in this 
section.

### `Process.hs`
- `processStep` takes in a `Process`, and outputs a `(Maybe Process, Maybe 
  Process)`. It partially deconstructs the input, to give the `Process` that 
  was deconstructed out, as well as the remainding `Process`.
- `networkProcessor` runs `processStep` once, on every input `Event Char`, and 
  outputs four things:
  - The `Process` that was deconstructed out,
  - The remaining `Process`,
  - Whether `processStep` returned `Nothing`, and
  - The number of times that `networkProcessor` was run.

### `Frontend.hs`
- `networkTransmit` takes as input the output from `networkProcessor`, and 
  outputs a single data structure `Transmit` that represents the item to be 
  drawn on the GUI.
- `networkTransmitAccum` accumulates outputs from `networkTransmit`, and 
  outputs the accumulation of `[Transmit]`.
- `networkDraw` draws part of the `[Transmit]` outputted by 
  `networkTransmitAccum`.

## Global Protocol
- `Process` is a tree with `Expr GlobalProtocol` as leaves, and nodes that 
  indicate whether its subtree is concurrent or sequential.
- `Transmit` represents the "transmit arrow" that is drawn from the timeline of 
  one party to the timeline of the receiving party. `Transmit` contains the 
  x-coordinates of the sending timeline, the receiving timeline, the 
  y-coordinate of the "transmit arrow", and the text description to be shown on 
  top of the "transmit arrow". So, the type of `Transmit` is `(Float, Float, 
  Float, String)`.

## Projected Party Protocol
- We define a new datatype, `PartyState`, that represents the state of the 
  party. A party is either `Sending` or `Receiving`.
- `Process` is a tree with `Expr PartyProtocol` as leaves, and nodes that 
  indicate whether its subtree is concurrent or sequential.
- `Transmit` represents separate things, and so should be defined as a tuple of 
  two elements:
  - The "transmit arrow" that is drawn from the timeline of one party to the 
    timeline of the receiving party. `Transmit` here is different than the 
    Global Protocol, because there are multiple party protocols, each getting 
    reduced at the same time, hence it is possible for multiple transmissions 
    to happen at the same time. The type here is `[(Float, Float, Float, 
    String)]`.
  - The states of some parties, either `sending` or `receiving`, paired with 
    their x-coordinates. The type here is [(Float, PartyState)].
  So, the type of `Transmit` is `([(Float, Float, Float, String)], [(Float, 
  PartyState)])`.
- `networkTransmitAccum` has an input `transmit`, which is a `Transmit` from 
  `networkTransmit`, and keeps a list of previous party states `transmits`, 
  with type `[Transmit]`.  Note that `transmit` is only defined for some 
  parties, whereas `transmits` is defined for all parties. 
  `networkTransmitAccum` generates the next party states by the following 
  algorithm:
  - If the `Float` in `transmit` matches any of the existing `Float`s in 
    `transmits`, then update the `PartyState` of that `Float` in `transmits`.
  - Otherwise, take the `PartyState` from the last element in `transmits`.
  - Move all "transmit arrows" from `transmit` into `transmits`.
  `networkTransmitAccum` outputs `transmits`.
