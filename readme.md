# Convex CPOS viz

A visualization of [Convex](https://convex.world/developer)'s consensus algorithm.

To run

```
git clone https://github.com/jjttjj/cpos-viz
cd cpos-viz
clj -M -m cpos-viz
```

The gui has the following commands:

* **reset** (param: `n`): Reset the chain and start a new one with `n` peers
* **share all**: all peers merge their belief with all other peers
* **random tx** (`n`): create `n` transactions, each from a random peer to a random different peer
* **gossip** (`n`): each peer merges its beliefs with `n` random other peers
* **step!**: combines the two previous steps: sends transactions then gossips.

# todo
* [ ] Visualize and allow ajusting the actual stakes of each peer
* [ ] Allow seeding/saving/replaying of observed simulations
