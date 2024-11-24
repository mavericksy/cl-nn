# Let's AI like it's 1989

## Delta rule neural network.

Implements a simple back-propogation neural network.

`(setf network (newdeltanetwork '(2 2))` will create a two-layer network with 2 input and 2 output neurons.

Training against this network inside a loop is simple:

`(setf network (deltalearn network '(((1 0) (0 1)) ((0 1) (1 0)))))`

The training list consists of inputs to the network and expected outputs.

See the test folder for examples.

## Hopfield neural network.
