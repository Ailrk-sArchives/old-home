-- tag note lambda-calculus
-- title computability, lambda calculus
-- date 2020-10-04
-- source https://crypto.stanford.edu/~blynn/lambda/
          https://en.wikipedia.org/wiki/Turing_completeness
          https://en.wikipedia.org/wiki/Church%E2%80%93Turing_thesis
;;
# Lambda Calculus

## Compare with turing machine
Both lambda calclus and turing machines are both turing complete computational model.

#### Turing machine
Basic idea: A state machine read and write on a infinite tape.
- Pros
    - The model match today's hardware very well. (You can easily make the analogy between pointer-tape and cpu-memory.)
    - It's easy to measure conplexity. (The amount of instructions can be a straight forward measurement.)
    - The model is easy to reason about: (Write data to memory, and read it laster to decide the future actions.)

#### Lambda Calculus
- Pros
    - Simpler to represent lost of operation. (Some simple tasks like multiplication is hard to implement in turing machine. Look at how mutiplication is implemented at instruction level.)
    - Solves the halting problem with typed lambda.
    - Provable. (Curry Howard isomorphism. The isomorphism is between typed lambda calculus and propositions, there is no such notion for turing machine)


