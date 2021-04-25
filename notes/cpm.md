Notes on Computation, Proof, Machine by Gilles Dowek

# Introduction

Mathematics changed more in the 20th century than in all previous
centuries combined, and the 21st may see equally dramatic changes.

Since the 1970s, the notion of proof has been changing quickly, driven
by new understandings of the "somewhat underrated" concept of computation.

Since Euclid and until recently, computation has been relegated to
the boring parts of mathematics, and completely insulated from the
process of proof by axioms and inference rules.  This is an ironic
oversight.

Recent discoveries about the axiomatic method, "challenging the
primacy of reason over computing", suggest a closer relationship
between these activities.

This shift also bears on the relationship between mathematics and
the natural sciences, and on philosophical issues like the notion of
analytic and synthetic judgements.

This "revolution" also reveals new ways of solving mathematical
problems and of thinking about mathematical concepts.

Two theories launched the "crisis of the axiomatic method" and
brought computing to the fore: Computability and constructivity.


# Part 1

# Chapter 1

A tablet found in Mesopotamia dating to 2500 CE records the
earliest-known mathematical activity.  It describes an arithmetic
problem relating to the distribution of grain and shows that
Mesopotamian accountants understood how to perform division.
Other accountants and surveyors from the pre-Greek world could
measure the area of rectangles and circles, and had an understanding
of how to solve quadratic equations.

The start of the "history of mathematics" is placed by many in the
5th century BCE.  Why?

The search for an isosceles right triangle by the Pythagoreans
required a solution to the equation 2x² = y².

The nonexistence of this solution led to the discovery of irrational
numbers, but this took many centuries.

The way this problem was approached by the Pythagoreans is
revolutionary.  It was a highly abstract problem, more so than
the grain distribution and land measurement problems of the
Mesopotamian tablet.  The Pythagoreans were dealing with abstract
triangles and numbers, not with barns and measures of grain.  This
abstractive step is crucial to the history of mathematics, but it
meant a very different approach to solving the problem at hand.
As long as the objects of mathematics were physical objects, an
exhaustive solution might be sufficient.  Since they were dealing
with abstract objects, a general solution had to be found.

The "rift ... between mathematical objects, which are abstract,
and concrete, natural objects" which this problem and its solution
opened "was the big breakthrough of the 5th century BCE".

Comparing the concrete problems solved by the Mesopotamians and
Egyptians with the abstract problems of the Greeks, we see that
different ways of finding a solution were used: the concrete
problems were solved by computing, while the abstract ones
required reasoning.  The first was just a matter of applying an
algorithm; the second required imagination.

There was a certain determinism to the division problems of the
Mesopotamian accountants--after applying the algorithm, a result
was sure to be found.  The people applying the kind of reasoning
needed to prove the irrationality of sqrt(2) had no clear path,
and no assurance what kind of result they would find, or whether
there would be a result.

Mathematics takes imagination (hard); applying an algorithm is
just following steps (easy).

Why did mathematicians drop computation?

What puts abstract problems out of the reach of computation?

The difference between these types of problems is the "irruption
of the infinite".  "Reasoning" problems require solutions covering
an infinite number of objects (e.g. all possible right triangles).

If we only had to solve a problem for a finite set of objects, we
could solve it by computation.

What is reasoning?

Inference rules are the core of mathematical reasoning.

These were of course first formally investigated by the Greeks,
particularly Aristotle and "the Stoics".

Oddly, the logical discoveries of these philosophers had barely
any influence on the mathematicians, even though the same techniques
were used in reasoning and argument.

Euclid's works use deductive reasoning to prove everything, yet
they never make any reference to the techniques of Aristotle, etc.

Possibly this was due to the coarseness of the logic of the time.
With only atomic propositions of the form "Socrates is mortal" and
conjunctions, logic was a blunt tool.

Aristotle's logic also lacked a way to denote individuals.

Even without this defect (e.g. as extended by the Medieval
philosophers), Aristotelian logic can't express propositions like
"4 is less than 5".  This makes it a rather poor tool for mathematical
reasoning.

No one seriously tried to develop a logic usable for mathematics
until the 17th century (Leibniz).  Things really moved forward with
Frege in 1879, followed by Russell & Whitehead, Hilbert, etc.

"The inference rules and the grammar of mathematical propositions
simply remained implicit until the 19th century."

For 2000 years, Euclid's *Elements* was the model of mathematical
reasoning.  Axioms and inference lead to theorems.

There is no computation, and the Greek mathematicians did not try
to understand how the reasoning approach might be related to the
computational approach of their predecessors.  They seemingly
abandoned computation as a serious mathematical tool.

# Chapter 2

Even if computation was discounted, mathematicians continued to
develop algorithms.  This is the "hidden history" of mathematics.

Euclid, whose geometrical work is entirely "reasoning-based" but
who also discovered an algorithm for finding GCDs, is a good
example of this split in mathematical history.

The simplest algorithm for finding the GCD of two integers is
to enumerate the divisors of both numbers and to take the largest
integer appearing in both lists.

Euclid's algorithm is less tedious and is guaranteed to find a
result in a finite number of division steps.  Its validity rests
on several theorems--a crucial point.  For integers a and b,
b ≤ a,

* If the remainder resulting from dividing a by b is 0, then b
  is the GCD.

* If r ≠ 0 is the remainder of a / b, then the common divisors
  of a and b are the same as those of b and r.

* The remainder of a division is always less than the divisor.

* A decreasing series of natural numbers is necessarily finite.

Euclid demonstrated these theorems by axiomatic reasoning.

Most of the time, constructing an algorithm requires a reasoning
process.

So how did the Mesopotamians and Egyptian mathematicians develop
algorithms to solve arithmetic problems?

Presumably the reasoning process was familiar to them, but implicit.
(NB: Sort of the reverse of the situation with inference rules in
Greek axiomatic methods.)

A hypothesis is that the "reasoning revolution" came about through
the process of thinking about and elaborating algorithms.

There is an apparent contradiction between mathematical discourse
(reasoning-based) and mathematical practice (computation).

How could computation be ignored by mathematicians who were aware
of and who worked on algorithms like Euclid's?

Another way to look at solving a problem with Euclid's algorithm
phrases the process entirely in terms of makeing inferences from
the underlying theorems.  The "algorithm" is then implicit.
We can think of this as a process of reasoning from axioms and
established results.

Computation -> Proof.

### Positional notation

The decimal Arabic notation for numbers, which allows simple
addition and subtraction algorithms to be used, is a descendent
of Mesopotamian systems used as early as 2000 BCE.  Refined
by Indian and Arabic mathematicians and introduced into Europe
through the works of al-Khwarizmi, this notation drastically
changed European mathematics.  It essentially comes from a
computational tradition; the Greek tradition had no comparable
notation.

### Calculus

Archimedes's first solution to finding the area of a parabola
involved finding upper and lower bounds by summing triangular
subdivisions.  In the 16th century, Stevin and Viète bypassed
this method by computing sums of infinite sequences.  Descartes's
analytic geometry then allowed many curves to be described by
equations.  The discovery of the link between differential and
integral calculus showed a way to find the area of these curves,
thanks to their equational representation.

Calculus was greatly simplified by the discovery of basic
theorems concerning differentiation:

* The derivative of a sum is the sum of the derivatives.

* The derivative of kf is k times the derivative of f.

* The derivative of xⁿ is nxⁿ⁻¹.

These theorems, proved by reasoning, allow us to solve derivatives
(some of them) through simple computation.

Similar theorems make finding antiderivatives more of a
computational process.

By calculating a primitive of the equation of a parabola (using
basically a computational process), we can bypass all of the
reasoning processes used by Archimedes, etc., and obtain an
area for the curve.

Until the twentieth century, finding primitives required a
mix of computation and reasoning.

But the 17th century development of calculus reduced many
problems important to geometry and physics to computations.
The areas of many figures that were beyond the reach of ancient
mathematicians were found thanks to these (algorithmic) tools.

# Chapter 3: Predicate Logic

The development of computation since the mid 19th century has
been in tandem with that of reasoning.  The turning point comes
with Frege, who was investigating Kant's notion of synthetic and
analytic judgements.

Frege's investigation into logic began with a disagreement with
Kant's belief that all mathematical judgements are synthetic,
a priori judgements.  (e.g. the judgement that 2 + 2 is 4.)
Frege believed that such judgements are analytic.

Reasoning, according to Frege, was the process of making explicit
the implicit properties of mathematical objects.  e.g. the property
of being equal to 4 is implicit in the definitions underlying the
number that is 2 + 2.

These implicit properties of definitions were not necessarily
forseen by anyone, including the creator of the definition.

This led Frege to first provide a definition of the natural numbers,
something that Dedekind and Peano had also investigated.

An interesting point: Before the late 19th century, no-one had
tried to establish arithmetic on a basis derived by reasoning.
Why not, considering that Euclid did this with geometry over
1k years earlier?

Did the effective algorithms which had been known for so long make
it seem unnecessary to prove the soundness of arithmetic?

Frege's definition of the integers depended on the notion of a
set (taking some inspiration from David Hume).

Frege took propositions to be formed from atomic propositions and
conjunctions.  Unlike earlier logicians, he allowed predicates to
be relational, e.g. "is smaller than", which relates two arguments.
Medieval ideas of predicates only allowed unary predicates.

Frege, like C.S. Peirce, allowed quantified variables to stand for
propositions.  This idea goes back to at least the 16th century, in
the works of François Viète.

He also provided a number of familiar inference rules.

## The Universality of Mathematics

Frege both developed a logic richer than that of previous logicians
and gave a formal definition of the natural numbers, including a
proof of the soundness of natural number addition.

By combining logic with mathematical reasoning, Frege united two
streams of thought which had, for some reason, been distinct.

"There is no specifically *mathematical* reasoning."

There are also no specifically "mathematical" objects, since numbers
were shown to be based in the very general notion of a set.
We should define mathematics in terms of how it *describes* its
objects, which are themselves universal.

## Predicate logic and set theory

Paradoxes were found in Frege's logic, first by Burali-Forti in 1897.
Russell and Whitehead created a revised version of the logic
incorporating types in 1903, which avoided some of these paradoxes.

Russell and Whitehead defined a hierarchy of types.  Non-set entities
(atoms) are of type 0, sets of atoms are of type 1, sets of type-1
entities are of type 2, and so on.  A result of this structure is that
the proposition "x is an element of y" is only valid when the type of
y is one greater than that of x.

In this system, there can be no set of all sets.

Like Frege's logic, Russell and Whitehead's logic combined set theory
with logic (e.g. inference rules).  Both systems included notions
specific to sets, an artifact of Frege's view of sets as concepts.

Hilbert created a set-less refinement called "predicate logic" in
the 1920s, which is the basis of all later logic.  Set-specific
axioms, on the other hand, were relegated to set theory.

This apparently destroyed Russell's thesis of the universality of
mathematics.  To do mathematics within predicate logic, it was
apparently necessary to pull in set theory.

So is mathematics the set-theoretical side of logic?

Gödel did show in 1930 that any theory can be translated into set
theory, which revives the universality idea.

## The problem of axioms

Predicate logic alone cannot define the natural numbers.  Some
axioms are needed.  Peano provided some which were equivalent to
the old set-theoretical ones, but simpler.

What does the need for axioms do to Frege's thesis that
mathematical judgements are analytic?  Is the acceptance of
an axiom analytic or synthetic?  More generally, why should
we accept axioms in the first place?

Poincaré tried to answer this question: axioms provide definitions
of concepts.  The true propositions which contain a word--"point",
"line", or anything--are constitutive of that word's meaning.
"There is only one straight line through two points" is thus a
valid axiom, since it forms part of the definition of the concepts
of "straight line", "point", and "through".

## The results of Frege's project

So axioms *were* needed to establish arithmetic.  If we extend
the notion of definition to include axioms (per Poincaré), then
Frege succeeded: "2 + 2 = 4" is a consequence of the axioms of
arithmetic, and thus of the defs. of the natural numbers, etc.

So whether mathematical judgements are "analytic" or "synthetic"
hinges on your view of axioms.

And there are other notions of analyticity.  Basically, Kant's
division of concepts was based on a rather hazy distinction.

But out of this project came many discoveries, most critically
that of predicate logic, which Hilbert "released" in 1928.  This
was the biggest advance in the understanding of reasoning since
the BCE years.


# Chapter 4

Two critical theories that emerged at the beginning of the 20th
century: computability and constructivity.  Both were accounts
of computation and had many similarities, but these went
unremarked for a while.

OTOH, they were developed to solve different problems.

Predicate logic, as it was developed, deals with inference rules and
allows the construction of proofs through inference steps.  There is
no computation, apparently; predicate logic went back to "the axiomatic
vision".

Solving a problem in predicate logic amounts to showing that a
proposition is true/false.

From a predicate logic perspective, Euclid's algorithm is a proof
technique--it allows us to decide whether propositions of the form
"the greatest common divisor of the numbers x and y is the number z"
is provable or not.  This is a significant change.

A large number of algorithms were developed in the early 20th century
to determine the provability of classes of propositions.

Presburger (1929): algorithmic techniques to decide the validity of
arithmetic expressions with addition but not multplication.

Skolem (1930): The same, for multiplication without addition.

Tarski (1930) came up with an algorithm that covered both.
This implied that all geometrical problems of the kind conceived by
Euclid could be solved by computation.

## The decision problem

The obvious question that this work suggests: Can algorithms replace
reasoning in mathematics as a whole?

Hilbert posed this as the Entscheidungsproblem.

A "computable" function was one for which an algorithm exists which
can compute its value.  Is their a computable function which takes
any proposition to a Boolean value indicating its provability?

Clearly, this calls for a metamathematical computable function.

Studying predicate logic--the means of reasoning--as an object in
itself was required.

Which the discovery of such an algorithm would have immense
practical value for mathematicians, Hilbert's other idea was the
avoidance of inconsistencies in predicate logic; similar things
hidden in Frege's logic led to its downfall.

## Elimination of the infinite

By specializing a quantified expression to a finite range (of, say,
integers), Presburger, Skolem, Tarski, etc. could "eliminate the
infinite".  As discussed earlier, the need to develop solutions
general to an infinite number of cases was probably one of the main
reasons that ancient mathematicians turned to reasoning as their
primary tool.

The success of quantifier elimination in tackling arithmetic
propositions led people to hope for a similar, general technique.

## Church's theorem

Church and Turing both found negative answers to the
Entscheidungsproblem in 1936.

By this point in the 20th century, reasoning had been studied and
formal models given.

To solve the Entscheidungsproblem, Turing and Church had to do the
same thing with computation: they needed formal definitions of the
idea of an algorithm and of a computable function.

Herbrand & Gödel, Church, Turing, and Kleene all proposed models
for computation, which eventually turned out to be equivalent.

All of these models had the notion of transformation, or rewriting,
steps.  "Computation is thus defined as the gradual transformation
of one expression into another, the process being guided by a body
of rules."

This is surprisingly similar to the formalized notion of reasoning,
in which expressions are replaced by others via inference rules.

The difference was identified by precisely the same people.  It was
the kernel of the reason why Hilbert's program was impossible:
the kind of replacement steps occurring in a reasoning process may
extend infinitely.

Critical to the formal models of computation by the above people
was the notion of termination: an "algorithm" reaches a result in
a finite number of steps.

By this definition, Euclid's "algorithm" *is* one.

Also by this definition, Hilbert's sought-after "algorithm"
couldn't be.

But this didn't rule-out replacing reasoning with a "computation
method", since inference rules now clearly had computational
equivalents!

So Hilbert wasn't merely searching for a "computational method"
which could replace reasoning; he was looking for a method with
a guarantee of termination.  That the former was possible was
apparently obvious by this point.

## Algorithms as objects of computation

An interpreter is an algorithm which operates on sets of computation
rules, as well as some input.  (From this perspective, it is a
function of two arguments.)

e.g. if U is an interpreter, a the set of rules defining Euclid's
algorithm, and b the pair (90, 21), then U applied to a and b computes
the GCD of 90 and 21.

## The halting problem

The discovery of the halting problem concerned an interpreter.
Namely, the idea was to construct an interpreter A which would
give the result 1 if its computation ruleset halted on the given
input, and 0 otherwise.

It was proven in 1936 by Turing, Church, and Kleene (independenty)
that A cannot exist, and that the problem is undecidable.

The obvious thing to do was to apply this theorem to Hilbert's
decision problem.

Church's theorem establishes that, were there a decision procedure
like that imagined by Hilbert, it could be used to solve the halting
problem.  So there is no such decision algorithm.

This demonstrates the gulf between reasoning and computation.  There
are some problems which cannot be solved by computation.

## Analytic does not mean obvious

A traditional complaint about "analytic" judgements is that they
reveal nothing new; an analytic process is a "tautology machine",
since it only produces the results implicit in the axioms of the
system.

Church's Theorem shows this view to be simplistic.  If analytic
reasoning were trivial, there would certainly be an algorithm to
replace it.  But there can't be, so reasoning must produce results
which cannot be discovered through (provably terminating)
computation.

Critical quote: "Metaphorically, this operation may be compared to
the work of a prospector, panning for gold in a river, sifting the
sand for months in search of a nugget.  Of course, the nugget was
in the sand from the very start, but it would be an overstatement
to claim that finding it is as trivial as bending over and picking
it up.  Even when a result is implicit in the axioms, making it
explicit yields information and knowledge."

Many "analytic" propositions which can be decided by algorithms may
yet be very much non-obvious, in the sense that they may take years
of computation to solve.


# Chapter 5

After finding an answer to Hilbert's decision problem,
mathematicians continued to solidify the notion of an algorithm.
The various systems they developed all turned out to be equivalent,
a remarkable result.

These systems would now be called "programming languages".

In the 30s, it was hard to believe in their being "one" notion
of computation.

The equivalence of all computational models is known as Church's
Thesis.  (More often, the Church-Turing Thesis.)

This thesis claims the identity of (a) any computational model
and (b) "the" common concept of computation.  (b) is hard to
define precisely.

The claim sounds a bit extreme.

So the "common" concept of an algorithm needed a clearer statement.

Two versions of the Church-Turing thesis:

* Any algorithm that a human being can execute to solve a specific
  problem can be expressed by a set of computation rules. (The
  "psychological form")

* All the algorithms that a machine is capable of executing
  "systematically" to solve a specific problem can be expressed by
  a set of computation rules.  (The "physical" form)

For people holding materialist beliefs, the psychological form is
just a consequence of the physical form.

But the two forms are not equivalent.

One interesting consequence of this thesis is the notion of "the
computational completeness of human beings".  If the psychological
form is true (whether as a consequence of the physical form or not),
then human beings cannot be beaten as computing devices.

In a way, this is "the converse of materialism".  While it would
also be true that human computation is always expressable by
computation rules, the further consequence is that anything that
*can* be computed can be computed on a human being.

Let:

    R = the set of algorithms expressible by a body of
        computation rules
    M = the set of algorithms that can be computed by a physical
        system
    H = the set of algorithms that can be computed by a human

The theses are:

* M ⊂ R: the physical form of the Church-Turing thesis
* H ⊂ R: the psychological form
* H ⊂ M: the "materialist" thesis
* M ⊂ H: the computational completeness of human beings

It certainly holds that R ⊂ M and R ⊂ H.

## The physical form of Church's thesis

Neither form of the Church-Turing thesis can be proved by
mathematics alone.

Robin Gandy proposed (1978) a proof of the physical form.

This proof goes way beyond computation rules and proposes a physical
model of the states and tranformations of computing mechanisms.
Here, computation is a series of state transitions, each one
depending only on the previous state of the system, with the result
of the computation being the final state.

Fundamental assumptions of Gandy's argument: Euclidean space,
finite density of information, finite information transmission
speed.

Are these assumptions too much?  Would Gandy's argument still work
with adjustments for more accurate physics?

In any case, the Church-Turing thesis has held up very well, so far.

## Mathematizing nature

A question related to the Church-Turing thesis is why (or whether)
mathematical concepts are so good at describing nature.

There are dramatic examples of mathematical objects being almost
perfectly suited to describing phenomena first studied centuries
after the development of the mathematical tools.  (e.g. ellipses
and Kepler's Laws.)

Is it because mathematics is vaguely based on empirical models?
Are natural scientists simply ignoring what doesn't yield to
mathematical tools?  If the latter, why do the un-overlooked
phenomena fit so well?

Of course, there is approximation and simplfication.

Sometimes, a phenomenon's conformance to some mathematical model
is not explained until it is better understood.

The discovery of the atomic masses of the elements is a good
example of this.  The amazing regularity of the periodic table,
and Mendeleev's successful prediction of the existence of scandium,
gallium, and germanium based purely on the structure of the table,
was later found to be a consequence of the number of particles in
each atom's nucleus.  Mendeleev "merely" foretold the possibility
of nuclei made up of 45, 68, or 70 particles.  (Of course, this
opened up more puzzles of mathematical regularity.)

Physical phenomena, e.g. gravity, often turn out to be
fairly deterministic, which the Church-Turing thesis doesn't have
anything to say about.

More relevant to this thesis is the fact that we can represent
properties of physical processes, e.g. the relationship between
time and distance, by mathematical propositions.

The entire physical system comprising an experiment (the book's
example is a system of a tower, a ball, a clock, and a height gauge
used to measure the ball's movement over a given time) is, from the
Church-Turing perspective, a calculating machine.  It takes a time
measured in seconds (or whatever) to a distance.  The thesis claims
that this system is performing a calculation which could as well
be performed by any system equipped with the appropriate algorithm.

So the physical Church-Turing thesis implies that the law of
gravitation can be expressed in mathematical language!

It expresses a remarkable idea about nature: that the behavior
of natural phenomena can be captured by computation rules.
This is a direct consequence of the physical form of Church's
thesis, or, alternatively, of the psychological form and the
theory of the computational completeness of human beings.

Even more generally: *Our* computing capacity is merely a
consequence of the computing capacity of nature.

Although "laws" of phenomena may be inherently mathematizable by
this thesis, this still would not account for their (sometimes)
remarkable simplicity when mathematized.

## The form of natural laws

This explanation-sketch of the mathematizability of the theories
of natural phenomena leads to an investigation of the *form* of
these mathematical propositions.

An example proposition is the equation relating the distance
covered by a free-falling ball and the time during which it falls,
d = 1/2gt².

Why does this proposition exist?

Does it hold because there exists an algorithm which lets you
calculate d from a given t?  If so, then it's an algorithm which
connects these physical quantities.

If nature is indeed mathematizable *and* computable, then we can
express theories about it not only in propositions, but in
algorithms.

An example: grammars.

Enunciations (grammatical utterances) take place in nature, so
the Church-Turing thesis suggests that there exists an algorithm
or at least a computing method which can evaluate an utterance in
a given language.

The psychological form further suggests that a speaker of a language
must have a method of evaluating utterances, and that, therefore,
it must be possible to express a grammar as an algorithm, not just
a series of rules (propositions).

Reformulating grammars as algorithms raises many new questions.


# Chapter 6: Lambda calculus

As well as providing a negative answer to the decision problem, the
development of computability was part of an attempt to bring computation
back into mathematics.  Lambda calculus was the central component of
this attempt.

The "functional expression" notation for functions, e.g. x ↦ x × x,
was probably first used by the Bourbaki group around 1930.  Almost
simultaneously, Church introduced the notation λx(x × x).
(The story about the publisher's corruption of "hat-x" to "λx" is
bogus.  Church's choice of λ was either an adaptation of
Russell & Whitehead's hat-x notation or completely arbitrary; Church's
explanations varied.)

Church allowed functions to be treated as first-class values.
Russell & Whitehead had suggested something like this; they thought
that replacing the f variable in f(4) with x ↦ x × x gave 4 × 4.
Church objected that the real result of this substitution is
(x ↦ x × x)(4); only by using the further rule of application would
4 × 4 be derived.  With this objection, Church divided two things
that had traditionally been confuted: substitution and application.

Using application (beta reduction), substitution, and the rest,
Church was able to construct a computationally complete system.
Lambda calculus is thus equivalent to a universal Turing machine
or to Herbrand-Gödel equations.  This took a while for mathematicians
(including Gödel) to accept.

The even more interesting part is Church's proposal of a new
formalization of mathematics based on lambda calculus, which he
provided (also in the early 30s).

Every function in such a system would be expressed directly by
an algorithm.

Kleene and Rosser showed in 1935 that this system was inconsistent.

Haskell Curry, with help from Church, tried to rework the theory.
Church tried an approach similar to what Russell's types and tried
eliminating self-application; this restored consistency, but at the
cost of the full power of lambda calculus.

In sum, Church et al's attempt to provide a computational
formalization for mathematics failed.


# Chapter 7: Constructivity

The theory of constructivity was developed independently of that
of computability.

The original insight was probably to notice that some proofs show
the existence of an object satisfying a certain property *without
giving us such an object*, whereas other proofs establish their
claim by providing an example object.

Proofs of the first kind are called *nonconstructive*.  Those of
the second kind are called constructive.

The only inference rule which allows us to prove propositions of
the form "there exists x such that A" is the *existential
quantification introduction rule*.  To use this, we must have an
instance of A--a proposition similar to A with some expression
replacing x, e.g. from "Vienna is an Austrian city at which the
Orient Express stops" we can use existential quantification
introduction to obtain "There exists an Austrian city at which
the Orient Express stops".

So the use of such a rule requires a witness, which can always be
found in the instance of A used.

So how can we "lose" witnesses in nonconstructive proofs?

"The principle of excluded middle is an inference rule that allows
us to prove a proposition of the form 'A or not A' without having
to prove a premise." (Good statement.)

This is what allows us to produce proofs of existence which do
not provide witnesses.

## Brouwer's constructivism

Before the debate on constructive vs. non-constructive reasoning,
many mathematicians used the principle of excluded middle out of
convenience; they were not necessarily writing nonconstructive
proofs.

The first real nonconstructive proofs probably appeared in the
late 19th century.  Kronecker and Poincaré were among those who
questioned the validity of such proofs.

Luitzen Egbertus Jan (how often do you see it written out?!)
Brouwer was the first to propose avoiding PEM in mathematics.

Doing so would immediately throw into question all known theorems
whose proofs required PEM.

A moderate view of the question was to consider nonconstructive
proofs merely uninteresting.  As a pragmatic matter, there is
not much value in a proof which doesn't provide any way to find
an object satisfying the related theorem.

On the far other hand, you might say that nonconstructive proofs
are simply false.  This was Brouwer's view.

The debate became one between Brouwer and Hilbert (who already
had a bad history) and escalated into a personal argument.

Brouwer also got associated with the term "intuitionism", due
to his claim "that our intuition about mathematical objects
matters more than the knowledge we gain about those objects
through proofs."  Brouwer's rather mystical adherence to this
theory repelled some more moderate folks from his constructive
theories.

## Resolving the crisis

Obviously, this crisis has parallels with the crisis around
non-Euclidean geometries.

If the constructivists were using different inference rules from
other mathematicians, it was simply a matter of having different
axioms for the words "and", "or", etc.

In particular, the constructivists had a very different definition
of the proposition "there exists an object that...".  For them,
this meant "we know of an object that...", whereas that "traditional"
mathematicians took it to mean "there must exist an object that...
even if we don't know the object".

As usual, the furious debate about constructivism revealed
hitherto ignored distinctions in the ideas mathematicians had been
using for a long time.

## Constructivity today (good magazine title?)

The two versions of "there exists" are not necessarily mutually
exclusive.

The only thing really necessary to resolve the constructivism
crisis was to define two distinct expressions for these two
versions.

Gödel proposed such a logic in 1933.

In summary, PEM is sort of like the Axiom of Choice.  Some
proofs might be proved first with PEM, then again constructively.
Rather than warring over it, it's proven more useful to see what
can and can't be done without this axiom.


# Chapter 8

So what does constructive proof have to do with computation?

## Cut elimination

Does a constructive existence proof (i.e. one not using PEM) always
provide a witness?  If so, can this be proven?

One of the first proofs that such a proof always provides a witness
uses Gerhard Gentzen's "cut elimination algorithm", which he published
in 1935.

Interestingly, this algorithm is applied to *proofs*.

"Cut elimination" refers to the simplification of arguments in a
proof.  e.g. a general result followed only by its application to
a specific case might be replaced by a direct proof of the specific
case.  When simplified by this algorithm, an existence proof that
does not use PEM always ends with existential quantifier introduction,
and thus explicitly presents a witness.

Here we have an explicit connection between the notion of proof and
that of an algorithm: in particular, proofs are seen to be objects
that we can compute with.

A function like x ↦ x × x can be given a constructive proof, but
other functions (like x ↦ sqrt(x)) cannot be given a (finite) proof
without PEM.

A function with a constructive proof has the following advantage:
the proof of a function f is an algorithm which, when applied to
an element a in the domain of f, computes the object b = f(a).
(This was proved by Kleene.)

This gives us a new way to define an algorithm, rather surprisingly:
An algorithm is a function which can be defined constructively.

## Constructive proofs as algorithms

This is "the tip of a vast iceberg", the "algorithmic interepretation"
of constructive proofs.  Major advances here took place in the 60s,
due to the work of Haskell Curry, Nicolaas Govert de Bruijn, and
William Howard.

This interpretation provides a new answer to the question of the
meaning of "proof".

How are proofs constructed, and how are they used?

A proof of the proposition "if A then B" can be used to construct
a proof of B from a proof of A.  It is *used*, in other words,
like an algorithm for transforming proofs.

It becomes reasonable, then, to simply define proofs as the
algorithms which have the same use-patterns.  (Along with some
other proofs of this correspondance.)

So proofs, the foundation of mathematics since at least Euclid,
turn out to be founded on algorithms, something the Mesopotamians
started out with.

The proof of the proposition "for all numbers x and y, there
exists a number z that is the greatest common divisor of x and y"
is, in the algorithmic interpretation, an algorithm which gives
us, for each pair (a, b), a pair of a number c and a proof that
c is the GCD of a and b.  (NB: read that carefully)

Here we go: "Algorithmic interpretation of proofs thus shows not
only that proofs are algorithms, but also that cut elimination
acts as an *interpreter* for these algorithms."

So both constructivity and computability place great importance
on the notions of algorithm and computation, the former because
it provides it defines constructive proofs as algorithms.
The proofs of interest to constructive mathematics, however, are
proofs from the axiomatic (non-computational) tradition!  And
the suggestion is that this is the important part of mathematics;
once again, *not* computation.

# Part 3

# Chapter 9: Intuitionistic type theory

The axiomatic method was challenged simultaneously in several
disciplines of mathematics and computer science during the 1970s.

In the late 60s there were many advances in constructive mathematics.
The algorithmic interpretation of proofs was developed by Curry,
de Bruijn, and Howard and cut elimination was extended to new
theories (Tait, Martin-Löf, Girard), in particular Church's typed
lambda calculus.

This last development led to Martin-Löf's creation of intuitionistic
type theory, a minimal, constructive basis for mathematics.
It excludes not only PEM, but several axioms from Church's type
theory: extensionality, the axiom of choice, and impredicative
comprehension.

"Vast sections of mathematics" have by now been expressed within
this system and its extensions.

In Martin-Löf's type theory, proofs are again defined as algorithms.
It also introduces a notion not found in Church's types or in set
theory, "equality by definition".

Church's type theory has only one notion of equality, which is the
same notion in all cases--whether equality is definitional or the
result of some reasoning process.  Martin-Löf type theory has both
the "ordinary" notion equality and equality by definition.

Definitions, in this theory, are neither axioms nor inference rules.
They state that proving one proposition is equivalent to proving
another, thus establishing what might better be called "equality by
computation".  This is related to Church's dilemma with beta
reduction, where he at first called (x ↦ (x × x))(4) → 4 × 4 a 
computation step, then later a simple equality.  In intuitionistic
type theory, the definition of ↦ leads to the fact that these two
expressions are equal in the definitional sense: (x ↦ t)(u) is
equal by definition to the expression t[x := u] (t with u
replacing x).

## Equality by definition and analytic judgements

"Equality by definition" is not as strong as Poincaré's idea of
axioms as implicit definitions, in which any two things which can
be proved equal are so by definition.

Also, in intuitionistic type theory, equality by definition is
always decidable.  Church's theorem shows that equality by *implicit*
definition is not.

(I'm not clear on what distinction is being made in the following
paragraphs.)

The notions of analytic and synthetic judgement appear differently
in Martin-Löf type theory; a judgement is analytic when it requires
only computation, and synthetic when "demonstration" is required.
The judgement "2 + 2 equals 4" is analytic, but judging the proposition
"the sum of the angles in a triangle is 180°" is synthetic.

("Demonstration" is a notion that isn't described here.)

## Shorter proofs, longer proof checking

With proofs being algorithms, a distinction arises between proofs
based on the complexity of the proof and the time it takes to
execute the algorithm (i.e. check the proof).

Notions may take a long or short time to write and check depending
on how they are defined.

A proof that a certain number is composite (i.e. not prime) might
be state as "f(91) = 1", where f(x) is an algorithm that returns 1
if x is composite and 0 otherwise.  This proposition is equal by
definition to the proposition "1 = 1", which has a very short proof
using only the axiom "∀x.x = x".  Checking this proof, though, may
involve the evaluation of f(91), which (in the simple implementation
which tests the divisibility of 91 by all smaller natural numbers)
is lengthy.

Another proof of the compositeness of 91 might be to claim
"∃y.g(91, y) = 1", where g(x, y) computes 1 iff y is a divisor of
x, and 0 otherwise.  The proof is longer, since it's necessary to
introduce a y, then to make the claim of it.  But checking it is
easy: we just have to compute g(91, 7).  This is an example of a
proof whose statement and demonstration are both relatively simple.

The proofs presented all make use of some computation rules in
their demonstration (see above).  The "low level" proof, which uses
*no* computation rules, must decompose the multiplication algorithm,
applied to 7 and 13, to its tiniest steps, resulting in a very long
proof *and* check.

The "best" proofs, the one with f(x) and the one with g(x, y), are
only possible because intuitionistic type theory allows proofs to
use axioms, inference rules, and computation rules.

So Martin-Löf's "equality by definition" fully introduced
computation into mathematics in the early 1970s.  Computation rules
were finally available as tools to mathematicians.


# Chapter 10: Automated theorem proving

The idea of constructing proofs with computation rules as well
as inference rules and axioms also developed in computer science
in the 1970s.  The critical research in this development was in
automated theorem proving.

The workers in type theory and in automated theorem proving,
though researching similar ideas at the same time, ignored
each other.

Automated theorem prover: A program which, given a collection of
axioms and a proposition, attempt to prove the proposition from
the axioms.

Church's theorem implies a fundamental limit for the project of
automated theorem proving: no program can determine whether the
given proposition has a proof.

# The fantasy of "intelligent machines"

As with most AI topics in the 1950s, early claims about the future
of automated theorem proving were massively inflated.

One interesting claim concerned the possibility of a theorem-prover
which is as good as a human being at constructing proofs.  This
relates to the Church-Turing thesis, which, if it holds (in the
psychological form), guarantees the possibility of such a system.

If we accept the materialist hypothesis and (per Gandy) that
information has finite density and transmission speed, then it
must be possible, in theory, for the reasoning processes of a
human proof-constructor to be simulated by a program.

In any case, provers are not nearly there yet.  However, theorem
proving has made "steady and significant progress" since the
early days.

What are the ideas that have made this progress possible?

## "Resolution" and paramodulation

The earliest methods of automated proof, including resolution
(A. Robinson 1965) and paramodulation (Wos & G. Robinson 1969),
searched for proofs within predicate logic.

Both of these methods use the unification algorithm, which compares
expressions and constructs substitutions which can be applied to
make the expressions identical.

Paramodulation is similar to resolution using subexpressions of
axioms to transform expressions into identical forms.

The idea of the unification algorithm may actually go back to
Herbrand's research into the Entscheidungsproblem.

## Turning axioms of equality into computation rules

Given an axiom of the form t = u, paramodulation allows us to replace
any instance of t with the corresponding u instance, and vice versa.

This technique often requires a lot of computation, even for simple
problems.  It can also be difficult to choose how to apply
paramodulation at each step in a computation process.  This leads to
widely branching process trees.

By restricting the use of paramodulation to produce a "normal form"
of some sort, rather than testing all possible ways to create identical
expressions (e.g. always shifting left with associativity applications),
shorter computations may be possible.

This might give us an approach in which, with an axiom t = u, we only
replace t with u, say.  This transforms an axiom into a computation
rule.

This assumes a set of *confluent* computation rules, AKA rules which
have the Church-Rosser property, AKA rules which give a computation
process which terminates with the same result regardless of the
order in which the rules are used.

Knuth & Bendix suggested (1970) that axioms could be transformed
into a confluent set of rules.

## From unification to equation solving

Transforming axioms into computation rules means that certain
quantified propositions can no longer be proved.

Plotkin's research in 1972 showed a way to transform axioms without
leaving out any kind of proof.  Plotkin's version of unification has
"associativity built in": in proving the proposition "there exists y
such that a + y = (a + b) + c", this algorithm compares the two
expressions and produces y = b + c.  While more complicated than
Robinson's unification, this allows many more proofs to be constructed.

It was later shown that other axioms could be integrated into the
unification algorithm.

Essentially, automated proving was increasingly starting to include
equation-solving as a tool for constructing proofs.  By integrating
arithmetic axioms, e.g., a system could prove propositions like
"there exists x such that x + 2 = 4" without resorting to computation
rules.

This puts a new focus on equations themselves.

A simple way to describe an equation is as a pair of expressions
containing variables; its solution is a *substitution* which assigns
expressions to the variables such that the two expressions are equal,
along with a proof showing that the two expressnions so produced
compute the same value.

But some equations, e.g. a + 2 = 4, given an expression a, do not
need a proof that a + 2 = 4; it suffices to compute 2 + 2, which is
equal by definition to 4.

So there are two classes of equations: those requiring solutions and
proofs, and those requiring only solutions and computation.

## Church's type theory

In the early 70s, computer scientists adopted a similar approach to
Plotkin and built specialized provers for Church's type theory and
set theory.  Andrews (1971) suggested building in beta reduction;
this was accomplished a year later by Huet.

So a theme of all this work of the 1970s is transforming axioms into
computation rules which can be built into some sort of unification
algorithm.

This approach is one of the "crucial" reasons for the progress made
in the automated theorem proving field since the 50s.  Had the axiomatic
conception of mathematics remained the primary model, propositions like
"2 + 2 = 4" might have required enormous numbers of axioms to prove;
adding computation rules makes it just a matter of "doing the addition".


# Ch. 11: Proof checking

An alternative to theorem provers was proof-checking systems.
Though less ambitious, this area has produced a large number of results
quickly in verifying a very wide range of proofs.

The use of proof checkers has also enabled the writing of much longer
and more detailed proofs than were previously useful.  In many
cases, the length and detail has been unavoidable, making automated

The first proof checker, Automath, was developed by De Bruijn in
1967, and already allowed proofs constructed with axioms, inference
rules, and (limited) computation rules.  Automath did not allow a
proof of 2 + 2 = 4 using simple addition; a reasoning process had
to be constructed.  De Bruijn noted the weirdness of this.

Later proof checkers used Martin-Löf's type theory or other systems
which allowed more scope for computation.


## The correctness of programs

The proof-checking of programs (i.e. not specifically
math-related algorithms) has a longer history.  Here, too, the
length and complexity of programs skyrocketed during the late
twentieth century, thus (presumably) making verification systems
a necessity.

Unlike in mathematics, where proofs of a single proposition often come
in different lengths, the length of a program proof is at least
proportional to the length of the program.  Verifying these proofs by
hand is thus very difficult.

In such proofs, computation rules are a natural way to prove results.

Milner's LCF, and the ACL proof-checker (Boyer & Moore) were two such
systems.  In these, even inference rules are themselves expressed as
computation rules.  Church's theorem thus sets a limit to this sort of
system, but this nonetheless represents a partial realization of
Hilbert's program.

Thus, research in both areas revealed the importance of computation
rules in constructing proofs.


# Chapter 12: News from the field

The move to include computation rules in proofs has had important
results in mathematical practice.

This chapter includes some examples.

## The four-color theorem

The four-color problem began with Francis Guthrie's conjecture
that any map could be colored (i.e. with distinct colors for
neighboring regions) with a maximum of four colors.  He showed
this to be possible with a map of Great Britain, but failed to
prove the general case.

A proof-attempt by Alfred Kempe (1878) was found to be mistaken
by Heawood in 1890.  The problem was solved by Appel and Haken
in 1976.

Kempe's approach was an inductive one.  It rests on showing that,
with a partially colored (with a maximum of four colors) map, it
is possible to select a valid color from the four to color an
uncolored region.

An interesting thought is to assume that Kempe's proof works
for all maps in which > 10 regions have already been colored.  It
then would remain only to show that the (finite) set of all maps
with 10 or fewer regions satisfied the theorem.

Although much more complex, the approach that Appel and Haken
took used a similar method: reducing the problem to a finite set
of about 1500 maps, then showing exhaustively that each of these
maps could be four-colored.

Using a computer, they were able to check the entire set in 1200
hours of computation.  No "hand" solution to the problem is
currently known.

Prior to 1976, computers had been used to test the primality
of large numbers, to calculate mathematical constants, to
construct approximate solutions to equations, etc.  These tasks
can be regarded as the construction of proofs of (usually very
specific) theorems.

The statements of these proofs are generally very long: "n is
prime", where n is represented by thousands of digits, or "the
highest temperature in an object of shape R is 80° C", where R
describes a very complex shape, e.g.

The four-color theorem differs: It is a proof with a short
statement, but a very long proof.

## Symbolic computation and computer algebra systems

Computer algebra systems were used early on to detect errors in
physical calculations.

A proof of Morley's Theorem using linear equations is workable
only with a CAS.  Unlike the four-color theorem, though, shorter
proofs are known.

Within the category of proofs which are too long to be constructed
by hand, there are those whose statements are already long and
those with short statements.  The latter category can be divided
into those proofs with proofs of different lengths, and those with
only long proofs known.

Along with the four-color theorem, several more big theorems with
proofs too long to write by hand were proved in the 70s and 80s.
These included Hales's theorem, AKA Kepler's conjecture, which had
been an unsolved problem since 1610.

## Understanding why

The appearance of computer-aided proof in 1976 caused a massive
controversy and raised questions about what constituted an
acceptable mathematical proof.

Rather than the turn-of-the-20th-century debates about which
axioms or inference rules could be used, this was a debate about
the validity of proofs too long to for humans to practically
read or write.

This debate was small until the 90s and 2000s.  At this time,
the number of computer-assisted proofs began to increase
dramatically.

Two attacks on these proofs: (1) they were not "explicative",
and (2) they were hard to prove correct.

The essence of the first criticism is this: A proof like that
of the four-color theorem does not provide a unique *reason* for
the fact that it proves.  It provides about 1500 reasons, but
"the very principle of the scientific method is the necessity
to find a unique reason accounting for the regularity of
phenomena".  So Appel & Haken's proof establishes the theorem
without given a clear explanation (whatever that is) of the
result.

However, proving a theorem by case analysis is an old approach,
and proofs by this method might also be said to fail to give
their result a unified reason.

No-one seriously suggests getting rid of case-by-case proofs.
And what really differentiates these from a proof by
computer-assisted case analysis?

The distinction seems to be merely a question of the number
of cases.  Or is it?

If that criticism can be disposed of, what about the correctness
of these proofs?

Appel & Haken's proof was verified, though not until 1995 by
Robertson, et al, who also used a computer to complete their
verification.  Until then, there was a lingering question of
whether Appel & Haken's software contained any errors.

Suddenly, we have a notion of "reproducibility" in mathematics.

Presumably Appel & Haken proved the program they used, right?
No, as it turns out.

In 2005, Gonthier & Werner rewrote Robertson, et al's proof of
Appel & Haken's proof using Coq.

More recently, the proof of Hales's theorem has be checkid using
HOL (completed in 2014).

## The size of proofs and Church's Theorem

With all of the debate about long proofs, can we say anything
about any connection between the size of a proposition and that
of its proof?

A consequence of Church's theorem is that there is not, in general.

## Can it be proved that a theorem only has long proofs?

We don't know, at the moment.

## New spaces
Church's theorem gives us three categories of provable propositions:

* Propositionss which have short axiomatic proofs.

* Propositions which have no short axiomatic proofs, but which
  have short proofs provided one resorts to computation.

* Propositions which have only long proofs.

Before the 1970s, all theorems were in the first category.  The
second is under investigation now; the third is still currently
out of reach.


# Chapter 13: Instruments

Until the 1970s, mathematics was the only science which had not
undergone an "instrumental revolution".  Other sciences had been
irrevocably changed by the introduction of instruments (microscopes,
telescopes, etc.), but mathematicians were still working with pencil
and paper.

"In 1976, mathematics entered the instrumented part of its history."
The instruments, i.e. computers, are unlike the characteristic
instruments of astronomy or biology, say, in that they don't extend
our senses, but rather "the faculties of our understanding".

This doesn't seem very accurate: "When an instrument is introduced
in a science, a change occurs which is more quantitative than
qualitative."  I think this is completely false when applied to
cases where a totally new instrument appeared--e.g. microscopes
fundamentally changed our understanding of the microscopic world,
which, as its name suggests, wasn't even an idea before this tool
was introduced.

It's better at describing less dramatic technological developements,
e.g. more powerful microscopes.

(OK, Dowek adds very shortly after that "the use of instruments
can sometimes achieve a qualitative change, as well".  So why
the earlier statement?  Maybe he forgot an 'often'.)

## Experimental results in mathematics

Once again, we come back to the apparent distinction between
analytic and a posteriori judgements.  In using an instrument in
mathematical work, one is *observing* the behavior of the tool
and using these observations to establish an analytic result.

But isn't observation supposed to be useless for proving analytic
results?

The proposition 2 + 2 = 4 can, it seems, be established as an
analytic judgement through synthetic means; if 2 + 2 is proven to
be a natural number, a simple counting experiment with objects of
any kind "seems sufficient to refute the hypothesis that 2 + 2 is
any other integer than 4".  (This has a Karl Popper sound to it.)

Similar examples of "analytic a posteriori judgements" are the
four-color theorem and Hales's theorem.

## Wind tunnels as analogical computers

Aeronautics experiments with wind tunnels have some relevance here.
Unlike most scientific experiments in which the result predicted by
theory is known in advance (otherwise, the theory is just speculation),
wind tunnel tests are carried out to determine results and to provide
data for later predictions.

These tests are thus not "experiments" in the natural science sense
of the word.

One view is that such tests are purely data-gathering on the wing
(etc.) under "real conditions".  But the conditions are simulated,
and the wing is often a scale model.  Similar experiments in geology
make extensive use of scale and other adjustments in their tests.
Here, theory must be used to predict the factors by which various
elements are scaled.

Another, "more satisfactory explanation": The test is an attempt to
solve a problem, e.g. the speed of airflow around a wing.  A theory
allows this problem to be translated to a mathematical problem,
which is too difficult to solve.  So theory again allows this problem
to be translated to a scaled-down physical test system, which allows
us to *compute* the solution to the mathematical problem, and thus
to the original problem.

So the scale model is here an analog computer designed to solve a
mathematical problem.

A critical difference between such "analog computations" and
experiments in the natural sciences is that the former can and often
have been replaced by simulations, whereas the latter absolutely
cannot.

## Building instruments

Unlike theories in the natural sciences, mathematical results are
considered valid for all time once proved.

But what about mathematical results proved with instrumental aid,
i.e. which rely on some interaction with nature?

Proofs acquired through instruments appear to be analytic results
resting on synthetic knowledge (i.e. about how to build machines).

Calculations on a hand-calculator require building a calculator,
the components of which are known to function correctly by virtue
of our current theories of physics.

But no one assumes our understanding of physics is wrong when they
get a different result than the calculator.

In fact, tools seem to have made our reasoning more reliable.
CASs and proof-checkers have been used to catch errors in the work
of Newton, Delaunay, etc..

But isn't this introducing (hypothetical), science-based knowledge
into analytic results, and thus introducing a source of error?

The fundamental mistake here is to assume that mental calculations
are themselves completely reliable, or even analytic.

(The latter point is very interesting.)  In any case, the
"instrumented age" of mathematics has revealed the error-prone-ness
of our understanding.


# Chapter 14: The end of axioms?

The idea of using computation in proofs arose, as we saw, in
different areas at around the same time.  Each of the streams of
type theory, automated theorem proving, proof checking, and practical
mathematical work (the four-color theorem proof, etc.) added something.

In the 90s, Dowek, Hardin, & Kirchner reformulated the notion of
mathematical proof within predicate logic.  In this extension,
"deduction modulo", proofs are constructed with axioms, inference
rules, and computation rules.  The goal was to express the new
view of proof "in the most general framework possible".

A surprising result of this and related projects was the discovery
that many classical axioms could be replaced with computation rules.

So why not get rid of axioms altogether?

"Axioms have been marring mathematics ever since Hilbert's days--if
not since Euclid's!"

We don't know if this is possible, as yet.


# Conclusion

Unresolved problems encountered in this book:

We know that their are provable, short propositions which have only
long proofs, but we have no techniques for showing that a proposition
is such.

Is a mathematics without axioms possible?  Computation rules have
nicer properties overall, and "every time one successfully replaces
an axiom with a computation rule, there is cause to rejoice".
If axioms *are* required, in which cases?

An algorithmic formulation of natural laws is possible, as shown
by the Church-Turing thesis.

How far can we go with the Church-Turing thesis's ability to explain
the mathematizability of natural phenomena?

Can the varying utility of computational tools to the branches of
mathematics be determined?

Will the ways in which mathematics is expressed change as a result of
the use of computation?  Will more algorithms and fewer axiom/inference
examples appear in mathematical writing?

How far will the "instrumented age" of mathematics take us from the
chalk-and-blackboard days?

--------------

[CC0](https://creativecommons.org/publicdomain/zero/1.0/) (public domain)