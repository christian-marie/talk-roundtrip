GODO: 
2. Clear section transitions.
8. Emphasise that th generated things are isomorphisms between the branches of
   the product type.
9. Emphasise that syntax is now defined.
10. Explain mplus ( or try <|>)
11. Aeson value (what it is, when it is mentioned)
12. explain that V is vector
13. Explain why we give up merging
15. Explain jsonField operators such as ^?
15. is combinator

16. Emphasise duality

17. Reverse JSON balls, present JSON first
18. Remove ballpit

19. Conclusion


emphasise single character things

% Partial isomorphisms for error-free JSON round-trips

# Intro

1. Almost every non-trivial software project has to, at some point: shuffle data
about from native values to a foreign format and back again. 

The way that we generally go about this is with an idiom I will refer to as
"get/put", where you write one function to get the data from a foreign format,
then write another function to put it back.

I think that this idiom can be improved upon. I think that we can take a
slightly more principled approach that leaves us feeling a little less dirty
every time we want to do a round trip to some foreign data format (such as
JSON, XML or binary wire formats).

The plan for tonight tonight is:

1. Firstly, define our problem a little better. We're going to explore what's
   wrong with the current approach and a few proposed solutions.
2. Summarise an interesting paper for you invertible syntax descriptions, so
   that I have someone to blame for the wacky syntax. Which we did not invent.
3. Build your own invertible syntax descriptions, so that you can do this with
   whatever data format you like.

# What are we fixing?

I want to show you an example of some redundant and error prone code to get us
started.

Most people seem to go about this the same way, write some function to convert
it one way, then some other function to go back. For example: binary get/put,
aeson fromJson toJSON, etc. There's also usually some degree of partiality in
either direction.

Are these functions related?  Well, we certainly treat them like they are, but
there isn't *really* a link beyond you testing that this is a valid isomorphism
on a certain subset of valid input. An isomorphism is defined in the handout.

I'm going to show you a way of connecting these functions together at a
fundamental level so that you can get both more safety and type less.

Now, you might think to yourself: why bother with this? The binary, serial, or
whatever library already provides a whole host of isomorphisms that I can trust
because they've been tested. I'm not going to argue that the binary get/put
instances are untrustworthy, I think they're fine. I'm going to argue that you
are untrustworthy, and are likely to mis-type something or change one side and
not the other.

Libraries don't make bugs, users of libraries make bugs. It always amuses me
when a junior developer is debugging a complex issue and jumps straight to:
"the library is obviously buggy". They're almost always calling it wrong.

Next time someone goes to blame the library, try this: politely ask them them
to print out the data on ingress to and egress from the library for the bug
report. Conveniently, you'll almost never get the bug report.

So that's why programming with get/put isn't ideal for specifying isomorphisms.
Too much flexibility, flexibility you don't need such as the ability to put a
little endian integer one way, and read a big endian the other.

Finally, before we start, what's wrong with Generics? I think they're great, if
you're 100% happy with the generic representation and never want to change it.
That's not the case for protocols or things like JSON APIs.

So:
* get/put is evil, because you are a terrible person and can't type very well
* Generics and TH are inflexible

What do I propose? Well, it may seem like magic but we can quite easily write a
printer and a parser in one step. That's what this paper is about.

# A wild paper appears!

2010 - Invertible Syntax Descriptions: Unifying Parsing and Pretty Printing
Tillmann Rendel
Klaus Ostermann
University of Marburg, Germany

We're going to go over the important parts over the next 10 minutes. Some bits
have been simplified and/or omitted due to being superflous, or nearly
superflous.

# Invertible Syntax Descriptions: way of the get/put

So the paper starts off with the familiar get/put idiom, given a data type:

We parse it with one function

And print it with another. There's no guarantee that these produce the same
thing, although this implementation is correct.

Now, the goal here is to define this just *once*. The obvious way is to
overload the applicative syntax, and this is the way we'll go. Now, in my
defence, I did not chose the syntax and I certainly wouldn't have suggested
that overloading with the *same* syntax is a good idea.


# Invertible Syntax Descriptions: co/contravariance

Let's start off with fmap and a simple parser that takes a String to a
"non-deterministic" result. This definition should be pretty natural for most
of you.

So, given a function from a to b, we can apply it to the result of the parser
of a's on the way "out" and get a parser of bs. I'll leave the implementation
for the imagination, it's trivial.

Now's when it gets weird: That was called a covariant functor, can anyone tell
what this is? A contravariant functor.

Given a function from a to the pretty printed document, it doesn't make sense
to apply a function from a to b because we then can't do anything with it. So,
we take a function from b to a, and apply it *before* it gets printed, so that
given one b, we can print it after converting it to an a.

These types obviously do not unify.

We want to unify these under a united fmap. For this to work we'll need to
"lift" any partiality that could occur into the encoding also, so that the
partiality may be encoded. We're just going to stick a maybe in for that.
i
What we end up with is 
