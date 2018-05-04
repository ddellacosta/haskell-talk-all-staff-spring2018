
#### Why should we care?

(Everything in this section probably applies to any language in the ML family, I'll note otherwise) <!-- .element: class="fragment haskell-talk-small" data-fragment-index="1" -->

(Definitely not Java though) <!-- .element: class="fragment haskell-talk-super-small" data-fragment-index="2" -->


### Clarity and Insight Into Our Programs

Types can tell us things about our programs that are otherwise harder to understand in a system without a (good) static type system.


### Safety

With a static type system you can catch many errors at compile time. 


### Programmer Efficiency and Automation

With type inference, you get a lot of this "for free"--you don't have to annotate functions (although we probably will because it is so useful), refactoring with safety is a breeze, and the type checker can even help you understand your values before you've figured them out for yourself.


### Encode Behavior in Types!

Taking all of the advantages listed above, you can put all of that together and get something amazing: automated validation of behavior at compile-time. (http://dev.stephendiehl.com/types_behavior.pdf)

(This is probably the most Haskell-specific point, because of how insanely deep the type system is.) <!-- .element: class="fragment haskell-talk-small" data-fragment-index="1" -->




