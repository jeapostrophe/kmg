kmg - Jay and Kimball make a language

This is an experiment in either:

A. making an implementation of a fully "dualized" language where all features
appear as production and consumption forms.

B. making a compiler that is "least effort" rather than "best effort", where
performance is guaranteed, but you may "opt-in" to high-level features and must
annotate whether the particular instance /must/ be optimized away or /may/ be
slow.

We're not sure exactly how to do both of these at the same time, so B might be a
sub-language of A.

For A, we're influenced by Paul Downen's work:
- https://ix.cs.uoregon.edu/~pdownen/publications/sequent-intro.pdf
- https://arxiv.org/pdf/1907.13227.pdf

For B, we're mostly on our own, but maybe influence by Jay's prior work, adqc.
