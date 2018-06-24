-- | SVP and CVP in lattices.

module Module7v5 () where

----------------------------------------------------------------------------
-- 7.15
----------------------------------------------------------------------------

{-

(a) First inclusion is obvious -- every element by constraint lies in the
ball, so the union of these sets is a subset of the ball.
Second inclusion is correct, because:
* Union of all (F+v) for all v is exactly L (it covers all the lattice)
* Thus, if B is smaller than the area covered by the lattice (which is
  the adequate intuitive premise), it's covered by some union of (F+v).
* All (F+v) that cover B have non-zero intersection with it, so union
  of all (F+v) such that intersection is non-empty covers B

(b) Obvious, though I'll notice some important (also obious) points:
* All (F+v) are disjoint, thus the union of them is N * vol(F).
  (ok, now I've noticed the exactly this statement in the hint, thanks)
* The definitions of measure -- subsets have smaller measure, measure
  of the union of disjoint sets' is a sum, etc.

(c) I'm not sure a rigorous proof here is expected and even possible, but...

(I've spent an hour trying to prove it, but in the end here's hand-waving):

First of all, F is a parallelotrope, and as a convex set it has diameter
(maximum distance between two points). Since it's a parallelotrope,
the maximum diameter is one of its diagonals (and it even may be possible
to find it). Let d be this diagonal.

Then, assume that current radius of the ball is R. S1 is Fv that are
completely inside, S2 is Fv that have nonzero intersection. Let's show
that when R → ∞, |S2 \ S1| → 0; Let S3 = S2 \ S1.

In the worst case, B contains ε of some Fv and most of it is outside of
the ball. Let's increase R to R' so that S3 ⊂ B_(R'). It's clear that
ΔR = R' - R < d, so every time we perform this operation, we inrease
the radius up to a constant factor.

Ball's volume difference is ΔV = sqrt(2πe/n)^n * ((R+d)^n - R^n) according
to the ball volume approximation formula. Since for the fixed n first
element of multiplication, it's easy to see that ΔV → 0 when R → ∞.

Another idea of the proof is to put F into the ball of radius d and show
that B_R has approximately Vol(B_R) / Vol(B_d) elements already, and also
show how many are added by increasing R, which is small.

(d) In (c) we showed that #S2 = #S1 + ε, so since Vol(B_R) ∈ [#S1 * Vol(F), #S2 * Vol(F)],
the rest is straihtforward.
-}

----------------------------------------------------------------------------
-- 7.16
----------------------------------------------------------------------------

{-
Sadly, we can't compute the Hadamard ratio since the basis is not given.

Minkowski theorem (with hypercube model) gives us the upper estimate:
√n * det(L)^{1/n} = sqrt(251) * 2^{2251.58 / 251} = 7947.084

The Gaussian Heuristic:

Approximate: 3845.925
Exact (with Γ): 3897.37

-}
