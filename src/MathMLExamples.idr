module MathMLExamples

import MathML

%default total


-- Function returning a fraction: 1/2
public export
fracExample : MathML
fracExample = Mfrac (Mn "1") (Mn "2")

{- Corresponding MathML representation:
<mfrac>
  <mn>1</mn>
  <mn>2</mn>
</mfrac>
-}

-- Function returning a square root: √(x + 1)
public export
sqrtExample : MathML
sqrtExample = Msqrt (Mrow [Mo "√", Mrow [Mo "(", Mrow [Mi "x", Mo "+", Mn "1"], Mo ")"]])

{- Corresponding MathML representation:
<msqrt>
  <mrow>
    <mo>√</mo>
    <mrow>
      <mo>(</mo>
      <mrow>
        <mi>x</mi>
        <mo>+</mo>
        <mn>1</mn>
      </mrow>
      <mo>)</mo>
    </mrow>
  </mrow>
</msqrt>
-}

-- Function returning a subscript: a₁
public export
subExample : MathML
subExample = Msub (Mi "a") (Mn "1")

{- Corresponding MathML representation:
<msub>
  <mi>a</mi>
  <mn>1</mn>
</msub>
-}

-- Function returning a superscript: x²
public export
supExample : MathML
supExample = Msup (Mi "x") (Mn "2")

{- Corresponding MathML representation:
<msup>
  <mi>x</mi>
  <mn>2</mn>
</msup>
-}

-- Function returning a fraction with a superscript: a²/b³
public export
fracSupExample : MathML
fracSupExample = Mfrac (Msup (Mi "a") (Mn "2")) (Msup (Mi "b") (Mn "3"))

{- Corresponding MathML representation:
<mfrac>
  <msup>
    <mi>a</mi>
    <mn>2</mn>
  </msup>
  <msup>
    <mi>b</mi>
    <mn>3</mn>
  </msup>
</mfrac>
-}
