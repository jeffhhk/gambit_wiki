\_\_NOTOC\_\_ \_\_NOEDITSECTION\_\_

The following tables contain the execution time of the Gambit benchmarks
on various implementations of Scheme in different situations. For a
given benchmark and situation, the entry in green indicates which Scheme
system has the fastest execution and the number given is the CPU time in
milliseconds. Other entries give the execution time relative to the
green entry. Blank entries indicate that this benchmark was not executed
(possibly because the system did not accept to compile the program, or
because a given situation could not be simulated with that
implementation of Scheme).

The benchmarks were run in four different situations. This was done by
using compiler options and/or declarations.

1.  <b>Safe with generic arithmetic and mutable bindings.</b>
    This situation approximates the R5RS semantics:
      - generic arithmetic operations
      - mutable bindings for the definitions in the benchmark
      - mutable predefined bindings (for +, car, ...)
      - safe execution (i.e. an exception must be signalled on errors)
2.  <b>Safe with generic arithmetic.</b>
    This situation corresponds to the proposed R6RS semantics:
      - generic arithmetic operations
      - overflow detection on fixnum arithmetic (either produce a
        `    bignum or signal an exception)`
      - immutable bindings for the definitions in the benchmark
      - immutable predefined bindings (for +, car, ...)
      - safe execution (i.e. an exception must be signalled on errors)
3.  <b>Safe with fixnum/flonum specializations.</b>
    This situation corresponds to the proposed R6RS semantics, with the
    use of fixnum and flonum specific functions for arithmetic:
      - arithmetic operations are specialized to fixnum or flonum
        `    arguments as appropriate (for example turning + into`  
        `    fl+) and the fixnum operations may wrap on overflow`
      - immutable bindings for the definitions in the benchmark
      - immutable predefined bindings (for +, car, ...)
      - safe execution (i.e. an exception must be signalled on errors)
4.  <b>Unsafe with fixnum/flonum specializations.</b>
    This situation corresponds to the proposed R6RS semantics, with the
    use of fixnum and flonum specific functions for arithmetic and the
    use of the declaration `(declare unsafe)`:
      - like "Safe with fixnum/flonum specializations" but
        `    errors are not checked`

Note: the tables are best viewed with a wide window.

Scheme systems used: Bigloo 2.8c, Chicken 2 Build 41, Gambit 4.0 beta
18, MzScheme 352, Scheme48 1.3

  

<table>

<tr>

<td colspan="1" align="center">

<h3>

Safe with generic arithmetic and mutable bindings

</h3>

</td>

<td>

 

</td>

<td colspan="1" align="center">

<h3>

Safe with generic arithmetic

</h3>

</td>

<td>

 

</td>

<td colspan="1" align="center">

<h3>

Safe with fixnum/flonum specializations

</h3>

</td>

<td>

 

</td>

<td colspan="1" align="center">

<h3>

Unsafe with fixnum/flonum specializations

</h3>

</td>

</tr>

<tr>

<td colspan="1" align="center">

<h3>

(R5RS)

</h3>

</td>

<td>

 

</td>

<td colspan="1" align="center">

<h3>

(R6RS)

</h3>

</td>

<td>

 

</td>

<td colspan="1" align="center">

<h3>

(R6RS fixflo)

</h3>

</td>

<td>

 

</td>

<td colspan="1" align="center">

<h3>

(R6RS fixflo unsafe)

</h3>

</td>

</tr>

<tr>

<td>

<table>

<tr>

<td colspan="1" align="center">

Program

</td>

<td colspan="1" align="center">

Bigloo

</td>

<td colspan="1" align="center">

Chicken

</td>

<td colspan="1" align="center">

Gambit

</td>

<td colspan="1" align="center">

MzScheme

</td>

<td colspan="1" align="center">

Scheme48

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`boyer`

</td>

<td align="center">

</td>

<td align="right">

14.11

</td>

<td align="center" bgcolor="#80f080">

<i> 2050</i>

</td>

<td align="right">

5.48

</td>

<td align="right">

28.08

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`browse`

</td>

<td align="center">

</td>

<td align="right">

8.78

</td>

<td align="center" bgcolor="#80f080">

<i> 5445</i>

</td>

<td align="right">

5.93

</td>

<td align="right">

19.24

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`cpstak`

</td>

<td align="center">

</td>

<td align="right">

6.14

</td>

<td align="center" bgcolor="#80f080">

<i> 2267</i>

</td>

<td align="right">

7.83

</td>

<td align="right">

17.08

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`ctak`

</td>

<td align="center">

</td>

<td align="right">

2.54

</td>

<td align="center" bgcolor="#80f080">

<i> 1760</i>

</td>

<td align="right">

87.60

</td>

<td align="right">

9.46

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`dderiv`

</td>

<td align="center">

</td>

<td align="right">

6.51

</td>

<td align="center" bgcolor="#80f080">

<i> 3306</i>

</td>

<td align="right">

7.92

</td>

<td align="right">

18.22

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`deriv`

</td>

<td align="center">

</td>

<td align="right">

6.59

</td>

<td align="center" bgcolor="#80f080">

<i> 1934</i>

</td>

<td align="right">

10.64

</td>

<td align="right">

19.55

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`destruc`

</td>

<td align="center">

</td>

<td align="right">

10.41

</td>

<td align="center" bgcolor="#80f080">

<i> 1957</i>

</td>

<td align="right">

6.41

</td>

<td align="right">

32.17

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`diviter`

</td>

<td align="center">

</td>

<td align="right">

13.11

</td>

<td align="center" bgcolor="#80f080">

<i> 1911</i>

</td>

<td align="right">

10.68

</td>

<td align="right">

39.62

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`divrec`

</td>

<td align="center">

</td>

<td align="right">

7.94

</td>

<td align="center" bgcolor="#80f080">

<i> 3096</i>

</td>

<td align="right">

7.87

</td>

<td align="right">

17.94

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`puzzle`

</td>

<td align="center">

</td>

<td align="right">

9.85

</td>

<td align="center" bgcolor="#80f080">

<i> 2537</i>

</td>

<td align="right">

7.50

</td>

<td align="right">

24.90

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`takl`

</td>

<td align="center">

</td>

<td align="right">

10.36

</td>

<td align="center" bgcolor="#80f080">

<i> 3936</i>

</td>

<td align="right">

4.38

</td>

<td align="right">

27.04

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`triangl`

</td>

<td align="center">

</td>

<td align="right">

9.53

</td>

<td align="center" bgcolor="#80f080">

<i> 3491</i>

</td>

<td align="right">

5.48

</td>

<td align="right">

24.29

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`fft`

</td>

<td align="center">

</td>

<td align="right">

10.21

</td>

<td align="center" bgcolor="#80f080">

<i> 1446</i>

</td>

<td align="right">

7.06

</td>

<td align="right">

20.95

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`fib`

</td>

<td align="center">

</td>

<td align="right">

5.24

</td>

<td align="center" bgcolor="#80f080">

<i> 4917</i>

</td>

<td align="right">

4.41

</td>

<td align="right">

12.27

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`fibfp`

</td>

<td align="center">

</td>

<td align="right">

4.56

</td>

<td align="center" bgcolor="#80f080">

<i> 2499</i>

</td>

<td align="right">

8.28

</td>

<td align="right">

11.31

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`mbrot`

</td>

<td align="center">

</td>

<td align="right">

6.74

</td>

<td align="center" bgcolor="#80f080">

<i> 2123</i>

</td>

<td align="right">

10.79

</td>

<td align="right">

18.45

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`pnpoly`

</td>

<td align="center">

</td>

<td align="right">

10.06

</td>

<td align="center" bgcolor="#80f080">

<i> 1903</i>

</td>

<td align="right">

6.52

</td>

<td align="center">

crash

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`sum`

</td>

<td align="center">

</td>

<td align="right">

12.30

</td>

<td align="center" bgcolor="#80f080">

<i> 1453</i>

</td>

<td align="right">

8.88

</td>

<td align="right">

35.51

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`sumfp`

</td>

<td align="center">

</td>

<td align="right">

6.76

</td>

<td align="center" bgcolor="#80f080">

<i> 1518</i>

</td>

<td align="right">

14.00

</td>

<td align="right">

19.30

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`tak`

</td>

<td align="center">

</td>

<td align="right">

8.02

</td>

<td align="center" bgcolor="#80f080">

<i> 3207</i>

</td>

<td align="right">

5.16

</td>

<td align="right">

19.81

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`ack`

</td>

<td align="center">

</td>

<td align="right">

10.86

</td>

<td align="center" bgcolor="#80f080">

<i> 246</i>

</td>

<td align="right">

6.54

</td>

<td align="right">

22.80

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`array1`

</td>

<td align="center">

</td>

<td align="right">

9.31

</td>

<td align="center" bgcolor="#80f080">

<i> 1029</i>

</td>

<td align="right">

5.83

</td>

<td align="right">

22.76

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`cat`

</td>

<td align="center">

</td>

<td align="right">

2.83

</td>

<td align="right">

1.55

</td>

<td align="center" bgcolor="#80f080">

<i> 1122</i>

</td>

<td align="right">

1.83

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`string`

</td>

<td align="center">

</td>

<td align="center" bgcolor="#80f080">

<i> 135</i>

</td>

<td align="right">

2.10

</td>

<td align="right">

1.73

</td>

<td align="right">

61.26

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`sum1`

</td>

<td align="center">

</td>

<td align="right">

2.75

</td>

<td align="center" bgcolor="#80f080">

<i> 217</i>

</td>

<td align="right">

1.16

</td>

<td align="right">

11.47

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`sumloop`

</td>

<td align="center">

</td>

<td align="right">

13.01

</td>

<td align="center" bgcolor="#80f080">

<i> 1437</i>

</td>

<td align="right">

10.28

</td>

<td align="right">

36.00

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`tail`

</td>

<td align="center">

</td>

<td align="right">

2.21

</td>

<td align="center" bgcolor="#80f080">

<i> 1277</i>

</td>

<td align="right">

1.24

</td>

<td align="right">

4.94

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`wc`

</td>

<td align="center">

</td>

<td align="right">

3.97

</td>

<td align="center" bgcolor="#80f080">

<i> 863</i>

</td>

<td align="right">

1.71

</td>

<td align="right">

6.14

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`conform`

</td>

<td align="center">

</td>

<td align="right">

4.24

</td>

<td align="center" bgcolor="#80f080">

<i> 2628</i>

</td>

<td align="right">

3.29

</td>

<td align="right">

13.44

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`dynamic`

</td>

<td align="center">

</td>

<td align="right">

3.18

</td>

<td align="center" bgcolor="#80f080">

<i> 1282</i>

</td>

<td align="right">

1.24

</td>

<td align="right">

7.68

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`earley`

</td>

<td align="center">

</td>

<td align="right">

11.00

</td>

<td align="center" bgcolor="#80f080">

<i> 1337</i>

</td>

<td align="right">

6.79

</td>

<td align="right">

25.20

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`fibc`

</td>

<td align="center">

</td>

<td align="right">

3.27

</td>

<td align="center" bgcolor="#80f080">

<i> 1744</i>

</td>

<td align="right">

64.70

</td>

<td align="right">

10.15

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`graphs`

</td>

<td align="center">

</td>

<td align="right">

7.83

</td>

<td align="center" bgcolor="#80f080">

<i> 2170</i>

</td>

<td align="right">

8.45

</td>

<td align="right">

20.73

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`lattice`

</td>

<td align="center">

</td>

<td align="right">

13.47

</td>

<td align="center" bgcolor="#80f080">

<i> 3295</i>

</td>

<td align="right">

5.08

</td>

<td align="right">

38.31

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`matrix`

</td>

<td align="center">

</td>

<td align="right">

8.84

</td>

<td align="center" bgcolor="#80f080">

<i> 2162</i>

</td>

<td align="right">

5.36

</td>

<td align="right">

21.27

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`maze`

</td>

<td align="center">

</td>

<td align="right">

5.87

</td>

<td align="center" bgcolor="#80f080">

<i> 3138</i>

</td>

<td align="right">

4.12

</td>

<td align="right">

12.96

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`mazefun`

</td>

<td align="center">

</td>

<td align="right">

6.61

</td>

<td align="center" bgcolor="#80f080">

<i> 2475</i>

</td>

<td align="right">

4.65

</td>

<td align="right">

18.21

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`nqueens`

</td>

<td align="center">

</td>

<td align="right">

11.33

</td>

<td align="center" bgcolor="#80f080">

<i> 2482</i>

</td>

<td align="right">

6.85

</td>

<td align="right">

29.58

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`paraffins`

</td>

<td align="center">

</td>

<td align="right">

17.38

</td>

<td align="center" bgcolor="#80f080">

<i> 2164</i>

</td>

<td align="right">

4.77

</td>

<td align="right">

14.91

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`peval`

</td>

<td align="center">

</td>

<td align="right">

9.09

</td>

<td align="center" bgcolor="#80f080">

<i> 1895</i>

</td>

<td align="right">

5.10

</td>

<td align="right">

21.86

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`primes`

</td>

<td align="center">

</td>

<td align="right">

12.83

</td>

<td align="center" bgcolor="#80f080">

<i> 2006</i>

</td>

<td align="right">

8.04

</td>

<td align="right">

25.70

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`ray`

</td>

<td align="center">

</td>

<td align="right">

4.00

</td>

<td align="center" bgcolor="#80f080">

<i> 1980</i>

</td>

<td align="right">

5.68

</td>

<td align="right">

104.72

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`scheme`

</td>

<td align="center">

</td>

<td align="right">

5.54

</td>

<td align="center" bgcolor="#80f080">

<i> 2282</i>

</td>

<td align="right">

3.51

</td>

<td align="right">

13.76

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`simplex`

</td>

<td align="center">

</td>

<td align="right">

7.51

</td>

<td align="center" bgcolor="#80f080">

<i> 2279</i>

</td>

<td align="right">

5.82

</td>

<td align="right">

55.46

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`slatex`

</td>

<td align="center">

</td>

<td align="right">

1.65

</td>

<td align="center" bgcolor="#80f080">

<i> 1528</i>

</td>

<td align="right">

1.11

</td>

<td align="right">

2.79

</td>

</tr>

</table>

</td>

<td>

</td>

<td>

<table>

<tr>

<td colspan="1" align="center">

Program

</td>

<td colspan="1" align="center">

Bigloo

</td>

<td colspan="1" align="center">

Chicken

</td>

<td colspan="1" align="center">

Gambit

</td>

<td colspan="1" align="center">

MzScheme

</td>

<td colspan="1" align="center">

Scheme48

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`boyer`

</td>

<td align="center" bgcolor="#80f080">

<i> 650</i>

</td>

<td align="right">

2.48

</td>

<td align="right">

1.56

</td>

<td align="right">

3.81

</td>

<td align="right">

28.77

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`browse`

</td>

<td align="center" bgcolor="#80f080">

<i> 2940</i>

</td>

<td align="right">

2.64

</td>

<td align="right">

1.27

</td>

<td align="right">

5.44

</td>

<td align="right">

14.53

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`cpstak`

</td>

<td align="right">

5.72

</td>

<td align="right">

1.95

</td>

<td align="center" bgcolor="#80f080">

<i> 1775</i>

</td>

<td align="right">

8.35

</td>

<td align="right">

12.03

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`ctak`

</td>

<td align="right">

60.67

</td>

<td align="right">

1.85

</td>

<td align="center" bgcolor="#80f080">

<i> 1729</i>

</td>

<td align="right">

70.26

</td>

<td align="right">

8.34

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`dderiv`

</td>

<td align="right">

3.11

</td>

<td align="right">

3.27

</td>

<td align="center" bgcolor="#80f080">

<i> 1579</i>

</td>

<td align="right">

15.54

</td>

<td align="right">

23.04

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`deriv`

</td>

<td align="right">

4.21

</td>

<td align="right">

4.03

</td>

<td align="center" bgcolor="#80f080">

<i> 1111</i>

</td>

<td align="right">

19.00

</td>

<td align="right">

21.70

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`destruc`

</td>

<td align="right">

1.20

</td>

<td align="right">

2.35

</td>

<td align="center" bgcolor="#80f080">

<i> 1482</i>

</td>

<td align="right">

3.32

</td>

<td align="right">

20.72

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`diviter`

</td>

<td align="right">

5.41

</td>

<td align="center" bgcolor="#80f080">

<i> 814</i>

</td>

<td align="right">

1.43

</td>

<td align="right">

22.89

</td>

<td align="right">

23.87

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`divrec`

</td>

<td align="right">

3.15

</td>

<td align="right">

1.62

</td>

<td align="center" bgcolor="#80f080">

<i> 1599</i>

</td>

<td align="right">

14.08

</td>

<td align="right">

13.63

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`puzzle`

</td>

<td align="right">

2.32

</td>

<td align="right">

2.57

</td>

<td align="center" bgcolor="#80f080">

<i> 1909</i>

</td>

<td align="right">

3.32

</td>

<td align="right">

10.75

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`takl`

</td>

<td align="right">

1.63

</td>

<td align="center" bgcolor="#80f080">

<i> 491</i>

</td>

<td align="right">

2.45

</td>

<td align="right">

4.86

</td>

<td align="right">

74.03

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`triangl`

</td>

<td align="right">

1.26

</td>

<td align="right">

2.99

</td>

<td align="center" bgcolor="#80f080">

<i> 2243</i>

</td>

<td align="right">

2.24

</td>

<td align="right">

14.96

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`fft`

</td>

<td align="right">

3.19

</td>

<td align="right">

2.06

</td>

<td align="center" bgcolor="#80f080">

<i> 1291</i>

</td>

<td align="right">

3.50

</td>

<td align="right">

8.81

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`fib`

</td>

<td align="right">

2.39

</td>

<td align="right">

3.74

</td>

<td align="center" bgcolor="#80f080">

<i> 2288</i>

</td>

<td align="right">

1.25

</td>

<td align="right">

11.45

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`fibfp`

</td>

<td align="right">

4.15

</td>

<td align="right">

2.29

</td>

<td align="center" bgcolor="#80f080">

<i> 1733</i>

</td>

<td align="right">

9.58

</td>

<td align="right">

7.81

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`mbrot`

</td>

<td align="right">

4.86

</td>

<td align="right">

1.44

</td>

<td align="center" bgcolor="#80f080">

<i> 1816</i>

</td>

<td align="right">

12.20

</td>

<td align="right">

10.51

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`pnpoly`

</td>

<td align="right">

8.20

</td>

<td align="right">

2.64

</td>

<td align="center" bgcolor="#80f080">

<i> 1522</i>

</td>

<td align="right">

3.83

</td>

<td align="center">

crash

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`sum`

</td>

<td align="right">

5.22

</td>

<td align="right">

5.06

</td>

<td align="right">

1.43

</td>

<td align="center" bgcolor="#80f080">

<i> 740</i>

</td>

<td align="right">

19.66

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`sumfp`

</td>

<td align="right">

5.30

</td>

<td align="right">

1.74

</td>

<td align="center" bgcolor="#80f080">

<i> 1331</i>

</td>

<td align="right">

12.48

</td>

<td align="right">

9.96

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`tak`

</td>

<td align="right">

2.17

</td>

<td align="right">

3.73

</td>

<td align="center" bgcolor="#80f080">

<i> 1536</i>

</td>

<td align="right">

1.52

</td>

<td align="right">

14.41

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`ack`

</td>

<td align="right">

3.38

</td>

<td align="right">

6.20

</td>

<td align="center" bgcolor="#80f080">

<i> 133</i>

</td>

<td align="right">

1.47

</td>

<td align="right">

18.20

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`array1`

</td>

<td align="right">

1.75

</td>

<td align="right">

2.65

</td>

<td align="center" bgcolor="#80f080">

<i> 830</i>

</td>

<td align="right">

1.47

</td>

<td align="right">

9.52

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`cat`

</td>

<td align="center" bgcolor="#80f080">

<i> 230</i>

</td>

<td align="right">

11.85

</td>

<td align="right">

7.14

</td>

<td align="right">

3.66

</td>

<td align="right">

2.74

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`string`

</td>

<td align="center" bgcolor="#80f080">

<i> 30</i>

</td>

<td align="right">

4.50

</td>

<td align="right">

9.50

</td>

<td align="right">

9.57

</td>

<td align="right">

275.33

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`sum1`

</td>

<td align="center" bgcolor="#80f080">

<i> 80</i>

</td>

<td align="right">

7.25

</td>

<td align="right">

2.04

</td>

<td align="right">

2.96

</td>

<td align="right">

30.62

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`sumloop`

</td>

<td align="right">

2.37

</td>

<td align="right">

4.31

</td>

<td align="center" bgcolor="#80f080">

<i> 1037</i>

</td>

<td align="right">

2.83

</td>

<td align="right">

15.22

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`tail`

</td>

<td align="center" bgcolor="#80f080">

<i> 640</i>

</td>

<td align="right">

2.86

</td>

<td align="right">

1.92

</td>

<td align="right">

2.09

</td>

<td align="right">

6.39

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`wc`

</td>

<td align="center" bgcolor="#80f080">

<i> 230</i>

</td>

<td align="right">

7.38

</td>

<td align="right">

3.36

</td>

<td align="right">

3.77

</td>

<td align="right">

7.43

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`conform`

</td>

<td align="center" bgcolor="#80f080">

<i> 800</i>

</td>

<td align="right">

2.95

</td>

<td align="right">

1.49

</td>

<td align="right">

6.92

</td>

<td align="right">

33.72

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`dynamic`

</td>

<td align="center" bgcolor="#80f080">

<i> 520</i>

</td>

<td align="right">

4.72

</td>

<td align="right">

1.98

</td>

<td align="right">

2.13

</td>

<td align="right">

16.67

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`earley`

</td>

<td align="right">

1.61

</td>

<td align="right">

5.26

</td>

<td align="center" bgcolor="#80f080">

<i> 1127</i>

</td>

<td align="right">

4.62

</td>

<td align="right">

19.36

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`fibc`

</td>

<td align="right">

33.08

</td>

<td align="right">

2.14

</td>

<td align="center" bgcolor="#80f080">

<i> 1365</i>

</td>

<td align="right">

70.66

</td>

<td align="right">

10.56

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`graphs`

</td>

<td align="right">

2.08

</td>

<td align="right">

2.08

</td>

<td align="center" bgcolor="#80f080">

<i> 1358</i>

</td>

<td align="right">

9.86

</td>

<td align="right">

20.09

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`lattice`

</td>

<td align="right">

1.50

</td>

<td align="right">

1.82

</td>

<td align="center" bgcolor="#80f080">

<i> 2721</i>

</td>

<td align="right">

3.26

</td>

<td align="right">

34.55

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`matrix`

</td>

<td align="right">

1.37

</td>

<td align="right">

3.03

</td>

<td align="center" bgcolor="#80f080">

<i> 1727</i>

</td>

<td align="right">

4.33

</td>

<td align="right">

16.03

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`maze`

</td>

<td align="center" bgcolor="#80f080">

<i> 1410</i>

</td>

<td align="right">

3.03

</td>

<td align="right">

1.13

</td>

<td align="right">

3.53

</td>

<td align="right">

14.74

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`mazefun`

</td>

<td align="right">

2.02

</td>

<td align="right">

2.41

</td>

<td align="center" bgcolor="#80f080">

<i> 1359</i>

</td>

<td align="right">

2.44

</td>

<td align="right">

15.54

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`nqueens`

</td>

<td align="right">

1.73

</td>

<td align="right">

2.59

</td>

<td align="center" bgcolor="#80f080">

<i> 1733</i>

</td>

<td align="right">

3.12

</td>

<td align="right">

15.74

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`paraffins`

</td>

<td align="right">

2.78

</td>

<td align="right">

5.32

</td>

<td align="center" bgcolor="#80f080">

<i> 1748</i>

</td>

<td align="right">

4.50

</td>

<td align="right">

8.63

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`peval`

</td>

<td align="center" bgcolor="#80f080">

<i> 880</i>

</td>

<td align="right">

4.53

</td>

<td align="right">

1.21

</td>

<td align="right">

5.47

</td>

<td align="right">

22.76

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`primes`

</td>

<td align="right">

2.22

</td>

<td align="right">

8.24

</td>

<td align="center" bgcolor="#80f080">

<i> 1339</i>

</td>

<td align="right">

8.98

</td>

<td align="right">

19.24

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`ray`

</td>

<td align="right">

3.42

</td>

<td align="right">

1.22

</td>

<td align="center" bgcolor="#80f080">

<i> 1127</i>

</td>

<td align="right">

5.82

</td>

<td align="right">

177.25

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`scheme`

</td>

<td align="center" bgcolor="#80f080">

<i> 1460</i>

</td>

<td align="right">

1.75

</td>

<td align="right">

1.26

</td>

<td align="right">

3.08

</td>

<td align="right">

14.34

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`simplex`

</td>

<td align="right">

1.93

</td>

<td align="right">

2.40

</td>

<td align="center" bgcolor="#80f080">

<i> 1325</i>

</td>

<td align="right">

4.70

</td>

<td align="right">

78.38

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`slatex`

</td>

<td align="center" bgcolor="#80f080">

<i> 930</i>

</td>

<td align="right">

1.78

</td>

<td align="right">

1.57

</td>

<td align="right">

1.60

</td>

<td align="right">

3.52

</td>

</tr>

</table>

</td>

<td>

</td>

<td>

<table>

<tr>

<td colspan="1" align="center">

Program

</td>

<td colspan="1" align="center">

Bigloo

</td>

<td colspan="1" align="center">

Chicken

</td>

<td colspan="1" align="center">

Gambit

</td>

<td colspan="1" align="center">

MzScheme

</td>

<td colspan="1" align="center">

Scheme48

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`boyer`

</td>

<td align="center" bgcolor="#80f080">

<i> 640</i>

</td>

<td align="center">

</td>

<td align="right">

1.60

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`browse`

</td>

<td align="center" bgcolor="#80f080">

<i> 2990</i>

</td>

<td align="center">

</td>

<td align="right">

1.27

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`cpstak`

</td>

<td align="right">

5.31

</td>

<td align="center">

</td>

<td align="center" bgcolor="#80f080">

<i> 1660</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`ctak`

</td>

<td align="right">

59.02

</td>

<td align="center">

</td>

<td align="center" bgcolor="#80f080">

<i> 1768</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`dderiv`

</td>

<td align="right">

2.90

</td>

<td align="center">

</td>

<td align="center" bgcolor="#80f080">

<i> 1707</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`deriv`

</td>

<td align="right">

4.08

</td>

<td align="center">

</td>

<td align="center" bgcolor="#80f080">

<i> 1158</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`destruc`

</td>

<td align="center" bgcolor="#80f080">

<i> 1360</i>

</td>

<td align="center">

</td>

<td align="right">

1.06

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`diviter`

</td>

<td align="right">

3.72

</td>

<td align="center">

</td>

<td align="center" bgcolor="#80f080">

<i> 1201</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`divrec`

</td>

<td align="right">

3.16

</td>

<td align="center">

</td>

<td align="center" bgcolor="#80f080">

<i> 1620</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`puzzle`

</td>

<td align="right">

1.90

</td>

<td align="center">

</td>

<td align="center" bgcolor="#80f080">

<i> 1745</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`takl`

</td>

<td align="center" bgcolor="#80f080">

<i> 830</i>

</td>

<td align="center">

</td>

<td align="right">

1.50

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`triangl`

</td>

<td align="center" bgcolor="#80f080">

<i> 1440</i>

</td>

<td align="center">

</td>

<td align="right">

1.56

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`fft`

</td>

<td align="right">

1.11

</td>

<td align="center">

</td>

<td align="center" bgcolor="#80f080">

<i> 1210</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`fib`

</td>

<td align="center" bgcolor="#80f080">

<i> 1000</i>

</td>

<td align="center">

</td>

<td align="right">

2.08

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`fibfp`

</td>

<td align="right">

1.60

</td>

<td align="center">

</td>

<td align="center" bgcolor="#80f080">

<i> 1739</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`mbrot`

</td>

<td align="center" bgcolor="#80f080">

<i> 240</i>

</td>

<td align="center">

</td>

<td align="right">

7.67

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`pnpoly`

</td>

<td align="right">

4.10

</td>

<td align="center">

</td>

<td align="center" bgcolor="#80f080">

<i> 1433</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`sum`

</td>

<td align="center" bgcolor="#80f080">

<i> 100</i>

</td>

<td align="center">

</td>

<td align="right">

6.64

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`sumfp`

</td>

<td align="center" bgcolor="#80f080">

<i> 100</i>

</td>

<td align="center">

</td>

<td align="right">

13.80

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`tak`

</td>

<td align="center" bgcolor="#80f080">

<i> 1090</i>

</td>

<td align="center">

</td>

<td align="right">

1.21

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`ack`

</td>

<td align="right">

1.30

</td>

<td align="center">

</td>

<td align="center" bgcolor="#80f080">

<i> 92</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`array1`

</td>

<td align="right">

1.35

</td>

<td align="center">

</td>

<td align="center" bgcolor="#80f080">

<i> 853</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`cat`

</td>

<td align="center" bgcolor="#80f080">

<i> 240</i>

</td>

<td align="center">

</td>

<td align="right">

6.29

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`string`

</td>

<td align="center" bgcolor="#80f080">

<i> 40</i>

</td>

<td align="center">

</td>

<td align="right">

7.32

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`sum1`

</td>

<td align="center" bgcolor="#80f080">

<i> 70</i>

</td>

<td align="center">

</td>

<td align="right">

2.40

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`sumloop`

</td>

<td align="center" bgcolor="#80f080">

<i> 220</i>

</td>

<td align="center">

</td>

<td align="right">

3.40

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`tail`

</td>

<td align="center" bgcolor="#80f080">

<i> 630</i>

</td>

<td align="center">

</td>

<td align="right">

1.99

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`wc`

</td>

<td align="center" bgcolor="#80f080">

<i> 160</i>

</td>

<td align="center">

</td>

<td align="right">

4.90

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`conform`

</td>

<td align="center" bgcolor="#80f080">

<i> 800</i>

</td>

<td align="center">

</td>

<td align="right">

1.54

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`dynamic`

</td>

<td align="center" bgcolor="#80f080">

<i> 510</i>

</td>

<td align="center">

</td>

<td align="right">

2.05

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`earley`

</td>

<td align="right">

1.42

</td>

<td align="center">

</td>

<td align="center" bgcolor="#80f080">

<i> 1016</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`fibc`

</td>

<td align="right">

34.71

</td>

<td align="center">

</td>

<td align="center" bgcolor="#80f080">

<i> 1339</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`graphs`

</td>

<td align="right">

1.95

</td>

<td align="center">

</td>

<td align="center" bgcolor="#80f080">

<i> 1293</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`lattice`

</td>

<td align="right">

1.45

</td>

<td align="center">

</td>

<td align="center" bgcolor="#80f080">

<i> 2877</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`matrix`

</td>

<td align="right">

1.17

</td>

<td align="center">

</td>

<td align="center" bgcolor="#80f080">

<i> 1772</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`maze`

</td>

<td align="right">

1.08

</td>

<td align="center">

</td>

<td align="center" bgcolor="#80f080">

<i> 973</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`mazefun`

</td>

<td align="right">

1.16

</td>

<td align="center">

</td>

<td align="center" bgcolor="#80f080">

<i> 1279</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`nqueens`

</td>

<td align="right">

1.01

</td>

<td align="center">

</td>

<td align="center" bgcolor="#80f080">

<i> 1563</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`paraffins`

</td>

<td align="right">

2.70

</td>

<td align="center">

</td>

<td align="center" bgcolor="#80f080">

<i> 1829</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`peval`

</td>

<td align="center" bgcolor="#80f080">

<i> 880</i>

</td>

<td align="center">

</td>

<td align="right">

1.21

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`primes`

</td>

<td align="right">

2.15

</td>

<td align="center">

</td>

<td align="center" bgcolor="#80f080">

<i> 1360</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`ray`

</td>

<td align="right">

1.02

</td>

<td align="center">

</td>

<td align="center" bgcolor="#80f080">

<i> 1091</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`scheme`

</td>

<td align="center" bgcolor="#80f080">

<i> 1490</i>

</td>

<td align="center">

</td>

<td align="right">

1.20

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`simplex`

</td>

<td align="right">

1.02

</td>

<td align="center">

</td>

<td align="center" bgcolor="#80f080">

<i> 1145</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`slatex`

</td>

<td align="center" bgcolor="#80f080">

<i> 930</i>

</td>

<td align="center">

</td>

<td align="right">

1.59

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

</table>

</td>

<td>

</td>

<td>

<table>

<tr>

<td colspan="1" align="center">

Program

</td>

<td colspan="1" align="center">

Bigloo

</td>

<td colspan="1" align="center">

Chicken

</td>

<td colspan="1" align="center">

Gambit

</td>

<td colspan="1" align="center">

MzScheme

</td>

<td colspan="1" align="center">

Scheme48

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`boyer`

</td>

<td align="center" bgcolor="#80f080">

<i> 470</i>

</td>

<td align="right">

3.39

</td>

<td align="right">

1.43

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`browse`

</td>

<td align="center" bgcolor="#80f080">

<i> 2500</i>

</td>

<td align="right">

2.57

</td>

<td align="right">

1.50

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`cpstak`

</td>

<td align="right">

6.02

</td>

<td align="right">

1.62

</td>

<td align="center" bgcolor="#80f080">

<i> 1438</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`ctak`

</td>

<td align="right">

49.19

</td>

<td align="right">

1.65

</td>

<td align="center" bgcolor="#80f080">

<i> 1728</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`dderiv`

</td>

<td align="right">

3.00

</td>

<td align="right">

2.72

</td>

<td align="center" bgcolor="#80f080">

<i> 1500</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`deriv`

</td>

<td align="right">

3.72

</td>

<td align="right">

2.91

</td>

<td align="center" bgcolor="#80f080">

<i> 1179</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`destruc`

</td>

<td align="center" bgcolor="#80f080">

<i> 1200</i>

</td>

<td align="right">

1.12

</td>

<td align="right">

1.06

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`diviter`

</td>

<td align="right">

5.17

</td>

<td align="center" bgcolor="#80f080">

<i> 785</i>

</td>

<td align="right">

1.29

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`divrec`

</td>

<td align="right">

3.03

</td>

<td align="right">

1.84

</td>

<td align="center" bgcolor="#80f080">

<i> 1409</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`puzzle`

</td>

<td align="right">

3.58

</td>

<td align="right">

4.28

</td>

<td align="center" bgcolor="#80f080">

<i> 660</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`takl`

</td>

<td align="center" bgcolor="#80f080">

<i> 450</i>

</td>

<td align="right">

1.10

</td>

<td align="right">

1.96

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`triangl`

</td>

<td align="center" bgcolor="#80f080">

<i> 950</i>

</td>

<td align="right">

4.63

</td>

<td align="right">

1.20

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`fft`

</td>

<td align="right">

3.17

</td>

<td align="right">

4.66

</td>

<td align="center" bgcolor="#80f080">

<i> 328</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`fib`

</td>

<td align="center" bgcolor="#80f080">

<i> 770</i>

</td>

<td align="right">

1.49

</td>

<td align="right">

1.24

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`fibfp`

</td>

<td align="right">

2.26

</td>

<td align="right">

2.55

</td>

<td align="center" bgcolor="#80f080">

<i> 1173</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`mbrot`

</td>

<td align="center" bgcolor="#80f080">

<i> 230</i>

</td>

<td align="right">

5.30

</td>

<td align="right">

4.68

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`pnpoly`

</td>

<td align="center" bgcolor="#80f080">

<i> 190</i>

</td>

<td align="right">

14.73

</td>

<td align="right">

1.51

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`sum`

</td>

<td align="center" bgcolor="#80f080">

<i> 100</i>

</td>

<td align="right">

1.48

</td>

<td align="right">

1.70

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`sumfp`

</td>

<td align="center" bgcolor="#80f080">

<i> 100</i>

</td>

<td align="right">

11.15

</td>

<td align="right">

12.04

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`tak`

</td>

<td align="center" bgcolor="#80f080">

<i> 740</i>

</td>

<td align="right">

1.26

</td>

<td align="right">

1.42

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`ack`

</td>

<td align="right">

2.05

</td>

<td align="center" bgcolor="#80f080">

<i> 44</i>

</td>

<td align="right">

1.34

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`array1`

</td>

<td align="right">

1.28

</td>

<td align="right">

2.75

</td>

<td align="center" bgcolor="#80f080">

<i> 431</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`cat`

</td>

<td align="center" bgcolor="#80f080">

<i> 200</i>

</td>

<td align="right">

10.44

</td>

<td align="right">

7.86

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`string`

</td>

<td align="center" bgcolor="#80f080">

<i> 20</i>

</td>

<td align="right">

6.55

</td>

<td align="right">

14.55

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`sum1`

</td>

<td align="center" bgcolor="#80f080">

<i> 70</i>

</td>

<td align="right">

6.26

</td>

<td align="right">

2.36

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`sumloop`

</td>

<td align="center" bgcolor="#80f080">

<i> 100</i>

</td>

<td align="right">

9.55

</td>

<td align="right">

3.85

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`tail`

</td>

<td align="center" bgcolor="#80f080">

<i> 560</i>

</td>

<td align="right">

2.51

</td>

<td align="right">

2.20

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`wc`

</td>

<td align="center" bgcolor="#80f080">

<i> 130</i>

</td>

<td align="right">

9.89

</td>

<td align="right">

5.58

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`conform`

</td>

<td align="center" bgcolor="#80f080">

<i> 630</i>

</td>

<td align="right">

3.75

</td>

<td align="right">

1.50

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`dynamic`

</td>

<td align="center" bgcolor="#80f080">

<i> 500</i>

</td>

<td align="right">

4.25

</td>

<td align="right">

1.93

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`earley`

</td>

<td align="right">

1.49

</td>

<td align="right">

6.55

</td>

<td align="center" bgcolor="#80f080">

<i> 767</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`fibc`

</td>

<td align="right">

29.12

</td>

<td align="right">

1.71

</td>

<td align="center" bgcolor="#80f080">

<i> 1358</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`graphs`

</td>

<td align="right">

1.90

</td>

<td align="right">

2.39

</td>

<td align="center" bgcolor="#80f080">

<i> 979</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`lattice`

</td>

<td align="right">

1.75

</td>

<td align="right">

2.39

</td>

<td align="center" bgcolor="#80f080">

<i> 2067</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`matrix`

</td>

<td align="right">

1.17

</td>

<td align="right">

2.39

</td>

<td align="center" bgcolor="#80f080">

<i> 1481</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`maze`

</td>

<td align="right">

1.47

</td>

<td align="right">

4.38

</td>

<td align="center" bgcolor="#80f080">

<i> 524</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`mazefun`

</td>

<td align="right">

1.26

</td>

<td align="right">

1.25

</td>

<td align="center" bgcolor="#80f080">

<i> 1000</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`nqueens`

</td>

<td align="right">

1.23

</td>

<td align="right">

1.59

</td>

<td align="center" bgcolor="#80f080">

<i> 1081</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`paraffins`

</td>

<td align="right">

2.65

</td>

<td align="right">

5.53

</td>

<td align="center" bgcolor="#80f080">

<i> 1736</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`peval`

</td>

<td align="center" bgcolor="#80f080">

<i> 780</i>

</td>

<td align="right">

4.43

</td>

<td align="right">

1.10

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`primes`

</td>

<td align="right">

2.15

</td>

<td align="right">

1.43

</td>

<td align="center" bgcolor="#80f080">

<i> 1079</i>

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`ray`

</td>

<td align="center" bgcolor="#80f080">

<i> 130</i>

</td>

<td align="right">

10.81

</td>

<td align="right">

2.94

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`scheme`

</td>

<td align="center" bgcolor="#80f080">

<i> 1060</i>

</td>

<td align="right">

2.31

</td>

<td align="right">

1.44

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#dddddd">

<td>

`simplex`

</td>

<td align="center" bgcolor="#80f080">

<i> 350</i>

</td>

<td align="center">

crash

</td>

<td align="right">

1.33

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

<tr bgcolor="#eeeeee">

<td>

`slatex`

</td>

<td align="center" bgcolor="#80f080">

<i> 880</i>

</td>

<td align="right">

1.70

</td>

<td align="right">

1.60

</td>

<td align="center">

</td>

<td align="center">

</td>

</tr>

</table>

</td>

</tr>

</table>

  - The test machine is a MacBook Pro, 2 GHz Intel Core, 2 GB DDR2
    SDRAM, Mac OS X 10.4.7 .
  - The following options were given to each system:
      - <b>Bigloo Version 2.8c</b>
          - <b>R5RS:</b> situation not possible (car, +, etc cannot be
            mutated)
          - <b>R6RS:</b> `bigloo -O6 -copt -O3 -copt
            -fomit-frame-pointer`
          - <b>R6RS fixflo:</b> `bigloo -Obench -copt -O3 -copt
            -fomit-frame-pointer`
          - <b>R6RS fixflo unsafe:</b> `bigloo -Obench -copt -O3 -copt
            -fomit-frame-pointer`
        Note that with these options the compiler emits run time type
        checks but does not detect fixnum arithmetic overflows.
      - <b>Chicken Version 2, Build 41</b>
          - <b>R5RS:</b> `csc -d0 -no-trace -no-usual-integrations`
          - <b>R6RS:</b> `csc -d0 -O3 -no-trace -block`
          - <b>R6RS fixflo:</b> situation not possible (fixnum/flonum
            specific operations are unsafe)
          - <b>R6RS fixflo unsafe:</b> `csc -d0 -O3 -no-trace -block
            -unsafe -unsafe-libraries`
        The following program options were used for all situations:
        `-:hi10M -:hs0`
      - <b>Gambit Version 4.0 beta 18</b>
          - <b>R5RS:</b> no declarations
          - <b>R6RS:</b> `(declare (standard-bindings)
            (extended-bindings) (block))`
          - <b>R6RS fixflo:</b> `(declare (standard-bindings)
            (extended-bindings) (block))`
          - <b>R6RS fixflo unsafe:</b> `(declare (standard-bindings)
            (extended-bindings) (block) (not safe))`
        The programs were compiled with `gsc -dynamic`. The following
        program options were used for all situations: `-:m10000,d-`
        (this gives a 10 MByte heap)
      - <b>MzScheme Version 352</b>
          - <b>R5RS:</b> flat program (toplevel definitions) loaded with
            "`load`"
          - <b>R6RS:</b> program wrapped in a module and loaded with
            "`require`"
          - <b>R6RS fixflo:</b> situation not possible (no fixnum/flonum
            specific operations)
          - <b>R6RS fixflo unsafe:</b> situation not possible (no unsafe
            mode)
      - <b>Scheme48 Version 1.3</b>
          - <b>R5RS:</b> `,bench off & ,open time posix bitwise ascii &
            ,load bench.scm`
          - <b>R6RS:</b> `,bench on & ,open time posix bitwise ascii &
            ,load bench.scm`
          - <b>R6RS fixflo:</b> situation not possible (no fixnum/flonum
            specific operations)
          - <b>R6RS fixflo unsafe:</b> situation not possible (no unsafe
            mode)
        The following program options were used for all situations:
        `-h 5000000`
  - Most of the benchmarks take parameters that affect the computation
    that is performed (number of iterations, initial arguments, etc).
    The benchmarks were carefully written to defeat some compiler
    optimizations such as partial or total evaluation of the benchmark
    at compile time when the benchmark parameters are known. Our
    position is that each benchmark should be thought of as a function
    exported from a separately compiled module, and the function is
    called from another module with the benchmark parameters (so the
    compilation of the benchmark module should not take into account the
    specific values that are passed to the function or even the type of
    value passed). The benchmark parameters are passed to the function
    using a call to `apply`. For the systems tested this seems to
    achieve the desired effect (at least for the versions used). It
    should be noted that some of the systems could generate faster code
    if this approach was not used. In particular, for the Bigloo
    compiler, if the types of the benchmark parameters are known, better
    code can often be generated. Moreover it is common in Bigloo to
    specify the type of parameters on exported functions, so the
    performance obtained with this experimental setup may not be
    representative of the performance commonly achieved by programmers
    accustomed to Bigloo.
