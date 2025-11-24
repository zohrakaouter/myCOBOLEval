       IDENTIFICATION DIVISION.
       PROGRAM-ID. DERIVATIVE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 N PIC 9(10) VALUE 3.  * Number of coefficients in the polynomial.
       01 I PIC 9(10) VALUE 1.
       01 FACTOR PIC 9(10) VALUE 0.
       
       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-XS OCCURS 3 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NJ PIC S9(10).

      * xs represent coefficients of a polynomial.
      * xs[0] + xs[1] * x + xs[2] * x^2 + ....
      * Return derivative of this polynomial in the same form.
      * >>> derivative([3, 1, 2])
      * [1, 4]
      * >>> derivative([1, 2, 3])
      * [2, 6]
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I >= N
               SET NI TO I
               MULTIPLY L-XS (NI) BY I GIVING FACTOR
               SET NJ TO I
               MOVE FACTOR TO RESULT (NJ)
           END-PERFORM

           EXIT PROGRAM.

       GOBACK.

       END PROGRAM DERIVATIVE.
