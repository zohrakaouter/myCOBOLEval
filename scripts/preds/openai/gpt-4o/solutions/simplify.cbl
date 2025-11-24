       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIMPLIFY.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01  X-NUMERATOR PIC 9(8).
       01  X-DENOMINATOR PIC 9(8).
       01  N-NUMERATOR PIC 9(8).
       01  N-DENOMINATOR PIC 9(8).
       01  PRODUCT-NUMERATOR PIC 9(16).
       01  PRODUCT-DENOMINATOR PIC 9(16).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-X PIC X(100).
           05 L-N PIC X(100).
           05 RESULT PIC 9.

      * Your task is to implement a function that will simplify the expression
      * x * n. The function returns True if x * n evaluates to a whole number and False
      * otherwise. Both x and n, are string representation of a fraction, and have the following format,
      * <numerator>/<denominator> where both numerator and denominator are positive whole numbers.
      * 
      * You can assume that x, and n are valid fractions, and do not have zero as denominator.
      * 
      * simplify("1/5", "5/1") = True
      * simplify("1/6", "2/1") = False
      * simplify("7/10", "10/2") = False
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

       MAIN-PROCEDURE.
           PERFORM PARSE-FRACTION L-X GIVING X-NUMERATOR X-DENOMINATOR
           PERFORM PARSE-FRACTION L-N GIVING N-NUMERATOR N-DENOMINATOR
           PERFORM MULTIPLY-FRACTIONS

           IF PRODUCT-NUMERATOR MOD PRODUCT-DENOMINATOR = 0
              MOVE 1 TO RESULT
           ELSE
              MOVE 0 TO RESULT
           END-IF.

           GOBACK.

       PARSE-FRACTION SECTION.
       PARSE-FRACTION.
           UNSTRING L-X DELIMITED BY '/'
               INTO X-NUMERATOR, X-DENOMINATOR
           UNSTRING L-N DELIMITED BY '/'
               INTO N-NUMERATOR, N-DENOMINATOR
           .

       MULTIPLY-FRACTIONS SECTION.
       MULTIPLY-FRACTIONS.
           COMPUTE PRODUCT-NUMERATOR = X-NUMERATOR * N-NUMERATOR
           COMPUTE PRODUCT-DENOMINATOR = X-DENOMINATOR * N-DENOMINATOR
           .

       END PROGRAM SIMPLIFY.
