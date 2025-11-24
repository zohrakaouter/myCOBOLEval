       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIMPLIFY.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       

       01 WS-VARIABLES.
           05 WS-X-NUMERATOR PIC 9(10).
           05 WS-X-DENOMINATOR PIC 9(10).
           05 WS-N-NUMERATOR PIC 9(10).
           05 WS-N-DENOMINATOR PIC 9(10).
           05 WS-RESULT-NUMERATOR PIC 9(20).
           05 WS-RESULT-DENOMINATOR PIC 9(20).
           05 WS-TEMP PIC 9(20).
           05 WS-REM PIC 9(20).
           05 WS-I PIC 9(10).
           05 WS-SLASH-POS PIC 9(3).

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

           * Parse x fraction
           INSPECT L-X TALLYING WS-SLASH-POS FOR CHARACTERS BEFORE '/'
           STRING L-X(1:WS-SLASH-POS) DELIMITED BY SIZE INTO WS-X-NUMERATOR-STR
           STRING L-X(WS-SLASH-POS + 2:) DELIMITED BY SIZE INTO WS-X-DENOMINATOR-STR
           COMPUTE WS-X-NUMERATOR = FUNCTION NUMVAL(WS-X-NUMERATOR-STR)
           COMPUTE WS-X-DENOMINATOR = FUNCTION NUMVAL(WS-X-DENOMINATOR-STR)

           * Parse n fraction
           INSPECT L-N TALLYING WS-SLASH-POS FOR CHARACTERS BEFORE '/'
           STRING L-N(1:WS-SLASH-POS) DELIMITED BY SIZE INTO WS-N-NUMERATOR-STR
           STRING L-N(WS-SLASH-POS + 2:) DELIMITED BY SIZE INTO WS-N-DENOMINATOR-STR
           COMPUTE WS-N-NUMERATOR = FUNCTION NUMVAL(WS-N-NUMERATOR-STR)
           COMPUTE WS-N-DENOMINATOR = FUNCTION NUMVAL(WS-N-DENOMINATOR-STR)

           * Multiply fractions: (x_num * n_num) / (x_den * n_den)
           COMPUTE WS-RESULT-NUMERATOR = WS-X-NUMERATOR * WS-N-NUMERATOR
           COMPUTE WS-RESULT-DENOMINATOR = WS-X-DENOMINATOR * WS-N-DENOMINATOR

           * Simplify fraction by dividing numerator and denominator by GCD
           PERFORM FIND-GCD
           COMPUTE WS-RESULT-NUMERATOR = WS-RESULT-NUMERATOR / WS-TEMP
           COMPUTE WS-RESULT-DENOMINATOR = WS-RESULT-DENOMINATOR / WS-TEMP

           * Check if denominator is 1 (whole number)
           IF WS-RESULT-DENOMINATOR = 1
               MOVE 1 TO RESULT
           ELSE
               MOVE 0 TO RESULT
           END-IF

           GOBACK.

       FIND-GCD.
           * Find greatest common divisor using Euclidean algorithm
           MOVE WS-RESULT-NUMERATOR TO WS-TEMP
           MOVE WS-RESULT-DENOMINATOR TO WS-REM

           PERFORM UNTIL WS-REM = 0
               COMPUTE WS-I = WS-TEMP / WS-REM
               COMPUTE WS-I = WS-I * WS-REM
               SUBTRACT WS-I FROM WS-TEMP GIVING WS-TEMP
               MOVE WS-REM TO WS-I
               MOVE WS-TEMP TO WS-REM
               MOVE WS-I TO WS-TEMP
           END-PERFORM.

           MOVE WS-TEMP TO WS-I.

       01 WS-X-NUMERATOR-STR PIC X(10).
       01 WS-X-DENOMINATOR-STR PIC X(10).
       01 WS-N-NUMERATOR-STR PIC X(10).
       01 WS-N-DENOMINATOR-STR PIC X(10).
