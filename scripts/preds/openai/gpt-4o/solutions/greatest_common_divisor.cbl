       IDENTIFICATION DIVISION.
       PROGRAM-ID. GREATEST-COMMON-DIVISOR.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01  A-INT           PIC S9(10).
       01  B-INT           PIC S9(10).
       01  REMAINDER       PIC S9(10).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-A PIC S9(10).
           05 L-B PIC S9(10).
           05 RESULT PIC S9(10).

      * Return a greatest common divisor of two integers a and b
      * >>> greatest_common_divisor(3, 5)
      * 1
      * >>> greatest_common_divisor(25, 15)
      * 5
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

       BEGIN.
           MOVE L-A TO A-INT.
           MOVE L-B TO B-INT.
           PERFORM CALCULATE-GCD.
           MOVE A-INT TO RESULT.

           GOBACK.

       CALCULATE-GCD.
           PERFORM UNTIL B-INT = 0
               COMPUTE REMAINDER = FUNCTION MOD(A-INT B-INT)
               MOVE B-INT TO A-INT
               MOVE REMAINDER TO B-INT
           END-PERFORM.

       END PROGRAM GREATEST-COMMON-DIVISOR.
