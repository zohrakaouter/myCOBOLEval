       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRUNCATE-NUMBER.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-INTEGER-PART COMP-2.
       01 WS-DECIMAL-PART COMP-2.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-NUMBER COMP-2.
           05 RESULT COMP-2.

      * Given a positive floating point number, it can be decomposed into
      * and integer part (largest integer smaller than given number) and decimals
      * (leftover part always smaller than 1).
      * 
      * Return the decimal part of the number.
      * >>> truncate_number(3.5)
      * 0.5
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

           * Decompose the number into its integer part and decimal part
           COMPUTE WS-INTEGER-PART = FUNCTION INTEGER-PART(L-NUMBER)
           COMPUTE WS-DECIMAL-PART = L-NUMBER - WS-INTEGER-PART

           * Store the result (decimal part) in the given RESULT variable
           MOVE WS-DECIMAL-PART TO RESULT

           * Exit the program
           GOBACK.

       END PROGRAM TRUNCATE-NUMBER.
