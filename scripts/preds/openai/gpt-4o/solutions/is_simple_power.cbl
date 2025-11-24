       IDENTIFICATION DIVISION.
       PROGRAM-ID. IS-SIMPLE-POWER.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 TEMP-RESULT PIC S9(10) COMP-3.
       01 I PIC S9(10).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-X PIC S9(10).
           05 L-N PIC S9(10).
           05 RESULT PIC 9.

      * Your task is to write a function that returns true if a number x is a simple
      * power of n and false in other cases.
      * x is a simple power of n if n**int=x
      * For example:
      * is_simple_power(1, 4) => true
      * is_simple_power(2, 2) => true
      * is_simple_power(8, 2) => true
      * is_simple_power(3, 2) => false
      * is_simple_power(3, 1) => false
      * is_simple_power(5, 3) => false
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

       BEGIN-CALCULATION.
           IF L-X = 1 THEN
               MOVE 1 TO RESULT
               GOBACK
           END-IF

           IF L-N <= 1 THEN
               MOVE 0 TO RESULT
               GOBACK
           END-IF

           MOVE 1 TO TEMP-RESULT
           MOVE 0 TO I

           PERFORM UNTIL TEMP-RESULT >= L-X
               COMPUTE I = I + 1
               COMPUTE TEMP-RESULT = L-N ** I

               IF TEMP-RESULT = L-X THEN
                   MOVE 1 TO RESULT
                   GOBACK
               END-IF
           END-PERFORM

           MOVE 0 TO RESULT

       END-CALCULATION.

       GOBACK.
       END PROGRAM IS-SIMPLE-POWER.
