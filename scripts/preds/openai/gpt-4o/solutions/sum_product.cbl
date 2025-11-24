       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUM-PRODUCT.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       77 WS-SUM        PIC S9(10) VALUE 0.
       77 WS-PRODUCT    PIC S9(10) VALUE 1.
       01 COUNTERS.
           05 I               PIC 9(3) VALUE 1.
           05 LIST-LENGTH     PIC 9(3) VALUE 4.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-NUMBERS OCCURS 4 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NJ PIC S9(10).

      * For a given list of integers, return a tuple consisting of a sum and a product of all the integers in a list.
      * >>> sum_product([0, 0, 0, 1])
      * (1, 0)
      * >>> sum_product([1, 2, 3, 4])
      * (10, 24)
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LIST-LENGTH
               SET NI TO I
               ADD L-NUMBERS(NI) TO WS-SUM
               MULTIPLY WS-PRODUCT BY L-NUMBERS(NI)
               IF WS-PRODUCT IS NOT ZERO
                   THEN
                       COMPUTE WS-PRODUCT = WS-PRODUCT * L-NUMBERS(NI)
                   ELSE
                       CONTINUE
               END-IF
           END-PERFORM

           MOVE WS-SUM TO RESULT (1)
           MOVE WS-PRODUCT TO RESULT (2)
       
           EXIT PROGRAM.

       GOBACK.
