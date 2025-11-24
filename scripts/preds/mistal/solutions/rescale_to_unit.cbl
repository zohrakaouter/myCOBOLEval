       IDENTIFICATION DIVISION.
       PROGRAM-ID. RESCALE-TO-UNIT.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARIABLES.
           05 WS-MIN-VALUE COMP-2.
           05 WS-MAX-VALUE COMP-2.
           05 WS-RANGE    COMP-2.
           05 WS-INDEX    PIC 9(3).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-NUMBERS OCCURS 5 TIMES INDEXED BY NI COMP-2.
           05 RESULT OCCURS 100 TIMES INDEXED BY NJ COMP-2.

      * Given list of numbers (of at least two elements), apply a linear transform to that list,
      * such that the smallest number will become 0 and the largest will become 1
      * >>> rescale_to_unit([1.0, 2.0, 3.0, 4.0, 5.0])
      * [0.0, 0.25, 0.5, 0.75, 1.0]
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           PERFORM FIND-MIN-MAX
           PERFORM CALCULATE-RESULTS
           GOBACK.

       FIND-MIN-MAX.
           SET NI TO 1
           MOVE L-NUMBERS(NI) TO WS-MIN-VALUE WS-MAX-VALUE
           PERFORM VARYING NI FROM 2 BY 1 UNTIL NI > 5
               IF L-NUMBERS(NI) < WS-MIN-VALUE
                   MOVE L-NUMBERS(NI) TO WS-MIN-VALUE
               END-IF
               IF L-NUMBERS(NI) > WS-MAX-VALUE
                   MOVE L-NUMBERS(NI) TO WS-MAX-VALUE
               END-IF
           END-PERFORM
           SUBTRACT WS-MIN-VALUE FROM WS-MAX-VALUE GIVING WS-RANGE.

       CALCULATE-RESULTS.
           PERFORM VARYING NJ FROM 1 BY 1 UNTIL NJ > 5
               SUBTRACT WS-MIN-VALUE FROM L-NUMBERS(NJ)
               DIVIDE BY WS-RANGE GIVING RESULT(NJ)
           END-PERFORM.
