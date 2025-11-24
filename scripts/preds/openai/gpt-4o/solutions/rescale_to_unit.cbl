       IDENTIFICATION DIVISION.
       PROGRAM-ID. RESCALE-TO-UNIT.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-MIN-VALUE    COMP-2.
       01 WS-MAX-VALUE    COMP-2.
       01 WS-RANGE        COMP-2.
       01 WS-INDEX        COMP.
       01 WS-TEMP-VALUE   COMP-2.

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

       MAIN-LOGIC.

           PERFORM FIND-MIN-AND-MAX.
           MOVE WS-MAX-VALUE TO WS-TEMP-VALUE
           SUBTRACT WS-MIN-VALUE FROM WS-TEMP-VALUE GIVING WS-RANGE

           IF WS-RANGE = 0.0
               MOVE 0.0 TO ALL RESULT
               EXIT
           END-IF

           PERFORM RESCALE-NUMBERS

           GOBACK.

       FIND-MIN-AND-MAX.

           SET NI TO 1
           EVALUATE TRUE
               WHEN NI = 1
                   MOVE L-NUMBERS(NI) TO WS-MIN-VALUE
                   MOVE L-NUMBERS(NI) TO WS-MAX-VALUE
               WHEN ANY
                   IF L-NUMBERS(NI) < WS-MIN-VALUE
                       MOVE L-NUMBERS(NI) TO WS-MIN-VALUE
                   END-IF
                   IF L-NUMBERS(NI) > WS-MAX-VALUE
                       MOVE L-NUMBERS(NI) TO WS-MAX-VALUE
                   END-IF
           END-EVALUATE

           SET NI UP BY 1
           WHEN NI <= 5
               PERFORM FIND-MIN-AND-MAX
           END-WHEN

           .

       RESCALE-NUMBERS.

           SET NI TO 1
           SET NJ TO 1

           PERFORM UNTIL NI > 5
               COMPUTE RESULT(NJ) = (L-NUMBERS(NI) - WS-MIN-VALUE) / WS-RANGE
               SET NI UP BY 1
               SET NJ UP BY 1
           END-PERFORM

           .

       END PROGRAM RESCALE-TO-UNIT.
