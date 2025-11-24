       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIND-CLOSEST-ELEMENTS.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       

       01 WS-VARIABLES.
           05 WS-MIN-DIFFERENCE COMP-2 VALUE 1.7E38.
           05 WS-CURRENT-DIFFERENCE COMP-2.
           05 WS-I PIC 9(3).
           05 WS-J PIC 9(3).
           05 WS-RESULT-1 COMP-2.
           05 WS-RESULT-2 COMP-2.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-NUMBERS OCCURS 6 TIMES INDEXED BY NI COMP-2.
           05 RESULT OCCURS 100 TIMES INDEXED BY NJ COMP-2.

      * From a supplied list of numbers (of length at least two) select and return two that are the closest to each
      * other and return them in order (smaller number, larger number).
      * >>> find_closest_elements([1.0, 2.0, 3.0, 4.0, 5.0, 2.2])
      * (2.0, 2.2)
      * >>> find_closest_elements([1.0, 2.0, 3.0, 4.0, 5.0, 2.0])
      * (2.0, 2.0)
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               PERFORM VARYING WS-J FROM WS-I + 1 BY 1 UNTIL WS-J > 6
                   COMPUTE WS-CURRENT-DIFFERENCE =
                       FUNCTION ABS(L-NUMBERS(WS-I) - L-NUMBERS(WS-J))
                   IF WS-CURRENT-DIFFERENCE < WS-MIN-DIFFERENCE
                       MOVE WS-CURRENT-DIFFERENCE TO WS-MIN-DIFFERENCE
                       MOVE L-NUMBERS(WS-I) TO WS-RESULT-1
                       MOVE L-NUMBERS(WS-J) TO WS-RESULT-2
                   END-IF
               END-PERFORM
           END-PERFORM

           IF WS-RESULT-1 > WS-RESULT-2
               MOVE WS-RESULT-2 TO RESULT(1)
               MOVE WS-RESULT-1 TO RESULT(2)
           ELSE
               MOVE WS-RESULT-1 TO RESULT(1)
               MOVE WS-RESULT-2 TO RESULT(2)
           END-IF

           GOBACK.
