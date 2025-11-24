       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIND-CLOSEST-ELEMENTS.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       77 MIN-DIFFERENCE         COMP-2 VALUE HIGH-VALUE.
       77 CURRENT-DIFFERENCE     COMP-2 VALUE 0.
       77 INDEX1                 COMP-2 VALUE 1.
       77 INDEX2                 COMP-2 VALUE 2.
       77 FOUND-INDEX1           COMP-2 VALUE 0.
       77 FOUND-INDEX2           COMP-2 VALUE 0.

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

       MAIN-LOGIC.
           PERFORM VARYING INDEX1 FROM 1 BY 1
               UNTIL INDEX1 = 6
               PERFORM VARYING INDEX2 FROM INDEX1 + 1 BY 1
                   UNTIL INDEX2 = 6
                   COMPUTE CURRENT-DIFFERENCE = FUNCTION ABS(
                       L-NUMBERS(INDEX1) - L-NUMBERS(INDEX2))
                   IF CURRENT-DIFFERENCE < MIN-DIFFERENCE
                       MOVE CURRENT-DIFFERENCE TO MIN-DIFFERENCE
                       MOVE INDEX1 TO FOUND-INDEX1
                       MOVE INDEX2 TO FOUND-INDEX2
                   END-IF
               END-PERFORM
           END-PERFORM

           IF L-NUMBERS(FOUND-INDEX1) < L-NUMBERS(FOUND-INDEX2)
               MOVE L-NUMBERS(FOUND-INDEX1) TO RESULT(1)
               MOVE L-NUMBERS(FOUND-INDEX2) TO RESULT(2)
           ELSE
               MOVE L-NUMBERS(FOUND-INDEX2) TO RESULT(1)
               MOVE L-NUMBERS(FOUND-INDEX1) TO RESULT(2)
           END-IF

           GOBACK.

       END PROGRAM FIND-CLOSEST-ELEMENTS.
