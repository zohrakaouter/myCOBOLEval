       IDENTIFICATION DIVISION.
       PROGRAM-ID. REMOVE-DUPLICATES.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       

       01 WS-COUNTERS.
           05 WS-I PIC S9(10).
           05 WS-J PIC S9(10).
           05 WS-K PIC S9(10).
           05 WS-DUPLICATE-FLAG PIC X VALUE 'N'.
               88 DUPLICATE-FOUND VALUE 'Y'.
               88 NO-DUPLICATE-FOUND VALUE 'N'.
           05 WS-RESULT-COUNT PIC S9(10) VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-NUMBERS OCCURS 5 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NJ PIC S9(10).

      * From a list of integers, remove all elements that occur more than once.
      * Keep order of elements left the same as in the input.
      * >>> remove_duplicates([1, 2, 3, 2, 4])
      * [1, 3, 4]
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               MOVE 'N' TO WS-DUPLICATE-FLAG
               PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > WS-I - 1
                   IF L-NUMBERS(WS-I) = L-NUMBERS(WS-J)
                       MOVE 'Y' TO WS-DUPLICATE-FLAG
                   END-IF
               END-PERFORM

               IF NO-DUPLICATE-FOUND
                   ADD 1 TO WS-RESULT-COUNT
                   MOVE L-NUMBERS(WS-I) TO RESULT(WS-RESULT-COUNT)
               END-IF
           END-PERFORM

           GOBACK.
       END PROGRAM REMOVE-DUPLICATES.
