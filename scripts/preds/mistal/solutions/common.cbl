       IDENTIFICATION DIVISION.
       PROGRAM-ID. COMMON.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-COUNTERS.
           05 WS-I PIC 9(2) VALUE 0.
           05 WS-J PIC 9(2) VALUE 0.
           05 WS-K PIC 9(2) VALUE 0.
           05 WS-FOUND-FLAG PIC X VALUE 'N'.
               88 FOUND VALUE 'Y'.
               88 NOT-FOUND VALUE 'N'.
       01 WS-TEMP-RESULT OCCURS 100 TIMES PIC S9(10).
       01 WS-TEMP-COUNT PIC 9(3) VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-L1 OCCURS 4 TIMES INDEXED BY NI PIC S9(10).
           05 L-L2 OCCURS 4 TIMES INDEXED BY NJ PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NK PIC S9(10).

      * Return sorted unique common elements for two lists.
      * >>> common([1, 4, 3, 34], [5, 7])
      * []
      * >>> common([5, 3, 2, 8], [3, 2])
      * [2, 3]
      * 
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
       MAIN-PARA.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 4
                   IF L-L1(WS-I) = L-L2(WS-J)
                       MOVE 'N' TO WS-FOUND-FLAG
                       PERFORM VARYING WS-K FROM 1 BY 1
                           UNTIL WS-K > WS-TEMP-COUNT OR FOUND
                           IF L-L1(WS-I) = WS-TEMP-RESULT(WS-K)
                               MOVE 'Y' TO WS-FOUND-FLAG
                           END-IF
                       END-PERFORM
                       IF NOT-FOUND
                           ADD 1 TO WS-TEMP-COUNT
                           MOVE L-L1(WS-I) TO WS-TEMP-RESULT(WS-TEMP-COUNT)
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM

           PERFORM SORT-TEMP-RESULT

           PERFORM VARYING WS-K FROM 1 BY 1 UNTIL WS-K > WS-TEMP-COUNT
               MOVE WS-TEMP-RESULT(WS-K) TO RESULT(WS-K)
           END-PERFORM

           GOBACK.

       SORT-TEMP-RESULT.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-TEMP-COUNT - 1
               PERFORM VARYING WS-J FROM WS-I + 1 BY 1 UNTIL WS-J > WS-TEMP-COUNT
                   IF WS-TEMP-RESULT(WS-I) > WS-TEMP-RESULT(WS-J)
                       MOVE WS-TEMP-RESULT(WS-I) TO WS-TEMP-RESULT(100)
                       MOVE WS-TEMP-RESULT(WS-J) TO WS-TEMP-RESULT(WS-I)
                       MOVE WS-TEMP-RESULT(100) TO WS-TEMP-RESULT(WS-J)
                   END-IF
               END-PERFORM
           END-PERFORM.
