       IDENTIFICATION DIVISION.
       PROGRAM-ID. UNIQUE-DIGITS.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARIABLES.
           05 WS-I PIC 9(3).
           05 WS-J PIC 9(3).
           05 WS-K PIC 9(3).
           05 WS-NUM PIC 9(10).
           05 WS-DIGIT PIC 9(1).
           05 WS-TEMP PIC 9(10).
           05 WS-VALID-FLAG PIC X VALUE 'Y'.
               88 VALID-NUMBER VALUE 'Y'.
               88 INVALID-NUMBER VALUE 'N'.
           05 WS-RESULT-COUNT PIC 9(3) VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-X OCCURS 4 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NJ PIC S9(10).

      * Given a list of positive integers x. return a sorted list of all
      * elements that hasn't any even digit.
      * 
      * Note: Returned list should be sorted in increasing order.
      * 
      * For example:
      * >>> unique_digits([15, 33, 1422, 1])
      * [1, 15, 33]
      * >>> unique_digits([152, 323, 1422, 10])
      * []
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
       MAIN-PARA.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               MOVE L-X(WS-I) TO WS-NUM
               MOVE 'Y' TO WS-VALID-FLAG
               PERFORM CHECK-DIGITS
               IF VALID-NUMBER
                   ADD 1 TO WS-RESULT-COUNT
                   MOVE WS-NUM TO RESULT(WS-RESULT-COUNT)
               END-IF
           END-PERFORM

           PERFORM SORT-RESULT

           GOBACK.

       CHECK-DIGITS.
           MOVE WS-NUM TO WS-TEMP
           PERFORM UNTIL WS-TEMP = 0 OR INVALID-NUMBER
               DIVIDE WS-TEMP BY 10 GIVING WS-TEMP REMAINDER WS-DIGIT
               IF WS-DIGIT = 0 OR WS-DIGIT = 2 OR WS-DIGIT = 4
                   OR WS-DIGIT = 6 OR WS-DIGIT = 8
                   MOVE 'N' TO WS-VALID-FLAG
               END-IF
           END-PERFORM.

       SORT-RESULT.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-RESULT-COUNT - 1
               PERFORM VARYING WS-J FROM WS-I + 1 BY 1 UNTIL WS-J > WS-RESULT-COUNT
                   IF RESULT(WS-I) > RESULT(WS-J)
                       MOVE RESULT(WS-I) TO WS-TEMP
                       MOVE RESULT(WS-J) TO RESULT(WS-I)
                       MOVE WS-TEMP TO RESULT(WS-J)
                   END-IF
               END-PERFORM
           END-PERFORM.

       END PROGRAM UNIQUE-DIGITS.
