       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIX-SPACES.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARS.
           05 WS-I PIC 9(3) VALUE 0.
           05 WS-O PIC 9(3) VALUE 0.
           05 WS-SPACE-COUNT PIC 9(3) VALUE 0.
           05 WS-CURRENT-CHAR PIC X.
           05 WS-REPLACE-CHAR PIC X.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-TEXT PIC X(100).
           05 RESULT PIC X(100).

      * 
      * Given a string text, replace all spaces in it with underscores,
      * and if a string has more than 2 consecutive spaces,
      * then replace all consecutive spaces with -
      * 
      * fix_spaces("Example") == "Example"
      * fix_spaces("Example 1") == "Example_1"
      * fix_spaces(" Example 2") == "_Example_2"
      * fix_spaces(" Example   3") == "_Example-3"
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 100
               MOVE L-TEXT(WS-I:1) TO WS-CURRENT-CHAR

               IF WS-CURRENT-CHAR = SPACE
                   ADD 1 TO WS-SPACE-COUNT
                   IF WS-SPACE-COUNT > 2
                       MOVE '-' TO WS-REPLACE-CHAR
                   ELSE
                       MOVE '_' TO WS-REPLACE-CHAR
                   END-IF
               ELSE
                   IF WS-SPACE-COUNT > 0
                       IF WS-SPACE-COUNT > 2
                           MOVE '-' TO RESULT(WS-O:1)
                           ADD 1 TO WS-O
                       ELSE
                           PERFORM VARYING WS-J FROM 1 BY 1
                                   UNTIL WS-J > WS-SPACE-COUNT
                               MOVE '_' TO RESULT(WS-O:1)
                               ADD 1 TO WS-O
                           END-PERFORM
                       END-IF
                       MOVE 0 TO WS-SPACE-COUNT
                   END-IF
                   MOVE WS-CURRENT-CHAR TO RESULT(WS-O:1)
                   ADD 1 TO WS-O
               END-IF
           END-PERFORM

           IF WS-SPACE-COUNT > 0
               IF WS-SPACE-COUNT > 2
                   MOVE '-' TO RESULT(WS-O:1)
                   ADD 1 TO WS-O
               ELSE
                   PERFORM VARYING WS-J FROM 1 BY 1
                           UNTIL WS-J > WS-SPACE-COUNT
                       MOVE '_' TO RESULT(WS-O:1)
                       ADD 1 TO WS-O
                   END-PERFORM
               END-IF
           END-IF

           GOBACK.
       END PROGRAM FIX-SPACES.
