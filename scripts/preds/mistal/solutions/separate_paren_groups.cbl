       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEPARATE-PAREN-GROUPS.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARIABLES.
           05 WS-INDEX            PIC 9(3) VALUE 0.
           05 WS-LENGTH           PIC 9(3) VALUE 0.
           05 WS-OPEN-COUNT       PIC 9(3) VALUE 0.
           05 WS-CURRENT-POS      PIC 9(3) VALUE 0.
           05 WS-START-POS        PIC 9(3) VALUE 0.
           05 WS-RESULT-INDEX     PIC 9(3) VALUE 0.
           05 WS-TEMP-STRING      PIC X(100) VALUE SPACES.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-PAREN-STRING PIC X(100).
           05 RESULT OCCURS 100 TIMES INDEXED BY NI PIC X(100).

      * Input to this function is a string containing multiple groups of nested parentheses. Your goal is to
      * separate those group into separate strings and return the list of those.
      * Separate groups are balanced (each open brace is properly closed) and not nested within each other
      * Ignore any spaces in the input string.
      * >>> separate_paren_groups('( ) (( )) (( )( ))')
      * ['()', '(())', '(()())']
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > LENGTH OF L-PAREN-STRING
               IF L-PAREN-STRING(WS-INDEX:1) NOT EQUAL SPACE
                   MOVE L-PAREN-STRING(WS-INDEX:1)
                     TO WS-TEMP-STRING(WS-CURRENT-POS:1)
                   ADD 1 TO WS-CURRENT-POS
                   IF L-PAREN-STRING(WS-INDEX:1) EQUAL '('
                       ADD 1 TO WS-OPEN-COUNT
                       IF WS-OPEN-COUNT EQUAL 1
                           MOVE WS-INDEX TO WS-START-POS
                       END-IF
                   ELSE
                       IF L-PAREN-STRING(WS-INDEX:1) EQUAL ')'
                           SUBTRACT 1 FROM WS-OPEN-COUNT
                           IF WS-OPEN-COUNT EQUAL 0
                               MOVE WS-TEMP-STRING(1:WS-CURRENT-POS - 1)
                                 TO RESULT(WS-RESULT-INDEX + 1)
                               ADD 1 TO WS-RESULT-INDEX
                               MOVE SPACES TO WS-TEMP-STRING
                               MOVE 0 TO WS-CURRENT-POS
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.

           GOBACK.
       END PROGRAM SEPARATE-PAREN-GROUPS.
