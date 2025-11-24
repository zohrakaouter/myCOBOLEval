       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEPARATE-PAREN-GROUPS.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 TEMP-STRING PIC X(100).
       01 TEMP-CHAR   PIC X.
       01 OPEN-COUNT  PIC 9(03) VALUE 0.
       01 START-IDX   PIC 9(03) VALUE 1.
       01 END-IDX     PIC 9(03) VALUE 1.
       01 RESULT-COUNTER  PIC 9(03) VALUE ZERO.

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

           PERFORM PROCESS-PAREN-GROUPS

           GOBACK.
       
       PROCESS-PAREN-GROUPS.
           MOVE FUNCTION TRIM(L-PAREN-STRING) TO TEMP-STRING.
           PERFORM VARYING END-IDX FROM 1 BY 1
                   UNTIL END-IDX > FUNCTION LENGTH(TEMP-STRING)
               MOVE TEMP-STRING(END-IDX:1) TO TEMP-CHAR
               EVALUATE TEMP-CHAR
                   WHEN '('
                       ADD 1 TO OPEN-COUNT
                       IF OPEN-COUNT = 1
                           MOVE END-IDX TO START-IDX
                       END-IF
                   WHEN ')'
                       SUBTRACT 1 FROM OPEN-COUNT
                       IF OPEN-COUNT = 0
                           ADD 1 TO RESULT-COUNTER
                           MOVE TEMP-STRING(START-IDX:END-IDX-START-IDX+1) 
                               TO RESULT(RESULT-COUNTER)
                       END-IF
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-PERFORM.
           .

       END PROGRAM SEPARATE-PAREN-GROUPS.
