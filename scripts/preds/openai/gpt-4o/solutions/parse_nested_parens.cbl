       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARSE-NESTED-PARENS.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-STRING-COPY PIC X(100).
       01 DEPTH-VAR.
           05 MAX-DEPTH    PIC S9(4) VALUE 0.
           05 CURRENT-DEPTH PIC S9(4) VALUE 0.
       01 INDEXES.
           05 START-INDEX   PIC S9(4) VALUE 1.
           05 END-INDEX     PIC S9(4) VALUE 1.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-PAREN-STRING PIC X(100).
           05 RESULT OCCURS 100 TIMES INDEXED BY NI PIC S9(10).

      * Input to this function is a string represented multiple groups for nested parentheses separated by spaces.
      * For each of the group, output the deepest level of nesting of parentheses.
      * E.g. (()()) has maximum two levels of nesting while ((())) has three.
      * 
      * >>> parse_nested_parens('(()()) ((())) () ((())()())')
      * [2, 3, 1, 3]
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
       
           PERFORM VARYING START-INDEX FROM 1 BY 1
               UNTIL START-INDEX > FUNCTION LENGTH(L-PAREN-STRING)
               
               EVALUATE L-PAREN-STRING(START-INDEX:1)
               
                   WHEN '('
                       COMPUTE CURRENT-DEPTH = CURRENT-DEPTH + 1
                       IF CURRENT-DEPTH > MAX-DEPTH
                           SET MAX-DEPTH TO CURRENT-DEPTH
                       END-IF

                   WHEN ')'
                       COMPUTE CURRENT-DEPTH = CURRENT-DEPTH - 1

                   WHEN ' '
                       IF CURRENT-DEPTH = 0
                           SET END-INDEX TO END-INDEX + 1
                           MOVE MAX-DEPTH TO RESULT (END-INDEX)
                           PERFORM RESET-DEPTH
                       END-IF
                       
               END-EVALUATE

           END-PERFORM

           IF CURRENT-DEPTH = 0 AND START-INDEX > FUNCTION LENGTH(L-PAREN-STRING)
               SET END-INDEX TO END-INDEX + 1
               MOVE MAX-DEPTH TO RESULT (END-INDEX)
           END-IF

           GOBACK.

       RESET-DEPTH SECTION.
           COMPUTE MAX-DEPTH = 0
           COMPUTE CURRENT-DEPTH = 0
           EXIT.
           
       END PROGRAM PARSE-NESTED-PARENS.
