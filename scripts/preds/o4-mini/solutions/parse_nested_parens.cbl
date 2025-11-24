       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARSE-NESTED-PARENS.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARIABLES.
           05 WS-GROUP-COUNT          PIC 9(3) VALUE 0.
           05 WS-GROUP-LEN            PIC 9(3) VALUE 0.
           05 WS-GROUP-START          PIC 9(3) VALUE 0.
           05 WS-GROUP-END            PIC 9(3) VALUE 0.
           05 WS-CURRENT-DEPTH        PIC S9(3) VALUE 0.
           05 WS-MAX-DEPTH            PIC S9(3) VALUE 0.
           05 WS-I                   PIC 9(3) VALUE 0.
           05 WS-CHAR                 PIC X VALUE SPACE.

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
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 100
               MOVE 0 TO RESULT(WS-I)
           END-PERFORM

           PERFORM VARYING WS-GROUP-START FROM 1 BY 1
               UNTIL WS-GROUP-START > FUNCTION LENGTH(FUNCTION TRIM(L-PAREN-STRING))
                   MOVE SPACE TO WS-CHAR
                   INSPECT FUNCTION TRIM(L-PAREN-STRING)
                       TALLYING WS-GROUP-LEN FOR CHARACTERS
                       BEFORE INITIAL SPACE
                   IF WS-GROUP-LEN = 0
                       EXIT PERFORM
                   END-IF

                   MOVE 0 TO WS-CURRENT-DEPTH
                   MOVE 0 TO WS-MAX-DEPTH
                   PERFORM VARYING WS-I FROM WS-GROUP-START BY 1
                       UNTIL WS-I > WS-GROUP-START + WS-GROUP-LEN - 1
                           OR L-PAREN-STRING(WS-I:1) = SPACE
                       MOVE L-PAREN-STRING(WS-I:1) TO WS-CHAR
                       IF WS-CHAR = '('
                           ADD 1 TO WS-CURRENT-DEPTH
                           IF WS-CURRENT-DEPTH > WS-MAX-DEPTH
                               MOVE WS-CURRENT-DEPTH TO WS-MAX-DEPTH
                           END-IF
                       ELSE IF WS-CHAR = ')'
                           SUBTRACT 1 FROM WS-CURRENT-DEPTH
                       END-IF
                   END-PERFORM

                   ADD 1 TO WS-GROUP-COUNT
                   MOVE WS-MAX-DEPTH TO RESULT(WS-GROUP-COUNT)
                   ADD WS-GROUP-LEN TO WS-GROUP-START
                   ADD 1 TO WS-GROUP-START
           END-PERFORM

           GOBACK.
       END PROGRAM PARSE-NESTED-PARENS.
