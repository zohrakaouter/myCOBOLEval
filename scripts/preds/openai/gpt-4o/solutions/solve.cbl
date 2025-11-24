       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOLVE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-STRING LENGTH OF L-S PIC 9(3).
       01 WS-INDEX PIC 9(3) VALUE 1.
       01 WS-HAS-LETTER PIC X VALUE 'N'.
       01 WS-CHAR PIC X.
       01 WS-REVERSED PIC X(100).
       01 WS-RESULT PIC X(100).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-S PIC X(100).
           05 RESULT PIC X(100).

      * You are given a string s.
      * if s[i] is a letter, reverse its case from lower to upper or vise versa,
      * otherwise keep it as it is.
      * If the string contains no letters, reverse the string.
      * The function should return the resulted string.
      * Examples
      * solve("1234") = "4321"
      * solve("ab") = "AB"
      * solve("#a@C") = "#A@c"
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           MOVE FUNCTION LENGTH(L-S) TO WS-STRING.
           MOVE SPACES TO WS-REVERSED.
           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > WS-STRING
               MOVE L-S(WS-INDEX:1) TO WS-CHAR
               EVALUATE TRUE
                   WHEN WS-CHAR IS ALPHABETIC
                       MOVE 'Y' TO WS-HAS-LETTER
                       IF WS-CHAR IS LOWER-CASE
                           INSPECT FUNCTION CONVERT(WS-CHAR) 
                               CONVERTING 'abcdefghijklmnopqrstuvwxyz' 
                               TO 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
                               INTO WS-CHAR
                       ELSE
                           INSPECT FUNCTION CONVERT(WS-CHAR) 
                               CONVERTING 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' 
                               TO 'abcdefghijklmnopqrstuvwxyz'
                               INTO WS-CHAR
                       END-IF
                       STRING WS-REVERSED DELIMITED BY SPACE
                              WS-CHAR DELIMITED BY SIZE
                              INTO WS-REVERSED
                   WHEN OTHER
                       STRING WS-REVERSED DELIMITED BY SPACE
                              WS-CHAR DELIMITED BY SIZE
                              INTO WS-REVERSED
               END-EVALUATE
           END-PERFORM
           
           IF WS-HAS-LETTER = 'N'
               INSPECT WS-REVERSED REPLACING CHARACTERS BY CHARACTERS
                   FROM WS-STRING BY 1 FOR WS-STRING IN REVERSE
                   INTO WS-REVERSED
           END-IF
           
           MOVE WS-REVERSED TO RESULT.

           GOBACK.
       END PROGRAM SOLVE.
