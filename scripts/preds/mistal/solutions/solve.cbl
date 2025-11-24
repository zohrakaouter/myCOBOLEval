       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOLVE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARIABLES.
           05 WS-I PIC 9(3) VALUE 0.
           05 WS-LEN PIC 9(3) VALUE 0.
           05 WS-HAS-LETTER PIC X VALUE 'N'.
               88 HAS-LETTER VALUE 'Y'.
               88 NO-LETTERS VALUE 'N'.
           05 WS-CHAR PIC X.
           05 WS-TEMP-STR PIC X(100) VALUE SPACES.
           05 WS-TEMP-LEN PIC 9(3) VALUE 0.

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
           MAIN-PARA.
               PERFORM INITIALIZE-VARS
               PERFORM PROCESS-STRING
               PERFORM FINALIZE-RESULT
               GOBACK.

           INITIALIZE-VARS.
               INSPECT L-S TALLYING WS-LEN FOR CHARACTERS
               MOVE SPACES TO WS-TEMP-STR
               MOVE 0 TO WS-TEMP-LEN.

           PROCESS-STRING.
               PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-LEN
                   MOVE L-S(WS-I:1) TO WS-CHAR
                   IF WS-CHAR IS ALPHABETIC
                       SET HAS-LETTER TO TRUE
                       IF WS-CHAR IS ALPHABETIC-LOWER
                           MOVE FUNCTION UPPER-CASE(WS-CHAR) TO WS-CHAR
                       ELSE
                           MOVE FUNCTION LOWER-CASE(WS-CHAR) TO WS-CHAR
                       END-IF
                   END-IF
                   STRING WS-CHAR DELIMITED BY SIZE INTO WS-TEMP-STR
                       WITH POINTER WS-TEMP-LEN
               END-PERFORM.

           FINALIZE-RESULT.
               IF NO-LETTERS
                   PERFORM REVERSE-STRING
               ELSE
                   MOVE WS-TEMP-STR TO RESULT
               END-IF.

           REVERSE-STRING.
               MOVE SPACES TO RESULT
               PERFORM VARYING WS-I FROM WS-LEN BY -1 UNTIL WS-I < 1
                   MOVE L-S(WS-I:1) TO WS-CHAR
                   STRING WS-CHAR DELIMITED BY SIZE INTO RESULT
                       WITH POINTER WS-TEMP-LEN
               END-PERFORM.

       END PROGRAM SOLVE.
