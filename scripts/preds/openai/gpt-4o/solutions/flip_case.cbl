       IDENTIFICATION DIVISION.
       PROGRAM-ID. FLIP-CASE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-INDEX PIC 9(3) VALUE 1.
       01 WS-CHAR PIC X.
       01 WS-CHAR-UPPER PIC X.
       01 WS-CHAR-LOWER PIC X.
       01 L-STRING-LEN PIC 9(3).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-STRING PIC X(100).
           05 RESULT PIC X(100).

      * For a given string, flip lowercase characters to uppercase and uppercase to lowercase.
      * >>> flip_case('Hello')
      * 'hELLO'
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

       MAIN-PROCEDURE.
           MOVE FUNCTION LENGTH(TRIM(L-STRING)) TO L-STRING-LEN
           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > L-STRING-LEN
               MOVE L-STRING(WS-INDEX:1) TO WS-CHAR
               IF WS-CHAR IS ALPHABETIC
                   IF WS-CHAR = FUNCTION LOWER-CASE(WS-CHAR)
                       COMPUTE WS-CHAR-UPPER = FUNCTION UPPER-CASE(WS-CHAR)
                       MOVE WS-CHAR-UPPER TO RESULT(WS-INDEX:1)
                   ELSE
                       COMPUTE WS-CHAR-LOWER = FUNCTION LOWER-CASE(WS-CHAR)
                       MOVE WS-CHAR-LOWER TO RESULT(WS-INDEX:1)
                   END-IF
               ELSE
                   MOVE WS-CHAR TO RESULT(WS-INDEX:1)
               END-IF
           END-PERFORM

           GOBACK.

       END PROGRAM FLIP-CASE.
