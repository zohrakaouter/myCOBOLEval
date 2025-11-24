       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILTER-BY-SUBSTRING.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-COUNT PIC 9(3) VALUE 0.
       01 WS-I PIC 9(3).
       01 WS-J PIC 9(3).
       01 WS-SUBSTRING-LEN PIC 9(3).
       01 WS-STRING-LEN PIC 9(3).
       01 WS-FOUND-FLAG PIC X VALUE 'N'.
           88 FOUND VALUE 'Y'.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-STRINGS OCCURS 3 TIMES INDEXED BY NI PIC X(3).
           05 L-SUBSTRING PIC X(100).
           05 RESULT OCCURS 100 TIMES INDEXED BY NJ PIC X(100).

      * Filter an input list of strings only for ones that contain given substring
      * >>> filter_by_substring(['abc', 'b', 'c'], 'a')
      * ['abc']
      * >>> filter_by_substring(['bacd', 'cde', 'array'], 'a')
      * ['bacd', 'array']
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

           COMPUTE WS-SUBSTRING-LEN = FUNCTION LENGTH(FUNCTION TRIM(L-SUBSTRING))

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3
               MOVE 'N' TO WS-FOUND-FLAG
               COMPUTE WS-STRING-LEN = FUNCTION LENGTH(FUNCTION TRIM(L-STRINGS(WS-I)))

               PERFORM VARYING WS-J FROM 1 BY 1
                   UNTIL WS-J > WS-STRING-LEN OR FOUND
                   IF L-STRINGS(WS-I)(WS-J:WS-SUBSTRING-LEN) =
                      L-SUBSTRING(1:WS-SUBSTRING-LEN)
                      MOVE 'Y' TO WS-FOUND-FLAG
                   END-IF
               END-PERFORM

               IF FOUND
                   ADD 1 TO WS-COUNT
                   MOVE L-STRINGS(WS-I) TO RESULT(WS-COUNT)
               END-IF
           END-PERFORM

           GOBACK.
       END PROGRAM FILTER-BY-SUBSTRING.
