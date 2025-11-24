       IDENTIFICATION DIVISION.
       PROGRAM-ID. HOW-MANY-TIMES.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01  WS-INDEX          PIC 9(03) VALUE 0.
       01  WS-STRING-LENGTH  PIC 9(03) VALUE 0.
       01  WS-SUBSTR-LENGTH  PIC 9(03) VALUE 0.
       01  WS-COUNTER        PIC 9(03) VALUE 0.
       01  WS-END-INDEX      PIC 9(03) VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-STRING PIC X(100).
           05 L-SUBSTRING PIC X(100).
           05 RESULT PIC S9(10).

      * Find how many times a given substring can be found in the original string. Count overlaping cases.
      * >>> how_many_times('', 'a')
      * 0
      * >>> how_many_times('aaa', 'a')
      * 3
      * >>> how_many_times('aaaa', 'aa')
      * 3
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           MOVE ZERO TO RESULT

           COMPUTE WS-STRING-LENGTH = FUNCTION LENGTH(TRIM(L-STRING))
           COMPUTE WS-SUBSTR-LENGTH = FUNCTION LENGTH(TRIM(L-SUBSTRING))

           IF WS-SUBSTR-LENGTH = 0
               MOVE ZERO TO RESULT
           ELSE
               PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > (WS-STRING-LENGTH - WS-SUBSTR-LENGTH + 1)
                   IF FUNCTION INSPECT(L-STRING(WS-INDEX:WS-SUBSTR-LENGTH))
                       = L-SUBSTRING
                       ADD 1 TO WS-COUNTER
                   END-IF
               END-PERFORM
               MOVE WS-COUNTER TO RESULT
           END-IF

           GOBACK
           .

       END PROGRAM HOW-MANY-TIMES.
