       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALL-PREFIXES.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-COUNTERS.
           05 WS-I PIC 9(3) VALUE 0.
           05 WS-J PIC 9(3) VALUE 0.
           05 WS-LEN PIC 9(3) VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-STRING PIC X(100).
           05 RESULT OCCURS 100 TIMES INDEXED BY NI PIC X(100).

      * Return list of all prefixes from shortest to longest of the input string
      * >>> all_prefixes('abc')
      * ['a', 'ab', 'abc']
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL L-STRING(WS-I:1) = SPACE OR WS-I > 100
               CONTINUE
           END-PERFORM
           COMPUTE WS-LEN = WS-I - 1

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-LEN
               MOVE SPACES TO RESULT(WS-I)
               PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > WS-I
                   MOVE L-STRING(WS-J:1) TO RESULT(WS-I)(WS-J:1)
               END-PERFORM
           END-PERFORM

           GOBACK.
       END PROGRAM ALL-PREFIXES.
