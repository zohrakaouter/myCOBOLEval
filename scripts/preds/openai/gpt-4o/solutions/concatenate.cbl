       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONCATENATE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-INDEX PIC 9(3) VALUE 0.
       01 WS-TEMP-STRING PIC X(3) VALUE SPACE.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-STRINGS OCCURS 3 TIMES INDEXED BY NI PIC X(3).
           05 RESULT PIC X(100).

      * Concatenate list of strings into a single string
      * >>> concatenate(['b', 'c', 'd'])
      * 'bcd'
      * >>> concatenate(['a', 'b', 'c'])
      * 'abc'
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 3
               STRING L-STRINGS(NI) DELIMITED BY SPACE INTO WS-TEMP-STRING
               STRING RESULT DELIMITED BY SPACE,
                      WS-TEMP-STRING DELIMITED BY SPACE
                      INTO RESULT
           END-PERFORM
           GOBACK.
       END PROGRAM CONCATENATE.
