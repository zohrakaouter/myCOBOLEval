       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALL-PREFIXES.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-LOOP-INDEX           PIC 9(3) VALUE 0.
       01 WS-STRING-LENGTH        PIC 9(3) VALUE 0.

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

       MAIN-PARAGRAPH.
           COMPUTE WS-STRING-LENGTH = FUNCTION LENGTH( L-STRING )
           PERFORM VARYING WS-LOOP-INDEX FROM 1 BY 1 UNTIL WS-LOOP-INDEX > WS-STRING-LENGTH
               STRING L-STRING(1:WS-LOOP-INDEX) DELIMITED BY SIZE INTO RESULT(WS-LOOP-INDEX)
           END-PERFORM

           GOBACK. 
           
       END PROGRAM ALL-PREFIXES.
