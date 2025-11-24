       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILTER-BY-PREFIX.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-INDEX PIC 9(2) VALUE 1.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-STRINGS OCCURS 4 TIMES INDEXED BY NI PIC X(4).
           05 L-PREFIX PIC X(100).
           05 RESULT OCCURS 100 TIMES INDEXED BY NJ PIC X(100).

      * Filter an input list of strings only for ones that start with a given prefix.
      * >>> filter_by_prefix(['abc', 'bcd', 'cde', 'array'], 'ab')
      * ['abc']
      * >>> filter_by_prefix(['abc', 'bcd', 'cde', 'array'], 'a')
      * ['abc', 'array']
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

       BEGIN-PROGRAM.
           MOVE 1 TO NI.
           MOVE 1 TO NJ.
           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 4
               IF L-STRINGS(NI:2) = L-PREFIX(1:2)
                   MOVE L-STRINGS(NI) TO RESULT(NJ)
                   ADD 1 TO NJ
               END-IF
           END-PERFORM
           DISPLAY "Filtered results with prefix " L-PREFIX ":"
           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > NJ - 1
               DISPLAY RESULT(WS-INDEX)
           END-PERFORM
           .
           
       GOBACK.
       END PROGRAM FILTER-BY-PREFIX.
