       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILTER-BY-SUBSTRING.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-SUBSTRINGS.
           05 WS-TEMP-STRING PIC X(100).
           05 WS-COMP-INDEX PIC 9(3) VALUE 1.
           05 WS-RESULT-INDEX PIC 9(3) VALUE 1.

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

       MAIN-LOGIC SECTION.

           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 3
               SET WS-COMP-INDEX TO 1
               UNSTRING L-STRINGS(NI) DELIMITED BY SPACE
                       INTO WS-TEMP-STRING
               WHEN L-SUBSTRING IS INITIAL
                   EXIT PERFORM
               END-UNSTRING
            
               IF FUNCTION POS(L-SUBSTRING IN WS-TEMP-STRING) > 0
                   MOVE WS-TEMP-STRING TO RESULT(WS-RESULT-INDEX)
                   ADD 1 TO WS-RESULT-INDEX
               END-IF
           END-PERFORM

       GOBACK.
       END PROGRAM FILTER-BY-SUBSTRING.
