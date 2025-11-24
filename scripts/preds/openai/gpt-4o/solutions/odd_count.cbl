       IDENTIFICATION DIVISION.
       PROGRAM-ID. ODD-COUNT.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-STRLEN       PIC 9(4).
       01 WS-ODD-COUNT    PIC 9(4).
       01 WS-STRING       PIC X(100).
       01 WS-RESULT-STRING PIC X(100).
       
       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-LST OCCURS 2 TIMES INDEXED BY NI PIC X(2).
           05 RESULT OCCURS 100 TIMES INDEXED BY NJ PIC X(100).

      * Given a list of strings, where each string consists of only digits, return a list.
      * Each element i of the output should be "the number of odd elements in the
      * string i of the input." where all the i's should be replaced by the number
      * of odd digits in the i'th string of the input.
      * 
      * >>> odd_count(['3',"11111111"])
      * ["the number of odd elements 1n the str1ng 1 of the 1nput.",
      * "the number of odd elements 8n the str8ng 8 of the 8nput."]
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
       
       BEGIN.
           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > LENGTH OF L-LST
               MOVE L-LST(NI) TO WS-STRING
               MOVE FUNCTION LENGTH(WS-STRING) TO WS-STRLEN
               MOVE 0 TO WS-ODD-COUNT
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-STRLEN
                   IF WS-STRING(I:1) IS EQUAL TO "1"
                       OR WS-STRING(I:1) IS EQUAL TO "3"
                       OR WS-STRING(I:1) IS EQUAL TO "5"
                       OR WS-STRING(I:1) IS EQUAL TO "7"
                       OR WS-STRING(I:1) IS EQUAL TO "9"
                       ADD 1 TO WS-ODD-COUNT
                   END-IF
               END-PERFORM
               
               MOVE "the number of odd elements " TO WS-RESULT-STRING
               STRING FUNCTION NUMVAL-C WS-ODD-COUNT DELIMITED BY SIZE
                      "n the str" WS-STRING(1:1) DELIMITED BY SIZE
                      "ng " WS-STRING(1:1) DELIMITED BY SIZE
                      " of the " WS-STRING(1:1) DELIMITED BY SIZE
                      "nput." DELIMITED BY SIZE
                  INTO WS-RESULT-STRING
                  
               MOVE WS-RESULT-STRING TO RESULT(NI)
           END-PERFORM
           .
           
       GOBACK.
       END PROGRAM ODD-COUNT.
