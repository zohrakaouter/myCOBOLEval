       IDENTIFICATION DIVISION.
       PROGRAM-ID. ODD-COUNT.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-COUNTERS.
           05 WS-I PIC 9(3) VALUE 0.
           05 WS-J PIC 9(3) VALUE 0.
           05 WS-ODD-COUNT PIC 9(3) VALUE 0.
           05 WS-CHAR PIC X(1).
           05 WS-NUMBER PIC 9(1).
       01 WS-TEMPLATE.
           05 WS-TEMPLATE-TEXT PIC X(100) VALUE
              "the number of odd elements 0n the str0ng 0 of the 0nput.".

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
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 2
               MOVE 0 TO WS-ODD-COUNT
               PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 2
                   MOVE L-LST(WS-I)(WS-J:1) TO WS-CHAR
                   IF WS-CHAR IS NUMERIC THEN
                       MOVE FUNCTION NUMVAL(WS-CHAR) TO WS-NUMBER
                       IF FUNCTION MOD(WS-NUMBER, 2) NOT EQUAL TO 0 THEN
                           ADD 1 TO WS-ODD-COUNT
                       END-IF
                   END-IF
               END-PERFORM

               MOVE WS-TEMPLATE-TEXT TO RESULT(WS-I)
               INSPECT RESULT(WS-I) REPLACING ALL '0' BY FUNCTION CHAR(WS-ODD-COUNT + 48)
           END-PERFORM.

       GOBACK.
       END PROGRAM ODD-COUNT.
