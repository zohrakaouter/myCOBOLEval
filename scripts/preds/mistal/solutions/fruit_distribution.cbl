       IDENTIFICATION DIVISION.
       PROGRAM-ID. FRUIT-DISTRIBUTION.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARIABLES.
           05 WS-APPLES-COUNT PIC 9(10).
           05 WS-ORANGES-COUNT PIC 9(10).
           05 WS-TOTAL-FRUITS PIC S9(10).
           05 WS-TEMP-STRING PIC X(100).
           05 WS-POS PIC 9(3).
           05 WS-LEN PIC 9(3).
           05 WS-I PIC 9(3).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-S PIC X(100).
           05 L-N PIC S9(10).
           05 RESULT PIC S9(10).

      * 
      * In this task, you will be given a string that represents a number of apples and oranges
      * that are distributed in a basket of fruit this basket contains
      * apples, oranges, and mango fruits. Given the string that represents the total number of
      * the oranges and apples and an integer that represent the total number of the fruits
      * in the basket return the number of the mango fruits in the basket.
      * for examble:
      * fruit_distribution("5 apples and 6 oranges", 19) ->19 - 5 - 6 = 8
      * fruit_distribution("0 apples and 1 oranges",3) -> 3 - 0 - 1 = 2
      * fruit_distribution("2 apples and 3 oranges", 100) -> 100 - 2 - 3 = 95
      * fruit_distribution("100 apples and 1 oranges",120) -> 120 - 100 - 1 = 19
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           MOVE L-S TO WS-TEMP-STRING
           MOVE L-N TO WS-TOTAL-FRUITS

           * Extract apples count
           INSPECT WS-TEMP-STRING TALLYING WS-POS FOR CHARACTERS
               BEFORE INITIAL ' '
           MOVE WS-TEMP-STRING(1:WS-POS) TO WS-APPLES-COUNT

           * Find position of 'and'
           MOVE 0 TO WS-POS
           INSPECT WS-TEMP-STRING TALLYING WS-POS FOR CHARACTERS
               BEFORE INITIAL ' and '

           * Extract oranges count
           COMPUTE WS-LEN = WS-POS + 5
           MOVE WS-TEMP-STRING(WS-LEN:3) TO WS-I
           COMPUTE WS-POS = WS-POS + 5 + WS-I + 1
           MOVE WS-TEMP-STRING(WS-LEN + WS-I + 1:WS-POS - (WS-LEN + WS-I)) TO WS-ORANGES-COUNT

           * Calculate mango count
           COMPUTE RESULT = WS-TOTAL-FRUITS - WS-APPLES-COUNT - WS-ORANGES-COUNT

           GOBACK.
       END PROGRAM FRUIT-DISTRIBUTION.
