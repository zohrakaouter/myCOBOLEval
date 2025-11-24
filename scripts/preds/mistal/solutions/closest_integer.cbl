       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLOSEST-INTEGER.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARIABLES.
           05 WS-NUMBER PIC S9(10)V9(10).
           05 WS-INTEGER-PART PIC S9(10).
           05 WS-FRACTIONAL-PART PIC 9(10).
           05 WS-ROUNDED-NUMBER PIC S9(10).
           05 WS-LEN PIC 9(4).
           05 WS-POS PIC 9(4).
           05 WS-SIGN PIC X.
               88 NEGATIVE VALUE '-'.
           05 WS-TEMP PIC X(20).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-VALUE COMP-2.
           05 RESULT PIC S9(10).

      * '''
      * Create a function that takes a value (string) representing a number
      * and returns the closest integer to it. If the number is equidistant
      * from two integers, round it away from zero.
      * 
      * Examples
      * >>> closest_integer("10")
      * 10
      * >>> closest_integer("15.3")
      * 15
      * 
      * Note:
      * Rounding away from zero means that if the given number is equidistant
      * from two integers, the one you should return is the one that is the
      * farthest from zero. For example closest_integer("14.5") should
      * return 15 and closest_integer("-14.5") should return -15.
      * '''

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           MOVE L-VALUE TO WS-TEMP
           INSPECT WS-TEMP TALLYING WS-LEN FOR CHARACTERS
           PERFORM VARYING WS-POS FROM 1 BY 1 UNTIL WS-POS > WS-LEN
               IF WS-TEMP(WS-POS:1) = '-'
                   SET NEGATIVE TO TRUE
                   MOVE WS-TEMP(WS-POS + 1:) TO WS-TEMP
                   SUBTRACT 1 FROM WS-LEN
               END-IF
               IF WS-TEMP(WS-POS:1) = '.'
                   MOVE WS-TEMP(1:WS-POS - 1) TO WS-INTEGER-PART
                   MOVE WS-TEMP(WS-POS + 1:WS-LEN - WS-POS) TO WS-FRACTIONAL-PART
                   EXIT PERFORM
               END-IF
           END-PERFORM

           IF WS-FRACTIONAL-PART = 0
               MOVE WS-INTEGER-PART TO WS-ROUNDED-NUMBER
           ELSE
               COMPUTE WS-NUMBER = FUNCTION NUMVAL(WS-TEMP)
               IF NEGATIVE
                   COMPUTE WS-ROUNDED-NUMBER = FUNCTION FLOOR(WS-NUMBER + 0.5)
               ELSE
                   COMPUTE WS-ROUNDED-NUMBER = FUNCTION CEILING(WS-NUMBER - 0.5)
               END-IF
           END-IF

           MOVE WS-ROUNDED-NUMBER TO RESULT
           GOBACK.
       END PROGRAM CLOSEST-INTEGER.
