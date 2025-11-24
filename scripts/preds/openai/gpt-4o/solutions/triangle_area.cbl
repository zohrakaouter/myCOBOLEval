       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRIANGLE-AREA.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01  WS-A                  PIC S9(10) COMP-2.
       01  WS-B                  PIC S9(10) COMP-2.
       01  WS-C                  PIC S9(10) COMP-2.
       01  WS-S                  PIC S9(10) COMP-2.
       01  WS-AREA               PIC S9(10) COMP-2.
       01  WS-VALID-TRIANGLE     PIC X VALUE 'N'.
       
       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-A PIC S9(10).
           05 L-B PIC S9(10).
           05 L-C PIC S9(10).
           05 RESULT COMP-2.

      * '''
      * Given the lengths of the three sides of a triangle. Return the area of
      * the triangle rounded to 2 decimal points if the three sides form a valid triangle.
      * Otherwise return -1
      * Three sides make a valid triangle when the sum of any two sides is greater
      * than the third side.
      * Example:
      * triangle_area(3, 4, 5) == 6.00
      * triangle_area(1, 2, 10) == -1
      * '''

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

       MAIN-PARA.
           MOVE L-A TO WS-A
           MOVE L-B TO WS-B
           MOVE L-C TO WS-C
           
           * Check if the sides form a valid triangle
           IF WS-A + WS-B > WS-C
               IF WS-A + WS-C > WS-B
                   IF WS-B + WS-C > WS-A
                       SET WS-VALID-TRIANGLE TO 'Y'
                   END-IF
               END-IF
           END-IF
           
           IF WS-VALID-TRIANGLE = 'Y'
               * Calculate the semi-perimeter
               COMPUTE WS-S = (WS-A + WS-B + WS-C) / 2

               * Calculate the area using Heron's formula
               COMPUTE WS-AREA = FUNCTION SQRT(WS-S * 
                                (WS-S - WS-A) *
                                (WS-S - WS-B) *
                                (WS-S - WS-C))

               * Round area to two decimal places
               COMPUTE RESULT = FUNCTION NUMVAL-C (FUNCTION NUMVAL(WS-AREA:6:2))
           ELSE
               MOVE -1 TO RESULT
           END-IF

       GOBACK.

       END PROGRAM TRIANGLE-AREA.
