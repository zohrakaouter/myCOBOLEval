       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRIANGLE-AREA.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARIABLES.
           05 WS-S PIC S9(10)V99 COMP-3.
           05 WS-AREA PIC S9(10)V99 COMP-3.
           05 WS-TEMP PIC S9(10)V99 COMP-3.

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

           IF L-A <= 0 OR L-B <= 0 OR L-C <= 0
               MOVE -1 TO RESULT
               GOBACK
           END-IF

           IF (L-A + L-B) <= L-C OR
              (L-A + L-C) <= L-B OR
              (L-B + L-C) <= L-A
               MOVE -1 TO RESULT
               GOBACK
           END-IF

           COMPUTE WS-S = (L-A + L-B + L-C) / 2
           COMPUTE WS-TEMP = WS-S * (WS-S - L-A) * (WS-S - L-B) * (WS-S - L-C)
           COMPUTE WS-AREA = FUNCTION SQRT(WS-TEMP)
           COMPUTE RESULT = FUNCTION NUMVAL-C(FUNCTION NUMVAL(WS-AREA) ":9.99")

           GOBACK.
