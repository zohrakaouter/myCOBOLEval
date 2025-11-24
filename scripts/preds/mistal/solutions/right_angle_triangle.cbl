       IDENTIFICATION DIVISION.
       PROGRAM-ID. RIGHT-ANGLE-TRIANGLE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-A-SQUARED PIC S9(20).
       01 WS-B-SQUARED PIC S9(20).
       01 WS-C-SQUARED PIC S9(20).
       01 WS-TEMP     PIC S9(20).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-A PIC S9(10).
           05 L-B PIC S9(10).
           05 L-C PIC S9(10).
           05 RESULT PIC 9.

      * '''
      * Given the lengths of the three sides of a triangle. Return True if the three
      * sides form a right-angled triangle, False otherwise.
      * A right-angled triangle is a triangle in which one angle is right angle or
      * 90 degree.
      * Example:
      * right_angle_triangle(3, 4, 5) == True
      * right_angle_triangle(1, 2, 3) == False
      * '''

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

           COMPUTE WS-A-SQUARED = L-A * L-A
           COMPUTE WS-B-SQUARED = L-B * L-B
           COMPUTE WS-C-SQUARED = L-C * L-C

           IF WS-A-SQUARED + WS-B-SQUARED = WS-C-SQUARED THEN
               MOVE 1 TO RESULT
           ELSE IF WS-A-SQUARED + WS-C-SQUARED = WS-B-SQUARED THEN
               MOVE 1 TO RESULT
           ELSE IF WS-B-SQUARED + WS-C-SQUARED = WS-A-SQUARED THEN
               MOVE 1 TO RESULT
           ELSE
               MOVE 0 TO RESULT
           END-IF

           GOBACK.
