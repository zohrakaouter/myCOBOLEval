       IDENTIFICATION DIVISION.
       PROGRAM-ID. RIGHT-ANGLE-TRIANGLE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 A-SQUARE PIC S9(10).
       01 B-SQUARE PIC S9(10).
       01 C-SQUARE PIC S9(10).

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

       MAIN-PROCEDURE.
           MOVE L-A TO A-SQUARE
           COMPUTE A-SQUARE = L-A * L-A
           MOVE L-B TO B-SQUARE
           COMPUTE B-SQUARE = L-B * L-B
           MOVE L-C TO C-SQUARE
           COMPUTE C-SQUARE = L-C * L-C

       CHECK-TRIANGLE.
           IF (A-SQUARE = B-SQUARE + C-SQUARE) OR
              (B-SQUARE = A-SQUARE + C-SQUARE) OR
              (C-SQUARE = A-SQUARE + B-SQUARE)
               MOVE 1 TO RESULT
           ELSE
               MOVE 0 TO RESULT
           END-IF

       END-MAIN.
           GOBACK.
       END PROGRAM RIGHT-ANGLE-TRIANGLE.
