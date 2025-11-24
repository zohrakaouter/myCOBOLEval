       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLOSEST-INTEGER.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 NUM-STR PIC X(20).
       01 NUM-FLOAT COMP-2.
       01 INTEGER-PART PIC S9(10).

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
           MOVE FUNCTION NUMVAL-C(L-VALUE) TO NUM-FLOAT
           COMPUTE INTEGER-PART = FUNCTION INTEGER(NUM-FLOAT)
           
           IF NUM-FLOAT > 0 THEN
               IF NUM-FLOAT = INTEGER-PART THEN
                   MOVE INTEGER-PART TO RESULT
               ELSE
                   IF NUM-FLOAT > INTEGER-PART + 0.5 THEN
                       ADD 1 TO INTEGER-PART
                       MOVE INTEGER-PART TO RESULT
                   ELSE
                       MOVE INTEGER-PART TO RESULT
                   END-IF
               END-IF
           ELSE
               IF NUM-FLOAT = INTEGER-PART THEN
                   MOVE INTEGER-PART TO RESULT
               ELSE
                   IF NUM-FLOAT < INTEGER-PART - 0.5 THEN
                       SUBTRACT 1 FROM INTEGER-PART
                       MOVE INTEGER-PART TO RESULT
                   ELSE
                       MOVE INTEGER-PART TO RESULT
                   END-IF
               END-IF
           END-IF.

           GOBACK.
       END PROGRAM CLOSEST-INTEGER.
