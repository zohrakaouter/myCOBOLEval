       IDENTIFICATION DIVISION.
       PROGRAM-ID. VALID-DATE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-MONTH        PIC 99.
       01 WS-DAY          PIC 99.
       01 WS-YEAR         PIC 9999.
       01 WS-MAX-DAY      PIC 99.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-DATE PIC X(100).
           05 RESULT PIC 9.

      * You have to write a function which validates a given date string and
      * returns True if the date is valid otherwise False.
      * The date is valid if all of the following rules are satisfied:
      * 1. The date string is not empty.
      * 2. The number of days is not less than 1 or higher than 31 days for months 1,3,5,7,8,10,12. And the number of days is not less than 1 or higher than 30 days for months 4,6,9,11. And, the number of days is not less than 1 or higher than 29 for the month 2.
      * 3. The months should not be less than 1 or higher than 12.
      * 4. The date should be in the format: mm-dd-yyyy
      * 
      * for example:
      * valid_date('03-11-2000') => True
      * 
      * valid_date('15-01-2012') => False
      * 
      * valid_date('04-0-2040') => False
      * 
      * valid_date('06-04-2020') => True
      * 
      * valid_date('06/04/2020') => False
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

       BEGIN.

           MOVE 0 TO RESULT

           IF L-DATE IS EQUAL TO SPACES OR L-DATE IS EQUAL TO LOW-VALUES THEN
               EXIT PROGRAM
           END-IF

           UNSTRING L-DATE DELIMITED BY '-'
               INTO WS-MONTH, WS-DAY, WS-YEAR
           END-UNSTRING

           IF WS-MONTH < 1 OR WS-MONTH > 12 THEN
               EXIT PROGRAM
           END-IF

           EVALUATE WS-MONTH
               WHEN 1, 3, 5, 7, 8, 10, 12
                   MOVE 31 TO WS-MAX-DAY
               WHEN 4, 6, 9, 11
                   MOVE 30 TO WS-MAX-DAY
               WHEN 2
                   MOVE 29 TO WS-MAX-DAY
               WHEN OTHER 
                   MOVE 0 TO WS-MAX-DAY
           END-EVALUATE

           IF WS-DAY < 1 OR WS-DAY > WS-MAX-DAY THEN
               EXIT PROGRAM
           END-IF

           IF FUNCTION LENGTH(L-DATE) NOT = 10 THEN
               EXIT PROGRAM
           END-IF

           IF FUNCTION NUMVAL(WS-MONTH) = 0 OR FUNCTION NUMVAL(WS-DAY) = 0 OR FUNCTION NUMVAL(WS-YEAR) = 0 THEN
               EXIT PROGRAM
           END-IF

           IF L-DATE(3:1) NOT = '-' OR L-DATE(6:1) NOT = '-' THEN
               EXIT PROGRAM
           END-IF

           MOVE 1 TO RESULT

       END-PROCEDURE.

       GOBACK.
END PROGRAM VALID-DATE.
