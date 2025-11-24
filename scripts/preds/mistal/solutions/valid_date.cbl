       IDENTIFICATION DIVISION.
       PROGRAM-ID. VALID-DATE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-DATE-STRUCTURE.
           05 WS-MONTH PIC 99.
           05 WS-DAY PIC 99.
           05 WS-YEAR PIC 9(4).
           05 WS-LEAP-YEAR-FLAG PIC 9 VALUE 0.
               88 LEAP-YEAR VALUE 1.
               88 NOT-LEAP-YEAR VALUE 0.
       01 WS-COUNTERS.
           05 WS-I PIC 99.
       01 WS-TEMP.
           05 WS-TEMP-DATE PIC X(10).
           05 WS-TEMP-MONTH PIC 99.
           05 WS-TEMP-DAY PIC 99.
           05 WS-TEMP-YEAR PIC 9(4).
       01 WS-FLAGS.
           05 WS-VALID-FORMAT PIC 9 VALUE 1.
               88 VALID-FORMAT VALUE 1.
               88 INVALID-FORMAT VALUE 0.
           05 WS-VALID-DATE PIC 9 VALUE 1.
               88 VALID-DATE VALUE 1.
               88 INVALID-DATE VALUE 0.

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
           MOVE SPACES TO WS-TEMP-DATE
           MOVE 0 TO RESULT

           IF FUNCTION LENGTH(FUNCTION TRIM(L-DATE)) = 0
               MOVE 0 TO RESULT
               GOBACK
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(L-DATE)) <> 10
               MOVE 0 TO RESULT
               GOBACK
           END-IF

           MOVE L-DATE TO WS-TEMP-DATE

           IF WS-TEMP-DATE(3:1) NOT = "-" OR
              WS-TEMP-DATE(6:1) NOT = "-"
               MOVE 0 TO RESULT
               GOBACK
           END-IF

           UNSTRING WS-TEMP-DATE DELIMITED BY "-"
               INTO WS-MONTH, WS-DAY, WS-YEAR
           END-UNSTRING

           IF WS-MONTH IS NOT NUMERIC OR
              WS-DAY IS NOT NUMERIC OR
              WS-YEAR IS NOT NUMERIC
               MOVE 0 TO RESULT
               GOBACK
           END-IF

           MOVE FUNCTION NUMVAL(WS-MONTH) TO WS-TEMP-MONTH
           MOVE FUNCTION NUMVAL(WS-DAY) TO WS-TEMP-DAY
           MOVE FUNCTION NUMVAL(WS-YEAR) TO WS-TEMP-YEAR

           IF WS-TEMP-MONTH < 1 OR WS-TEMP-MONTH > 12
               MOVE 0 TO RESULT
               GOBACK
           END-IF

           IF WS-TEMP-YEAR < 1
               MOVE 0 TO RESULT
               GOBACK
           END-IF

           EVALUATE WS-TEMP-MONTH
               WHEN 1 OR 3 OR 5 OR 7 OR 8 OR 10 OR 12
                   IF WS-TEMP-DAY < 1 OR WS-TEMP-DAY > 31
                       MOVE 0 TO RESULT
                       GOBACK
                   END-IF
               WHEN 4 OR 6 OR 9 OR 11
                   IF WS-TEMP-DAY < 1 OR WS-TEMP-DAY > 30
                       MOVE 0 TO RESULT
                       GOBACK
                   END-IF
               WHEN 2
                   COMPUTE WS-I = WS-TEMP-YEAR / 4
                   IF WS-I * 4 = WS-TEMP-YEAR
                       COMPUTE WS-I = WS-TEMP-YEAR / 100
                       IF WS-I * 100 = WS-TEMP-YEAR
                           COMPUTE WS-I = WS-TEMP-YEAR / 400
                           IF WS-I * 400 = WS-TEMP-YEAR
                               SET LEAP-YEAR TO TRUE
                           ELSE
                               SET NOT-LEAP-YEAR TO TRUE
                           END-IF
                       ELSE
                           SET LEAP-YEAR TO TRUE
                       END-IF
                   ELSE
                       SET NOT-LEAP-YEAR TO TRUE
                   END-IF

                   IF LEAP-YEAR
                       IF WS-TEMP-DAY < 1 OR WS-TEMP-DAY > 29
                           MOVE 0 TO RESULT
                           GOBACK
                       END-IF
                   ELSE
                       IF WS-TEMP-DAY < 1 OR WS-TEMP-DAY > 28
                           MOVE 0 TO RESULT
                           GOBACK
                       END-IF
                   END-IF
           END-EVALUATE

           MOVE 1 TO RESULT
           GOBACK.
       END PROGRAM VALID-DATE.
