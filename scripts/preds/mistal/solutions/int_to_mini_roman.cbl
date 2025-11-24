       IDENTIFICATION DIVISION.
       PROGRAM-ID. INT-TO-MINI-ROMAN.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01  ROMAN-VALUES.
           05  FILLER PIC X(7) VALUE "M    D".
           05  FILLER PIC X(7) VALUE "C    L".
           05  FILLER PIC X(7) VALUE "X    V".
           05  FILLER PIC X(7) VALUE "I    ".
       01  FILLER REDEFINES ROMAN-VALUES.
           05  ROMAN-CHARS OCCURS 7 TIMES PIC X.
       01  ROMAN-NUMS.
           05  FILLER PIC 9(4) VALUE 1000.
           05  FILLER PIC 9(4) VALUE 0500.
           05  FILLER PIC 9(4) VALUE 0100.
           05  FILLER PIC 9(4) VALUE 0050.
           05  FILLER PIC 9(4) VALUE 0010.
           05  FILLER PIC 9(4) VALUE 0005.
           05  FILLER PIC 9(4) VALUE 0001.
       01  FILLER REDEFINES ROMAN-NUMS.
           05  ROMAN-NUM OCCURS 7 TIMES PIC 9(4).
       01  I PIC 99.
       01  TEMP-NUM PIC 9(4).
       01  TEMP-RESULT PIC X(100) VALUE SPACES.
       01  RESULT-LEN PIC 99 VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-NUMBER PIC S9(10).
           05 RESULT PIC X(100).

      * 
      * Given a positive integer, obtain its roman numeral equivalent as a string,
      * and return it in lowercase.
      * Restrictions: 1 <= num <= 1000
      * 
      * Examples:
      * >>> int_to_mini_roman(19) == 'xix'
      * >>> int_to_mini_roman(152) == 'clii'
      * >>> int_to_mini_roman(426) == 'cdxxvi'
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           MOVE L-NUMBER TO TEMP-NUM
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 7
               PERFORM UNTIL TEMP-NUM < ROMAN-NUM(I)
                   ADD 1 TO RESULT-LEN
                   STRING ROMAN-CHARS(I) DELIMITED BY SIZE
                       INTO TEMP-RESULT WITH POINTER RESULT-LEN
                   SUBTRACT ROMAN-NUM(I) FROM TEMP-NUM
               END-PERFORM
           END-PERFORM

           MOVE FUNCTION LOWER-CASE(TEMP-RESULT) TO RESULT
           GOBACK.
       END PROGRAM INT-TO-MINI-ROMAN.
