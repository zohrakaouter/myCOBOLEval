       IDENTIFICATION DIVISION.
       PROGRAM-ID. DECIMAL-TO-BINARY.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARIABLES.
           05 WS-NUMBER PIC S9(10) VALUE 0.
           05 WS-REMAINDER PIC 9(10) VALUE 0.
           05 WS-BINARY-STRING PIC X(100) VALUE SPACES.
           05 WS-INDEX PIC 9(3) VALUE 0.
           05 WS-TEMP PIC X(100) VALUE SPACES.
           05 WS-LENGTH PIC 9(3) VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-DECIMAL PIC S9(10).
           05 RESULT PIC X(100).

      * You will be given a number in decimal form and your task is to convert it to
      * binary format. The function should return a string, with each character representing a binary
      * number. Each character in the string will be '0' or '1'.
      * 
      * There will be an extra couple of characters 'db' at the beginning and at the end of the string.
      * The extra characters are there to help with the format.
      * 
      * Examples:
      * decimal_to_binary(15)   # returns "db1111db"
      * decimal_to_binary(32)   # returns "db100000db"
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           MOVE L-DECIMAL TO WS-NUMBER

           IF WS-NUMBER = 0
               STRING 'db0db' DELIMITED BY SIZE INTO RESULT
               GOBACK
           END-IF

           PERFORM UNTIL WS-NUMBER = 0
               DIVIDE WS-NUMBER BY 2 GIVING WS-NUMBER
                   REMAINDER WS-REMAINDER
               IF WS-REMAINDER = 0
                   MOVE '0' TO WS-BINARY-STRING(WS-INDEX:1)
               ELSE
                   MOVE '1' TO WS-BINARY-STRING(WS-INDEX:1)
               END-IF
               ADD 1 TO WS-INDEX
           END-PERFORM

           MOVE SPACES TO WS-TEMP
           SUBTRACT 1 FROM WS-INDEX GIVING WS-LENGTH
           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > WS-LENGTH
               MOVE WS-BINARY-STRING(WS-LENGTH - WS-INDEX + 1:1)
                 TO WS-TEMP(WS-INDEX:1)
           END-PERFORM

           STRING 'db' DELIMITED BY SIZE
                  WS-TEMP(1:WS-LENGTH) DELIMITED BY SPACE
                  'db' DELIMITED BY SIZE
                  INTO RESULT

           GOBACK.
       END PROGRAM DECIMAL-TO-BINARY.
