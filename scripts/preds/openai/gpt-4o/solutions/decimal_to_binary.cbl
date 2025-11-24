       IDENTIFICATION DIVISION.
       PROGRAM-ID. DECIMAL-TO-BINARY.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       77 BINARY-REPRESENTATION PIC X(96) VALUE SPACES.
       77 CURRENT-INDEX PIC 9(3) VALUE 96.
       77 TEMP-DECIMAL        PIC S9(10).

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

       MAIN-PROCEDURE.
           MOVE L-DECIMAL TO TEMP-DECIMAL
           PERFORM CONVERT-TO-BINARY
           PERFORM FORMAT-RESULT
           GOBACK.

       CONVERT-TO-BINARY.
           PERFORM UNTIL TEMP-DECIMAL = 0
               EVALUATE REMAINDER (TEMP-DECIMAL, 2) 
                   WHEN 0
                       MOVE '0' TO BINARY-REPRESENTATION (CURRENT-INDEX:)
                   WHEN OTHER
                       MOVE '1' TO BINARY-REPRESENTATION (CURRENT-INDEX:)
               END-EVALUATE
               DIVIDE TEMP-DECIMAL BY 2 GIVING TEMP-DECIMAL
               ADD 1 TO CURRENT-INDEX
           END-PERFORM.

       REMAINDER (NUMERATOR, DENOMINATOR) RETURNS REMAINDER.
           COMPUTE REMAINDER = NUMERATOR MODULUS DENOMINATOR.

       FORMAT-RESULT.
           STRING 
               "db" DELIMITED BY SIZE
               BINARY-REPRESENTATION (CURRENT-INDEX:96-CURRENT-INDEX) DELIMITED BY SPACE
               "db" DELIMITED BY SIZE
               INTO RESULT.
           
       END PROGRAM DECIMAL-TO-BINARY.
