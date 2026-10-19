       IDENTIFICATION DIVISION.
       PROGRAM-ID. RECON01.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN1 ASSIGN TO IN1.
           SELECT IN2 ASSIGN TO IN2.
           SELECT MATCHED ASSIGN TO MATCHED.
           SELECT UNMATCH ASSIGN TO UNMATCH.
           SELECT ERRFILE ASSIGN TO ERRFILE.
       DATA DIVISION.
       FILE SECTION.
       FD  IN1 RECORDING MODE F BLOCK CONTAINS 0 RECORDS.
       01  IN1-REC.
           05 IN1-ID       PIC X(6).
           05 IN1-NAME     PIC X(20).
           05 IN1-AMT      PIC X(9).
           05 FILLER       PIC X(45).
       FD  IN2 RECORDING MODE F BLOCK CONTAINS 0 RECORDS.
       01  IN2-REC.
           05 IN2-REF      PIC X(10).
           05 IN2-ID       PIC X(6).
           05 IN2-NAME     PIC X(20).
           05 IN2-AMT      PIC X(9).
           05 FILLER       PIC X(35).
       FD  MATCHED RECORDING MODE F BLOCK CONTAINS 0 RECORDS.
       01  MATCHED-REC     PIC X(80).
       FD  UNMATCH RECORDING MODE F BLOCK CONTAINS 0 RECORDS.
       01  UNMATCH-REC     PIC X(80).
       FD  ERRFILE RECORDING MODE F BLOCK CONTAINS 0 RECORDS.
       01  ERR-REC         PIC X(80).
       WORKING-STORAGE SECTION.
       01  OUT-LINE        PIC X(80).
       01  CNT-IN1         PIC 9(6) VALUE 0.
       01  CNT-IN2         PIC 9(6) VALUE 0.
       01  CNT-MATCH       PIC 9(6) VALUE 0.
       01  CNT-UNM         PIC 9(6) VALUE 0.
       01  CNT-ERR         PIC 9(6) VALUE 0.
       PROCEDURE DIVISION.
       MAIN-PARA.
           OPEN INPUT IN1 IN2
                OUTPUT MATCHED UNMATCH ERRFILE
           PERFORM READ-IN1
           PERFORM READ-IN2
           PERFORM UNTIL IN1-ID = HIGH-VALUES
                     AND IN2-ID = HIGH-VALUES
              EVALUATE TRUE
                 WHEN IN1-ID = IN2-ID
                    PERFORM CHECK-MATCH
                    PERFORM READ-IN1
                    PERFORM READ-IN2
                 WHEN IN1-ID < IN2-ID
                    MOVE SPACES TO OUT-LINE
                    STRING 'ONLY IN FILE1: ' IN1-ID ' ' IN1-NAME
                       DELIMITED BY SIZE INTO OUT-LINE
                    WRITE UNMATCH-REC FROM OUT-LINE
                    ADD 1 TO CNT-UNM
                    PERFORM READ-IN1
                 WHEN OTHER
                    MOVE SPACES TO OUT-LINE
                    STRING 'ONLY IN FILE2: ' IN2-ID ' ' IN2-NAME
                       DELIMITED BY SIZE INTO OUT-LINE
                    WRITE UNMATCH-REC FROM OUT-LINE
                    ADD 1 TO CNT-UNM
                    PERFORM READ-IN2
              END-EVALUATE
           END-PERFORM
           CLOSE IN1 IN2 MATCHED UNMATCH ERRFILE
           DISPLAY 'FILE1 RECORDS : ' CNT-IN1
           DISPLAY 'FILE2 RECORDS : ' CNT-IN2
           DISPLAY 'MATCHED       : ' CNT-MATCH
           DISPLAY 'UNMATCHED     : ' CNT-UNM
           DISPLAY 'ERRORS        : ' CNT-ERR
           STOP RUN.
       CHECK-MATCH.
           IF IN1-AMT IS NOT NUMERIC OR IN2-AMT IS NOT NUMERIC
              MOVE SPACES TO OUT-LINE
              STRING 'BAD AMOUNT   : ' IN1-ID ' ' IN1-AMT ' '
                 IN2-AMT DELIMITED BY SIZE INTO OUT-LINE
              WRITE ERR-REC FROM OUT-LINE
              ADD 1 TO CNT-ERR
           ELSE
              IF IN1-AMT NOT = IN2-AMT
                 MOVE SPACES TO OUT-LINE
                 STRING 'AMT MISMATCH : ' IN1-ID ' ' IN1-AMT ' '
                    IN2-AMT DELIMITED BY SIZE INTO OUT-LINE
                 WRITE ERR-REC FROM OUT-LINE
                 ADD 1 TO CNT-ERR
              ELSE
                 MOVE SPACES TO OUT-LINE
                 STRING IN1-ID ' ' IN1-NAME ' ' IN1-AMT
                    DELIMITED BY SIZE INTO OUT-LINE
                 WRITE MATCHED-REC FROM OUT-LINE
                 ADD 1 TO CNT-MATCH
              END-IF
           END-IF.
       READ-IN1.
           READ IN1
              AT END MOVE HIGH-VALUES TO IN1-ID
              NOT AT END ADD 1 TO CNT-IN1
           END-READ.
       READ-IN2.
           READ IN2
              AT END MOVE HIGH-VALUES TO IN2-ID
              NOT AT END ADD 1 TO CNT-IN2
           END-READ.

