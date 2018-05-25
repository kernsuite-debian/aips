      SUBROUTINE TABKEY (OPCODE, KEYWRD, NUMKEY, BUFFER, LOCS, VALUES,
     *   KEYTYP, IERR)
C-----------------------------------------------------------------------
C! reads/writes the Keyword section of an AIPS table file
C# EXT-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998-2000
C;  Associated Universities, Inc. Washington DC, USA.
C;
C;  This program is free software; you can redistribute it and/or
C;  modify it under the terms of the GNU General Public License as
C;  published by the Free Software Foundation; either version 2 of
C;  the License, or (at your option) any later version.
C;
C;  This program is distributed in the hope that it will be useful,
C;  but WITHOUT ANY WARRANTY; without even the implied warranty of
C;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C;  GNU General Public License for more details.
C;
C;  You should have received a copy of the GNU General Public
C;  License along with this program; if not, write to the Free
C;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
C;  MA 02139, USA.
C;
C;  Correspondence concerning AIPS should be addressed as follows:
C;         Internet email: aipsmail@nrao.edu.
C;         Postal address: AIPS Project Office
C;                         National Radio Astronomy Observatory
C;                         520 Edgemont Road
C;                         Charlottesville, VA 22903-2475 USA
C-----------------------------------------------------------------------
C   Reads or writes KEYWORDs from or to an AIPS table file header.  The
C   order of the keywords is arbitrary. Table file must have been
C   previously opened with TABINI.
C   Inputs:
C      OPCODE   C*4        Operation desired, 'READ', 'WRIT',
C                             'ALL ' => Read all.
C      KEYWRD   C(*)*8     Keywords to read/write
C      BUFFER   I(*)       Buffer being use for table I/O >= 512 words.
C   In/out:
C      NUMKEY   I          Number of keywords to read/write.
C                             Input on OPCODE='ALL' = max. to read.
C                             Output on OPCODE='ALL' = no. read.
C      LOCS     I(NUMKEY)  The word offset of first short integer
C                          word of keyword value in array VALUES.
C                          Output on READ, input on WRIT.
C                          On READ this value will be -1 for keywords
C                          not found.
C      VALUES   I          The array of keyword values; due to word
C                          alignment problems on some machines values
C                          longer than an integer should be copied,
C                          eg. if the  5th keyword (XXX) is a D:
C                               IPOINT = LOCS(5)
C                               CALL COPY (NWDPDP, VALUES(IPOINT), XXX)
C                          Output on READ, input on WRIT
C     KEYTYP    I(NUMKEY)  The type code of the keywords:
C                             1 = Double precision floating
C                             2 = Single precision floating (deprecated)
C                             3 = Character string (8 HOLLERITH chars)
C                             4 = integer
C                             5 = Logical
C                             6 = integer
C   Output:
C      IERR     I          Return code, 0=>OK,
C                             1-10 =>TABIO error
C                             19   => unrecognized data type.
C                             20   => bad OPCODE
C                             20+n => n keywords not found on READ.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4, KEYWRD(*)*8
      INTEGER   NUMKEY, BUFFER(*), LOCS(*), KEYTYP(*), IERR
      REAL      VALUES(*)
C
      INTEGER   J, NHKEY, RECORD(10), IPOINT, NWORD, JSAV, K,
     *   CHWORD(5), COUNT, MAXKEY, HEDKEY, I, RECNO, RECOFF
      CHARACTER OP*4, CTEST*8
      LOGICAL   DOREAD, REDALL, FRSTWR
      REAL      RECR(10)
      HOLLERITH RECH(10)
      DOUBLE PRECISION KEYVAD
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (RECORD, RECH, RECR)
C-----------------------------------------------------------------------
      IERR = 0
      MAXKEY = NUMKEY
C                                       Initialize data type lengths
      CHWORD(1) = NWDPDP
      CHWORD(2) = 1
      CHWORD(3) = 2
      CHWORD(4) = 1
      CHWORD(5) = 1
C                                       Check OPCODE
      OP = 'WRIT'
      REDALL = OPCODE.EQ.'ALL'
      IF ((REDALL) .AND. (NUMKEY.LE.0)) IERR = 20
      IF (NUMKEY.LE.0) GO TO 999
      DOREAD = (OPCODE.EQ.'READ') .OR. (REDALL)
      IF (DOREAD) OP = 'READ'
C                                       Unknown opcode
      IF ((OPCODE.NE.'WRIT') .AND. (.NOT.DOREAD)) THEN
         IERR = 20
         WRITE (MSGTXT,1000) OPCODE
         GO TO 990
         END IF
C                                       Fill LOCS with -1 on READ
      IF (DOREAD) CALL FILL (NUMKEY, -1, LOCS)
C                                       On READ loop through header
C                                       keywords.
      NHKEY = BUFFER(53)
      HEDKEY = NHKEY
C                                       See if first write
      FRSTWR = (.NOT.DOREAD) .AND. (NHKEY.LE.0)
      JSAV = 0
C                                       Initialize output pointer.
      IPOINT = 1
C                                       On WRITe loop thru input
C                                       keywords.
      IF (.NOT.DOREAD) NHKEY = NUMKEY
      RECOFF = 0
      IF (.NOT.DOREAD) RECOFF = BUFFER(53)
      DO 200 I = 1,NHKEY
C                                       WRITing
         RECNO = I
         IF (DOREAD) GO TO 100
            J = KEYTYP(I)
            NWORD = CHWORD(J)
C                                       Unknown data type
            IF ((J.LE.0) .OR. (J.GT.5)) THEN
               WRITE (MSGTXT,1011) J
               IERR = 19
               GO TO 990
               END IF
C                                       Ok type
            IF (.NOT.FRSTWR) THEN
C                                       Search existing keywords
               IF (HEDKEY.GT.0) THEN
                  DO 50 K = 1,HEDKEY
                     RECNO = K
                     CALL TABIO ('READ', 5, RECNO, RECORD, BUFFER, IERR)
                     IF (IERR.GT.0) THEN
                        WRITE (MSGTXT,1010) IERR, 'READ'
                        GO TO 990
                        END IF
C                                       Compare
                     CALL H2CHR (8, 1, RECH, CTEST)
                     IF (CTEST.EQ.KEYWRD(I)) GO TO 70
 50                  CONTINUE
                  END IF
C                                       Add new keyword
               RECNO = RECNO + 1
               HEDKEY = RECNO
               END IF
C                                       Load RECORD
 70         IPOINT = LOCS(I)
            CALL CHR2H (8, KEYWRD(I), 1, RECH)
            RECORD(4) = 0
C                                       force R*4 to R*8
            IF (J.NE.2) THEN
               CALL COPY (NWORD, VALUES(IPOINT), RECORD(3))
            ELSE
               KEYVAD = VALUES(IPOINT)
               CALL RCOPY (NWDPDP, KEYVAD, RECORD(3))
               J = 1
               END IF
            RECORD(5) = J
C                                       Do table I/O
 100     CALL TABIO (OP, 5, RECNO, RECORD, BUFFER, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1010) IERR, OP
            GO TO 990
            END IF
C                                       Check if read
         IF (DOREAD) THEN
C                                       Reading
            JSAV = JSAV + 1
            CALL H2CHR (8, 1, RECH, CTEST)
            IF (.NOT.REDALL) THEN
C                                       Loop through input keywords
               DO 160 J = 1,NUMKEY
                  JSAV = J
                  IF (KEYWRD(J).EQ.CTEST) GO TO 170
 160              CONTINUE
C                                       Don't want this KEYWORD
               GO TO 200
               END IF
C                                       Found match.
C                                       Find data type.
 170        J = RECORD(5)
            NWORD = CHWORD(J)
C                                       Unknown data type
            IF ((J.LE.0) .OR. (J.GT.5)) THEN
               WRITE (MSGTXT,1011) J
               IERR = 19
               GO TO 990
               END IF
C                                       Ok type, check max no. keywords
C                                       Missing keywords
            IF (JSAV.GT.MAXKEY) THEN
               WRITE (MSGTXT,1180) CTEST, MAXKEY
               CALL MSGWRT (6)
               GO TO 200
               END IF
            CALL COPY (NWORD, RECORD(3), VALUES(IPOINT))
            LOCS(JSAV) = IPOINT
C                                       force double precision
            IF (J.EQ.2) THEN
               KEYVAD = VALUES(IPOINT)
               CALL RCOPY (NWDPDP, KEYVAD, VALUES(IPOINT))
               J = 1
               END IF
            KEYTYP(JSAV) = J
            IF (REDALL) KEYWRD(JSAV) = CTEST
C                                       Set pointer for next value
            IPOINT = IPOINT + CHWORD(J)
            END IF
 200     CONTINUE
      IF (REDALL) NUMKEY = NHKEY
C                                       Done if WRITing or REDALL
      IF ((.NOT.DOREAD) .OR. (REDALL)) GO TO 999
C                                       Make sure all found
      COUNT = 0
      DO 300 I = 1,NUMKEY
C                                       Not found
         IF (LOCS(I).LE.0) THEN
            COUNT = COUNT + 1
            WRITE (MSGTXT,1200) KEYWRD(I)
            CALL MSGWRT (6)
            END IF
 300     CONTINUE
      IERR = 0
      IF (COUNT.GT.0) IERR = 20 + COUNT
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
      WRITE (MSGTXT,1990) OPCODE
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TABKEY: UNKNOWN OPCODE = ',A4)
 1010 FORMAT ('TABKEY: ERROR',I4,1X,A4,'ING TABLE FILE KEYWORDS')
 1011 FORMAT ('TABKEY: UNKNOWN KEYWORD DATA TYPE, CODE = ',I3)
 1180 FORMAT ('TABKEY: KEYWORD ',A8,' IN EXCESS OF LIMIT',I4)
 1200 FORMAT ('TABKEY: TABLE KEYWORD ',A8,' NOT FOUND')
 1990 FORMAT ('TABKEY: ERROR OCCURRED DURING TABLE KEYWORD ACCESS,',
     *   ' OPCODE=',A4)
      END
