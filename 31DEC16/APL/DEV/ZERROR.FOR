      SUBROUTINE ZERROR (ZRTNAM, SYSERR, PNAME, FCB, MAP)
C-----------------------------------------------------------------------
C! prints strings associated with system error codes for Z routines
C# Z Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2004
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
C   This routine will attempt to translate the system error code and if
C   appropriate (i.e., FCB(1).NE.-999) print the name of the file or
C   device on which the error occurred as well as the contents of the
C   the file control block (blocks, if map I/O).
C   Inputs:
C      ZRTNAM   C*6    Z-routine where the error occurred
C      SYSERR   I      System error code (also stored in FCB)
C      PNAME    C*48   Physical file name; ' ' => unknown (omit)
C      FCB      I(*)   File control block in FTAB for the file (or
C                      device) also containing the system error code
C                      FCB(1) = -999 => omit
C      MAP      L      Map or non-map I/O involved?
C   Common: DMSG.INC
C      DBGAIP   I      MOD(DBGAIP,10) > 2 => force ZERRO2 call
C                      MOD(DBGAIP,10) > 2 => dump FCB(s)
C   Generic version - calls ZERRO2.
C-----------------------------------------------------------------------
      CHARACTER ZRTNAM*(*), PNAME*(*)
      INTEGER   SYSERR, FCB(*)
      LOGICAL   MAP
C
      INTEGER   ELEN, SIZFCB, I, I1, I2, J1, LDBG, IL, ITRIM
      CHARACTER ERRMSG*80, CHRFCB*8
      HOLLERITH HERR(20)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DZCH.INC'
C-----------------------------------------------------------------------
      LDBG = MOD (DBGAIP, 10)
      SIZFCB = NMFCB
      IF (MAP) SIZFCB = MFCB
C                                       file name is known
      IF (PNAME.NE.' ') THEN
         IL = ITRIM (PNAME)
         IL = MIN (IL, 64)
         WRITE (MSGTXT,1000) PNAME(:IL)
         CALL MSGWRT (7)
         END IF
C                                       FCB(1) = -999 => no FCB
C                                       Otherwise it is I/O error
 10   ELEN = 80
      IF (FCB(1).NE.-999) THEN
         IF ((LDBG.GE.3) .OR. (FCB(1+FCBERR).NE.0)) THEN
            CALL ZERRO2 (FCB(1+FCBERR), ELEN, HERR)
            CALL H2CHR (80, 1, HERR, ERRMSG)
            IF (ELEN.GT.0) THEN
               WRITE (MSGTXT,1010) ZRTNAM, ERRMSG(1:MIN (ELEN,54))
               CALL MSGWRT (7)
               IF (ELEN.GT.54) THEN
                  WRITE (MSGTXT,1011) ERRMSG(55:ELEN)
                  CALL MSGWRT (7)
                  END IF
            ELSE IF (FCB(1+FCBERR).NE.0) THEN
               WRITE (MSGTXT,1012) FCB(1+FCBERR)
               CALL MSGWRT (7)
               END IF
            END IF
         IF ((SYSERR.NE.0) .AND. ((LDBG.GE.3) .OR.
     *      (FCB(1+FCBERR).NE.SYSERR))) THEN
            CALL ZERRO2 (SYSERR, ELEN, HERR)
            CALL H2CHR (80, 1, HERR, ERRMSG)
            IF (ELEN.GT.0) THEN
               WRITE (MSGTXT,1010) ZRTNAM, ERRMSG(1:MIN (ELEN,54))
               CALL MSGWRT (7)
               IF (ELEN.GT.54) THEN
                  WRITE (MSGTXT,1011) ERRMSG(55:ELEN)
                  CALL MSGWRT (7)
                  END IF
            ELSE IF (SYSERR.NE.0) THEN
               WRITE (MSGTXT,1013) SYSERR
               CALL MSGWRT (7)
               END IF
            END IF
C                                       Dump the file control block(s)
C                                       in I and A formats.
 20      IF (LDBG.GE.4) THEN
            J1 = 1
            DO 25 I = 1,SIZFCB
               IF (FCB(I).NE.0) J1 = MIN (I+1, SIZFCB)
 25            CONTINUE
            DO 30 I = 1,J1
               CALL H2CHR (8, 1, FCB(I), CHRFCB)
               WRITE (MSGTXT,1020) I, FCB(I), CHRFCB
               CALL MSGWRT (7)
 30            CONTINUE
            END IF
C                                       If map I/O, also dump the system
C                                       error of second buffer.
         IF (MAP) THEN
            IF ((LDBG.GE.3) .OR. (FCB(1+SIZFCB+FCBERR).NE.0)) THEN
               ELEN = 80
               CALL ZERRO2 (FCB(1+SIZFCB+FCBERR), ELEN, HERR)
               CALL H2CHR (80, 1, HERR, ERRMSG)
               IF (ELEN.GT.0) THEN
                  WRITE (MSGTXT,1010) ZRTNAM, ERRMSG(1:MIN(ELEN,54))
                  CALL MSGWRT (7)
                  IF (ELEN.GT.54) THEN
                     WRITE (MSGTXT,1011) ERRMSG(55:ELEN)
                     CALL MSGWRT (7)
                     END IF
               ELSE IF (FCB(1+FCBERR+SIZFCB).NE.0) THEN
                  WRITE (MSGTXT,1012) FCB(1+FCBERR+SIZFCB)
                  CALL MSGWRT (7)
                  END IF
               END IF
C                                       and its FCB
            IF (LDBG.GE.4) THEN
               I1 = SIZFCB + 1
               I2 = 2 * SIZFCB
               J1 = I1
               DO 45 I = I1,I2
                  IF (FCB(I).NE.0) J1 = MIN (I+1, I2)
 45               CONTINUE
               DO 50 I = I1,J1
                  CALL H2CHR (8, 1, FCB(I), CHRFCB)
                  WRITE (MSGTXT,1020) I, FCB(I), CHRFCB
                  CALL MSGWRT (7)
 50               CONTINUE
               END IF
            END IF
C                                       Not an I/O error.  Just get the
C                                       text of the system error message
      ELSE
         CALL ZERRO2 (SYSERR, ELEN, HERR)
         CALL H2CHR (80, 1, HERR, ERRMSG)
         IF (ELEN.GT.0) THEN
            WRITE (MSGTXT,1010) ZRTNAM, ERRMSG(1:MIN(ELEN,54))
            CALL MSGWRT (7)
            IF (ELEN.GT.54) THEN
               WRITE (MSGTXT,1011) ERRMSG(55:ELEN)
               CALL MSGWRT (7)
               END IF
         ELSE IF (SYSERR.NE.0) THEN
            WRITE (MSGTXT,1013) SYSERR
            CALL MSGWRT (7)
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZERROR: ON FILE ',A)
 1010 FORMAT ('ZERROR: IN ',A,1X,A)
 1011 FORMAT ('ZERROR: ',A)
 1012 FORMAT ('ZERROR: SYSTEM I/O ERROR CODE = ',I10)
 1013 FORMAT ('ZERROR: SYSTEM ERROR CODE = ',I10)
 1020 FORMAT ('ZERROR: FCB(',I4,') = (I6,2X,A8) = ',I6,2X,A8)
      END
