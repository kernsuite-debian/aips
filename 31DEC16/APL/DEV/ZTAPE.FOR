      SUBROUTINE ZTAPE (OP, LUN, FIND, COUNT, IERR)
C-----------------------------------------------------------------------
C! mount, dismount, position, write EOF, etc. for tapes
C# Z Tape
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000, 2004
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
C   Performs standard tape manipulating functions.
C   Inputs:
C       OP      C*4   Operation to be performed. 4 characters ASCII.
C                     'ADVF' = advance file marks
C                     'ADVR' = advance records
C                     'BAKF' = backspace file marks.
C                     'BAKR' = backspace records.
C                     'REWI' = rewind the tape on unit LUN
C                     'WEOF' = write end of file on unit LUN: writes 2
C                              EOFs, positions tape after first one
C                     'BEGW' = "begin write": backs just over the last
C                              EOF, writes 2 EOFs, backs over the 2nd -
C                              should be done before writing.  It is
C                              required on Exabyte 8200s anywhere except
C                              BOM and EOM.
C                     'AEOI' = Advance to end of information (2 EOFs)
C       LUN     I     logical unit number == 30 + DriveNumber
C       FIND    I     FTAB pointer.
C   In/out:         Out not yet used
C       COUNT   I     In: Number of records or file marks to skip.
C                     Out: (AEOI) > 0 EOI after COUNT files
C                                 < 0 EOI after skipping -COUNT files
C   Outputs:
C       IERR  I       Error return: 0 => ok
C                        1 = File not open
C                        2 = Input specification error.
C                        3 = I/O error.
C                        4 = End Of File
C                        5 = Beginning Of Medium
C                        6 = End Of Medium
C   Generic version - uses ZTAP2 and ZTAPR.  No longer does Mount or
C   dismount.  Differs from older versions in that it has COUNT passed
C   through to ZTAP2 as an extra argument.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   LUN, FIND, COUNT, IERR
C
      LOGICAL   MAP
      CHARACTER ERRNAM(4)*8, OPTAB*32, HER*4, LOP*4
      HOLLERITH HOPTAB(8)
      INTEGER   MTLUN1, IER, IOP, NOP, FOFF, IEREOF, IERBOM, IEREOM,
     *   IERIO, I, JC, I1
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DZCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA MTLUN1 /128/
      DATA IEREOF, IERBOM, IEREOM, IERIO /4, 5, 6, 3/
      DATA ERRNAM /'IO ERROR', 'END FILE','BEG.TAPE','END TAPE'/
      DATA NOP, OPTAB /8, 'ADVFADVRBAKFBAKRREWIWEOFBEGWAEOI'/
C-----------------------------------------------------------------------
C                                       check input errors
      IF (FTAB(FIND).NE.LUN) THEN
         IERR = 1
         WRITE (MSGTXT,1000) LUN
         GO TO 990
         END IF
      IERR = 2
      IF ((LUN.LE.MTLUN1-NTAPED) .OR. (LUN.GT.MTLUN1)) THEN
         WRITE (MSGTXT,1010) LUN
         GO TO 990
         END IF
      DO 20 IOP = 1,NOP
         I1 = IOP * 4 - 3
         IF (OP.EQ.OPTAB(I1:I1+3)) GO TO 25
 20      CONTINUE
      WRITE (MSGTXT,1020) OP
      GO TO 990
 25   IF ((IOP.LE.4) .AND. (COUNT.LE.0)) THEN
         WRITE (MSGTXT,1025) COUNT, OP
         GO TO 990
         END IF
C                                       input OK !
C                                       Remote tapes:
      IF (TPNAME(MTLUN1-LUN+1).NE.'LOCAL') THEN
         CALL ZTAPR (OP, LUN, FIND, COUNT, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1030) IERR
            IF (IERR.EQ.3) THEN
               CALL MSGWRT (7)
               MSGTXT = 'This usually means PARITY ERROR'
            ELSE IF (IERR.EQ.4) THEN
               CALL MSGWRT (7)
               MSGTXT = 'This usually means unexpected END-OF-FILE'
            ELSE IF (IERR.EQ.5) THEN
               CALL MSGWRT (7)
               MSGTXT = 'This usually means unexpected ' //
     *            'BEGINNING-OF-TAPE'
            ELSE IF (IERR.EQ.6) THEN
               CALL MSGWRT (7)
               MSGTXT = 'This usually means you''re at END-OF-TAPE'
               END IF
            GO TO 990
            END IF
         GO TO 999
         END IF
C                                       Local tapes
C                                       map or non-map file?
 50   I = (NBTB1*NTAB1 + NBTB2*NTAB2)/2
      MAP = FIND.GT.I
      FOFF = FIND + NMOFF
      IF (MAP) FOFF = FIND + MOFF
      IERR = 0
      JC = 1
      CALL CHR2H (32, OPTAB, 1, HOPTAB)
C                                       branch to OP
      GO TO (100, 200, 300, 400, 500, 600, 700, 800), IOP
C                                       Advance file
 100  LOP = 'ADVF'
         CALL ZTAP2 (HOPTAB(IOP), COUNT, FTAB(FOFF), IER)
C                                       EOF condition normal: no error
         IF ((IER.NE.0) .AND. (IER.NE.IEREOF)) GO TO 900
         GO TO 999
C                                       Advance records; leave just
C                                       beyond EOF if encountered
 200  LOP = 'ADVR'
         CALL ZTAP2 (HOPTAB(IOP), COUNT, FTAB(FOFF), IER)
C                                       No conditions normal
         IF (IER.NE.0) GO TO 900
         GO TO 999
C                                       Back COUNT file marks
C                                       then advance 1 unless BOM
 300  LOP = 'BAKF'
         CALL ZTAP2 (HOPTAB(IOP), COUNT, FTAB(FOFF), IER)
C                                       EOF normal
         IF ((IER.NE.0) .AND. (IER.NE.IEREOF) .AND. (IER.NE.IERBOM))
     *      GO TO 900
         GO TO 999
C                                       Back COUNT records
C                                       leave just beyond EOF if needed
 400  LOP = 'BAKR'
         CALL ZTAP2 (HOPTAB(IOP), COUNT, FTAB(FOFF), IER)
         IF (IER.NE.0) GO TO 900
         GO TO 999
C                                       Rewind
 500  LOP = 'REWI'
         CALL ZTAP2 (HOPTAB(IOP), 1, FTAB(FOFF), IER)
C                                       EOF + BOM normal
         IF ((IER.EQ.0) .OR. (IER.EQ.IERBOM)) GO TO 999
         GO TO 900
C                                       Write EOF
 600  LOP = 'WEOF'
         CALL ZTAP2 (HOPTAB(IOP), 1, FTAB(FOFF), IER)
         IF ((IER.NE.0) .AND. (IER.NE.IEREOF)) GO TO 900
         GO TO 999
C                                       Begin write to tape w BAKF
 700  LOP = 'BEGW'
         CALL ZTAP2 (HOPTAB(IOP), 1, FTAB(FOFF), IER)
         IF ((IER.NE.0) .AND. (IER.NE.IEREOF) .AND. (IER.NE.IERBOM))
     *      GO TO 900
         GO TO 999
C                                       Advance to end-of-info
 800  LOP = 'AEOI'
         COUNT = 1
         CALL ZTAP2 (HOPTAB(IOP), COUNT, FTAB(FOFF), IER)
         IF ((IER.NE.0) .AND. (IER.NE.IEREOF) .AND. (IER.NE.IEREOM))
     *      GO TO 900
         GO TO 999
C                                       handle errors 3 - 6.
 900  IERR = IER
      I = IERR - IERIO + 1
      IER = FTAB(FOFF+2)
      CALL ZHEX (IER, 4, HER)
      WRITE (MSGTXT,1900) LUN, ERRNAM(I), JC, LOP, HER
      CALL MSGWRT (7)
C                                       extra for unknowns
      IF (IERR.EQ.IERIO) THEN
         CALL ZERROR ('ZTAP2', IER, ' ', FTAB(FOFF), MAP)
         END IF
      GO TO 999
C
 990  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZTAPE: LUN ',I4,' NOT OPEN')
 1010 FORMAT ('ZTAPE: LUN',I4,' NOT ALLOWED FOR TAPES')
 1020 FORMAT ('ZTAPE: INVALID OPERATION: ',A4)
 1025 FORMAT ('ZTAPE: COUNT ',I6,' OP ',A4,' INVALID')
 1030 FORMAT ('REMOTE ZTAPE RETURNS ERROR',I6)
 1900 FORMAT ('ZTAPE: LUN',I3,1X,A8,' AT',I4,' OP OF ',A4,
     *   ' SYS ERR CODE ',A4)
      END
