      SUBROUTINE ZFIO (OPER, LUN, FIND, NREC, BUFF, IERR)
C-----------------------------------------------------------------------
C! reads and writes single 256-integer records to non-map disk files
C# Z IO-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2004, 2007
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
C   Transfer one logical record between an I/O buffer and device LUN.
C   For disk devices, the record length is always 256 local small
C   integers and NREC is the random access record number.  For non-disk
C   devices, NREC is the number of 8-bit bytes.
C   Inputs:
C      OPER   C*4      Operation code 'READ' or 'WRIT'
C      LUN    I        Logical unit number
C      FIND   I        Index in FTAB to file control block for LUN
C      NREC   I        Random access record number (1-relative) for
C                      disk transfers or number of 8-bit bytes for
C                      sequential device transfers (e.g., Tektronix
C                      terminals)
C      BUFF   I(256)   I/O buffer
C   Output:
C      IERR   I        Error return code: 0 => no error
C                         1 => file not open
C                         2 => input error
C                         3 => I/O error
C                         4 => end of file
C                         1000+n => read n (<256) words ONLY
C   Generic version.
C   No longer performs I/O to TV devices as of the 15MAR84 release.
C   No longer performs I/O to tape devices as of the 15APR87 release.
C   Only performs disk and Tektronix device I/O now.
C-----------------------------------------------------------------------
      CHARACTER OPER*4
      INTEGER   LUN, FIND, NREC, BUFF(256), IERR
C
      INTEGER   ERRLUN, NBYTES, IEREOF, J, JTRIM, I, IBYTES
      LOGICAL   MAP
      HOLLERITH HOPER, HMSG(20)
      CHARACTER MSG*80
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DZCH.INC'
      INCLUDE 'INCS:DSUMIO.INC'
      DATA ERRLUN, IEREOF, MAP /12, 4, .FALSE./
C-----------------------------------------------------------------------
C                                       Check inputs.
      IERR = 2
C                                       Valid opcode?
      MSGTXT = ' '
      IF ((OPER.NE.'READ') .AND. (OPER.NE.'WRIT')) THEN
         WRITE (MSGTXT,1000) OPER
C                                       LUN in range?
      ELSE IF ((LUN.LE.0) .OR. (LUN.GT.120)) THEN
         WRITE (MSGTXT,1010) LUN
C                                       Does not handle message files.
      ELSE IF (LUN.EQ.ERRLUN) THEN
         WRITE (MSGTXT,1020) LUN
C                                       Does not handle TVs (as of
C                                       the 15MAR84 release).
      ELSE IF (DEVTAB(LUN).EQ.4) THEN
         WRITE (MSGTXT,1030)
C                                       Does not handle tapes (as of
C                                       the 15APR87 release).
      ELSE IF ((LUN.LT.129) .AND. (LUN.GE.129-NTAPED)) THEN
         WRITE (MSGTXT,1040)
C                                       Valid record # or byte request?
      ELSE IF (NREC.LE.0) THEN
         WRITE (MSGTXT,1050) NREC
C                                       Proper device type?
      ELSE IF (MOD (DEVTAB(LUN), 2).NE.0) THEN
         WRITE (MSGTXT,1060) LUN, DEVTAB(LUN)
C                                       File open in FTAB?
      ELSE IF (FTAB(FIND).NE.LUN) THEN
         IERR = 1
         WRITE (MSGTXT,1070) LUN
         END IF
      IF (MSGTXT.NE.' ') GO TO 995
C                                       Disk device
      IF (DEVTAB(LUN).NE.2) THEN
         MSG = ' '
         IBYTES = 256 * (NBITWD / 8)
         IF (OPER.EQ.'READ') THEN
            NRCOUN(2) = NRCOUN(2) + 1
            NRBYTE(2) = NRBYTE(2) + IBYTES
         ELSE
            NWCOUN(2) = NWCOUN(2) + 1
            NWBYTE(2) = NWBYTE(2) + IBYTES
            END IF
         IF (SYSTYP.EQ.'VMS') THEN
            CALL ZFI2 (OPER, FTAB(FIND+NMOFF), BUFF, NREC, MSG, IERR)
         ELSE
            CALL CHR2H (4, OPER, 1, HOPER)
            CALL CHR2H (80, MSG, 1, HMSG)
            CALL ZFI2 (HOPER, FTAB(FIND+NMOFF), BUFF, NREC, HMSG, IERR)
            CALL H2CHR (80, 1, HMSG, MSG)
            END IF
         IF ((IERR.NE.0) .AND. (IERR.NE.IEREOF) .AND. (IERR.LE.1000))
     *      THEN
            WRITE (MSGTXT,1080) OPER, LUN, NREC
            CALL MSGWRT (7)
            J = JTRIM (MSG)
            IF (J.GT.0) THEN
               MSGTXT = MSG(1:J)
               CALL MSGWRT (7)
               END IF
            CALL ZERROR ('ZFI2  ', FTAB(FIND+NMOFF+FCBERR), ' ',
     *         FTAB(FIND+NMOFF), MAP)
C                                       fill remainder of record with 0
         ELSE IF (IERR.GT.1000) THEN
            J = 1256 - IERR
            I = 256 - J + 1
            IF (J.GT.0) CALL FILL (J, 0, BUFF(I))
            END IF
         GO TO 999
C                                       Non-disk (NREC = # 8-bit bytes).
C                                       Tektronix.
      ELSE IF (LUN.EQ.7) THEN
         NBYTES = NREC
         IF (SYSTYP.EQ.'VMS') THEN
            CALL ZTKFI2 (OPER, FTAB(FIND+NMOFF), BUFF, NBYTES, IERR)
         ELSE
            CALL CHR2H (4, OPER, 1, HOPER)
            CALL ZTKFI2 (HOPER, FTAB(FIND+NMOFF), BUFF, NBYTES, IERR)
            END IF
         IF ((IERR.NE.0) .AND. (IERR.NE.IEREOF)) THEN
            WRITE (MSGTXT,1090) OPER, LUN, NBYTES
            CALL MSGWRT (7)
            CALL ZERROR ('ZTKFI2', FTAB(FIND+NMOFF+FCBERR), ' ',
     *         FTAB(FIND+NMOFF), MAP)
            END IF
         GO TO 999
C                                       Other input error (should never
C                                       get here).
      ELSE
         IERR = 2
         WRITE (MSGTXT,1100) OPER, LUN, NREC
         END IF
C
 995  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZFIO: INVALID OPERATION = ',A4)
 1010 FORMAT ('ZFIO: LUN = ',I6,' OUT OF RANGE')
 1020 FORMAT ('ZFIO: ILLEGAL LUN = ',I2)
 1030 FORMAT ('ZFIO: DOES NOT PERFORM TV I/O AS OF 15MAR84')
 1040 FORMAT ('ZFIO: DOES NOT PERFORM TAPE I/O AS OF 15APR87')
 1050 FORMAT ('ZFIO: INVALID RECORD OR BYTE REQUEST = ',I6)
 1060 FORMAT ('ZFIO: IMPROPER DEVICE TYPE DEVTAB(',I2,') = ',I2)
 1070 FORMAT ('ZFIO: LUN = ',I2,' NOT OPEN IN FTAB')
 1080 FORMAT ('ZFIO: OPER = ',A4,' LUN = ',I2,' NREC = ',I8)
 1090 FORMAT ('ZFIO: OPER = ',A4,' LUN = ',I2,' NBYTES = ',I6)
 1100 FORMAT ('ZFIO: INPUT ERROR.  OPER = ',A4,' LUN = ',I2,' NREC = ',
     *   I6)
      END
