      SUBROUTINE RUVTAB (NPV, FDVEC, TBIND, NAXIS, DISOUT, ISLOT, LAST,
     *   NSKIP, DOKEEP, TBUFF, UBUFF, TAPBUF, IERR)
C-----------------------------------------------------------------------
C! Read data from FITS 3-D UV table and write AIPS UV data set
C# FITS EXT-util tape UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1999, 2007, 2015-2016
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
C  This routine will read the data section of a FITS 3-D table and copy
C  the data to the associated AIPS uv data set.
C  Inputs:
C     NPV      I(4)       IPIECE, NPIECE, FIRSTVIS, LASTVIS
C     FDVEC    I(50)      File descriptor vector for TAPIO input
C     NAXIS    I(2)       Length of columns (in char), number of rows.
C     DISOUT   I          Output disk
C     ISLOT    I          Output slot number
C     LAST     L          Last piece?
C     DOKEEP   R          > 0 => keep fully flagged
C  In/Out:
C     TBIND    I          Buffer pointer in TAPBUF
C     NSKIP    I          Count of fully flagged records dropped
C     TBUFF    R(*)       Scratch buffer 3*MAXCIF in size
C     UBUFF    R(*)       I/O buffer UVBFSS in size
C     TAPBUF   I(*)       Tape I/O buffer.
C  Outputs:
C     IERR     I          Error code. 0=ok.
C-----------------------------------------------------------------------
      INTEGER   NPV(4), FDVEC(50), TBIND, NAXIS(2), DISOUT, ISLOT,
     *   NSKIP, TAPBUF(*), IERR
      LOGICAL   LAST
      REAL      DOKEEP, TBUFF(*), UBUFF(*)
C
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   TBYTCT, MXLREB, TLINE(XBPRSZ), ROWF, ROWL, LCTR, NCOPY,
     *   BYTCNT(7), KTYPE, I, IOFF, IT0, IVIS(2), J, II, TCOUNT(128),
     *   NEXT, NBYTE, TPTYPE(128), TOFF(128), IDLUN, IDFIND, ISIZE,
     *   IVMAX, IVCTR, NBLKOF, IBIND, WTOFF, IBLANK, IILOCB, IA1, IA2,
     *   ISUB
      REAL      WTSCL(2), BASEL
      CHARACTER UVNAME*48
      LOGICAL   COMPIN, GOOD
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DTHD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA BYTCNT /8,4,1,4,1,2,1/
      DATA IDLUN, IBLANK /16, -32767/
C-----------------------------------------------------------------------
      IERR = 0
      IF (NAXIS(2).EQ.0) THEN
         WRITE (MSGTXT,1000) ITYPE
         GO TO 990
         END IF
C                                       Calculate end & type
C                                       of Column values.
      TBYTCT = 0
      DO 10 I = 1,ITNCOL
         TPTYPE(I) = MOD (TFCODE(I),10)
         TCOUNT(I) = TFCODE(I) / 10
         TOFF(I) = I
C                                       Count bytes
         KTYPE = TPTYPE(I)
         TBYTCT = TBYTCT + TCOUNT(I) * BYTCNT(KTYPE)
 10      CONTINUE
      COMPIN = TPTYPE(ITNCOL).EQ.6
C                                       Check buffer size
      MXLREB = XBPRSZ * NBITWD / 8
C                                       Record too big
      IF (TBYTCT.GT.MXLREB) THEN
         IERR = 5
         WRITE (MSGTXT,1010) TBYTCT, MXLREB
         GO TO 990
         END IF
C                                       Crunch CATBLK: resize file
      CALL RUVFIL (NPV, NAXIS, DISOUT, ISLOT, IILOCB, IERR)
      IF (IERR.NE.0) GO TO 999
      MSGSUP = 32000
      CALL UVPGET (IERR)
      MSGSUP = 0
      WTOFF = -1
      IF (CATBLK(KINAX).EQ.1) THEN
         CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), WTOFF,
     *      IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'CAN''T FIND THE WEIGHT PARAMETER FOR OUTPUT'
            GO TO 990
            END IF
         END IF
C                                       Open UV file.
      CALL ZPHFIL ('UV', DISOUT, ISLOT, 1, UVNAME, IERR)
      CALL ZOPEN (IDLUN, IDFIND, DISOUT, UVNAME, .TRUE., .TRUE., .TRUE.,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
      ISIZE = 2 * UVBFSS
      IVMAX = 0
      NBLKOF = 1
      II = CATBLK(KIGCN) - NAXIS(2)
      ROWF = II + 1
      ROWL = CATBLK(KIGCN)
      CALL UVINIT ('WRIT', IDLUN, IDFIND, CATBLK(KIGCN), II, LREC,
     *   IVMAX, ISIZE, UBUFF, NBLKOF, IBIND, IERR)
      IF (IERR.NE.0) GO TO 999
      IVCTR = 0
      NCOPY = LREC - NRPARM
C                                       Read first record,
      CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
      NEXT = 1
      IF (IERR.NE.0) GO TO 999
C                                       Loop for all lines in table.
      DO 200 LCTR = ROWF,ROWL
         DO 180 I = 1,ITNCOL
C                                       Read a FITS table data entry.
            KTYPE = TPTYPE(I)
            NBYTE = BYTCNT(KTYPE) * TCOUNT(I)
            IF (KTYPE.EQ.7) NBYTE = 1 + (TCOUNT(I)-1) / 8
            CALL GTF3D (FDVEC, TBIND, NEXT, TAPBUF, NBYTE, TLINE, IERR)
            IF (IERR.NE.0) GO TO 999
C                                       Go to correct type
            IOFF = TOFF(I) + IBIND - 1
            IT0 = TPTYPE(I)
C                                       Single precision float
            IF (IT0.EQ.2) THEN
               IF (COMPIN) THEN
                  IF (I.EQ.ITNCOL-2) THEN
                     CALL ZR32RL (TCOUNT(I), 1, TLINE, WTSCL(1))
                     WTSCL(1) = WTSCL(1) * TSCAL(I) + TZERO(I)
                  ELSE IF (I.EQ.ITNCOL-1) THEN
                     CALL ZR32RL (TCOUNT(I), 1, TLINE, WTSCL(2))
                     WTSCL(2) = WTSCL(2) * TSCAL(I) + TZERO(I)
                  ELSE IF (I.EQ.IILOCB) THEN
                     CALL ZR32RL (1, 1, TLINE, BASEL)
                     IA1 = BASEL / 256.0
                     IA2 = BASEL - 256*IA1
                     ISUB = (BASEL - 256*IA1 - IA2) * 100.0 + 1.5
                     UBUFF(IOFF) = ISUB
                     UBUFF(IBIND+ILOCA1) = IA1
                     UBUFF(IBIND+ILOCA2) = IA2
                  ELSE
                     CALL ZR32RL (TCOUNT(I), 1, TLINE, UBUFF(IOFF))
                     DO 20 J = 1,TCOUNT(I)
                        UBUFF(IOFF+J-1) = UBUFF(IOFF+J-1) * TSCAL(I) +
     *                     TZERO(I)
 20                     CONTINUE
                     END IF
               ELSE
                  IF (I.EQ.ITNCOL) THEN
                     CALL ZR32RL (TCOUNT(I), 1, TLINE, TBUFF)
                     DO 25 J = 1,TCOUNT(I)
                        TBUFF(J) = TBUFF(J) * TSCAL(I) + TZERO(I)
 25                     CONTINUE
                  ELSE IF (I.EQ.IILOCB) THEN
                     CALL ZR32RL (1, 1, TLINE, BASEL)
                     IA1 = BASEL / 256.0
                     IA2 = BASEL - 256*IA1
                     ISUB = (BASEL - 256*IA1 - IA2) * 100.0 + 1.5
                     UBUFF(IOFF) = ISUB
                     UBUFF(IBIND+ILOCA1) = IA1
                     UBUFF(IBIND+ILOCA2) = IA2
                  ELSE
                     CALL ZR32RL (TCOUNT(I), 1, TLINE, UBUFF(IOFF))
                     DO 30 J = 1,TCOUNT(I)
                        UBUFF(IOFF+J-1) = UBUFF(IOFF+J-1) * TSCAL(I) +
     *                     TZERO(I)
 30                     CONTINUE
                     END IF
                  END IF
C                                       Short Integer.
            ELSE IF ((IT0.EQ.6) .AND. (I.EQ.ITNCOL)) THEN
               II = 0
               DO 150 J = 1,TCOUNT(I),2
                  CALL ZI16IL (2, J, TLINE, IVIS)
                  IF ((IVIS(1).EQ.IBLANK) .OR. (IVIS(2).EQ.IBLANK))
     *               THEN
                     TBUFF(II+1) = 0.0
                     TBUFF(II+2) = 0.0
                     TBUFF(II+3) = 0.0
                  ELSE
                     TBUFF(II+1) = IVIS(1) * WTSCL(2) * TSCAL(I) +
     *                  TZERO(I)
                     TBUFF(II+2) = IVIS(2) * WTSCL(2) * TSCAL(I) +
     *                  TZERO(I)
                     TBUFF(II+3) = WTSCL(1) * TSCAL(I) + TZERO(I)
                     END IF
                  II = II + 3
 150              CONTINUE
C                                       Wrong type!
            ELSE
               WRITE (MSGTXT,1150) I, IT0
               IERR = 2
               GO TO 990
               END IF
 180        CONTINUE
         GOOD = .TRUE.
         IF (DOKEEP.LE.0.0) THEN
            IF (WTOFF.GE.0) THEN
               DO 190 I = 3,3*NCOPY,3
                  IF (TBUFF(I).GT.0.0) GO TO 195
 190              CONTINUE
               GOOD = .FALSE.
            ELSE
               DO 191 I = 3,NCOPY,3
                  IF (TBUFF(I).GT.0.0) GO TO 195
 191              CONTINUE
               GOOD = .FALSE.
               END IF
            END IF
 195     IF (GOOD) THEN
            IF (CATBLK(KINAX).EQ.1) THEN
               CALL ZUVPAK (NCOPY, TBUFF, UBUFF(IBIND+WTOFF),
     *            UBUFF(IBIND+NRPARM))
            ELSE
               CALL RCOPY (NCOPY, TBUFF, UBUFF(IBIND+NRPARM))
               END IF
            IVCTR = IVCTR + 1
            IBIND = IBIND + LREC
C                                       Write this full buffer
            IF (IVCTR.GE.IVMAX) THEN
               CALL UVDISK ('WRIT', IDLUN, IDFIND, UBUFF, IVCTR, IBIND,
     *            IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1190) IERR
                  GO TO 990
                  END IF
               IVCTR = 0
               END IF
         ELSE
            NSKIP = NSKIP + 1
            END IF
 200     CONTINUE
C                                       Finish up any pending disk I/O.
      IVCTR = -IVCTR
      CALL UVDISK ('FLSH', IDLUN, IDFIND, UBUFF, IVCTR, IBIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1190) IERR
         GO TO 990
         END IF
      IF ((LAST) .AND. (NSKIP.GT.0)) THEN
         WRITE (MSGTXT,1200) NSKIP
         CALL MSGWRT (4)
         I = CATBLK(KIGCN) - NSKIP
         CALL UCMPRS (I, DISOUT, ISLOT, IDLUN, CATBLK, IERR)
         IERR = 0
         END IF
      CALL ZCLOSE (IDLUN, IDFIND, IERR)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Warning: table type ',A2,' is of zero length')
 1010 FORMAT ('ERROR: TABLE ROW =',I6,' BYTES, BUFFER SIZE =',I6)
 1150 FORMAT ('RUVTAB: COLUMN',I3,' TYPE',I2,' UNEXPECTED!')
 1190 FORMAT ('RUVTAB: ERROR',I5,' WRITING UV FILE')
 1200 FORMAT ('RUVTAB: omitted',I11,' totally flagged vis records')
      END
