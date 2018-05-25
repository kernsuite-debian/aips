      PROGRAM STARC
C-----------------------------------------------------------------------
C! Task converts RUN file of source flux,position and size into CC ext.
C# EXT-util Map-util Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
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
C-----------------------------------------------------------------------
C   STARC will convert a RUN file containing the positions and
C   uncertainties of points into a ST extension file attached to an
C   AIPS image.  The ST file will be in standard tables format.
C    INPUTS:   (from AIPS)
C              USERID   R   user number, 0 means use logon user
C                       number, 32000 means any user can be accessed.
C              INNAME   R(3)   name of primary file.
C              INCLASS  R(2)   class of primary file.
C              INSEQ    R   sequence number of primary file.
C              INDISK   R    disk volume number. 0 means try all.
C              OUTVERS  R    CC file version number.
C              INFILE   C*48   Input RUN file name
C-----------------------------------------------------------------------
      CHARACTER KARBUF*80, PRGNAM*6, HILINE*72, NAMIN*12, CLSIN*6,
     *   INFILE*48, TYPIN*2, ATIME*8, ADATE*12, STFILE*48, M*1,
     *   SBUF*80, ATYPE(18)*4, NTYPE(7)*8, CHTM12*12
      HOLLERITH XNAMIN(3), XCLSIN(2), XINFIL(12)
      DOUBLE PRECISION XPOS(2:3), DDX, DEGRAD
      REAL      DSKIN, PRUSER, SEQIN, XOUVER, RECORD(7)
      INTEGER  I, IWBUFF(256), IMFIND, IREC, IMLUN, IERR, IRETCD, ISEQ,
     *   INPRMS, ISLOT, IUSER, IVOL, MAGIC, IROUND, RLUN, SLUN, IS,
     *   HLUN, IT(3), ID(3), J, K, BUF(512), NKEY, NREC, NCOL, RIND,
     *   OUVER, ICTYPE(2:3), ICOL, KBPTR, IER, IRNO, DEPTH(5)
      LOGICAL   T, F, SAVE, EQUAL, QUICK, NEWVER
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      COMMON /INPARM/ PRUSER, XNAMIN, XCLSIN, SEQIN, DSKIN, XOUVER,
     *   XINFIL
      DATA MAGIC, IMLUN, RLUN, SLUN, HLUN /32000, 16,10,28,29/
      DATA DEPTH /5 * 1/
      DATA PRGNAM /'STARC '/
      DATA TYPIN /'  '/
      DATA T, F /.TRUE.,.FALSE./
      DATA ATYPE /'TIME','FREQ','LAMB','VELO','FELO','    ','DIST',
     *   'ANGL','ELON','ELAT', 'GLON','GLAT','RA  ','RA--','LL  ',
     *   'DEC ','DEC-','MM  '/
      DATA NTYPE /'SECONDS ', 'HERTZ   ', 'METERS  ',
     *   'METR/SEC', 'METR/SEC', 'PIXELS  ', 'DEGREES '/
C-----------------------------------------------------------------------
C                                       Initialize the IO parameters.
      CALL ZDCHIN (.TRUE., IWBUFF)
      CALL VHDRIN
      SAVE = F
C                                       Get input values from AIPS.
      INPRMS = 21
      IRETCD = 0
      CALL GTPARM (PRGNAM, INPRMS, QUICK, PRUSER, IWBUFF, IERR)
      IF (IERR.EQ.0) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         IRETCD = 8
 10   IF (QUICK) CALL RELPOP (IRETCD, IWBUFF, IERR)
      IF (IRETCD.NE.0) GO TO 990
      IRETCD = 8
C                                       Hollerith -> char
      CALL H2CHR (12, 1, XNAMIN, NAMIN)
      CALL H2CHR (6, 1, XCLSIN, CLSIN)
      CALL H2CHR (48, 1, XINFIL, INFILE)
C
      ISEQ = IROUND (SEQIN)
      IVOL = IROUND (DSKIN)
      IUSER = IROUND (PRUSER)
      IF (IUSER.EQ.0) IUSER = NLUSER
      IF (IUSER.EQ.MAGIC) IUSER = 0
      OUVER = IROUND (XOUVER)
C                                       Open map file & get header.
      CALL MAPOPN ('HDWR', IVOL, NAMIN, CLSIN, ISEQ, TYPIN, IUSER,
     *   IMLUN, IMFIND, ISLOT, CATBLK, IWBUFF, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Fill in defaults in PARMS
      DSKIN = IVOL
      PRUSER = IUSER
      DEGRAD = ATAN (1.0D0) / 45.0D0
C                                       Get coordinate info
      LOCNUM = 1
      CALL SETLOC (CATBLK, DEPTH)
      RECORD(7) = 1.
C                                       Open text file.
      CALL ZTXOPN ('READ', RLUN, RIND, INFILE, F, IERR)
      IF (IERR.EQ.0) GO TO 30
         WRITE (MSGTXT,1020) IERR
         CALL MSGWRT (8)
         GO TO 980
C                                       Create/open ST file
 30   NCOL = 7
      NKEY = 1
      NREC = 50
C                                       create/open
      CALL CCINI (SLUN, NCOL, IVOL, ISLOT, OUVER, CATBLK, BUF, IERR)
      NEWVER = IERR.EQ.-1
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1030) IERR, OUVER
         CALL MSGWRT (8)
         GO TO 970
      ENDIF
C                                       Figure out coords
      DO 55 I = 1,2
         ICTYPE(I+1) = 1
         J = (I-1) * 2 + KHCTP
         DO 50 K = 1,18
            CALL H2CHR (4, 1, CATH(J), CHTM12)
            EQUAL = CHTM12(1:4) .EQ. ATYPE(K)(1:4)
            IF (.NOT.EQUAL) GO TO 50
               IF (K.GT.12) ICTYPE(I+1) = 2
               IF (K.GT.15) ICTYPE(I+1) = 3
               J = I + 2
               GO TO 55
 50         CONTINUE
 55      CONTINUE
C                                       Read record loop
      DO 190 IREC = 1,99999
         CALL ZTXIO ('READ', RLUN, RIND, SBUF, IERR)
         IF (IERR.NE.0) THEN
            IF (IERR.EQ.2) GO TO 200
            WRITE (MSGTXT,1100) IERR
            CALL MSGWRT (8)
            GO TO 960
         ENDIF
         IRNO = IRNO + 1
C                                       Parse the input line
         KARBUF(1:80) = SBUF(1:80)
         KBPTR = 0
         DO 140 ICOL = 1, 6
            IS = 1
            J = 1
C                                       Find next non-blank
 115        KBPTR = KBPTR + 1
C                                       End of card
C                                       Positions missing
            IF (KBPTR.GT.80) THEN
               IF (ICOL.LE.3) THEN
                  WRITE (MSGTXT,1115) IRNO
                  CALL MSGWRT (7)
                  IRNO = IRNO - 1
                  GO TO 190
C                                       Sizes missing -> 1 pixel
               ELSE IF ((ICOL.GE.4) .AND. (ICOL.LE.5)) THEN
                  RECORD(ICOL) = ABS(CATR(KRCIC+ICOL-4))
C                                       Position angle missing
               ELSE
                  RECORD(ICOL) = 0.
                  END IF
               END IF
            M = KARBUF(KBPTR:KBPTR)
C                                       Squash blanks
            IF (M.EQ.' ') GO TO 115
C                                       Declination leading sign
            IF ((J.EQ.1) .AND. (ICTYPE(ICOL).EQ.3)) THEN
               J = J + 1
               IF (M .EQ. '+') THEN
                  GO TO 115
               ELSE IF (M .EQ. '-') THEN
                  IS = -1
                  GO TO 115
                  END IF
               END IF
C                                       So get the number
            CALL GETNUM (KARBUF, 80, KBPTR, DDX)
C                                       Flux or PA in degrees
            IF ((ICOL.EQ.1) .OR. (ICOL.EQ.6)) THEN
               RECORD(ICOL) = DDX
C                                       BMAX OR BMIN
            ELSE IF ((ICOL.EQ.4) .OR. (ICOL.EQ.5)) THEN
C                                       Convert to degrees
               RECORD(ICOL) = ABS(DDX/3600.)
C                                       Position
            ELSE
C                                       Not RA or DEC
               IF (ICTYPE(ICOL).EQ.1) THEN
                  XPOS(ICOL) = DDX
C                                       RA -> degrees
               ELSE IF (ICTYPE(ICOL).EQ.2) THEN
                  IF (J.EQ.1) XPOS(ICOL) = DDX
                  IF (J.EQ.2) XPOS(ICOL) = XPOS(ICOL) + DDX / 60.0D0
                  IF (J.EQ.3) XPOS(ICOL) = XPOS(ICOL) + DDX / 3600.0D0
                  J = J + 1
                  IF (J.LE.3) GO TO 115
                  XPOS(ICOL) = XPOS(ICOL) * 15.0D0
                  IF (XPOS(ICOL)-CATD(KDCRV+ICOL-1).GT.180.0D0) THEN
                     XPOS(ICOL) = XPOS(ICOL) - 360.0D0
                  ELSE IF (XPOS(ICOL)-CATD(KDCRV+ICOL-1) .LT.-180.0D0)
     *               THEN
                     XPOS(ICOL) = XPOS(ICOL) + 360.0D0
                     END IF
C                                       DEC -> degrees
               ELSE IF (ICTYPE(ICOL) .EQ. 3) THEN
                  IF (J.EQ.2) XPOS(ICOL) = DDX
                  IF (J.EQ.3) XPOS(ICOL) = XPOS(ICOL) + DDX / 60.0D0
                  IF (J.EQ.4) XPOS(ICOL) = XPOS(ICOL) + DDX / 3600.0D0
                  J = J + 1
                  IF (J.LE.4) GO TO 115
                  XPOS(ICOL) = XPOS(ICOL) * IS
                  END IF
               END IF
 140        CONTINUE
         CALL XYPIX (XPOS(2), XPOS(3), RECORD(2), RECORD(3), IERR)
         RECORD(2) = (RECORD(2) - RPLOC(1,LOCNUM)) * AXINC(1,LOCNUM)
         RECORD(3) = (RECORD(3) - RPLOC(2,LOCNUM)) * AXINC(2,LOCNUM)
         CALL TABIO ('WRIT', 0, IRNO, RECORD, BUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1150) IERR
            CALL MSGWRT (8)
            GO TO 960
            END IF
 190     CONTINUE
C                                       Normal EOF
 200  CALL TABIO ('CLOS', 0, IRNO, XPOS, BUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1150) IERR
         CALL MSGWRT (8)
         GO TO 960
         END IF
      WRITE (MSGTXT,1210) IRNO, OUVER
      CALL MSGWRT (4)
      IRETCD = 0
C                                       Add to history file
      CALL HIINIT (3)
      CALL HIOPEN (HLUN, IVOL, ISLOT, IWBUFF, IER)
      IF (IER.NE.0) GO TO 970
C                                       Prepare text and add to file
      CALL ZDATE (ID)
      CALL ZTIME (IT)
      CALL TIMDAT (IT, ID, ATIME, ADATE)
      WRITE (SBUF,1211,ERR=215) TSKNAM, OUVER, IRNO, ADATE, ATIME
 215  HILINE = SBUF
      CALL HIADD (HLUN, HILINE, IWBUFF, IER)
      CALL HICLOS (HLUN, T, IWBUFF, IER)
      GO TO 970
C                                       ERRORS:
C                                       Kill the ST file
 960  CALL ZCLOSE (SLUN, BUF(82), IERR)
      CALL H2CHR (24, 1, BUF(17), STFILE)
      CALL ZDESTR (IVOL, STFILE, IERR)
      DO 965 I = 1,KIEXTN
         J = I - 1
         CALL H2CHR (2, 1, CATH(KHEXT+J), CHTM12)
         EQUAL = CHTM12(1:2) .EQ. 'ST'
         IF (.NOT.EQUAL) GO TO 965
            IF (OUVER.EQ.CATBLK(KIVER+J)) CATBLK(KIVER+J) =
     *         CATBLK(KIVER+J) - 1
            SAVE = T
            GO TO 970
 965     CONTINUE
C                                       Close the text file
 970  CALL ZTXCLS (RLUN, RIND, IERR)
C                                       Close map file.
 980  CALL MAPCLS ('WRIT', IVOL, ISLOT, IMLUN, IMFIND, CATBLK, SAVE,
     *   IWBUFF, IERR)
C
 990  CALL DIETSK (IRETCD, QUICK, IWBUFF)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR GETTING PARAMETERS FROM AIPS. GTPARM ERR =',I5)
 1020 FORMAT ('ERROR',I4,' OPENING TEXT FILE')
 1030 FORMAT ('ERROR',I4,' CREATING/OPENING CC FILE VERSION',I4)
 1100 FORMAT ('ERROR',I4,' READING TEXT FILE')
 1115 FORMAT ('ERROR PARSING LINE',I6,': LINE IGNORED')
 1150 FORMAT ('ERROR',I4,' WRITING CC FILE')
 1210 FORMAT ('Wrote',I6,' lines in CC file version',I4)
 1211 FORMAT (A6,' Version=',I3,' Lines=',I5,
     *   '  / CC file created ',A,A)
      END
