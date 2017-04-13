      PROGRAM MF2ST
C-----------------------------------------------------------------------
C! Task converts MF file of components to a stars file for plotting
C# EXT-util Map-util Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2006, 2009
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
C   MF2ST will convert a MF (Model fit) extension file (usually produced
C   by SAD) to a ST file containing the positions and widths attached to
C   an AIPS image.  The ST file will be in standard tables format.
C   Inputs:   (from AIPS)
C      USERID    R      user number, 0 means use logon user
C                       number, 32000 means any user can be accessed.
C      INNAME    R(3)   name of primary file.
C      INCLASS   R(2)   class of primary file.
C      INSEQ     R      sequence number of primary file.
C      INDISK    R      disk volume number. 0 means try all.
C      INVERS    R      MF file version number
C      OUTVERS   R      ST file version number
C      FLUX      R      Min flux to include
C      CTYPE     R(4)   (1) Star type
C                       (2) Max # MF to include
C                       (3,4) If (4) > (3), use ST types 0 to (1) scaled
C                           overs fluxes (3) to (4)
C-----------------------------------------------------------------------
C                                       Max Number Columns, Label Length
      INTEGER MXSTCL, MXSTLB
      PARAMETER (MXSTCL=7, MXSTLB=24)
C
      CHARACTER PRGNAM*6, HILINE*72, NAMIN*12, CLSIN*6, TYPIN*2,
     *   ATIME*8, ADATE*12, STFILE*48, SBUF*80, TTITLE*28, ATYPE(18)*4,
     *   NTYPE(7)*8, CHTM12*12, TITLE*48, UNITS(MXSTCL)*8
      HOLLERITH HOLTMP(6), XNAMIN(3), XCLSIN(2), STLABL(6)
      DOUBLE PRECISION XPOS, YPOS, ZPOS
      REAL      DSKIN, SEQIN, XINVER, XOUVER, FLUX, XCTYPE(4), DXPOS,
     *   DYPOS, POSANG, STTYPE, RECORD(50), ICTYPE
      INTEGER  I, IWBUFF(256), IMFIND, IMLUN, IERR, IRETCD, ISEQ, FCOL,
     *   INPRMS, ISLOT, IUSER, IVOL, IROUND, MFLUN, SLUN, HLUN, IT(3),
     *   ID(3), J, K, BUF(512), XBUF(256), NKEY, NREC, NCOL, OUVER,
     *   ICOL, IER, IRNO, INVER, NMF, NST, IOBUF(768), PCOL, WCOL,
     *   DEPTH(5)
      LOGICAL   T, F, SAVE, QUICK, DOPIXR
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:PSTD.INC'
      COMMON /INPARM/ XNAMIN, XCLSIN, SEQIN, DSKIN, XINVER, XOUVER,
     *   FLUX, XCTYPE
      COMMON /MF2STD/ XPOS, YPOS, DXPOS, DYPOS, POSANG, STTYPE, STLABL
      DATA IMLUN, MFLUN, SLUN, HLUN /16, 27, 28, 29/
      DATA PRGNAM /'MF2ST '/
      DATA TYPIN /'MA'/
      DATA T, F /.TRUE.,.FALSE./
      DATA TTITLE /'AIPS ST star positions table'/
      DATA ATYPE /'TIME','FREQ','LAMB','VELO','FELO','    ','DIST',
     *   'ANGL','ELON','ELAT', 'GLON','GLAT','RA  ','RA--','LL  ',
     *   'DEC ','DEC-','MM  '/
      DATA NTYPE /'SECONDS ', 'HERTZ   ', 'METERS  ',
     *   'METR/SEC', 'METR/SEC', 'PIXELS  ', 'DEGREES '/
C-----------------------------------------------------------------------
C                                       Initialize the IO parameters.
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      SAVE = F
      CALL FILL (5, 1, DEPTH)
C                                       Get input values from AIPS.
      INPRMS = 14
      IRETCD = 0
      CALL GTPARM (PRGNAM, INPRMS, QUICK, XNAMIN, IWBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         IRETCD = 8
         END IF
      IF (QUICK) CALL RELPOP (IRETCD, IWBUFF, IERR)
      IF (IRETCD.NE.0) GO TO 990
      IRETCD = 8
C                                       Hollerith -> char
      CALL H2CHR (12, 1, XNAMIN, NAMIN)
      CALL H2CHR (6, 1, XCLSIN, CLSIN)
C
      ISEQ = IROUND (SEQIN)
      IVOL = IROUND (DSKIN)
      IUSER = NLUSER
      OUVER = IROUND (XOUVER)
      INVER = IROUND (XINVER)
      NMF = IROUND (XCTYPE(2))
      ICTYPE = IROUND (XCTYPE(1))
      ICTYPE = MAX (1.0, MIN (24.0, ICTYPE))
C                                       Open map file & get header.
 20   CALL MAPOPN ('HDWR', IVOL, NAMIN, CLSIN, ISEQ, TYPIN, IUSER,
     *   IMLUN, IMFIND, ISLOT, CATBLK, IWBUFF, IERR)
      IF (IERR.NE.0) GO TO 990
      LOCNUM = 1
      CALL SETLOC (DEPTH, .FALSE.)
C                                       get MF file open
      CALL FNDEXT ('MF', CATBLK, I)
      IF (I.LE.0) THEN
         MSGTXT = 'THERE APPEARS TO BE NO MF FILE - SORRY'
         CALL MSGWRT (7)
         GO TO 980
         END IF
      IF ((INVER.LE.0) .OR. (INVER.GT.I)) INVER = I
C                                       Open CC or MF table file
      NKEY = 0
      NCOL = 0
      NREC = 0
      CALL TABINI ('READ', 'MF', IVOL, ISLOT, INVER, CATBLK, MFLUN,
     *   NKEY, NREC, NCOL, IOBUF(513), IOBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, INVER
         CALL MSGWRT (7)
         GO TO 980
         END IF
      NST = IOBUF(5)
      IF (NMF.GT.0) NST = MIN (NST, NMF)
      CALL FNDCOL (1, 'I FLUX', 6, .FALSE., IOBUF, FCOL, IERR)
      CALL FNDCOL (1, 'DELTAX', 6, .FALSE., IOBUF, PCOL, IERR)
      CALL FNDCOL (1, 'MAJOR AX', 8, .FALSE., IOBUF, WCOL, IERR)
C                                       Fill in defaults in PARMS
      DSKIN = IVOL
      XINVER = INVER
C                                       Create/open ST file
      NCOL = MXSTCL
      NKEY = 1
      NREC = 50
C                                       data types: 2 D, 4 R, Chars
      CALL FILL (256, 0, XBUF)
C                                       First two entries are Ra+Dec
      XBUF(129) = 11
      XBUF(130) = 11
C                                       Second two are Star size
      XBUF(131) = 12
      XBUF(132) = 12
C                                       Fifth is orientation
      XBUF(133) = 12
C                                       Sixth is type (cross, ellipse)
      XBUF(134) = 12
C                                       Seventh is Character Star label
      XBUF(135) = (MXSTLB*10) + 3
C                                       create/open
      CALL TABINI ('WRIT', 'ST', IVOL, ISLOT, OUVER, CATBLK, SLUN, NKEY,
     *    NREC, NCOL, XBUF, BUF, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1030) IERR, OUVER
         CALL MSGWRT (8)
         GO TO 970
         END IF
C                                       table title
      CALL CHR2H (28, TTITLE, 1, BUF(101))
C                                       write column titles
      TITLE = ' '
      DO 45 ICOL = 1,NCOL
         IRNO = ICOL
         J = MOD (ICOL-1, 2) * 2
C                                       Star coordinates
         IF ((ICOL.EQ.1) .OR. (ICOL.EQ.2)) THEN
            CALL H2CHR (8, 1, CATH(KHCTP+J), TITLE)
            END IF
C                                       Star Position Angle
         IF (ICOL.EQ.3) TITLE = 'MAJOR AX'
         IF (ICOL.EQ.4) TITLE = 'MINOR AX'
         IF (ICOL.EQ.5) TITLE = 'POSANG'
         IF (ICOL.EQ.6) TITLE = 'STARTYPE'
         IF (ICOL.EQ.7) TITLE = 'LABEL'
         CALL CHR2H (MXSTLB, TITLE, 1, HOLTMP)
         CALL TABIO ('WRIT', 3, IRNO, HOLTMP, BUF, IERR)
         IF (IERR.NE.0) GO TO 960
 45      CONTINUE
C                                       Figure out units
      DO 55 I = 1,2
         J = (I-1) * 2 + KHCTP
         UNITS(I) = ' '
         DO 50 K = 1,18
            CALL H2CHR (4, 1, CATH(J), CHTM12)
            IF (CHTM12(1:4).EQ.ATYPE(K)(1:4)) THEN
               IF (K.LT.7) UNITS(I) = NTYPE(K)
               IF (K.GE.7) UNITS(I) = NTYPE(7)
               J = I + 2
               UNITS(J) = UNITS(I)
               GO TO 55
               END IF
 50         CONTINUE
 55      CONTINUE
C                                       Position angle is degrees
      UNITS(5) = NTYPE(7)
C                                       Star type is an index
C                                       0=cross, 1=elipse, 2=none
      UNITS(6) = 'INDEX   '
C                                       Lable  is an arbitary string
      UNITS(7) = 'STRING  '
C                                       write units
      DO 60 ICOL = 1,NCOL
         IRNO = ICOL
         CALL CHR2H (8, UNITS(ICOL), 1, HOLTMP)
         CALL TABIO ('WRIT', 4, IRNO, HOLTMP, BUF, IERR)
         IF (IERR.NE.0) GO TO 960
 60      CONTINUE
      IRNO = 0
C                                       Read record loop
      STTYPE = ICTYPE
      DOPIXR = XCTYPE(4).GT.XCTYPE(3)
      DO 100 I = 1,NST
         CALL TABIO ('READ', 0, I, RECORD, IOBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR
            CALL MSGWRT (8)
            GO TO 960
            END IF
         IF (RECORD(FCOL).GE.FLUX) THEN
            DXPOS = RPLOC(1,LOCNUM) + RECORD(PCOL)/AXINC(1,LOCNUM)
            DYPOS = RPLOC(2,LOCNUM) + RECORD(PCOL+1)/AXINC(2,LOCNUM)
            CALL XYVAL (DXPOS, DYPOS, XPOS, YPOS, ZPOS, IERR)
            IF (IERR.NE.0) GO TO 100
            DXPOS = RECORD(WCOL)
            DYPOS = RECORD(WCOL+1)
            POSANG = RECORD(WCOL+2)
            IF (DOPIXR) THEN
               STTYPE = (ICTYPE-1.0) * (RECORD(FCOL)-XCTYPE(3)) /
     *            (XCTYPE(4)-XCTYPE(3)) + 1.0
               STTYPE = IROUND (STTYPE)
               STTYPE = MAX (1.0, MIN (STTYPE, ICTYPE))
               END IF
            IF (RECORD(FCOL).GT.90.) THEN
               WRITE (SBUF,1101) RECORD(FCOL)
            ELSE IF (RECORD(FCOL).GT.9.) THEN
               WRITE (SBUF,1102) RECORD(FCOL)
            ELSE
               WRITE (SBUF,1103) RECORD(FCOL)
               END IF
            CALL CHR2H (24, SBUF, 1, STLABL)
            IRNO = IRNO + 1
            CALL TABIO ('WRIT', 0, IRNO, XPOS, BUF, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1150) IERR
               CALL MSGWRT (8)
               GO TO 960
               END IF
            END IF
 100     CONTINUE
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
C                                       FXHDEX already called > once
      DO 965 I = 1,KIEXTN
         J = I - 1
         CALL H2CHR (2, 1, CATH(KHEXT+J), CHTM12)
         IF (CHTM12(1:2).EQ.'ST') THEN
            IF (OUVER.EQ.CATBLK(KIVER+J)) CATBLK(KIVER+J) =
     *         CATBLK(KIVER+J) - 1
            SAVE = T
            GO TO 970
            END IF
 965     CONTINUE
C                                       Close the text file
 970  CALL TABIO ('CLOS', 0, I, RECORD, IOBUF, IERR)
C                                       Close map file.
 980  CALL MAPCLS ('WRIT', IVOL, ISLOT, IMLUN, IMFIND, CATBLK, SAVE,
     *   IWBUFF, IERR)
C
 990  CALL DIETSK (IRETCD, QUICK, IWBUFF)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR GETTING PARAMETERS FROM AIPS. GTPARM ERR =',I5)
 1010 FORMAT ('ERROR',I5,' OPENING MF FILE VERSION',I4)
 1030 FORMAT ('ERROR',I4,' CREATING/OPENING ST FILE VERSION',I4)
 1100 FORMAT ('ERROR',I4,' READING MF FILE')
 1101 FORMAT (F6.2)
 1102 FORMAT (F6.3)
 1103 FORMAT (F6.4)
 1150 FORMAT ('ERROR',I4,' WRITING ST FILE')
 1210 FORMAT ('Wrote',I6,' lines in ST file version',I4)
 1211 FORMAT (A6,' Version=',I3,' Lines=',I5,
     *   '  / Star file created ',A,A)
      END
