      PROGRAM STARS
C-----------------------------------------------------------------------
C! Task converts RUN file of point positions and errors into ST ext.
C# EXT-util Map-util Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2006, 2009, 2011-2013, 2016
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
C   STARS will convert a RUN file containing the positions and
C   uncertainties of points into a ST extension file attached to an
C   AIPS image.  The ST file will be in standard tables format.
C    INPUTS:   (from AIPS)
C              INNAME   R(3)   name of primary file.
C              INCLASS  R(2)   class of primary file.
C              INSEQ    R   sequence number of primary file.
C              INDISK   R    disk volume number. 0 means try all.
C              OUTVERS  R    ST file version number.
C              INFILE   C*48   Input RUN file name
C              EPOCHI   R   Input  Epoch of coordinates for stars
C              EPOCHO   R   Output Epoch of coordinates for stars
C-----------------------------------------------------------------------
C                                       Max Number Columns, Label Length
      INTEGER MXSTCL, MXSTLB
      PARAMETER (MXSTCL=7, MXSTLB=24)
C                                       Number of coordinate systems
      CHARACTER TABEP1*8, TABEP2*8, CRD1*40, CRD2*40, LABEP1*80,
     *   LABEP2*80
      CHARACTER KARBUF*80, PRGNAM*6, HILINE*72, NAMIN*12, CLSIN*6,
     *   INFILE*48, TYPIN*2, ATIME*8, ADATE*12, STFILE*48, SBUF*80,
     *   ATYPE(18)*4, CHTM12*12, STRCHR*24
      HOLLERITH HOLTMP(6), XNAMIN(3), XCLSIN(2), XINFIL(12)
      DOUBLE PRECISION STXY(2), XIN, YIN, ROTN, CRDPRM(11)
      REAL      DSKIN, SEQIN, XOUVER, EPOCHI, EPOCHO, DECIML, STWID(3)
      INTEGER  I, IWBUFF(256), IMFIND, IMLUN, IERR, IRETCD, ISEQ, IER,
     *   INPRMS, ISLOT, IUSER, IVOL, IROUND, RLUN, SLUN, HLUN, IT(3),
     *   ID(3), J, K, NKEY, NCOL, RIND, OUVER, ICTYPE(MXSTCL), IRNO,
     *   IEPOCH, OEPOCH, DEPTH(5), BUFFER(512), STKOLS(MXSTCL), INMVER,
     *   STNUMV(MXSTCL), ISTRNO, STTYPE, JT, JTRIM
      LOGICAL   T, F, SAVE, QUICK
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:PSTD.INC'
      COMMON /INPARM/ XNAMIN, XCLSIN, SEQIN, DSKIN, XOUVER, XINFIL,
     *   DECIML, EPOCHI, EPOCHO
      DATA IMLUN, RLUN, SLUN, HLUN /16,10,28,29/
      DATA PRGNAM /'STARS '/
      DATA TYPIN /'  '/
      DATA T, F /.TRUE.,.FALSE./
      DATA ATYPE /'TIME','FREQ','LAMB','VELO','FELO','    ','DIST',
     *   'ANGL','ELON','ELAT', 'GLON','GLAT','RA  ','RA--','LL  ',
     *   'DEC ','DEC-','MM  '/
C-----------------------------------------------------------------------
C                                       Initialize the IO parameters.
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      SAVE = F
C                                       Get input values from AIPS.
      INPRMS = 23
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
      CALL H2CHR (48, 1, XINFIL, INFILE)
      ISEQ = IROUND (SEQIN)
      IVOL = IROUND (DSKIN)
      IUSER = NLUSER
      OUVER = IROUND (XOUVER)
C                                       Set input  epoch
      CALL STEPCH ( EPOCHI, IEPOCH, TABEP1, CRD1, LABEP1)
C                                       Set output epoch
      CALL STEPCH ( EPOCHO, OEPOCH, TABEP2, CRD2, LABEP2)
C                                       Open map file & get header.
      CALL MAPOPN ('HDWR', IVOL, NAMIN, CLSIN, ISEQ, TYPIN, IUSER,
     *   IMLUN, IMFIND, ISLOT, CATBLK, IWBUFF, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       coordinate types
      LOCNUM =  1
      CALL FILL (5, 1, DEPTH)
      CALL SETLOC (DEPTH, .FALSE.)
      IF (AXTYP(LOCNUM).NE.1) THEN
         MSGTXT = 'NOT COORDINATE PAIR - NO PRECESSION'
         IF (CRD1.NE.CRD2) CALL MSGWRT (7)
         CRD2 = CRD1
         END IF
C                                       input max ver
      CALL FNDEXT ('ST', CATBLK, INMVER)
C                                       If a transformation
      IF (CRD1.NE.CRD2) THEN
C                                       Describe Input
         MSGTXT = 'Input  ' // LABEP1
         CALL MSGWRT (3)
C                                       Describe Output
         MSGTXT = 'Output ' // LABEP2
         CALL MSGWRT (3)
C                                       Set up for transform
         CALL CRDSET ( CRD1, CRD2, CRDPRM, IERR)
         END IF
C                                       Fill in defaults in PARMS
      DSKIN = IVOL
C                                       Open text file.
      CALL ZTXOPN ('READ', RLUN, RIND, INFILE, F, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         CALL MSGWRT (8)
         GO TO 980
         END IF
C                                       Create/open ST file
      CALL STINI ('WRIT', BUFFER, IVOL, ISLOT, OUVER, CATBLK, SLUN,
     *   ISTRNO, STKOLS, STNUMV, IERR)
      NCOL = MXSTCL
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1030) IERR, OUVER
         CALL MSGWRT (8)
         GO TO 970
         END IF
C                                       Put coordinate keyword in header
      IF (CRD1.NE.CRD2) THEN
         CALL CHR2H (8, TABEP2, 1, HOLTMP)
         NKEY = 1
         CALL TABKEY ('WRIT', 'TABEPOCH', NKEY, BUFFER, 1, HOLTMP, 3,
     *      IERR)
         END IF
C                                       Figure out units
      DO 55 I = 1,2
         ICTYPE(I) = 1
C                                       skip if degrees
         J = (I-1) * 2 + KHCTP
         DO 50 K = 1,18
            CALL H2CHR (4, 1, CATH(J), CHTM12)
            IF (CHTM12(:4).EQ.ATYPE(K)(:4)) THEN
               IF (K.GT.12) ICTYPE(I) = 2
               IF (K.GT.15) ICTYPE(I) = 3
               GO TO 55
               END IF
 50         CONTINUE
 55      CONTINUE
      ICTYPE(4) = -ICTYPE(1)
      ICTYPE(3) = -ICTYPE(2)
      IF (DECIML.GE.1.5) THEN
         IF (ICTYPE(1).EQ.2) ICTYPE(1) = 4
         IF (ICTYPE(1).EQ.3) ICTYPE(1) = 5
         IF (ICTYPE(2).EQ.2) ICTYPE(2) = 4
         IF (ICTYPE(2).EQ.3) ICTYPE(2) = 5
      ELSE IF (DECIML.GT.0.0) THEN
         IF (ICTYPE(1).EQ.2) ICTYPE(1) = 1
         IF (ICTYPE(1).EQ.3) ICTYPE(1) = 1
         IF (ICTYPE(2).EQ.2) ICTYPE(2) = 1
         IF (ICTYPE(2).EQ.3) ICTYPE(2) = 1
         END IF
C                                       Position angle is degrees
      ICTYPE(5) = 10
C                                       Star type is an index
      ICTYPE(6) = 10
C                                       Lable  is an arbitary string
      ICTYPE(7) = 10
      IRNO = 0
C                                       Read record loop
 100  CALL ZTXIO ('READ', RLUN, RIND, SBUF, IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.2)) THEN
         WRITE (MSGTXT,1100) IERR
         CALL MSGWRT (8)
         GO TO 960
      ELSE IF (IERR.EQ.0) THEN
         JT = JTRIM (SBUF)
         IF ((SBUF.EQ.' ') .OR. (SBUF(:1).EQ.'#')) GO TO 100
         IRNO = IRNO + 1
C                                       Parse the input line
         KARBUF(1:80) = SBUF(1:80)
         CALL STCARD (KARBUF, MXSTLB, MXSTCL, ICTYPE, CATD(KDCRV),
     *      CATR(KRCIC), STXY(1), STXY(2), STWID(1), STWID(2), STWID(3),
     *      STTYPE, STRCHR, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1115) IRNO
            CALL MSGWRT (7)
            GO TO 100
            END IF
         IF (AXTYP(LOCNUM).NE.1) THEN
             STWID(3) = 0.0
C                                       If input not output epoch
         ELSE IF (CRD1.NE.CRD2) THEN
C                                       copy input coords
            XIN = STXY(1)
            YIN = STXY(2)
C                                       Transform coordinates
            CALL CRDTRN (XIN, YIN, CRDPRM, STXY(1), STXY(2), ROTN)
C                                       Tell user about transform
            IF (IRNO.EQ.1) THEN
               MSGTXT = 'Transformation of first star coordinates:'
               CALL MSGWRT(3)
               WRITE (MSGTXT, 1050) 'Input  ', XIN, YIN
               CALL MSGWRT(3)
               WRITE (MSGTXT, 1050) 'Output ', STXY(1), STXY(2)
               CALL MSGWRT(3)
               END IF
            END IF
         CALL TABST ('WRIT', BUFFER, ISTRNO, STKOLS, STNUMV, STXY,
     *      STWID, STTYPE, STRCHR, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1150) IERR
            CALL MSGWRT (8)
            GO TO 960
            END IF
         GO TO 100
         END IF
C                                       Normal EOF
      CALL TABST ('CLOS', BUFFER, ISTRNO, STKOLS, STNUMV, STXY, STWID,
     *   STTYPE, STRCHR, IERR)
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
 960  CALL ZCLOSE (SLUN, BUFFER(82), IERR)
      IF (OUVER.GT.INMVER) THEN
         CALL H2CHR (24, 1, BUFFER(17), STFILE)
         CALL ZDESTR (IVOL, STFILE, IERR)
C                                       TABINI called FXHDEX for us
         DO 965 I = 1,KIEXTN
            J = I - 1
            CALL H2CHR (2, 1, CATH(KHEXT+J), CHTM12)
            IF (CHTM12(:2).EQ.'ST') THEN
               IF (OUVER.EQ.CATBLK(KIVER+J)) CATBLK(KIVER+J) =
     *            CATBLK(KIVER+J) - 1
               SAVE = T
               GO TO 970
               END IF
 965        CONTINUE
         END IF
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
 1030 FORMAT ('ERROR',I4,' CREATING/OPENING ST FILE VERSION',I4)
 1050 FORMAT (A,D14.5,',',D14.5,' Degrees')
 1100 FORMAT ('ERROR',I4,' READING TEXT FILE')
 1115 FORMAT ('ERROR PARSING LINE',I6,': LINE IGNORED')
 1150 FORMAT ('ERROR',I4,' WRITING ST FILE')
 1210 FORMAT ('Wrote',I6,' lines in ST file version',I4)
 1211 FORMAT (A6,' Version=',I3,' Lines=',I5,
     *   '  / Star file created ',A,A)
      END
      SUBROUTINE STCARD (KARBUF, MXSTLB, MXSTCL, ICTYPE, RADEC, CELLS,
     *   XPOS, YPOS, DXPOS, DYPOS, POSANG, STTYPE, STRCHR, IERR)
C-----------------------------------------------------------------------
C   Decodes one star card.
C   Input:
C      KARBUF   C*80    Input card to be decoded
C      MXSTLB   I       Max length of star label
C      MXSTCL   I       Max number of star file columns
C      ICTYPE   I*(?)   Types of the star columns
C      RADEC    D*(2)   Ra and Dec of image center (degrees)
C      CELLS    R*(2)   Pixel size (degrees)
C   Output:
C      XPOS     D       Star location (degrees)
C      YPOS     D       Star location (degrees)
C      DXPOS    R       Star size (degrees)
C      DYPOS    R       Star size (degrees)
C      POSANG   R       Star Orientation (degrees)
C      STTYPE   I       Star Type index: 0 -> +1 or -1 based on STRCHR
C      STRCHR   C*24    Star label
C      IERR     I       Error code 0=> Ok
C-----------------------------------------------------------------------
      CHARACTER KARBUF*80, STRCHR*24
      INTEGER   MXSTCL, MXSTLB, ICTYPE(*), STTYPE, IERR
      DOUBLE PRECISION RADEC(2), XPOS, YPOS
      REAL      CELLS(2), DXPOS, DYPOS, POSANG
c
      CHARACTER M*1, TMPCHR*24
      DOUBLE PRECISION DX(10), DDX
      INTEGER   IS, J, ICOL, KBPTR, JTRIM, NONBLK
      LOGICAL   ISNUMB
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      KBPTR = 0
      IERR  = 0
C                                       Set Defaults
      CALL DFILL (MXSTCL, 0.0D0, DX)
      STRCHR = ' '
C                                       For all columns in ST table
      DO 140 ICOL = 1,MXSTCL
         IS = 1
         J = 1
C                                       Find next non-blank
 115     KBPTR = KBPTR + 1
C                                       End of line reached
         IF (KBPTR.GT.80) THEN
            IF (ICOL.EQ.3) THEN
               DX(ICOL)   = ABS(CELLS(ICOL-2))
               DX(ICOL+1) = ABS(CELLS(ICOL-1))
            ELSE IF (ICOL.EQ.4) THEN
               DX(ICOL)   = ABS(CELLS(ICOL-2))
C                                       IF not enough colums, error
            ELSE IF (ICOL.LT.3) THEN
               IERR = ICOL
               END IF
            GO TO 990
            END IF
C                                       Else, not at end get next char
         M = KARBUF(KBPTR:KBPTR)
C                                       Find a non-blank
         IF (M.EQ.' ') GO TO 115
C                                       Declination leading sign
         IF ((J.EQ.1) .AND. (ICTYPE(ICOL).EQ.3)) THEN
            J = J + 1
C                                       If a sign
            IF ((M.EQ.'-') .OR. (M.EQ.'+')) THEN
               IF (M.EQ.'-') IS = -1
               GO TO 115
               END IF
C                                       If first char of declination
            END IF
C                                       IF char is number
         ISNUMB = M.EQ.'0'.OR.M.EQ.'1'.OR.M.EQ.'2'.OR.M.EQ.'3'.OR.
     *            M.EQ.'4'.OR.M.EQ.'5'.OR.M.EQ.'6'.OR.M.EQ.'7'.OR.
     *            M.EQ.'8'.OR.M.EQ.'9'.OR.M.EQ.'-'.OR.M.EQ.'+'.OR.
     *            M.EQ.'.'
C                                       IF label column or not a number
         IF (ICOL.EQ.MXSTCL .OR. .NOT. ISNUMB) THEN
C                                       Transfer Star label chars
            TMPCHR = KARBUF(KBPTR:MIN(KBPTR+MXSTLB-1,80))
C                                       Find number of non-blanks
            NONBLK = JTRIM(TMPCHR)
            STRCHR = TMPCHR(1:NONBLK)
            KBPTR  = 80
C                                       Else get the number
         ELSE
            CALL GETNUM (KARBUF, 80, KBPTR, DDX)
            IF (DDX.EQ.DBLANK) THEN
               MSGTXT = 'STCARD: BAD NUMBER ON CARD'
               CALL MSGWRT (8)
               IERR = 1
               GO TO 999
               END IF
            IF (ICTYPE(ICOL).LE.1) THEN
               DX(ICOL) = DDX
               IF (ICTYPE(ICOL).LT.-1) DX(ICOL) = DX(ICOL) / 3.6D3
               IF (ICOL.GT.2) THEN
                  DX(ICOL) = ABS (DX(ICOL))
                  IF (DX(ICOL).LE.0.0D0) DX(ICOL) = ABS(CELLS(ICOL-3))
                  END IF
C                                       RA sexagesimal
            ELSE IF (ICTYPE(ICOL).EQ.2) THEN
               IF (J.EQ.1) DX(ICOL) = DDX
               IF (J.EQ.2) DX(ICOL) = DX(ICOL) + DDX / 60.0D0
               IF (J.EQ.3) DX(ICOL) = DX(ICOL) + DDX / 3600.0D0
               J = J + 1
               IF (J.LE.3) GO TO 115
               DX(ICOL) = DX(ICOL) * 15.0D0
               IF (ABS(DX(ICOL)-RADEC(ICOL)).GT.180.0D0) THEN
                  IF (DX(ICOL)-RADEC(ICOL).GT.180.0D0)
     *               DX(ICOL) = DX(ICOL) - 360.0D0
                  IF (DX(ICOL)-RADEC(ICOL).LT.-180.0D0)
     *               DX(ICOL) = DX(ICOL) + 360.0D0
                  END IF
C                                       declination sexagesimal
            ELSE IF (ICTYPE(ICOL).EQ.3) THEN
               IF (J.EQ.2) DX(ICOL) = DDX
               IF (J.EQ.3) DX(ICOL) = DX(ICOL) + DDX / 60.0D0
               IF (J.EQ.4) DX(ICOL) = DX(ICOL) + DDX / 3600.0D0
               J = J + 1
               IF (J.LE.4) GO TO 115
               DX(ICOL) = DX(ICOL) * IS
C                                       RA offset
            ELSE IF (ICTYPE(ICOL).EQ.4) THEN
               DX(ICOL) = RADEC(ICOL) + DDX / 3600.0D0 /
     *            COS(DG2RAD * RADEC(3-ICOL))
C                                       DEC offset
            ELSE IF (ICTYPE(ICOL).EQ.5) THEN
               DX(ICOL) = RADEC(ICOL) + DDX / 3600.0D0
C                                       Save number for later
            ELSE
               DX(ICOL) = DDX
               END IF
            END IF
 140     CONTINUE
C                                       prepare to transfer to ST file
 990  XPOS = DX(1)
      IF (XPOS.LT.0.0) XPOS = XPOS + 360.0
      YPOS = DX(2)
      DXPOS = DX(3)
      DYPOS = DX(4)
      POSANG = DX(5)
      STTYPE = DX(6)
      IF (STTYPE.EQ.0) THEN
         IF (STRCHR.NE.' ') THEN
            STTYPE = -1
         ELSE
            STTYPE = 1
            END IF
         END IF
C
 999  RETURN
      END
