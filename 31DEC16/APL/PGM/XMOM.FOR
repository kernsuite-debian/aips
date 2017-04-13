LOCAL INCLUDE 'XMBUFRS'
      INCLUDE 'INCS:PMAD.INC'
      REAL     BUFF1(MABFSS), BUFF2(MABFSS), SCRTCH(512)
      COMMON /BUFRS/ BUFF1, BUFF2, SCRTCH
LOCAL END
LOCAL INCLUDE 'XMOM.INC'
      INCLUDE 'XMBUFRS'
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, OPTYPE*4
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XOPTYP, CATOH(256)
      REAL      XSEQIN, XDISKI, XSEQO, XDISKO, BLC(7), TRC(7),
     *   FCUT, ACUT, PBPARM(7), BADD(10)
      DOUBLE PRECISION CATOD(128), OFFS, FV, RV, DV, RA0, DE0
      REAL      CATOR(256), PMIN(5), PMAX(5), FI, RI, DI, FR, RR, DR,
     *   MROT
      INTEGER   CATOLD(256), SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO,
     *   OLDCNO, JBUFSZ, FAX, RAX, DAX
      LOGICAL   BIV
      COMMON /INPARM/ XNAMEI, XCLAIN, XSEQIN, XDISKI, XNAMOU, XSEQO,
     *   XDISKO, BLC, TRC, FCUT, ACUT, XOPTYP, PBPARM, BADD
      COMMON /CHPARM/ NAMEIN, CLAIN, NAMOUT, OPTYPE
      COMMON /PARMS/ CATOLD, OFFS, FV, RV, DV, RA0, DE0, PMAX, PMIN,
     *   SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO, OLDCNO, JBUFSZ, BIV, FI,
     *   RI, DI, FR, RR, DR, FAX, RAX, DAX, MROT
      EQUIVALENCE (CATOLD, CATOR, CATOH, CATOD)
LOCAL END
      PROGRAM XMOM
C-----------------------------------------------------------------------
C! XMOM fits 1-D moments to rows of an image, writes out results.
C# Map-util Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2000, 2002-2003, 2005, 2008, 2011, 2015
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
C   XMOM fits 1-dimensional moments to rows of an image.  It fits 4
C   moments and writes out 4 n-1 dimensional images of the results.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input image.
C      INCLASS        CLAIN         Class of input image.
C      INSEQ          SEQIN         Seq. of input image.
C      INDISK         DISKIN        Disk number of input image.
C      OUTNAME        NAMOUT        Name of the output image
C                                   Default output is input image.
C      OUTSEQ         SEQOUT        Seq. number of output image.
C      OUTDISK        DISKO         Disk number of the output image.
C      BLC(7)         BLC           Bottom left corner of subimage
C                                   of input image.
C      TRC(7)         TRC           Top right corner of subimage.
C      FLUX           FCUT          Flux cutoff: use only data >
C                                   FLUX.
C      ICUT           ACUT          Abs(data) cutoff: if > 0, drop
C                                   data < ICUT in abs value.  If
C                                   < 0, drop data > ICUT in abs.
C      OPTYPE         XOPTYP        '': Blank illegal velocities;
C                                   'NBIV': No Blank Illegal Velocities
C      BADD(10)       IBAD          Disk numbers to avoid.
C   Programmer Eric W. Greisen  September 1983
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      LOGICAL   ABLANK
      INCLUDE 'XMOM.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'XMOM  '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL XMOMIN (PRGM, IRET)
C                                       Call routine that sends data
C                                       to the user routine.
      IF (IRET.EQ.0) CALL XMOMDO (ABLANK, IRET)
      IF (IRET.EQ.0) CALL XMOMOU (ABLANK, IRET)
C                                       Close down files, etc.
      CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE XMOMIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   XMOMIN gets input parameters for XMOMS.
C   Inputs:  PRGN    C*6       Program name (2 chars/word)
C   Output:  IRET    I         Error code: 0 => ok
C                                4 => user routine detected error.
C                                5 => catalog troubles
C                                8 => can't start
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      CHARACTER   STAT*4, PRGN*6, OTYPE*8, CLAOUT*6, CTYPE(19)*4,
     *   CUNITS(9)*8, BUNITS(9)*4, SEQTYP(5)*8, CTEMP*8, MTYPE*2,
     *   C1TYP*8, SMTYP(2)*8
      INTEGER  IRET, IPT, I, IERR, NPARM, IROUND, IG, INC, ITYP,
     *   NAX, NTYP
      DOUBLE PRECISION V1, V2, VD, C1CRV, VSIGN
      REAL      CONST, C1CRP, C1CIC, C1CRT
      INCLUDE 'XMOM.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA NTYP, CTYPE /19, 'TIME','FREQ','LAMB','VELO','FELO','    ',
     *   'PIXE','DIST','ANGL','RA  ','RA--','LL  ','DEC ','DEC-',
     *   'MM  ','GLON','GLAT','ELON','ELAT'/
      DATA CUNITS /'SECONDS', 'HERTZ', 'METERS',
     *   'METR/SEC', 'METR/SEC', 'PIXELS', 'PIXELS',
     *   'DEGREES', 'UNKNOWN?'/
      DATA BUNITS /'*SEC', '*HZ ', '*M  ', '*M/S', '*M/S', '*PIX',
     *   '*PIX', '*DEG', '*UNK'/
      DATA SEQTYP /'XMOMNC', 'XMOM0', 'XMOM1', 'XMOM2', 'XMOM3'/
      DATA SMTYP /'MAX B', 'MAX XV'/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      JBUFSZ = 2 * MABFSS
      IRET = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
C                                       Get input parameters.
C                                       Fixed PPM 1996.09.30: was 38
      NPARM = 46
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, SCRTCH, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Crunch input parameters.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (12, 1, XOPTYP, OPTYPE)
C                                       BLank Illegal Velocities?
      BIV = (OPTYPE.NE.'NBIV')
      SEQIN = IROUND (XSEQIN)
      SEQOUT = IROUND (XSEQO)
      DISKIN = IROUND (XDISKI)
      DISKO = IROUND (XDISKO)
      DO 20 I = 1,10
         IBAD(I) = IROUND (BADD(I))
 20      CONTINUE
C                                       Get CATBLK from old file.
      OLDCNO = 1
      MTYPE = 'MA'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, MTYPE,
     *   NLUSER, STAT, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
C                                       Read CATBLK and mark 'READ'.
      CALL CATIO ('READ', DISKIN, OLDCNO, CATOLD, 'READ', SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
C                                       Copy old CATBLK to new.
      CALL COPY (256, CATOLD, CATBLK)
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, '      ', NAMOUT, CLAOUT,
     *   SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CATBLK(KIIMS) = SEQOUT
C                                       Set defaults on BLC,TRC
      CALL WINDOW (CATOLD(KIDIM), CATOLD(KINAX), BLC, TRC, IERR)
      IF (IERR.NE.0) GO TO 999
      V1 = CATOD(KDCRV) + CATOR(KRCIC) * (BLC(1) - CATOR(KRCRP))
      V2 = CATOD(KDCRV) + CATOR(KRCIC) * (TRC(1) - CATOR(KRCRP))
      VD = 2.0D0 * ABS ((V2 - V1) / (MAX (1.D-10, V1+V2)))
      IF (VD.GT.0.001) THEN
         OFFS = 0.0D0
      ELSE
         OFFS = (V1 + V2) / 2.0D0
         WRITE (MSGTXT,1050) OFFS
         CALL MSGWRT (7)
         END IF
C                                       Get user modification to CATBLK
      IRET = 4
      CALL MOMHED (IRET)
      IF (IRET.NE.0) GO TO 999
      NEWCNO = 0
C                                       Make names, classes, disks OK.
      CALL H2CHR (12, KHIMNO, CATH(KHIMN), NAMOUT)
      CONST = -1.E15
      CALL RFILL (5, CONST, PMAX)
      CONST = -CONST
      CALL RFILL (5, CONST, PMIN)
C                                       create output files in advance
C                                       Basic output header
      NAX = CATBLK(KIDIM) - 1
      INC = 2
C                                       save averaged axis to top
      C1CRP = (CATR(KRCRP) - 1.5) / CATBLK(KINAX) + 0.5
      C1CIC = CATR(KRCIC) * CATBLK(KINAX)
      C1CRT = CATR(KRCRT)
      C1CRV = CATD(KDCRV)
      CALL H2CHR (8, 1, CATH(KHCTP), C1TYP)
C                                       move other axes down
      DO 60 I = 1,NAX
         CATBLK(KINAX+I-1) = CATBLK(KINAX+I)
         CATR(KRCRP+I-1) = CATR(KRCRP+I)
         CATR(KRCRT+I-1) = CATR(KRCRT+I)
         CATR(KRCIC+I-1) = CATR(KRCIC+I)
         CATD(KDCRV+I-1) = CATD(KDCRV+I)
         IPT = KHCTP+I*INC
         CALL H2CHR (8, 1, CATH(IPT), CTEMP)
         IPT = KHCTP+(I-1)*INC
         CALL CHR2H (8, CTEMP, 1, CATH(IPT))
 60      CONTINUE
      CATR(KRCRP+NAX) = C1CRP
      CATR(KRCRT+NAX) = C1CRT
      CATR(KRCIC+NAX) = C1CIC
      CATD(KDCRV+NAX) = C1CRV
      IPT = KHCTP + NAX*INC
      CALL CHR2H (8, C1TYP, 1, CATH(IPT))
      DO 65 I = NAX,6
         CATBLK(KINAX+I) = 1
 65      CONTINUE
C                                       Find type of old axis
      CALL H2CHR (8, 1, CATOH(KHCTP), OTYPE)
      DO 70 ITYP = 1,NTYP
         IF (OTYPE(1:4).EQ.CTYPE(ITYP)) GO TO 75
 70      CONTINUE
      ITYP = 0
      WRITE (MSGTXT,1070) OTYPE
      CALL MSGWRT (6)
 75   IF (ITYP.GT.7) ITYP = 8
      IF (ITYP.EQ.0) ITYP = 9
C                                       creates
      IF (OPTYPE.NE.'MAX') THEN
         DO 80 IG = 1,5
            CALL CHR2H (6, SEQTYP(IG), KHIMCO, CATH(KHIMC))
            CTEMP = ' '
            CALL CHR2H (8, CTEMP, 1, CATH(KHBUN))
            IF (IG.EQ.2) THEN
               CALL H2CHR (8, 1, CATOH(KHBUN), CTEMP)
               CALL CHR2H (8, CTEMP, 1, CATH(KHBUN))
               END IF
            IF (IG.EQ.2) CALL CHR2H (4, BUNITS(ITYP), 5, CATH(KHBUN))
            IF (IG.GT.2) CALL CHR2H (8, CUNITS(ITYP), 1, CATH(KHBUN))
C                                       Create
            DISKO = XDISKO + 0.01
            NEWCNO = 1
            CALL MCREAT (DISKO, NEWCNO, SCRTCH, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1075) IERR, SEQTYP(IG)
               GO TO 990
               END IF
C                                       Record the creation
            NCFILE = NCFILE + 1
            FVOL(NCFILE) = DISKO
            FCNO(NCFILE) = NEWCNO
            FRW(NCFILE) = 2
            SEQOUT = CATBLK(KIIMS)
C                                       copy most keywords
            CALL KEYPCP (DISKIN, OLDCNO, DISKO, NEWCNO, 0, ' ', IERR)
 80         CONTINUE
C                                       MAX has only 2 of different type
      ELSE
         DO 90 IG = 1,2
            CALL CHR2H (6, SMTYP(IG), KHIMCO, CATH(KHIMC))
            IF (IG.EQ.2) CALL CHR2H (8, CUNITS(ITYP), 1, CATH(KHBUN))
C                                       Create
            DISKO = XDISKO + 0.01
            NEWCNO = 1
            CALL MCREAT (DISKO, NEWCNO, SCRTCH, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1075) IERR, SEQTYP(IG)
               GO TO 990
               END IF
C                                       Record the creation
            NCFILE = NCFILE + 1
            FVOL(NCFILE) = DISKO
            FCNO(NCFILE) = NEWCNO
            FRW(NCFILE) = 2
            SEQOUT = CATBLK(KIIMS)
C                                       copy most keywords
            CALL KEYPCP (DISKIN, OLDCNO, DISKO, NEWCNO, 0, ' ', IERR)
 90         CONTINUE
         END IF
      IRET = 0
C                                       PBCORR parameters
      IF (PBPARM(1).GT.0.0) THEN
         FAX = 0
         RAX = 0
         DAX = 0
         NAX = CATOLD(KIDIM)
         DO 100 I = 1,NAX
            CALL H2CHR (8, 1, CATOH(KHCTP+2*(I-1)), OTYPE)
            IF (OTYPE(:4).EQ.'FREQ') THEN
               FAX = I
               FV = CATOD(KDCRV+I-1)
               FI = CATOR(KRCIC+I-1)
               FR = CATOR(KRCRP+I-1)
            ELSE IF ((OTYPE(:4).EQ.'FELO') .OR. (OTYPE(:4).EQ.'VELO'))
     *         THEN
               FAX = I
               VSIGN = 1.0D0
               IF (OTYPE(:4).EQ.'VELO') VSIGN = -1.0D0
               FI = -CATOR(KRCIC+I-1) * CATOD(KDARV) /
     *            (VELITE + VSIGN * CATOD(KDCRV+I-1) + CATOR(KRCIC+I-1)
     *            * (CATOR(KRCRP+I-1) - CATOR(KRARP)))
               FV = CATOD(KDARV)
               FR = CATOR(KRARP)
            ELSE IF ((OTYPE(:2).EQ.'RA') .OR. (OTYPE(2:4).EQ.'LON'))
     *         THEN
               RAX = I
               RV = CATOD(KDCRV+I-1) * DG2RAD
               RI = CATOR(KRCIC+I-1) * DG2RAD
               RR = CATOR(KRCRP+I-1)
            ELSE IF ((OTYPE(:3).EQ.'DEC') .OR. (OTYPE(2:4).EQ.'LAT'))
     *         THEN
               DAX = I
               DV = CATOD(KDCRV+I-1) * DG2RAD
               DI = CATOR(KRCIC+I-1) * DG2RAD
               DR = CATOR(KRCRP+I-1)
               MROT = CATOR(KRCRT+I-1) * DG2RAD
               END IF
 100        CONTINUE
         IF (DAX*RAX*FAX.LE.0) THEN
            MSGTXT = 'FREQ/RA/DEC AXIS NOT FOUND: PBPARM TURNED OFF'
            CALL MSGWRT (7)
            PBPARM(1) = 0.0
         ELSE
            RA0 = CATOD(KDORA) * DG2RAD
            DE0 = CATOD(KDODE) * DG2RAD
            IF ((RA0.EQ.0.0D0) .AND. (DE0.EQ.0.0D0)) THEN
               RA0 = RV
               DE0 = DV
               END IF
            END IF
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('XMOMIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I2,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('WARNING: FIRST MOMENT OFFSET BY',1PE13.5)
 1070 FORMAT ('AXIS TYPE ',A8,' DOES NOT HAVE KNOWN UNITS')
 1075 FORMAT ('ERROR',I5,' CREATING FILE TYPE ',A6)
      END
      SUBROUTINE XMOMDO (TBLNKD, IRET)
C-----------------------------------------------------------------------
C   XMOMDO sends image one row at a time to the moment fitting
C   routine and then writes the modified data.
C   Output:
C      TBLNKD   L   Answers contain blanked pixels?
C      IRET     I   Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      LOGICAL   TBLNKD
      INTEGER   IRET
C
      CHARACTER IFILE*48, SFILE*48
      INTEGER   IROUND, LUNI, IERR, SIZE, NYI, NXI, WINI(4), BOI, BOO,
     *   LIM2, LIM3, LIM4, LIM5, LIM6, LIM7, I1, I2, I3, I4, I5, I6, I7,
     *   IPOS(7), CORN(7), BOTEMP, LIMIT, IBIND, INDI, I, LIM1, WINT(4),
     *   LUNT, INDT, NAXT(8), OBINDT, JBUFS3
      LOGICAL   T, F
      INCLUDE 'XMOM.INC'
      REAL      BUFFS(MABFSS)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUNI, LUNT /16,18/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      JBUFS3 = 2 * MABFSS
C                                       Open and init for read
      CALL ZPHFIL ('MA', DISKIN, OLDCNO, 1, IFILE, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, IFILE, T, F, T, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Create scratch files.
C                                       For answers: temp scratch
      CALL COPY (7, CATBLK(KINAX), NAXT(2))
      NAXT(1) = 5
      IF (OPTYPE.EQ.'MAX') NAXT(1) = 2
      I = CATBLK(KIDIM) + 1
      CALL MAPSIZ (I, NAXT, SIZE)
      CALL SCREAT (SIZE, SCRTCH, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
      CALL ZPHFIL ('SC', SCRVOL(NSCR), SCRCNO(NSCR), 1, SFILE, IERR)
      CALL ZOPEN (LUNT, INDT, SCRVOL(NSCR), SFILE, T, T, T, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1015) IRET
         GO TO 990
         END IF
C                                       Setup for I/O
C                                       remember names switched
      NXI = CATOLD(KINAX)
      NYI = CATOLD(KINAX+1)
      WINI(1) = IROUND (BLC(1))
      WINI(2) = IROUND (BLC(2))
      WINI(3) = IROUND (TRC(1))
      WINI(4) = IROUND (TRC(2))
      WINT(1) = 1
      WINT(2) = 1
      WINT(3) = NAXT(1)
      WINT(4) = NAXT(2)
C                                       Setup for looping
      LIM1 = TRC(1) - BLC(1) + 1.01
      LIM2 = TRC(2) - BLC(2) + 1.01
      LIM3 = TRC(3) - BLC(3) + 1.01
      LIM4 = TRC(4) - BLC(4) + 1.01
      LIM5 = TRC(5) - BLC(5) + 1.01
      LIM6 = TRC(6) - BLC(6) + 1.01
      LIM7 = TRC(7) - BLC(7) + 1.01
      CORN(7) = 1
      TBLNKD = .FALSE.
C                                       Loop
      DO 700 I7 = 1,LIM7
         IPOS(7) = BLC(7) + I7 - 0.9
         CORN(7) = I7
         DO 600 I6 = 1,LIM6
            IPOS(6) = BLC(6) + I6 - 0.9
            CORN(6) = I6
            DO 500 I5 = 1,LIM5
               IPOS(5) = BLC(5) + I5 - 0.9
               CORN(5) = I5
               DO 400 I4 = 1,LIM4
                  IPOS(4) = BLC(4) + I4 - 0.9
                  CORN(4) = I4
                  DO 300 I3 = 1,LIM3
                     IPOS(3) = BLC(3) + I3 - 0.9
                     CORN(3) = I3
C                                       Init. files, first input.
                     CALL COMOFF (CATOLD(KIDIM), CATOLD(KINAX), IPOS(3),
     *                  BOTEMP, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1099) IRET
                        GO TO 990
                        END IF
                     BOI = BOTEMP + 1
                     CALL MINIT ('READ', LUNI, INDI, NXI, NYI, WINI,
     *                  BUFF1, JBUFSZ, BOI, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1100) 'READ', IRET
                        GO TO 990
                        END IF
C                                       Init answer file.
                     I2 = CATBLK(KIDIM) + 1
                     CALL COMOFF (I2, NAXT, CORN(3), BOTEMP, IRET)
                     BOO = BOTEMP + 1
                     CALL MINIT ('WRIT', LUNT, INDT, NAXT, NAXT(2),
     *                  WINT, BUFFS, JBUFS3, BOO, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1100) 'WRIT', IRET
                        GO TO 990
                        END IF
                     DO 250 I2 = 1,LIM2
                        IPOS(2) = BLC(2) + I2 - 0.9
                        IPOS(1) = IROUND (BLC(1))
C                                       Read.
                        CALL MDISK ('READ', LUNI, INDI, BUFF1, IBIND,
     *                     IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1120) 'READ', IRET
                           GO TO 990
                           END IF
C                                       Write.
                        CALL MDISK ('WRIT', LUNT, INDT, BUFFS, OBINDT,
     *                     IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1120) 'WRIT', IRET
                           GO TO 990
                           END IF
C                                       Call DO1MOM
                        IF (OPTYPE.NE.'MAX') THEN
                           CALL DO1MOM (IPOS, BUFF1(IBIND),
     *                        BUFFS(OBINDT))
                        ELSE
                           CALL DOXMOM (IPOS, BUFF1(IBIND),
     *                        BUFFS(OBINDT))
                           END IF
C                                       Check max, min, blanking.
                        IF (.NOT.TBLNKD) THEN
                           LIMIT = OBINDT + WINT(3) - 1
                           DO 215 I1 = OBINDT,LIMIT
                              TBLNKD = (BUFFS(I1).EQ.FBLANK) .OR.
     *                           (TBLNKD)
 215                          CONTINUE
                           END IF
 250                    CONTINUE
C                                       Flush buffers.
                     CALL MDISK ('FINI', LUNT, INDT, BUFFS, OBINDT,
     *                  IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1120) 'FINI', IRET
                        GO TO 990
                        END IF
 300                 CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Close files
      CALL ZCLOSE (LUNI, INDI, IRET)
      CALL ZCLOSE (LUNT, INDT, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('XMOMDO: ERROR',I3,' OPENING INPUT FILE')
 1010 FORMAT ('XMOMDO: ERROR',I3,' CREATING ANSWER SCRATCH FILE')
 1015 FORMAT ('XMOMDO: ERROR',I3,' OPENING ANSWER SCRATCH FILE')
 1099 FORMAT ('XMOMDO: COMOF3 ERROR',I3)
 1100 FORMAT ('XMOMDO: INIT-FOR-',A4,' ERROR',I3)
 1120 FORMAT ('XMOMDO: ',A4,' ERROR',I3)
      END
      SUBROUTINE MOMHED (IRET)
C-----------------------------------------------------------------------
C   MOMHED modifies the new image header for the subimaging and for
C   replacing the first axis with Gaussian components.
C   Input:
C      CATBLK(256)    I     Output catalog header, also CATR, CATD,
C                           CATH
C      CATOLD(256)    I     Input catalog header, also CATOR, CATOD,
C                           CATOH
C   Output:
C      CATBLK(256)    I     Modified output catalog header.
C      IRET           I     Return error code, 0=>OK, otherwise abort.
C-----------------------------------------------------------------------
      CHARACTER FCHARS(3)*4, CHTM12*12
      INTEGER   I, IRET
      LOGICAL   EQUAL
      INCLUDE 'XMOM.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA FCHARS /'FREQ','VELO','FELO'/
C-----------------------------------------------------------------------
      IRET = 0
C                                       Check input axes
      DO 10 I = 1,3
         CALL H2CHR (4, 1, CATH(KHCTP), CHTM12)
         EQUAL = FCHARS(I) .EQ. CHTM12(1:4)
         IF (EQUAL) GO TO 20
 10      CONTINUE
      WRITE (MSGTXT,1010)
      CALL MSGWRT (4)
C                                       Set axes in output CATBLK.
 20   CALL SUBHDR (BLC, TRC, 1.0, 1.0)
C                                       Check input size
      I = JBUFSZ / 2
      IF (TRC(1)-BLC(1)+1.0.LE.I) GO TO 999
         WRITE (MSGTXT,1020) I
         CALL MSGWRT (8)
         IRET = 8
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('WARNING: First axis not frequency or velocity')
 1020 FORMAT ('Limited to rows <= ',I5,' pixels')
      END
      SUBROUTINE DO1MOM (IPOS, IDATA, RESULT)
C-----------------------------------------------------------------------
C   DO1MOM fits moments to a row of an image and returns the
C   answers in RESULT.
C   Inputs:
C      IDATA     R(*)   Input data (floated and scaled)
C   Values from commons:
C      FCUT      R      Flux cutoff
C      ACUT      R      ABS(Flux) cutoff
C      FBLANK    R      Value of blanked pixel.
C      CATOLD    I      Input catalog header (also CATOR, CATOD)
C   Output:
C      RESULT(*) R      Output row (count, 4 moments)
C-----------------------------------------------------------------------
      INTEGER   IRET, INPTS, I, IPOS(7)
      REAL      IDATA(*), RESULT(*), S0, S1, S2, S3, X, SN, SG, LFCUT,
     *   LACUT
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'XMOM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      LFCUT = FCUT
      LACUT = ACUT
      INPTS = TRC(1) - BLC(1) + 1.01
      CALL RFILL (5, FBLANK, RESULT)
      IF (INPTS.LE.1) GO TO 999
      SG = SIGN (1.0, ACUT)
      S0 = 0.0
      S1 = 0.0
      S2 = 0.0
      S3 = 0.0
      SN = 0.0
      DO 20 I = 1,INPTS
         X = IDATA(I)
         IF ((X.EQ.0.0) .OR. (X.EQ.FBLANK)) GO TO 20
         IF (PBPARM(1).GT.0.0) THEN
            CALL XMOMPB (I, IPOS, LFCUT, LACUT, IRET)
            IF (IRET.NE.0) GO TO 20
            END IF
         IF (X.LT.LFCUT) GO TO 20
         IF (SG * ABS(X).GE.LACUT) THEN
            S0 = S0 + X
            X = X * I
            S1 = S1 + X
            X = X * I
            S2 = S2 + X
            S3 = S3 + X * I
            SN = SN + 1.0
            END IF
 20      CONTINUE
      IF ((S0.GT.0.0) .AND. (SN.GT.0.0)) THEN
         S1 = S1 / S0
         IF (((S1.LT.1.0) .OR. (S1.GT.INPTS)).AND.BIV) GO TO 999
         S2 = S2 / S0
         S3 = S3 / S0
         RESULT(1) = SN
         RESULT(2) = S0
         RESULT(3) = S1
         RESULT(4) = 0.0
         X = S2 - S1 * S1
         IF (X.GT.0.0) RESULT(4) = SQRT (X)
         X = S3 - 3. * S1 * S2 + 2. * S1 * S1 * S1
         RESULT(5) = ABS (X)
         X = SIGN (1.0, X)
         RESULT(5) = X * (RESULT(5) ** 0.3333333)
C                                       Correct units
         X = CATOR(KRCIC)
         RESULT(5) = RESULT(5) * X
         RESULT(4) = RESULT(4) * ABS(X)
         RESULT(3) = X * (RESULT(3) + BLC(1) - 1.0 - CATOR(KRCRP))
     *      + (CATOD(KDCRV) - OFFS)
         RESULT(2) = RESULT(2) * ABS(X)
C                                       Max / Min
         DO 40 I = 1,5
            PMAX(I) = MAX (PMAX(I), RESULT(I))
            PMIN(I) = MIN (PMIN(I), RESULT(I))
 40         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE DOXMOM (IPOS, IDATA, RESULT)
C-----------------------------------------------------------------------
C   DOXMOM finds maximum in a row of an image and returns the
C   answers in RESULT.
C   Inputs:
C      IDATA     R(*)   Input data (floated and scaled)
C   Values from commons:
C      FCUT      R      Flux cutoff
C      ACUT      R      ABS(Flux) cutoff
C      FBLANK    R      Value of blanked pixel.
C      CATOLD    I      Input catalog header (also CATOR, CATOD)
C   Output:
C      RESULT(*) R      Output row (count, 4 moments)
C-----------------------------------------------------------------------
      INTEGER   IRET, INPTS, I, J, IPOS(7)
      REAL      IDATA(*), RESULT(*), S0, X, SG, LFCUT, LACUT
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'XMOM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      LFCUT = FCUT
      LACUT = ACUT
      INPTS = TRC(1) - BLC(1) + 1.01
      RESULT(1) = FBLANK
      RESULT(2) = FBLANK
      SG = SIGN (1.0, ACUT)
      S0 = -1.E10
      J = -100
      DO 20 I = 1,INPTS
         X = IDATA(I)
         IF ((X.EQ.0.0) .OR. (X.EQ.FBLANK)) GO TO 20
         IF (PBPARM(1).GT.0.0) THEN
            CALL XMOMPB (I, IPOS, LFCUT, LACUT, IRET)
            IF (IRET.NE.0) GO TO 20
            END IF
         IF (X.LT.LFCUT) GO TO 20
         IF (SG * ABS(X).GE.LACUT) THEN
            IF (X.GT.S0) THEN
               S0 = X
               J = I
               END IF
            END IF
 20      CONTINUE
      IF ((S0.NE.0.0) .AND. (J.GT.0)) THEN
         RESULT(1) = S0
         RESULT(2) = J
C                                       Correct units
         X = CATOR(KRCIC)
         RESULT(2) = X * (RESULT(2) + BLC(1) - 1.0 - CATOR(KRCRP))
     *      + (CATOD(KDCRV) - OFFS)
C                                       Max / Min
         DO 40 I = 1,2
            PMAX(I) = MAX (PMAX(I), RESULT(I))
            PMIN(I) = MIN (PMIN(I), RESULT(I))
 40         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE XMOMPB (IX, IPOS, FCC, ICC, IRET)
C-----------------------------------------------------------------------
C   XMOMPB computes the primary beam correction at the current pixel and
C   adjusts FCUT and ICUT for it.
C   Inputs:
C      IX     I      X pixel position
C      IPOS   I(7)   ?,Y,Z,... pixels
C   Output:
C      FCC    R      FCUT adjusted
C      ICC    R      ICUT adjusted
C      IRET   I      0 - okay, 1 outside the usable beam
C-----------------------------------------------------------------------
      INTEGER   IX, IPOS(*), IRET
      REAL      FCC, ICC
C
      INTEGER   LPOS(7), LF, LR, LD, CATSAV(256), LY
      DOUBLE PRECISION DX, DY, DT, X, Y, LAMBDA, ANGLE
      REAL      PBCORF
      CHARACTER ARRAY*8
      LOGICAL   OUTSID
      INCLUDE 'XMOM.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:PSTD.INC'
      SAVE LF, LR, LD, LY, LAMBDA, ANGLE
      DATA LF, LR, LD, LY /3*0,300000/
C-----------------------------------------------------------------------
      CALL COPY (6, IPOS(2), LPOS(2))
      LPOS(1) = IX
      IRET = 0
      CALL H2CHR (8, 1, CATH(KHTEL), ARRAY)
C                                       init common each plane
      IF (LPOS(2).LT.LY) THEN
        CALL COPY (256, CATBLK, CATSAV)
        LOCNUM = 1
        CALL COPY (256, CATOLD, CATBLK)
        CALL SETLOC (LPOS(3), .FALSE.)
        END IF
      LY = LPOS(2)
C                                       new frequency
      IF (LF.NE.LPOS(FAX)) THEN
         LF = LPOS(FAX)
         LAMBDA = FV + (LF - FR) * FI
         LAMBDA = VELITE / LAMBDA
         END IF
C                                       new coordinate
      IF ((LD.NE.LPOS(DAX)) .OR. (LR.NE.LPOS(RAX))) THEN
         DX = (LPOS(RAX) - RR) * RI
         DY = (LPOS(DAX) - DR) * DI
         DT = DX * COS (MROT) - DY * SIN (MROT)
         DY = DY * COS (MROT) + DX * SIN (MROT)
         DX = DT
         CALL NEWPOS (AXFUNC(KLOCL(LOCNUM)+1,LOCNUM), RA0, DE0, DX, DY,
     *      X, Y, IRET)
         IF (IRET.NE.0) THEN
            LD = -10
            GO TO 999
            END IF
         DT = SIN (DE0) * SIN (Y) + COS (DE0) * COS (Y) * COS (RA0-X)
         DT = MIN (1.0D0, DT)
         DT = MAX (-1.0D0, DT)
         ANGLE = RAD2DG * ACOS (DT)
         LD = LPOS(DAX)
         LR = LPOS(RAX)
         END IF
C                                       primary beam
      CALL PBCALC (ANGLE, LAMBDA, ARRAY, PBPARM(2), PBCORF, OUTSID)
      IF ((OUTSID) .OR. (PBCORF.LE.0.0) .OR. (PBCORF.LT.PBPARM(1))) THEN
         IRET = 1
      ELSE
         FCC = FCUT / PBCORF
         ICC = ACUT / PBCORF
         END IF
C
 999  RETURN
      END
      SUBROUTINE XMOMOU (BLNKD, IRET)
C-----------------------------------------------------------------------
C   XMOMOU creates and fills (via PSCALE) the individual moment maps.
C   It calls XMOMHI for history info for all images.
C   Inputs: BLNKD   L      Are any parameters blanked?
C   Output: IRET    I      0 => ok,  4 => real trouble.
C-----------------------------------------------------------------------
      CHARACTER CLAOUT*6, SEQTYP*6
      LOGICAL   BLNKD
      INTEGER   NXO, NYO, WINI(4), WINO(4), IERR, IG, IOFF, IP, IRET,
     *   NG
      INCLUDE 'XMOM.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       loop limits etc.
      NG = 5
      IF (OPTYPE.EQ.'MAX') NG = 2
      WINI(1) = 1
      WINI(2) = 1
      WINI(3) = NG
      WINO(1) = 1
      WINO(2) = 1
C                                       Output Gaussian parms
      DO 40 IG = 1,NG
         IP = NCFILE - NG + IG
         NEWCNO = FCNO(IP)
         DISKO = FVOL(IP)
         CALL CATIO ('READ', DISKO, NEWCNO, CATBLK, 'REST', SCRTCH,
     *      IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.6)) THEN
            WRITE (MSGTXT,1000) IERR, IP
            GO TO 990
            END IF
         CALL H2CHR (6, KHIMCO, CATH(KHIMC), SEQTYP)
         IOFF = IG
         WRITE (MSGTXT,1020) SEQTYP
         CALL MSGWRT (1)
         SEQOUT = CATBLK(KIIMS)
         CALL H2CHR (12, KHIMNO, CATH(KHIMNO), NAMOUT)
         CALL H2CHR (6, KHIMCO, CATH(KHIMC), CLAOUT)
         NXO = CATBLK(KINAX)
         NYO = CATBLK(KINAX+1)
         WINI(4) = NXO
         WINO(3) = NXO
         WINO(4) = NYO
C                                       Fill image
         CALL PSCALE (IOFF, NSCR, WINI, NEWCNO, DISKO, WINO, JBUFSZ,
     *      PMAX, PMIN, BLNKD, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1021) IERR, SEQTYP
            GO TO 990
            END IF
C                                       History, close
         CALL XMOMHI (IOFF, IP, CLAOUT)
 40      CONTINUE
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
      IRET = 4
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I5,' RECOVERING HEADER NUMBER',I5)
 1020 FORMAT ('Begin writing file of type ',A6)
 1021 FORMAT ('ERROR',I5,' MOVING DATA TO FILE TYPE ',A6)
      END
      SUBROUTINE PSCALE (IOFF, ISCR, WINI, NEWCNO, DISKO, WINO,
     *   JBUFSZ, PMAX, PMIN, BLNKD, IERR)
C-----------------------------------------------------------------------
C   PSCALE reads a floating point map file extracting one point per row
C   and writes a scaled integer (or floating) image out.
C   Inputs: IOFF    I        Pixel in row to extract (1-rel)
C           NG      I        Number images on 1st row of scr
C           ISCR    I        Scratch file number in CFIL common
C           WINI    I(4)     Input window
C           NEWCNO  I        Output catalog number
C           DISKO   I        Output disk number
C           WINO    I(4)     Output Window
C           JBUFSZ  I        Buffer size in bytes
C           PMAX    R(*)     Max values by columns
C           PMIN    R(*)     Min values by columns
C           BLNKD   L        Image is blanked
C   Output: IERR    I        0 -> ok, else IO error
C           CATBLK in common: change max/min and scaling and blanking
C           Buffers in common
C-----------------------------------------------------------------------
      CHARACTER    PHNAME*48
      INTEGER   IOFF, ISCR, WINI(4), NEWCNO, DISKO, WINO(4), JBUFSZ,
     *   IERR, NXO, L3, L4, L5, L6, L7, I2, I3, I4, I5, I6, I7, J, LUNI,
     *   LUNO, INDI, INDO, IPOS(8), NAXT(8), INDIM, BOTEMP, OBIND,
     *   IBIND, L, JERR
      REAL      PMAX(*), PMIN(*)
      LOGICAL   BLNKD, T
      INCLUDE 'XMBUFRS'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUNI, LUNO /16, 17/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Set maxima, clear blanking
      CATR(KRDMX) = PMAX(IOFF)
      CATR(KRDMN) = PMIN(IOFF)
      CATR(KRBLK) = 0.0
      IF (BLNKD) CATR(KRBLK) = FBLANK
C                                       loop limits
      L3 = CATBLK(KINAX+1)
      L4 = CATBLK(KINAX+2)
      L5 = CATBLK(KINAX+3)
      L6 = CATBLK(KINAX+4)
      L7 = CATBLK(KINAX+5)
      NXO = WINO(3)
C                                       Open files
      CALL ZPHFIL ('SC', SCRVOL(ISCR), SCRCNO(ISCR), 1, PHNAME, IERR)
      CALL ZOPEN (LUNI, INDI, SCRVOL(ISCR), PHNAME, T, T, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
      CALL ZPHFIL ('MA', DISKO, NEWCNO, 1, PHNAME, IERR)
      CALL ZOPEN (LUNO, INDO, DISKO, PHNAME, T, T, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1035) IERR
         CALL MSGWRT (8)
         GO TO 980
         END IF
C                                       Prepare to read
      IPOS(8) = 1
      CALL COPY (7, CATBLK(KINAX), NAXT(2))
      NAXT(1) = WINI(3)
      INDIM = CATBLK(KIDIM) + 1
C                                       loop
      DO 700 I7 = 1,L7
         IPOS(7) = I7
         DO 600 I6 = 1,L6
            IPOS(6) = I6
            DO 500 I5 = 1,L5
               IPOS(5) = I5
               DO 400 I4 = 1,L4
                  IPOS(4) = I4
C                                       Init output
      CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IPOS(4), BOTEMP, IERR)
      BOTEMP = BOTEMP + 1
      CALL MINIT ('WRIT', LUNO, INDO, WINO(3), WINO(4), WINO, BUFF2,
     *   JBUFSZ, BOTEMP, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1100) IERR
         GO TO 970
         END IF
      DO 300 I3 = 1,L3
         IPOS(3) = I3
         CALL COMOFF (INDIM, NAXT, IPOS(3), BOTEMP, IERR)
         BOTEMP = BOTEMP + 1
         CALL MINIT ('READ', LUNI, INDI, WINI(3), WINI(4), WINI, BUFF1,
     *      JBUFSZ, BOTEMP, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1110) IERR
            GO TO 970
            END IF
C                                       Init a write
         CALL MDISK ('WRIT', LUNO, INDO, BUFF2, OBIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1120) 'WRIT', IERR
            GO TO 970
            END IF
C                                       Loop thru input plane
         DO 200 I2 = 1,NXO
            CALL MDISK ('READ', LUNI, INDI, BUFF1, IBIND, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1120) 'READ', IERR
               GO TO 970
               END IF
            J = IBIND + IOFF - 1
            L = OBIND + I2 - 1
            BUFF2(L) = BUFF1(J)
 200        CONTINUE
 300     CONTINUE
C                                       Flush output plane
      CALL MDISK ('FINI', LUNO, INDO, BUFF2, OBIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1120) 'FINI', IERR
         GO TO 970
         END IF
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
      GO TO 975
C                                       Close down (error)
 970  CALL MSGWRT (8)
C                                       Close files
 975  CALL ZCLOSE (LUNO, INDO, JERR)
 980  CALL ZCLOSE (LUNI, INDI, JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('PSCALE: ERROR',I5,' OPENING SCRATCH FILE')
 1035 FORMAT ('PSCALE: ERROR',I5,' OPENING MAP FILE')
 1100 FORMAT ('PSCALE: ERROR',I5,' ON INIT MAP FILE')
 1110 FORMAT ('PSCALE: ERROR',I5,' ON INIT SCRATCH FILE')
 1120 FORMAT ('PSCALE: ',A4,' ERROR',I5)
      END
      SUBROUTINE XMOMHI (ITYP, NCN, CLAOUT)
C-----------------------------------------------------------------------
C   XMOMHI copies and updates history file.
C   Inputs: ITYP   I      Output map type: 0 => residual
C                         1 => answers (get 1st axis info also)
C           NCN    I      Index in catlgd files common
C           CLAOUT R(2)   Output map CLASS
C-----------------------------------------------------------------------
      CHARACTER CLAOUT*6, LABEL*8
      INTEGER   LUN1, LUN2, IERR, ITYP, NCN
      LOGICAL   T
      REAL      RTEMP
      INCLUDE 'XMOM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, OLDCNO, NEWCNO, CATBLK,
     *   SCRTCH(257), SCRTCH, IERR)
      IF (IERR.LE.2) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 50
C                                       New history
 10   CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, SCRTCH,
     *   IERR)
      IF (IERR.NE.0) GO TO 50
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2,
     *   SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       BLC
      WRITE (MSGTXT,2000) TSKNAM, BLC
      CALL HIADD (LUN2, MSGTXT, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       TRC
      WRITE (MSGTXT,2001) TSKNAM, TRC
      CALL HIADD (LUN2, MSGTXT, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       other Parms
      CALL H2CHR (8, 1, CATOH(KHBUN), LABEL)
      WRITE (MSGTXT,2002) TSKNAM, FCUT, LABEL
      CALL HIADD (LUN2, MSGTXT, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 50
      RTEMP = -ACUT
      WRITE (MSGTXT,2003) TSKNAM, ACUT, LABEL
      IF (ACUT.LT.0.0) WRITE (MSGTXT,2004) TSKNAM, RTEMP, LABEL
      IF (ACUT.NE.0.0) THEN
         CALL HIADD (LUN2, MSGTXT, SCRTCH, IERR)
         END IF
      IF (IERR.NE.0) GO TO 50
C                                       Old axis 1
      IF (ITYP.GT.0) THEN
         CALL H2CHR (8, 1, CATOH(KHCTP), LABEL)
         WRITE (MSGTXT,2020) TSKNAM, LABEL
         CALL HIADD (LUN2, MSGTXT, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 50
         WRITE (MSGTXT,2021) TSKNAM, CATOLD(KINAX)
         CALL HIADD (LUN2, MSGTXT, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 50
         WRITE (MSGTXT,2022) TSKNAM, CATOR(KRCRP)
         CALL HIADD (LUN2, MSGTXT, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 50
         WRITE (MSGTXT,2023) TSKNAM, CATOR(KRCIC)
         CALL HIADD (LUN2, MSGTXT, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 50
         WRITE (MSGTXT,2024) TSKNAM, CATOD(KDCRV)
         CALL HIADD (LUN2, MSGTXT, SCRTCH, IERR)
         END IF
      IF ((ITYP.EQ.3) .AND. (OFFS.NE.0.0D0)) THEN
         WRITE (MSGTXT,2030) TSKNAM, OFFS
         CALL HIADD (LUN2, MSGTXT, SCRTCH, IERR)
         END IF
C                                       Close HI file
 50   CALL HICLOS (LUN2, T, SCRTCH, IERR)
C                                        Update CATBLK and close
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'CLWR', SCRTCH, IERR)
      FRW(NCN) = -1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('XMOMHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 2000 FORMAT (A6,'BLC =',6(F6.0,','),F6.0)
 2001 FORMAT (A6,'TRC =',6(F6.0,','),F6.0)
 2002 FORMAT (A6,'FLUX =',1PE12.4,14X,'/ Use only t > flux ',A8)
 2003 FORMAT (A6,'ICUT =',1PE12.4,14X,'/ Use only abs(t) > icut ',
     *   A8)
 2004 FORMAT (A6,'ICUT =',1PE12.4,14X,'/ Use only abs(t) < icut ',
     *   A8)
 2020 FORMAT (A6,'CTYPE1  = ''',A8,'''',12X,'/ Old axis 1')
 2021 FORMAT (A6,'NAXIS1  = ',I6,16X,'/ Old axis 1')
 2022 FORMAT (A6,'CRPIX1  = ',F9.3,13X,'/ Old axis 1')
 2023 FORMAT (A6,'CDELT1  = ',1PE13.5,9X,'/ Old axis 1')
 2024 FORMAT (A6,'CRVAL1  = ',1PE18.10,4X,'/ Old axis 1')
 2030 FORMAT (A6,'OFFSET  = ',1PE18.10,4X,
     *   '/ add OFFSET to get correct value')
      END
