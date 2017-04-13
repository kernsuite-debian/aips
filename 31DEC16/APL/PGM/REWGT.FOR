LOCAL INCLUDE 'REWGT.INC'
C                                       Local include for REWGT
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOU(2), XOPTYP,
     *   XINTXT(12)
      REAL      XSIN, XDISIN, XSOUT, XDISO, FACTOR(64), XSUBA, XBIF,
     *   XCENT, BUFF1(UVBFSS), BUFF2(UVBFSS), WEGHTS(MAXCIF), DIFPIX
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, JBUFSZ, ILOCWT, LRECI,
     *   CATOLD(256), INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO, LRECO,
     *   NRPRMI, NRPRMO, BSUB, ESUB, BIF, EIF
      LOGICAL   ISCOMP
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6, OPTYPE*4,
     *   INTEXT*48
      DOUBLE PRECISION UVSCAL
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XNAMOU, XCLAOU,
     *   XSOUT, XDISO, XOPTYP, FACTOR, XSUBA, XBIF, XINTXT, XCENT
      COMMON /PARMS/ CATOLD, UVSCAL, WEGHTS, SEQIN, SEQOUT, DISKIN,
     *   DISKO, ILOCWT, INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO,
     *   LRECI, LRECO, NRPRMI, NRPRMO, ISCOMP, BSUB, ESUB, BIF, EIF,
     *   DIFPIX
      COMMON /CHARPM/ NAMEIN, CLAIN, NAMOUT, CLAOUT, OPTYPE, INTEXT
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
C                                       End local include for REWGT
LOCAL END
      PROGRAM REWGT
C-----------------------------------------------------------------------
C! Rescales visibility weights
C# UV UV-util Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2010-2011, 2014-2015
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
C   REWGT rescales visibility fluxes and weights.  It is FUDGE cleaned
C   up almost exactly.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input VU data.
C      OUTNAME        NAMOUT        Name of the output uv file.
C                                   Default output is input file.
C      OUTCLASS       CLAOUT        Class of the output uv file.
C      OUTSEQ         SEQOUT        Seq. number of output uv data.
C      OUTDISK        DISKO         Disk number of the output file.
C      FACTOR         FACTOR        User scaling factor
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'REWGT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'REWGT '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL REWGTI (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Call routine that sends data
C                                       to the scale routine.
      CALL REWGTU (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       history, ext copy
      CALL REWGTH
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE REWGTI (PRGN, JERR)
C-----------------------------------------------------------------------
C   REWGTI gets input parameters for REWGT and creates an output file
C   if necessary.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      JERR    I    Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      CHARACTER STAT*4, BLANK*6, PTYPE*2
      INTEGER   OLDCNO, IROUND, NPARM, IERR, INCX, I, J
      INCLUDE 'REWGT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA BLANK  /' '/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      JBUFSZ = UVBFSS * 2
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 94
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, BUFF1, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      CALL H2CHR (4, 1, XOPTYP, OPTYPE)
      CALL H2CHR (48, 1, XINTXT, INTEXT)
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISKO = IROUND (XDISO)
C                                       Example code scale factor
      J = 0
      DO 10 I = 1,64
         IF (FACTOR(I).EQ.0.0) FACTOR(I) = 1.0
         IF (FACTOR(I).NE.1.0) J = J + 1
 10      CONTINUE
      IF ((J.EQ.0) .AND. (OPTYPE.NE.'FILE')) THEN
         MSGTXT = 'PBSIZE all = 0 or 1, I QUIT'
         GO TO 990
         END IF
C                                       Create new file.
C                                       Get CATBLK from old file.
      OLDCNO = 1
      PTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'REST', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Save input CATBLK
      CALL COPY (256, CATBLK, CATOLD)
C                                       Compressed data?
      ISCOMP = CATBLK(KINAX).EQ.1
C                                       Find weight and scale.
      IF (ISCOMP) THEN
         CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), ILOCWT,
     *      JERR)
         IF (JERR.NE.0) THEN
            MSGTXT = 'ERROR FINDING WEIGHT FOR COMPRESSED DATA'
            JERR = 9
            GO TO 990
            END IF
         END IF
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
      CALL FNDEXT ('AN', CATBLK, I)
C                                       set up loops
      IF (OPTYPE.EQ.'SUBA') THEN
         BIF = XBIF + 0.1
         IF (BIF.LE.0) THEN
            BIF = 1
            EIF = 1
            IF (JLOCIF.GT.0) EIF = CATBLK(KINAX+JLOCIF)
         ELSE
            EIF = BIF
            END IF
         BSUB = 1
         ESUB = I
      ELSE
         IF (OPTYPE.NE.'FILE') OPTYPE = 'IF'
         BSUB = XSUBA + 0.1
         IF ((BSUB.GT.0) .AND. (BSUB.LE.I)) THEN
            ESUB = BSUB
         ELSE
            BSUB = 1
            ESUB = I
            END IF
         BIF = 1
         EIF = 1
         IF (JLOCIF.GT.0) EIF = CATBLK(KINAX+JLOCIF)
         END IF
C                                       file input
      IF (OPTYPE.EQ.'FILE') THEN
         CALL RFILL (MAXCIF, 1.0, WEGHTS)
         CALL WTFILE (INTEXT, BIF, EIF, CATBLK(KINAX+JLOCF), WEGHTS,
     *      IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'FAILED TO READ TEXT FILE, QUITTING'
            GO TO 990
            END IF
         END IF
C                                       Save input file info
      INCX = CATBLK(KINAX)
      LRECI = LREC
      NRPRMI = NRPARM
      INCSI = INCS / INCX
      INCFI = INCF / INCX
      INCIFI = INCIF / INCX
      IF (XCENT.LE.0.0) THEN
         UVSCAL = 1.0D0
         DIFPIX = 0.0
      ELSE
         INCX = CATBLK(KINAX+JLOCF) / 2 + 1
         DIFPIX = INCX - CATR(KRCRP+JLOCF)
         CATD(KDCRV+JLOCF) = CATD(KDCRV+JLOCF) + CATR(KRCIC+JLOCF) *
     *      DIFPIX
         CATR(KRCRP+JLOCF) = INCX
         UVSCAL = CATD(KDCRV+JLOCF) / FREQ
         END IF
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, BLANK, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
C                                       Create output file.
      CCNO = 1
      FRW(NCFILE+1) = 3
      JERR = 4
      CALL UVCREA (DISKO, CCNO, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.NE.2) THEN
            WRITE (MSGTXT,1050) IERR
            GO TO 990
            END IF
C                                       Only overwrite Input file
C                                       no destroy existing otherwise
         IF ((CCNO.NE.OLDCNO) .OR. (DISKO.NE.DISKIN)) THEN
            WRITE (MSGTXT,1060)
            GO TO 990
            END IF
C                                       Recover existing CATBLK
         FRW(NCFILE+1) = 2
         CALL CATIO ('READ', DISKO, CCNO, CATBLK, 'WRIT', BUFF1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1065) IERR
            CALL MSGWRT (6)
            END IF
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CCNO
      FRW(NCFILE) = FRW(NCFILE) - 1
C                                       Save output file info
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
      INCX = CATBLK(KINAX)
      LRECO = LREC
      NRPRMO = NRPARM
      INCSO = INCS / INCX
      INCFO = INCF / INCX
      INCIFO = INCIF / INCX
C                                        Put input file in READ
      PTYPE = 'UV'
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, 'READ', BUFF1, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
      JERR = 0
      SEQOUT = CATBLK(KIIMS)
C                                       copy keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISKO, CCNO, IERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('REWGTI: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1060 FORMAT ('MAY OVERWRITE INPUT FILE ONLY.  QUITTING')
 1065 FORMAT ('REWGTI: ERROR',I3,' UPDATING NEW CATBLK')
      END
      SUBROUTINE REWGTU (IRET)
C-----------------------------------------------------------------------
C   REWGTU sends uv data one point at a time to the scaling routine.
C   Output:
C      IRET    I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER OFILE*48, IFILE*48
      INTEGER   INIO, IPTRI, IPTRO, LUNI, LUNO, INDI, INDO, ILENBU,
     *   KBIND, NIOUT, NIOLIM, IBIND, I, IA1, IA2, INCX, BO, VO, NUMVIS,
     *   XCOUNT, NCORI, NCORO, NCOPY, ISUB, NF
      LOGICAL   T, F
      INCLUDE 'REWGT.INC'
      REAL      BASEN, CBUFF(UVBFSS), RESULT(UVBFSS)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUNI, LUNO /16, 17/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Dimension of complex axis
      INCX = CATBLK(KINAX)
      IF (ISCOMP) INCX = 3
C                                       Number of visibilities in input
C                                       and output files.
      NCORI = (LRECI - NRPRMI) / CATOLD(KINAX)
      NCORO = (LRECO - NRPRMO) / CATBLK(KINAX)
      NCOPY = LRECO - NRPRMO
C                                       Open and init for read
C                                       visibility file
      CALL ZPHFIL ('UV', DISKIN, FCNO(NCFILE), 1, IFILE, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, IFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Open vis file for write
      CALL ZPHFIL ('UV', DISKO, CCNO, 1, OFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, OFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Init vis file for write
      ILENBU = 0
      CALL UVINIT ('WRIT', LUNO, INDO, NVIS, VO, LRECO, ILENBU, JBUFSZ,
     *   BUFF2, BO, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
      IPTRO = KBIND
      NIOUT = 0
      NIOLIM = ILENBU
C                                       Init vis file for read.
      ILENBU = 0
      CALL UVINIT ('READ', LUNI, INDI, NVIS, VO, LRECI, ILENBU, JBUFSZ,
     *   BUFF1, BO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET
         GO TO 990
         END IF
      NUMVIS = 0
      XCOUNT = 0
      NF = 1
      IF (JLOCF.GE.0) NF = CATBLK(KINAX+JLOCF)
C                                       Loop
 100  CONTINUE
C                                       Read vis. record.
         CALL UVDISK ('READ', LUNI, INDI, BUFF1, INIO, IBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) IRET
            GO TO 990
            END IF
         IPTRI = IBIND
C                                       Out of data?
         IF (INIO.LE.0) GO TO 200
C                                       Loop over buffer
         DO 190 I = 1,INIO
            IF (ILOCB.GE.0) THEN
               BASEN = BUFF1(IPTRI+ILOCB)
               IA1 = BASEN / 256. + 0.1
               IA2 = BASEN - IA1*256. + 0.1
               ISUB = (BASEN - 256*IA1 - IA2) * 100.0 + 1.01
            ELSE
               IA1 = BUFF1(IPTRI+ILOCA1) + 0.1
               IA2 = BUFF1(IPTRI+ILOCA2) + 0.1
               ISUB = BUFF1(IPTRI+ILOCSA) + 0.1
               END IF
            NUMVIS = NUMVIS + 1
            BUFF1(IPTRI+ILOCU) = BUFF1(IPTRI+ILOCU) * UVSCAL
            BUFF1(IPTRI+ILOCV) = BUFF1(IPTRI+ILOCV) * UVSCAL
            BUFF1(IPTRI+ILOCW) = BUFF1(IPTRI+ILOCW) * UVSCAL
C                                       Compressed data.
            IF (ISCOMP) THEN
               CALL ZUVXPN (NCORI, BUFF1(IPTRI+NRPRMI),
     *            BUFF1(IPTRI+ILOCWT), CBUFF)
               CALL REWGTS (ISUB, NUMVIS, NF, CBUFF, INCX, WEGHTS,
     *            RESULT)
C                                       Un compressed data
            ELSE
               CALL REWGTS (ISUB, NUMVIS, NF, BUFF1(IPTRI+NRPRMI), INCX,
     *            WEGHTS, RESULT)
               END IF
            XCOUNT = XCOUNT + 1.0D0
            CALL RCOPY (NRPRMO, BUFF1(IPTRI), BUFF2(IPTRO))
C                                       Compressed
            IF (ISCOMP) THEN
               CALL ZUVPAK (NCORO, RESULT, BUFF2(IPTRO+ILOCWT),
     *            BUFF2(IPTRO+NRPRMO))
            ELSE
               CALL RCOPY (NCOPY, RESULT, BUFF2(IPTRO+NRPRMO))
               END IF
            IPTRO = IPTRO + LRECO
            NIOUT = NIOUT + 1
            IPTRI = IPTRI + LRECI
C                                       Write vis record.
            IF (NIOUT.GE.NIOLIM) THEN
               CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOLIM, KBIND,
     *            IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1150) IRET
                  GO TO 990
                  END IF
               IPTRO = KBIND
               NIOUT = 0
               END IF
 190        CONTINUE
C                                       Read next buffer.
         GO TO 100
C                                       Finish write
 200  NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNO, INDO, BUFF2, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1150) IRET
         GO TO 990
         END IF
C                                       Compress output file.
      NVIS = XCOUNT
      CALL UCMPRS (NVIS, DISKO, CCNO, LUNO, CATBLK, IRET)
C                                       Close files
      CALL ZCLOSE (LUNI, INDI, IRET)
      CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('REWGTU: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1010 FORMAT ('REWGTU: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('REWGTU: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1030 FORMAT ('REWGTU: ERROR',I3,' INIT-FOR-READ VIS FILE')
 1100 FORMAT ('REWGTU: ERROR',I3,' READING VIS FILE')
 1120 FORMAT ('REWGTU: REWGTS ERROR',I3)
 1150 FORMAT ('REWGTU: ERROR',I3,' WRITING VIS FILE')
      END
      SUBROUTINE REWGTH
C-----------------------------------------------------------------------
C   REWGTH copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      CHARACTER NOTTYP*2, HILINE*72
      INTEGER   LUN1, LUN2, IERR, I, NONOT, VER, JTRIM
      INCLUDE 'REWGT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN1, LUN2 /27,28/
      DATA NONOT, NOTTYP /0, '  '/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.

      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, FCNO(NCFILE),
     *   FCNO(NCFILE-1), CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 200
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
      WRITE (HILINE,1010) TSKNAM, OPTYPE
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
      IF (OPTYPE.EQ.'SUBA') THEN
         WRITE (HILINE,1020) TSKNAM, BIF
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         WRITE (HILINE,1021) TSKNAM, EIF
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         DO 20 I = 1,ESUB
            WRITE (HILINE,1022) TSKNAM, I, FACTOR(I)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 200
 20         CONTINUE
      ELSE
         WRITE (HILINE,1030) TSKNAM, BSUB
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         WRITE (HILINE,1031) TSKNAM, ESUB
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         IF (OPTYPE.NE.'FILE') THEN
            DO 30 I = 1,ESUB
               WRITE (HILINE,1022) TSKNAM, I, FACTOR(I)
               CALL HIADD (LUN2, HILINE, BUFF2, IERR)
               IF (IERR.NE.0) GO TO 200
 30            CONTINUE
         ELSE
            I = JTRIM (INTEXT)
            WRITE (HILINE,1032) TSKNAM, INTEXT(:I)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 200
            END IF
         END IF

C                                       Close HI file
 200  CALL HICLOS (LUN2, .TRUE., BUFF2, IERR)
C                                       copy index table, is unchanged
      VER = 1
      CALL TABCOP ('NX', VER, VER, LUN1, LUN2, DISKIN, DISKO, FCNO(2),
     *   FCNO(1), CATBLK, BUFF1, BUFF2, IERR)
C                                        Copy rest of tables
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKO, FCNO(2),
     *   FCNO(1), CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1200)
         CALL MSGWRT (6)
         END IF
C                                       correct for FQCENTER
      CALL CENTFQ (DISKO, FCNO(1), DIFPIX, BUFF1, BUFF2, IERR)
      IF (IERR.GT.0) THEN
         MSGTXT = 'REWGTH: ERROR CORRECTING FQ TABLE'
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, FCNO(NCFILE-1), CATBLK, 'REST',
     *   BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('REWGTH: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,'OPTYPE   =''',A,'''  / factors apply by')
 1020 FORMAT (A6,'BIF      =',I3,'    / Begin IF for which factor used')
 1021 FORMAT (A6,'EIF      =',I3,'    / End IF for which factor used')
 1022 FORMAT (A6,'FACTOR(',I2.2,')=',F10.4,' / Weight multiplied by')
 1030 FORMAT (A6,'BSUBA    =',I3,'    / Begin subarray for which',
     *   ' factor used')
 1031 FORMAT (A6,'ESUBA    =',I3,'    / End subarray for which',
     *   ' factor used')
 1032 FORMAT (A6,'INTEXT =''',A,'''  / file with weight factors')
 1200 FORMAT ('REWGTH: ERROR COPYING TABLES')
      END
      SUBROUTINE REWGTS (ISUB, NUMVIS, NF, VIS, INCX, WTS, RESULT)
C-----------------------------------------------------------------------
C   Rescales uv data
C   The history is written after the last call to REWGTS.
C   Inputs:
C      ISUB    I    Subarray number
C      NUMVIS  I    Visibility number, -1 => final call, no data
C                   passed but allows any operations to be completed.
C      VIS     R(INCX,*)  Visibilities in order real, imaginary, weight
C                   (Jy, Jy, unitless).  Weight <= 0 => flagged.
C                   NOTE: INCX may be any value .GE. 2
C   Output:
C      RESULT  R(INCX,*) Output visibilities selected in frequency.
C-----------------------------------------------------------------------
      INTEGER   ISUB, NUMVIS, NF, INCX
      REAL      VIS(INCX,*), WTS(NF,*), RESULT(INCX,*)
C
      INTEGER   JIF, JF, JS, NIF, NS, INDEXO, INDEXI, I, J
      REAL      WFACT
      LOGICAL   OK
      INCLUDE 'REWGT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                       EXAMPLE - copy data with scale
      IF (NUMVIS.GT.0) THEN
         NS = 1
         NIF = 1
         IF (JLOCS.GE.0) NS = CATBLK(KINAX+JLOCS)
         IF (JLOCIF.GE.0) NIF = CATBLK(KINAX+JLOCIF)
         DO 40 JIF = 1,NIF
            WFACT = 1.0
            OK = .FALSE.
            IF ((ISUB.GE.BSUB) .AND. (ISUB.LE.ESUB) .AND. (JIF.GE.BIF)
     *         .AND. (JIF.LE.EIF)) THEN
               J = JIF
               IF (OPTYPE.EQ.'SUBA') J = ISUB
               WFACT = FACTOR(J)
               IF (OPTYPE.EQ.'FILE') OK = .TRUE.
               END IF
            DO 30 JF = 1,NF
               IF (OK) WFACT = WTS(JF,JIF)
               DO 20 JS = 1,NS
                  INDEXI = (JIF-1) * INCIFI + (JF-1) * INCFI +
     *               (JS-1) * INCSI + 1
                  INDEXO = (JIF-1) * INCIFO + (JF-1) * INCFO +
     *               (JS-1) * INCSO + 1
C                                       Example does scaling
                  DO 10 I = 1,INCX
                     IF (I.LE.2) THEN
                        RESULT(I,INDEXO) = VIS(I,INDEXI)
                     ELSE
                        RESULT(I,INDEXO) = VIS(I,INDEXI) * WFACT
                        END IF
 10                  CONTINUE
 20               CONTINUE
 30            CONTINUE
 40         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE WTFILE (WFILE, BIF, EIF, NWEGHT, WEGHTS, IRET)
C-----------------------------------------------------------------------
C   WTFILE reads the WFILEE for W cards to set weights
C   Input:
C      WFILE    C*(*)   Box file name
C      BIF      I       First IF included
C      EIF      I       Last IF included
C      NWEGHT   I       Number of spectral channels
C   Output:
C      WEGHTS   R(*)    Weight multipliers (NWEGHT, BIF:EIF)
C      IRET     I       Error code
C-----------------------------------------------------------------------
      CHARACTER WFILE*(*)
      INTEGER   BIF, EIF, NWEGHT, IRET
      REAL      WEGHTS(*)
C
      INTEGER   LUN, I, J, FIND, IPARM(2), KBP, I1, I2, J1, J2, K
      REAL      WT
      CHARACTER LINE*132
      DOUBLE PRECISION X
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                        Open clean box file
      LUN = 11
      CALL ZTXOPN ('QRED', LUN, FIND, WFILE, .FALSE., IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN'
         CALL MSGWRT (6)
         GO TO 999
         END IF
      DO 50 K = 1,100000
         CALL ZTXIO ('READ', LUN, FIND, LINE, IRET)
         IF (IRET.EQ.2) GO TO 60
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ'
            CALL MSGWRT (6)
            GO TO 999
            END IF
C                                       A weight line
         CALL CHTRIM (LINE, 132, LINE, J)
         IF ((LINE(:1).EQ.'W') .OR. (LINE(:1).EQ.'w'))  THEN
            KBP = 2
            DO 10 J = 1,3
               CALL GETNUM (LINE, 132, KBP, X)
               IF (X.EQ.DBLANK) THEN
                  IF (J.EQ.1) GO TO 50
                  WRITE (MSGTXT,1020) I, J
                  CALL MSGWRT (6)
                  IRET = 1
                  GO TO 999
               ELSE IF (J.EQ.1) THEN
                  WT = X
               ELSE
                  IF (X.GE.0.0D0) THEN
                     IPARM(J-1) = X + 0.50D0
                  ELSE
                     IPARM(J-1) = X - 0.50D0
                     END IF
                  END IF
 10            CONTINUE
            IF (IPARM(1).LE.NWEGHT) THEN
               IF (IPARM(1).LE.0) THEN
                  J1 = 1
                  J2 = NWEGHT
               ELSE
                  J1 = IPARM(1)
                  J2 = J1
                  END IF
               IF (IPARM(2).EQ.0) THEN
                  I1 = BIF
                  I2 = EIF
               ELSE
                  I1 = IPARM(2)
                  I2 = I1
                  END IF
               DO 30 I = I1,I2
                  IF ((I.GE.BIF) .AND. (I.LE.EIF)) THEN
                     DO 20 J = J1,J2
                        WEGHTS(J+(I-BIF)*NWEGHT) = WT
 20                     CONTINUE
                     END IF
 30               CONTINUE
               END IF
            END IF
 50      CONTINUE
C
 60   CALL ZTXCLS (LUN, FIND, IRET)
      IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('WTFILE: ERROR',I4,1X,A,'ING THE INTEXT WEIGHTS FILE')
 1020 FORMAT ('WTFILE: PARSING ERROR ON LINE',I4,' PARAMETER',I2)
      END
