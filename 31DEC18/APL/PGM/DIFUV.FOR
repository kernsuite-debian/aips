LOCAL INCLUDE 'DIFUV.INC'
C                                       Local include for DIFUV
      INCLUDE 'INCS:ZPBUFSZ.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAME2(3), XCLAS2(2),
     *   XNAMOU(3), XCLAOU(2), XOPTYP
      REAL      XSIN, XDISIN, XS2, XDISK2, XSOUT, XDISO, SOLINT,
     *   XCENT, BUFF1(UVBFSS), BUFF2(UVBFSS), BUFFO(UVBFSS), DIFPIX
      INTEGER   SEQIN, SEQ2, SEQOUT, DISKIN, DISK2, DISKO,
     *   JBUFSZ, ILOCWT, CATOLD(256), INCSI, INCFI, INCIFI, INCSO,
     *   INCFO, INCIFO, LRECI, LRECO, NRPRMI, NRPRMO, CAT2(256),
     *   NVS2, NRPRM2, LREC2, NVIS2, ILOCW1, ILOCW2, JLOCA1, JLOCA2,
     *   JLOCT, JLOCB
      LOGICAL   ISCOMP
      CHARACTER NAMEIN*12, CLAIN*6, NAME2*12, CLAS2*6, OPTYPE*4,
     *   NAMOUT*12, CLAOUT*6
      DOUBLE PRECISION UVSCAL
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XNAME2, XCLAS2,
     *   XS2, XDISK2, XNAMOU, XCLAOU, XSOUT, XDISO, SOLINT, XOPTYP,
     *   XCENT
      COMMON /DIFUVP/ UVSCAL, SEQIN, SEQ2, SEQOUT, DISKIN, DISK2, DISKO,
     *   ILOCWT, INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO,LRECI,
     *   LRECO, NRPRMI, NRPRMO, ISCOMP, NVS2, NRPRM2, LREC2,NVIS2,
     *   ILOCW1, ILOCW2, DIFPIX, JLOCA1, JLOCA2, JLOCT, JLOCB
      COMMON /CHARPM/ NAMEIN, CLAIN, NAME2, CLAS2, NAMOUT, CLAOUT,
     *   OPTYPE
      COMMON /BUFRS/ CATOLD, CAT2, BUFF1, BUFF2, BUFFO, JBUFSZ
C                                       End local include for DIFUV
LOCAL END
      PROGRAM DIFUV
C-----------------------------------------------------------------------
C! Differences two matching uv data sets
C# Utility UV Singledish modeling
C-----------------------------------------------------------------------
C;  Copyright (C) 1996-1997, 1999-2000, 2008-2009, 2014-2015
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
C   DIFUV differences two data sets which must match very closely.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of 1st input UV data.
C      INCLASS        CLAIN         Class of 1st input UV data.
C      INSEQ          SEQIN         Seq. of 1st input UV data.
C      INDISK         DISKIN        Disk number of 1st input VU data.
C      IN2NAME        NAME2         Name of 2nd input file.
C      IN2CLASS       CLAS2         Class of 2nd input file.
C      IN2SEQ         SEQ2          Seq. no. of 2nd input file.
C      IN2DISK        DISK2         Vol. no. of 2nd input file.
C      OUTNAME        NAMOUT        Name of the output uv file.
C                                   Default output is 1st input file.
C      OUTCLASS       CLAOUT        Class of the output uv file.
C      OUTSEQ         SEQOUT        Seq. number of output uv data.
C      OUTDISK        DISKO         Disk number of the output file.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'DIFUV.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'DIFUV '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL BSDIIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Call routine that sends data
C                                       to the user routine.
      CALL BSDIDO (IRET)
      IF (IRET.NE.0) GO TO 990
      CALL BSDIHI
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE BSDIIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   BSDIIN gets input parameters for DIFUV and creates an output file
C   if necessary.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      JERR    I    Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Output in common:
C      LRECI   I  Input file record length
C      NRPRMI  I  Input number of random parameters.
C      INCSI   I  Input Stokes' increment in vis.
C      INCFI   I  Input frequency increment in vis.
C      INCIFI  I  Input IF increment in vis.
C      LRECO   I  Output file record length
C      NRPRMO  I  Output number of random parameters.
C      INCSO   I  Output Stokes' increment in vis.
C      INCFO   I  Output frequency increment in vis.
C      INCIFO  I  Output IF increment in vis.
C      ISCOMP  L  If true data is compressed
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      CHARACTER  STAT*4, BLANK*6, PTYPE*2
      INTEGER   OLDCNO, IROUND, NPARM, IERR, INCX
      LOGICAL   T
      INCLUDE 'DIFUV.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA BLANK  /'      '/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
      JBUFSZ = UVBFSS * 2
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 24
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
      CALL H2CHR (12, 1, XNAME2, NAME2)
      CALL H2CHR (6, 1, XCLAS2, CLAS2)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      CALL H2CHR (4, 1, XOPTYP, OPTYPE)
      IF (OPTYPE.NE.'DIV') OPTYPE = 'SUB'
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      SEQ2 = IROUND (XS2)
      DISKIN = IROUND (XDISIN)
      DISK2 = IROUND (XDISK2)
      DISKO = IROUND (XDISO)
      IF (SOLINT.LE.0.0) SOLINT = 1.0/6000.0
      SOLINT = SOLINT / (24. * 60.)
C                                       Open 2nd file
      OLDCNO = 1
      PTYPE = 'UV'
      CALL CATDIR ('SRCH', DISK2, OLDCNO, NAME2, CLAS2, SEQ2, PTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAME2, CLAS2, SEQ2, DISK2,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISK2, OLDCNO, CATBLK, 'READ', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISK2
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
C                                       Save input CATBLK
      CALL COPY (256, CATBLK, CAT2)
C                                       Compressed data?
      ISCOMP = CATBLK(KINAX).EQ.1
      CALL UVPGET (IERR)
      NVS2 = (LREC - NRPARM) / CATBLK(KINAX)
      NRPRM2 = NRPARM
      LREC2 = LREC
      NVIS2 = NVIS
      JLOCA1 = ILOCA1
      JLOCA2 = ILOCA2
      JLOCT = ILOCT
      JLOCB = ILOCB
C                                       Find weight and scale.
      IF (ISCOMP) THEN
         CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), ILOCW2,
     *      IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'ERROR FINDING WEIGHT FOR COMPRESSED DATA'
            JERR = 9
            GO TO 990
            END IF
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
      CALL UVPGET (IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Compressed data?
      IF (ISCOMP.NEQV.(CATBLK(KINAX).EQ.1)) THEN
         MSGTXT = 'BOTH FILES MUST BE COMPRESSED OR NOT COMPRESSED'
         JERR = 10
         GO TO 990
         END IF
C                                       Check input file compatibility
      IF (NVS2.NE.((LREC-NRPARM)/CATBLK(KINAX))) THEN
         JERR = 4
         MSGTXT = 'INPUT FILES ARE INCOMPATIBLE'
         GO TO 990
         END IF
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
C                                       more tests
      IF ((NVIS.NE.NVIS2) .OR. (ILOCWT.NE.ILOCW2) .OR.
     *   (NRPRM2.NE.NRPARM)) THEN
         MSGTXT = '*** WARNING: UV FILES DO NOT EXACTLY MATCH ***'
         CALL MSGWRT (6)
         MSGTXT = '*** THIS OPERATION MAY NOT BE FUNCTIONAL   ***'
         CALL MSGWRT (6)
         END IF
C                                       Save input file info
      INCX = CATBLK(KINAX)
      LRECI = LREC
      NRPRMI = NRPARM
      INCSI = INCS / INCX
      INCFI = INCF / INCX
      INCIFI = INCIF / INCX
      IF (JLOCF.LT.0) XCENT = -1.0
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
 1000 FORMAT ('BSDIIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1060 FORMAT ('MAY OVERWRITE INPUT FILE ONLY.  QUITTING')
 1065 FORMAT ('BSDIIN: ERROR',I3,' UPDATING NEW CATBLK')
      END
      SUBROUTINE BSDIDO (IRET)
C-----------------------------------------------------------------------
C   BSDIDO sends uv data one point at a time to the difference routine.
C   Input in common:
C      LRECI   I  Input file record length
C      NRPRMI  I  Input number of random parameters.
C      INCSI   I  Input Stokes' increment in vis.
C      INCFI   I  Input frequency increment in vis.
C      INCIFI  I  Input IF increment in vis.
C      LRECO   I  Output file record length
C      NRPRMO  I  Output number of random parameters.
C      INCSO   I  Output Stokes' increment in vis.
C      INCFO   I  Output frequency increment in vis.
C      INCIFO  I  Output IF increment in vis.
C      ISCOMP  L  If true data is compressed
C   Output:
C      IRET    I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER OFILE*48, IFILE*48
      INTEGER   INIO, IPTR1, IPTRO, LUN1, LUN2, LUNO, IND1, IND2, INDO,
     *   ILENBU, KBIND, NIOUT, NIOLIM, BIND1, I, IA1, IA2, INCX, BO, VO,
     *   NUMVIS, XCOUNT, NCORI, NCORO, NCOPY, BIND2, IPTR2, JA1, JA2,
     *   MVIS
      LOGICAL   T, F
      INCLUDE 'DIFUV.INC'
      REAL      BASEN, CBUFF(UVBFSS), DBUFF(UVBFSS), RESULT(UVBFSS)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN1, LUN2, LUNO /16, 17, 18/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Dimension of complex axis
      INCX = CATBLK(KINAX)
      IF (ISCOMP) INCX = 3
      MVIS = MIN (NVIS, NVIS2)
C                                       Number of visibilities in input
C                                       and output files.
      NCORI = (LRECI - NRPRMI) / CATOLD(KINAX)
      NCORO = (LRECO - NRPRMO) / CATBLK(KINAX)
      NCOPY = LRECO - NRPRMO
C                                       Open and init for read
C                                       visibility file
      CALL ZPHFIL ('UV', DISK2, FCNO(1), 1, IFILE, IRET)
      CALL ZOPEN (LUN2, IND2, DISK2, IFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN 2nd UV file'
         GO TO 990
         END IF
      CALL ZPHFIL ('UV', DISKIN, FCNO(NCFILE), 1, IFILE, IRET)
      CALL ZOPEN (LUN1, IND1, DISKIN, IFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN 1st UV file'
         GO TO 990
         END IF
C                                       Open vis file for write
      CALL ZPHFIL ('UV', DISKO, CCNO, 1, OFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, OFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN output file'
         GO TO 990
         END IF
C                                       Init vis file for write
      ILENBU = 0
      CALL UVINIT ('WRIT', LUNO, INDO, MVIS, VO, LRECO, ILENBU, JBUFSZ,
     *   BUFFO, BO, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT output file'
         GO TO 990
         END IF
      IPTRO = KBIND
      NIOUT = 0
      NIOLIM = ILENBU
C                                       Init vis file for read.
      ILENBU = 0
      CALL UVINIT ('READ', LUN2, IND2, MVIS, VO, LRECI, ILENBU, JBUFSZ,
     *   BUFF2, BO, BIND2, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT 2nd input file'
         GO TO 990
         END IF
      ILENBU = 0
      CALL UVINIT ('READ', LUN1, IND1, MVIS, VO, LRECI, ILENBU, JBUFSZ,
     *   BUFF1, BO, BIND1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT 1st input file'
         GO TO 990
         END IF
      NUMVIS = 0
      XCOUNT = 0
C                                       Loop
 100  CONTINUE
C                                       Read vis. record.
         CALL UVDISK ('READ', LUN2, IND2, BUFF2, INIO, BIND2, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ 2nd input file'
            GO TO 990
            END IF
         IPTR2 = BIND2
         CALL UVDISK ('READ', LUN1, IND1, BUFF1, INIO, BIND1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ 1st input file'
            GO TO 990
            END IF
         IPTR1 = BIND1
C                                       Out of data?
         IF (INIO.LE.0) GO TO 200
C                                       Loop over buffer
         DO 190 I = 1,INIO
            IF (ILOCB.GE.0) THEN
               BASEN = BUFF1(IPTR1+ILOCB)
               IA1 = BASEN / 256. + 0.1
               IA2 = BASEN - IA1*256. + 0.1
            ELSE
               IA1 = BUFF1(IPTR1+ILOCA1) + 0.1
               IA2 = BUFF1(IPTR1+ILOCA2) + 0.1
               END IF
            IF (JLOCB.GE.0) THEN
               BASEN = BUFF2(IPTR2+JLOCB)
               JA1 = BASEN / 256. + 0.1
               JA2 = BASEN - JA1*256. + 0.1
            ELSE
               JA1 = BUFF2(IPTR2+JLOCA1) + 0.1
               JA2 = BUFF2(IPTR2+JLOCA2) + 0.1
               END IF
            NUMVIS = NUMVIS + 1
            BUFF1(IPTR1+ILOCU) = BUFF1(IPTR1+ILOCU) * UVSCAL
            BUFF1(IPTR1+ILOCV) = BUFF1(IPTR1+ILOCV) * UVSCAL
            BUFF1(IPTR1+ILOCW) = BUFF1(IPTR1+ILOCW) * UVSCAL
C                                      Call difference routine.
            IF ((ABS(BUFF1(IPTR1+ILOCT)-BUFF2(IPTR2+JLOCT)).LE.SOLINT)
     *         .AND. (IA1.EQ.JA1) .AND. (IA2.EQ.JA2)) THEN
C                                       Compressed data.
               IF (ISCOMP) THEN
                  CALL ZUVXPN (NCORI, BUFF1(IPTR1+NRPRMI),
     *               BUFF1(IPTR1+ILOCWT), CBUFF)
                  CALL ZUVXPN (NCORI, BUFF2(IPTR2+NRPRM2),
     *               BUFF2(IPTR2+ILOCW2), DBUFF)
                  CALL DIFUVF (NUMVIS, CBUFF, DBUFF, INCX, RESULT, IRET)
C                                       Un compressed data
               ELSE
                  CALL DIFUVF (NUMVIS, BUFF1(IPTR1+NRPRMI),
     *               BUFF2(IPTR2+NRPRMI), INCX, RESULT, IRET)
                  END IF
C                                       Error (fatal)
               IF (IRET.GT.0) THEN
                  WRITE (MSGTXT,1120) IRET
                  GO TO 990
C                                       Copy to output.
               ELSE IF (IRET.EQ.0) THEN
                  XCOUNT = XCOUNT + 1.0D0
                  CALL RCOPY (NRPRMO, BUFF1(IPTR1), BUFFO(IPTRO))
C                                       Compressed
                  IF (ISCOMP) THEN
                     CALL ZUVPAK (NCORO, RESULT, BUFFO(IPTRO+ILOCWT),
     *                  BUFFO(IPTRO+NRPRMO))
                  ELSE
                     CALL RCOPY (NCOPY, RESULT, BUFFO(IPTRO+NRPRMO))
                     END IF
                  IPTRO = IPTRO + LRECO
                  NIOUT = NIOUT + 1
                  END IF
               END IF
C                                       OK, but no output please
            IPTR1 = IPTR1 + LRECI
            IPTR2 = IPTR2 + LRECI
C                                       Write vis record.
            IF (NIOUT.GE.NIOLIM) THEN
               CALL UVDISK ('WRIT', LUNO, INDO, BUFFO, NIOLIM, KBIND,
     *            IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRIT output file'
                  GO TO 990
                  END IF
               IPTRO = KBIND
               NIOUT = 0
               END IF
 190        CONTINUE
C                                       Read next buffer.
         GO TO 100
C                                       Final call to DIFUVF.
 200  NUMVIS = -1
      CALL DIFUVF (NUMVIS, BUFF1, BUFF2, INCX, RESULT, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1120) IRET
         GO TO 990
         END IF
C                                       Finish write
      NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNO, INDO, BUFFO, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'FLSH output file'
         GO TO 990
         END IF
C                                       Compress output file.
      NVIS = XCOUNT
      CALL UCMPRS (NVIS, DISKO, CCNO, LUNO, CATBLK, IRET)
C                                       Close files
      CALL ZCLOSE (LUN1, IND1, IRET)
      CALL ZCLOSE (LUN2, IND2, IRET)
      CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BSDIDO: ERROR',I3,' ON ',A)
 1120 FORMAT ('BSDIDO: DIFUVF ERROR',I3)
      END
      SUBROUTINE BSDIHI
C-----------------------------------------------------------------------
C   BSDIHI copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      CHARACTER NOTTYP*2, HILINE*72
      INTEGER   LUN1, LUN2, IERR, NONOT
      LOGICAL   T
      INCLUDE 'DIFUV.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T  /.TRUE./
      DATA NONOT, NOTTYP /0, '  '/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, FCNO(NCFILE),
     *   FCNO(NCFILE-1), CATBLK, BUFF1, BUFFO, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 200
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFFO,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HENCO2 (TSKNAM, NAME2, CLAS2, SEQ2, DISK2, LUN2, BUFFO, IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2, BUFFO,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
C                                       optype
      WRITE (HILINE,1010) TSKNAM, OPTYPE
      CALL HIADD (LUN2, HILINE, BUFFO, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Close HI file
 200  CALL HICLOS (LUN2, T, BUFFO, IERR)
C                                        Copy tables
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKO,
     *   FCNO(NCFILE), FCNO(NCFILE-1), CATBLK, BUFF1, BUFFO, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1200)
         CALL MSGWRT (6)
         END IF
      CALL CENTFQ (DISKO, FCNO(NCFILE-1), DIFPIX, BUFF1, BUFFO, IERR)
      IF (IERR.GT.0) THEN
         MSGTXT = 'BSDIHI: ERROR CORRECTING FQ TABLE'
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, FCNO(NCFILE-1), CATBLK, 'REST',
     *   BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BSDIHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,'OPTYPE = ''',A,'''  / operation done')
 1200 FORMAT ('BSDIHI: ERROR COPYING TABLES')
      END
      SUBROUTINE DIFUVF (NUMVIS, VIS1, VIS2, INCX, RESULT, IRET)
C-----------------------------------------------------------------------
C   Differences VIS1 - VIS2.
C   Inputs:
C      NUMVIS  I    Visibility number, -1 => final call, no data
C                   passed but allows any operations to be completed.
C      VIS1    R(INCX,*)  Visibilities in order real, imaginary, weight
C                   (Jy, Jy, unitless).  Weight <= 0 => flagged.
C                   NOTE: INCX may be any value .GE. 2
C      VIS2    R(INCX,*)  Visibilities in order real, imaginary, weight
C                   (Jy, Jy, unitless).  Weight <= 0 => flagged.
C                   NOTE: INCX may be any value .GE. 2
C   Inputs from COMMON:
C      RA         D       Right ascension (1950) of phase center. (deg)
C      DEC        D       Declination (1950) of phase center. (deg)
C      FREQ       D       Frequency of observation (Hz)
C      NRPARM     I       # random parameters.
C      NCOR       I       # correlators
C      CATBLK     I(256)  Catalog header record. See Going Aips for
C                         details.
C      LRECI      I    Input file record length
C      NRPRMI     I    Input number of random parameters.
C      INCSI      I    Input Stokes' increment in vis.
C      INCFI      I    Input frequency increment in vis.
C      INCIFI     I    Input IF increment in vis.
C      LRECO      I    Output file record length
C      NRPRMO     I    Output number of random parameters.
C      INCSO      I    Output Stokes' increment in vis.
C      INCFO      I    Output frequency increment in vis.
C      INCIFO     I    Output IF increment in vis.
C   Output:
C      RESULT  R(INCX,*) Output visibilities selected in frequency.
C      IRET       I    Return code  -1 => don't write
C                                    0 => OK
C                                   >0 => error, terminate.
C   Output in COMMON:
C      CATBLK    I         Catalog header block
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, INCX, IRET
      REAL      VIS1(INCX,*), VIS2(INCX,*), RESULT(INCX,*)
C
      INTEGER   JIF, JF, JS, NIF, NF, NS, INDEXO, INDEXI
      COMPLEX   CVIS1, CVIS2, CRES
      REAL      W1, W2
      INCLUDE 'DIFUV.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IF (NUMVIS.GT.0) THEN
         NS = 1
         NIF = 1
         NF = 1
         IF (JLOCS.GE.0) NS = CATBLK(KINAX+JLOCS)
         IF (JLOCIF.GE.0) NIF = CATBLK(KINAX+JLOCIF)
         IF (JLOCF.GE.0) NF = CATBLK(KINAX+JLOCF)
         DO 40 JIF = 1,NIF
            DO 30 JF = 1,NF
               DO 20 JS = 1,NS
                  INDEXI = (JIF-1) * INCIFI + (JF-1) * INCFI +
     *               (JS-1) * INCSI + 1
                  INDEXO = (JIF-1) * INCIFO + (JF-1) * INCFO +
     *               (JS-1) * INCSO + 1
                  W1 = VIS1(3,INDEXI)
                  W2 = VIS2(3,INDEXI)
                  IF (OPTYPE.NE.'DIV') THEN
                     RESULT(1,INDEXO) = VIS1(1,INDEXI) - VIS2(1,INDEXI)
                     RESULT(2,INDEXO) = VIS1(2,INDEXI) - VIS2(2,INDEXI)
                     IF (W1*W2.NE.0.0) RESULT(3,INDEXO) =
     *                  ABS (W1 * W2) / (ABS(W1) + ABS(W2))
                  ELSE
                     CVIS1 = CMPLX (VIS1(1,INDEXI), VIS1(2,INDEXI))
                     CVIS2 = CMPLX (VIS2(1,INDEXI), VIS2(2,INDEXI))
                     CRES = CMPLX (0.0, 0.0)
                     IF (ABS(CVIS2).GT.0.0) CRES = CVIS1 / CVIS2
                     RESULT(1,INDEXO) = REAL (CRES)
                     RESULT(2,INDEXO) = AIMAG (CRES)
                     IF (W1*W2.NE.0.0) RESULT(3,INDEXO) =
     *                  (ABS(CVIS2)**4) * ABS (W1 * W2) /
     *                  ((ABS(CVIS1)**2) * ABS(W1) +
     *                  (ABS(CVIS2)**2) * ABS(W2))
                     END IF
                  IF ((W1.LT.0.) .OR. (W2.LT.0.)) RESULT(3,INDEXO) =
     *               -ABS (RESULT(3,INDEXO))
                  IF ((W1.EQ.0.) .OR. (W2.EQ.0.)) RESULT(3,INDEXO) = 0.0
 20               CONTINUE
 30            CONTINUE
 40         CONTINUE
C                                       last call - no vis
      ELSE
         END IF
C
 999  RETURN
      END
