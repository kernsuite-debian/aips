LOCAL INCLUDE 'UVGLU.INC'
C                                       Local include for UVGLU
      INCLUDE 'INCS:ZPBUFSZ.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAME2(3), XCLAS2(2), XNAMOU(3),
     *   XCLAOU(2), CATH1(256), CATH2(256)
      REAL      XSIN, XDISIN, XS2, XDISK2, XSOUT, XDISO, BUFF1(UVBFSS),
     *   BUFF2(UVBFSS), BUFFO(UVBFSS), CATR1(256), CATR2(256)
      INTEGER   SEQIN, SEQ2, SEQOUT, DISKI, DISK2, DISKO, NUMHIS,
     *   JBUFSZ, ILOCWT, CAT1(256), CAT2(256), INCSI, INCFI, INCIFI,
     *   INCSO, INCFO, INCIFO, LREC1, LREC2, LRECO, NRPRMI, NRPRMO,
     *   NSTOKS, NIF, NVIS1, NVIS2
      LOGICAL   ISCOMP
      CHARACTER NAMEIN*12, CLAIN*6, NAME2*12, CLAS2*6, NAMOUT*12,
     *   CLAOUT*6, HISCRD(10)*64
      DOUBLE PRECISION CATD1(128), CATD2(128)
      EQUIVALENCE (CAT1, CATH1, CATD1, CATR1),
     *   (CAT2, CATH2, CATD2, CATR2)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XNAME2, XCLAS2, XS2,
     *   XDISK2, XNAMOU, XCLAOU, XSOUT, XDISO,
     *   SEQIN, SEQ2, SEQOUT, DISKI, DISK2, DISKO, NUMHIS, ILOCWT,
     *   INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO, LREC1, LREC2,
     *   LRECO, NRPRMI, NRPRMO, ISCOMP, NSTOKS, NIF, NVIS1, NVIS2
      COMMON /CHARPM/ NAMEIN, CLAIN, NAME2, CLAS2, NAMOUT, CLAOUT,
     *   HISCRD
      COMMON /BUFRS/ CAT1, CAT2, BUFF1, BUFF2, BUFFO, JBUFSZ
LOCAL END
      PROGRAM UVGLU
C-----------------------------------------------------------------------
C!  UVGLU Glues spectral line blocks of UV-data back together
C# Utility UV UV-util VLA VLB SPECTRAL
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2000, 2006-2008, 2010-2011, 2015
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
C   UVGLU Glues spectral line blocks of UV-data back together works
C   only if the data are exactly the same except for frequency.
C   Can only glue when parts fit (in frequency)
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input VU data.
C      IN2NAME        NAME2         Name of second file to glue
C      IN2CLASS       CLAS2         Class of second file
C      IN2SEQ         SEQ2          Seq number of second file.
C      IN2DISK        DISK2         Vol. no. of second file.
C      OUTNAME        NAMOUT        Name of the output uv file.
C                                   Default output is input file.
C      OUTCLASS       CLAOUT        Class of the output uv file.
C      OUTSEQ         SEQOUT        Seq. number of output uv data.
C      OUTDISK        DISKO         Disk number of the output file.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET, NCH1, NCH2
      INCLUDE 'UVGLU.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
C
      DATA PRGM /'UVGLU '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL UVGLIN (PRGM, NCH1, NCH2, IRET)
      IF (IRET.NE.0) GO TO 990
      WRITE (MSGTXT,1000) NCH2, NCH1
      CALL MSGWRT (4)
C                                       Call routine that sends data
C                                       to the user routine.
      CALL GLUFRQ (NCH1, NCH2, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL FUGHIS
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('Glueing ',I3,' channels to ',I3,' channels. ')
      END
      SUBROUTINE UVGLIN (PRGN, NCH1, NCH2, JERR)
C-----------------------------------------------------------------------
C   UVGLIN gets input parameters for UVGLU and creates an output file
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      JERR    I    Error code: 0 => ok
C                                1 => infiles don't match
C                                5 => catalog troubles
C                                8 => can't start
C   Output in common:
C      LREC1   I  Input file record length
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
      INTEGER   NCH1, NCH2, JERR
      CHARACTER PRGN*6
C
      DOUBLE PRECISION FQ2GL1, FQ2GL2
      CHARACTER STAT*4, BLANK*6, PTYPE*2
      INTEGER   OLDCN1, OLDCN2, IROUND, NPARM, IERR, INCX, I, ILOCDE,
     *   ILOCRA, ILOCIF, ILOCF, ILOCS
      LOGICAL   GLUFIT, T, F
      INCLUDE 'UVGLU.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA BLANK  /'      '/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
      JBUFSZ = UVBFSS * 2
      NUMHIS = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
C                                       Fixed PPM 1996.09.30: was 31
      NPARM = 21
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
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      SEQ2 = IROUND (XS2)
      DISKI = IROUND (XDISIN)
      DISK2 = IROUND (XDISK2)
      DISKO = IROUND (XDISO)
C                                       Create new file.
C                                       Get 1 CATBLK from old file.
      OLDCN1 = 1
      PTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKI, OLDCN1, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKI,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKI, OLDCN1, CATBLK, 'REST', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Save input CATBLK, => output
      CALL COPY (256, CATBLK, CAT1)
C                                       Get 2 CATBLK from old file
      OLDCN2 = 1
      CALL CATDIR ('SRCH', DISK2, OLDCN2, NAME2, CLAS2, SEQ2,
     *   PTYPE, NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAME2, CLAS2, SEQ2, DISK2,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISK2, OLDCN2, CAT2, 'REST', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Check a few things, input files
C                                       should only differ in frequency
      GLUFIT = T
      IF (CATH1(KHDOB).NE.CATH2(KHDOB).OR.
C                                       Checked observing date
     *    CAT1(KIPCN).NE.CAT2(KIPCN).OR.
C                                       Checked coordinates
     *    CAT1(KIDIM).NE.CAT2(KIDIM) ) GLUFIT = F
C                                       Get no. vis. in each file
      NVIS1 = CAT1(KIGCN)
      NVIS2 = CAT2(KIGCN)
C                                       Check coordinate axes
      DO 100 I=1,CAT1(KIPCN)
         IF (CATH1(KHPTP+(I-1)*2).NE.CATH2(KHPTP+(I-1)*2)) GLUFIT = F
 100     CONTINUE
C                                       Check coordinates
      CALL AXEFND (8, 'DEC     ', CAT1(KIDIM), CATH1(KHCTP), ILOCDE,
     *      JERR)
      CALL AXEFND (8, 'RA      ', CAT1(KIDIM), CATH1(KHCTP), ILOCRA,
     *      JERR)
      IF (ABS(CATD1(KDCRV+ILOCDE)-CATD2(KDCRV+ILOCDE)).GT..01.OR.
     *    ABS(CATD1(KDCRV+ILOCRA)-CATD2(KDCRV+ILOCRA)).GT..01)
     *    GLUFIT = F
C                                        Not OK then stop
      IF (.NOT.GLUFIT) THEN
         JERR = 1
         MSGTXT = 'HEADERS OF INPUT FILES DO NOT MATCH AT ALL'
         GO TO 990
         END IF
C                                       Check frequency
      CALL AXEFND (8, 'FREQ    ', CAT1(KIDIM), CATH1(KHCTP), ILOCF,
     *      JERR)
C                                       Glu freq is ref value +
C                                       (#pix + 1 - #ref) * inc
      FQ2GL1 = CATD1(KDCRV+ILOCF) +
     *  (CAT1(KINAX+ILOCF)-CATR1(KRCRP+ILOCF)+1) * CATR1(KRCIC+ILOCF)
      FQ2GL2 = CATD2(KDCRV+ILOCF) +
     *  (1.0 - CATR2(KRCRP+ILOCF)) * CATR2(KRCIC+ILOCF)
      IF (CATR1(KRCIC+ILOCF).NE.CATR2(KRCIC+ILOCF).OR.
     *    (ABS((FQ2GL1-FQ2GL2)/CATR2(KRCIC+ILOCF)).GT.0.1)) THEN
         JERR = 1
         MSGTXT = 'FREQUENCY BLOCKS DO NOT MATCH'
         GO TO 990
         END IF
C                                       Like to know NSTOKS & NIF
      CALL AXEFND (8, 'STOKES  ', CAT1(KIDIM), CATH1(KHCTP), ILOCS,
     *      JERR)
      IF (JERR.NE.0) THEN
         NSTOKS = 0
      ELSE
         NSTOKS = CATBLK(KINAX+ILOCS)
         END IF
      CALL AXEFND (8, 'IF      ', CAT1(KIDIM), CATH1(KHCTP), ILOCIF,
     *      JERR)
      IF (JERR.NE.0) THEN
         NIF = 0
      ELSE
         NIF = CATBLK(KINAX+ILOCIF)
         END IF
C                                       Compressed data?
      ISCOMP = CATBLK(KINAX).EQ.1
      IF (ISCOMP) THEN
C                                       Find weight and scale.
         CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), ILOCWT,
     *      JERR)
         IF (JERR.NE.0) THEN
            MSGTXT = 'ERROR FINDING WEIGHT FOR COMPRESSED DATA'
            JERR = 9
            GO TO 990
            END IF
         END IF
C                                       How many frequencies are there

      NCH1 = CAT1(KINAX + ILOCF)
      NCH2 = CAT2(KINAX + ILOCF)
      NUMHIS = NUMHIS + 1
      WRITE(HISCRD(NUMHIS),2000) NCH1, (NCH1+NCH2)
      NUMHIS = NUMHIS + 1
      WRITE(HISCRD(NUMHIS),2010) NAME2, CLAS2, SEQ2
C                                       Calculate # output channels
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Save input file info
      INCX = CATBLK(KINAX)
      LREC1 = LREC
      NRPRMI = NRPARM
      INCSI = INCS / INCX
      INCFI = INCF / INCX
      INCIFI = INCIF / INCX
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, BLANK, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KINAX + ILOCF) = NCH1 + NCH2
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
         IF ((CCNO.NE.OLDCN1) .OR. (DISKO.NE.DISKI)) THEN
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
      CALL CATDIR ('CSTA', DISKI, OLDCN1, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, 'READ', BUFF1, IERR)
      CALL CATDIR ('CSTA', DISK2, OLDCN2, NAME2, CLAS2, SEQ2,
     *   PTYPE, NLUSER, 'READ', BUFF1, IERR)

      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKI
      FCNO(NCFILE) = OLDCN1
      FRW(NCFILE) = 0
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISK2
      FCNO(NCFILE) = OLDCN2
      FRW(NCFILE) = 0
      JERR = 0
      SEQOUT = CATBLK(KIIMS)
C                                       copy header keywords
      CALL KEYCOP (DISKI, OLDCN1, DISKO, CCNO, IERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVGLIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1060 FORMAT ('MAY OVERWRITE INPUT FILE ONLY.  QUITTING')
 1065 FORMAT ('UVGLIN: ERROR',I3,' UPDATING NEW CATBLK')
 2000 FORMAT ('Appending ',I3,' channels to get ',
     *        I3,' frequency channels')
 2010 FORMAT ('from ',A12,'.',A8,';',I3,
     *        ' after a check of the headers')
      END
      SUBROUTINE GLUFRQ (NCH1, NCH2, IRET)
C-----------------------------------------------------------------------
C   GLUFRQ sends uv data one point at a time to the user supplied
C   routine and then writes the modified data if requested.
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
      INTEGER   IRET, NCH1, NCH2
C
      CHARACTER OFILE*48, IFIL1*48, IFIL2*48
      INTEGER   INIO, IPTR1, IPTR2, IPTRO, LUN1, LUN2, LUNO, NAXIS, BO,
     *   IND1, IND2, INDO, ILENBU, KBIND, NIOUT, NIOLIM, IBIND, I,
     *   INCX, VO, NUMVIS, XCOUNT, NCORI, NCORI2, NCORO, NCOPY, RNXRET,
     *   NREC1, NREC2, NVISO
      LOGICAL   T, F, WEOF1, WEOF2
      INCLUDE 'UVGLU.INC'
      REAL BASEN, BASE2, TIME1, TIME2, CBUFF1(UVBFSS), CBUFF2(UVBFSS),
     *   RESULT(UVBFSS)
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
C                                       open next input file?
C                                       Dimension of complex axis
      INCX = CATBLK(KINAX)
      IF (ISCOMP) INCX = 3
C                                       Number of visibilities in input
C                                       and output files.
      NCORI = (LREC1 - NRPRMI) / CAT1(KINAX)
      NCORO = (LRECO - NRPRMO) / CATBLK(KINAX)
      NCOPY = LRECO - NRPRMO
C     NCOPY = LRECI - NRPRMI
C                                       Open and init for read
C                                       visibility file
      CALL ZPHFIL ('UV', DISKI, FCNO(NCFILE-1), 1, IFIL1, IRET)
      CALL ZOPEN (LUN1, IND1, DISKI, IFIL1, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                        And second file
      CALL ZPHFIL ('UV', DISK2, FCNO(NCFILE), 1, IFIL2, IRET)
      CALL ZOPEN (LUN2, IND2, DISK2, IFIL2, T, F, F, IRET)
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
      LREC2 = 1
      NAXIS = CAT2(KIDIM)
      DO 10 I = 1,NAXIS
         LREC2 =  LREC2 * MAX (1, CAT2(KINAX+I-1))
 10      CONTINUE
      LREC2 = LREC2 + NRPARM
      NCORI2= (LREC2 - NRPRMI) / CAT1(KINAX)
C                                       make an index table
      CALL RNXGET (DISKI, FCNO(2), CAT1)
      CALL RNXINI (DISKO, CCNO, CATBLK, RNXRET)
C                                       Init vis file for write
      ILENBU = 0
      NVISO = MAX (NVIS1, NVIS2)
      CALL UVINIT ('WRIT', LUNO, INDO, NVISO, VO, LRECO, ILENBU,
     *   JBUFSZ, BUFFO, BO, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
      IPTRO = KBIND
      NIOUT = 0
      NIOLIM = ILENBU
C                                       Init vis file for read.
      ILENBU = 0
      CALL UVINIT ('READ', LUN1, IND1, NVIS1, VO, LREC1, ILENBU,
     *   JBUFSZ, BUFF1, BO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET
         GO TO 990
         END IF
      CALL UVINIT ('READ', LUN2, IND2, NVIS2, VO, LREC2, ILENBU,
     *   JBUFSZ, BUFF2, BO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET
         GO TO 990
         END IF
      NUMVIS = 0
      XCOUNT = 0
C                                       Initialisation
      NREC1 = 0
      NREC2 = 0
C                                       Loop
 100  CONTINUE
C                                       Read vis. rec. from file 1
         IF (NREC1.EQ.0) THEN
            CALL UVDISK ('READ', LUN1, IND1, BUFF1, INIO, IBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1100) IRET
               GO TO 990
               END IF
            WEOF1 = (INIO.LE.0)
            NREC1 = INIO
            IPTR1 = IBIND
            END IF
C                                       Read vis. rec. from file 2
         IF (NREC2.EQ.0) THEN
            CALL UVDISK ('READ', LUN2, IND2, BUFF2, INIO, IBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1100) IRET
               GO TO 990
               END IF
            WEOF2 = (INIO.LE.0)
            NREC2 = INIO
            IPTR2 = IBIND
            END IF
C                                       Out of data on either file ?
         IF (WEOF1.OR.WEOF2) GO TO 200
C                                       Extract time and baseline
C                                       random parameters.
         TIME1 = BUFF1(IPTR1+ILOCT)
         TIME2 = BUFF2(IPTR2+ILOCT)
         IF (ILOCB.GE.0) THEN
            BASEN = BUFF1(IPTR1+ILOCB)
            BASE2 = BUFF2(IPTR2+ILOCB)
         ELSE
            BASEN = 4096.*BUFF1(IPTR1+ILOCA1) + BUFF1(IPTR1+ILOCA2)
            BASE2 = 4096.*BUFF2(IPTR2+ILOCA1) + BUFF2(IPTR2+ILOCA2)
            END IF
C                                       Process further only if the
C                                       time and baseline stamps agree.
         IF ((TIME1.EQ.TIME2).AND.(BASEN.EQ.BASE2)) THEN
C
            NUMVIS = NUMVIS + 1
C                                      Copy data
            IF (ISCOMP) THEN
C                                      Compressed data.
               CALL ZUVXPN (NCORI, BUFF1(IPTR1+NRPRMI),
     *            BUFF1(IPTR1+ILOCWT), CBUFF1)
               CALL ZUVXPN (NCORI2, BUFF2(IPTR2+NRPRMI),
     *            BUFF2(IPTR2+ILOCWT), CBUFF2)
               CALL COPFRQ (NCH1, NCH2, CBUFF1, CBUFF2, INCX,
     *                      RESULT, IRET)
            ELSE
C                                       Un compressed data
               CALL COPFRQ ( NCH1, NCH2, BUFF1(IPTR1+NRPRMI),
     *            BUFF2(IPTR2+NRPRMI), INCX, RESULT, IRET)
               END IF
C                                       Check return code.
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1120) IRET
               GO TO 990
               END IF
C                                       Copy to output.
            XCOUNT = XCOUNT + 1.0D0
C                                       Random parameters from #1
C                                       THIS IS CRUCIAL, U,V,W, are
C                                       correct for frequency in
C                                       first header same in output
            CALL RCOPY (NRPRMO, BUFF1(IPTR1), BUFFO(IPTRO))
C                                       update NX table
            CALL RNXUPD (BUFFO(IPTRO), RNXRET)
C                                       move data
            IF (ISCOMP) THEN
C                                       Compressed
               CALL ZUVPAK (NCORO, RESULT, BUFFO(IPTRO+ILOCWT),
     *            BUFFO(IPTRO+NRPRMO))
            ELSE
               CALL RCOPY (NCOPY, RESULT, BUFFO(IPTRO+NRPRMO))
               END IF
            IPTRO = IPTRO + LRECO
            NIOUT = NIOUT + 1
C                                       Write vis. rec. if output
C                                       buffer is full.
            IF (NIOUT.EQ.NIOLIM) THEN
               CALL UVDISK ('WRIT', LUNO, INDO, BUFFO, NIOLIM,
     *            KBIND, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1150) IRET
                  GO TO 990
                  END IF
               IPTRO = KBIND
               NIOUT = 0
               END IF
C
            END IF
C                                       Increment record pointers.
         IF (TIME1.LT.TIME2) THEN
            IPTR1 = IPTR1 + LREC1
            NREC1 = NREC1 - 1
            END IF
C
         IF (TIME2.LT.TIME1) THEN
            IPTR2 = IPTR2 + LREC2
            NREC2 = NREC2 - 1
            END IF
C
         IF (TIME1.EQ.TIME2) THEN
            IF (BASEN.LE.BASE2) THEN
               IPTR1 = IPTR1 + LREC1
               NREC1 = NREC1 - 1
               END IF
            IF (BASE2.LE.BASEN) THEN
               IPTR2 = IPTR2 + LREC2
               NREC2 = NREC2 - 1
               END IF
            END IF
C                                       Loop back for next record.
         GO TO 100
C                                       Finish write
 200  NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNO, INDO, BUFFO, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1150) IRET
         GO TO 990
         END IF
C                                       Compress output file.
      NVIS = XCOUNT
      CALL UCMPRS (NVIS, DISKO, CCNO, LUNO, CATBLK, IRET)
C                                       Close files
      CALL ZCLOSE (LUN1, IND1, IRET)
      CALL ZCLOSE (LUN2, IND2, IRET)
      CALL ZCLOSE (LUNO, INDO, IRET)
C                                       close NX table
      CALL RNXCLS (RNXRET)
      IF (RNXRET.NE.0) THEN
         MSGTXT = 'OUTPUT NX TABLE, IF ANY, IS INCOMPLETE'
         CALL MSGWRT (6)
         END IF
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GLUFRQ: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1010 FORMAT ('GLUFRQ: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('GLUFRQ: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1030 FORMAT ('GLUFRQ: ERROR',I3,' INIT-FOR-READ VIS FILE')
 1100 FORMAT ('GLUFRQ: ERROR',I3,' READING VIS FILE')
 1120 FORMAT ('GLUFRQ: DIDDLE ERROR',I3)
 1150 FORMAT ('GLUFRQ: ERROR',I3,' WRITING VIS FILE')
      END
      SUBROUTINE FUGHIS
C-----------------------------------------------------------------------
C   FUGHIS copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      CHARACTER NOTTYP*2, HILINE*72, LABEL*8
      INTEGER   LUN1, LUNO, IERR, I, NONOT
      LOGICAL   T
      INCLUDE 'UVGLU.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN1, LUNO /27,28/
      DATA T /.TRUE./
      DATA NONOT, NOTTYP /1, 'NX'/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUNO, DISKI, DISKO, FCNO(NCFILE-1),
     *   FCNO(NCFILE-2), CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 200
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKI, LUNO, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUNO, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
C                                      Add any other history.
      IF (NUMHIS.LE.0) GO TO 200
         WRITE (LABEL,1010) TSKNAM
         DO 50 I = 1,NUMHIS
            HILINE = LABEL // HISCRD(I)
            CALL HIADD (LUNO, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 200
 50         CONTINUE
C                                       Close HI file
 200   CALL HICLOS (LUNO, T, BUFF2, IERR)
C                                        Copy tables
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUNO, DISKI, DISKO,
     *   FCNO(2), FCNO(1), CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1200)
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, FCNO(NCFILE-2), CATBLK, 'REST',
     *   BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FUGHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,' /')
 1200 FORMAT ('FUGHIS: ERROR COPYING TABLES')
      END
      SUBROUTINE COPFRQ (NCH1, NCH2, VIS1, VIS2, INCX, RESULT, IRET)
C-----------------------------------------------------------------------
C  Routine that actually glues the channels together.
C-----------------------------------------------------------------------
      INTEGER   NCH1, NCH2, INCX, IRET, NI, NS, J, I, J1
      REAL      VIS1(INCX,*), VIS2(INCX,*), RESULT(INCX,*)
C
      INCLUDE 'UVGLU.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      IRET = 0
      NS = MAX(1,NSTOKS)
      NI = MAX(1,NIF)
C                                     Just glue...
      J1 = NCH1 * NI * NS
      DO 100 J=1,J1
         DO 200 I=1,3
            RESULT(I,J) = VIS1(I,J)
 200        CONTINUE
 100     CONTINUE
      J1 = NCH2 * NI * NS
      DO 102 J=1,J1
         DO 202 I=1,3
            RESULT(I,NCH1*NI*NS+J) = VIS2(I,J)
 202        CONTINUE
 102     CONTINUE
C
 999  RETURN
      END
